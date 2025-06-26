# Load libraries
library(forecast)
library(xgboost)
library(randomForest)
library(caret)
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(zoo)
library(R6)
library(tidyr)
library(purrr)
library(raster)
library(glue)

source("lib/con_pg.R")

# Fetch discounts data
parts <- paste(sapply(names(filters), function(nm) sprintf("%s in ({%s*})", nm, nm)), collapse = " OR ")

query <- glue_data_sql(filters, paste("SELECT * FROM ceran.discounts where", 
                                      parts,"AND date >= '2024-01-01'"), .con = con)
discounts <- dbGetQuery(con, query)

# discounts <- dbGetQuery(con, "SELECT * FROM ceran.discounts where 
#                         --country = 'Costa Rica' 
#                         --OR 
#                         client IN('Mi Farma')--,'Farmatodo', 'Inkafarma','Mi Farma')
#                         ")


query <- glue_data_sql(filters, paste("SELECT DISTINCT
          date, year, month, client, 
          country, ean, sku, real_units, real_sales
          FROM ceran.sell_out WHERE", parts, "AND date >= '2024-01-01'"), .con = con)

# Fetch sell out data
sell_out <- dbGetQuery(con, query)
# sell_out <- dbGetQuery(con, "
#                         SELECT DISTINCT
#                         date, year, month, client, country, ean, sku, real_units, real_sales
#                         FROM ceran.sell_out
#                          where 
#                         --country = 'Costa Rica' 
#                         --OR 
#                         client IN('Mi Farma')--,'Farmatodo', 'Inkafarma','Mi Farma')
#                        ")
table(sell_out$client)

discounts2 <- discounts %>% 
  dplyr::select(date,client, country, ean, promo_type, promo_name, discount_pct) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         ean = as.character(ean)) %>% 
  unique()

sell_out <- sell_out %>% 
  filter(real_units > 0, country != 'Panama') %>% 
  dplyr::select(date, client, country, ean, sku, real_units, real_sales
                #, real_sales
  ) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         price = real_sales/real_units)

table(sell_out$client)

market <- sell_out %>% 
  left_join(., discounts2, by = c("date","client","country","ean")) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  group_by(date, client, country, ean, discount_pct) %>% 
  mutate(counts = row_number()) %>% 
  filter(counts == 1) %>% 
  group_by(date,client,country,ean,sku,promo_type,promo_name) %>% 
  summarise(real_units = mean(real_units),
            real_sales = mean(real_sales),
            price = mean(price),
            discount_pct = max(discount_pct)) %>% 
  dplyr::select(date,client,country,ean,sku,real_units,real_sales, price,promo_type, promo_name, discount_pct)

# Prepare market data
names(market) <- c("Date","Client","Country","EAN","SKU","Units","Sales", "price","promo_type", "promo_name", "Discount")

#rasterOptions(todisk = F)
market_df <- market %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         EAN = as.character(EAN)) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))


table(market_df$Client, market_df$Country)

# Extract variables for prediction
vars_to_predict <- c("price","DayOfMonth","DayOfWeek","WeekOfMonth","IsPaymentDay",
                     "d2","d3")

gc()

# Define the BaselineModel class
BaselineModel <- R6::R6Class(
  "BaselineModel",
  public = list(
    data = NULL,
    lr_model = NULL,
    tree_model = NULL,
    xgb_model = NULL,
    rf_model = NULL,  # Add Random Forest model attribute
    
    initialize = function(data) {
      self$data <- data
    },
    
    # Data profiling function
    data_profiling = function() {
      self$data <- self$data %>% rename(Client = Client)
      #self$data$Date <- ymd(self$data$Date)
      latest_date <- max(self$data$Date)
      three_months_ago <- latest_date #- months(6)
      
      self$data <- self$data %>%
        group_by(Client, EAN, Date) %>% #floor_date(Date, "month")) %>%
        mutate(MonthTopPrice = max(1)) %>% #max(Price)) %>%
        ungroup()
      
      #self$data$Discount <- 0.0
      #self$data$Discount[self$data$MonthTopPrice > 0] <- ((self$data$MonthTopPrice[self$data$MonthTopPrice > 0] - self$data$Price[self$data$MonthTopPrice[self$data$MonthTopPrice > 0]]) / self$data$MonthTopPrice[self$data$MonthTopPrice > 0]) * 100
      
      window_size <- 7
      self$data <- self$data %>%
        group_by(Country, Client, EAN, Date) %>%
        mutate(RollingMeanDiscount = zoo::rollapply(Discount, width = window_size, FUN = mean, partial = TRUE, align = "center")) %>%
        ungroup()
      
      
      # promotion_threshold <- 100
      # self$promo <- self$data
      # self$data$Promo <- self$data$RollingMeanDiscount >= promotion_threshold
      # self$promo$Promo <- self$data$RollingMeanDiscount <= promotion_threshold
      # 
      # self$data <- self$data %>% filter(!Promo)
    },
    
    # Function to remove outliers
    remove_outliers = function(column, threshold = 1.5) {
      Q1 <- quantile(self$data[[column]], 0.25)
      Q3 <- quantile(self$data[[column]], 0.75)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - threshold * IQR
      upper_bound <- Q3 + threshold * IQR
      self$data <- self$data #%>% filter(!!sym(column) >= lower_bound, !!sym(column) <= upper_bound)
      
    },
    
    # Function to calculate baseline using linear regression
    calculate_baseline = function(group,target_col = "Units") {
      # Calculate the median as a fallback baseline
      
      # Filter rows for training (Discount == 0) and testing (Discount != 0)
      train_data <- group
      
      # Prepare training data
      X_train <- train_data[, vars_to_predict, drop = FALSE]
      y_train <- train_data[[target_col]]
      
      # Determine the number of unique values in y_train
      num_unique_y <- length(unique(y_train))
      
      
      # Dynamically adjust the number of folds
      n_folds <- min(3, num_unique_y) # Or even min(5, num_unique_y) for smaller datasets
      if (n_folds < 3) { # Don't go below 3 as it's poor practice
        warning("Too few unique values in the target variable for reliable cross-validation.  Consider other evaluation methods or increasing your data volume.")
        n_folds <- num_unique_y # fallback to leave-one-out
      }
      
      # Train the linear regression model with adjusted folds
      self$lr_model <- train(
        x = X_train,
        y = y_train,
        method = "lm",
        trControl = trainControl(method = "cv", number = n_folds)  # Dynamic number of folds
      )
      # Save models
      model_id <- paste(unique(group$Country), unique(group$Client), unique(group$EAN), sep = "_")
      saveRDS(self$lr_model, file = file.path("data/models", paste0("LR_", model_id, ".rds")))
      
      
    },
    test_baseline = function(test){
      test <- test[,vars_to_predict]
      
      baseline <- predict(self$lr_model, newdata = test)
      r2 <- summary(self$lr_model)$r.squared
      
      return(list(baseline, r2))
      
    },
    
    # Function to calculate baseline using decision tree
    calculate_baseline_tree = function(group, target_col = "Units") {
      train_data <- group
      # Prepare training data
      X_train <- train_data[, vars_to_predict, drop = FALSE]
      y_train <- train_data[[target_col]]
      
      # Define a grid of hyperparameters to tune
      tune_grid <- expand.grid(
        cp = seq(0.001, 0.1, by = 0.01)  # Complexity parameter for pruning
      )
      
      # Determine the number of unique values in y_train
      num_unique_y <- length(unique(y_train))
      
      # Dynamically adjust the number of folds
      n_folds <- min(3, num_unique_y) # Or even min(5, num_unique_y) for smaller datasets
      if (n_folds < 3) { # Don't go below 3 as it's poor practice
        warning("Too few unique values in the target variable for reliable cross-validation.  Consider other evaluation methods or increasing your data volume.")
        n_folds <- num_unique_y # fallback to leave-one-out
      }
      
      # Train the decision tree model with tuning
      self$tree_model <- train(
        x = X_train,
        y = y_train,
        method = "rpart",
        tuneGrid = tune_grid,  # Use the defined grid
        trControl = trainControl(method = "cv", number = n_folds)  # 5-fold cross-validation
      )
      
      model_id <- paste(unique(group$Country), unique(group$Client), unique(group$EAN), sep = "_")
      saveRDS(self$tree_model, file = file.path("data/models", paste0("DT_", model_id, ".rds")))
      
      
    },
    test_baseline_tree = function(test){
      test <- test[,vars_to_predict]
      
      baseline <- predict(self$tree_model, newdata = test)
      r2 <- 0.5#summary(self$tree_model)$r.squared
      return(list(baseline, r2))
      
    },
    
    # Function to calculate baseline using XGBoost
    calculate_baseline_xgb = function(group, target_col = "Units") {
      
      train_data <- group
      
      # Prepare training data
      X_train <- train_data[, vars_to_predict, drop = FALSE]
      y_train <- train_data[[target_col]]
      
      # Determine the number of unique values in y_train
      num_unique_y <- length(unique(y_train))
      
      # Dynamically adjust the number of folds
      n_folds <- min(3, num_unique_y) # Or even min(5, num_unique_y) for smaller datasets
      if (n_folds < 3) { # Don't go below 3 as it's poor practice
        warning("Too few unique values in the target variable for reliable cross-validation.  Consider other evaluation methods or increasing your data volume.")
        n_folds <- num_unique_y # fallback to leave-one-out
      }
      
      # Set XGBoost parameters
      param_grid <- expand.grid(
        nrounds = c(80),
        eta = c(0.2),
        max_depth = c(2),
        gamma = c(0.35),
        colsample_bytree = c(0.33),
        min_child_weight = c(2),
        subsample = c(0.25)
      )
      
      self$xgb_model <- train(
        x = X_train,
        y = y_train,
        method = "xgbTree",
        tuneGrid = param_grid,
        trControl = trainControl(method = "cv", number = n_folds)#, #repeatedcv
        #nthread = parallel::detectCores() - 1
      )
      
      model_id <- paste(unique(group$Country), unique(group$Client), unique(group$EAN), sep = "_")
      saveRDS(self$xgb_model,  file = file.path("data/models", paste0("XGB_", model_id, ".rds")))
      
    },
    test_baseline_xgb = function(test){
      
      test <- test[,vars_to_predict]
      baseline <- predict(self$xgb_model, newdata = test)[1]
      r2 <- 0.5#summary(self$tree_model)$r.squared
      return(list(baseline, r2))
      
    },
    
    # Function to calculate baseline using Random Forest
    calculate_baseline_rf = function(group, target_col = "Units") {
      
      train_data <- group
      
      # Prepare training data
      X_train <- train_data[, vars_to_predict, drop = FALSE]
      y_train <- train_data[[target_col]]
      
      # Determine the number of unique values in y_train
      num_unique_y <- length(unique(y_train))
      
      # Dynamically adjust the number of folds
      n_folds <- min(3, num_unique_y) # Or even min(5, num_unique_y) for smaller datasets
      if (n_folds < 3) { # Don't go below 3 as it's poor practice
        warning("Too few unique values in the target variable for reliable cross-validation.  Consider other evaluation methods or increasing your data volume.")
        n_folds <- num_unique_y # fallback to leave-one-out
      }
      
      
      # Train the Random Forest model
      self$rf_model <- train(
        x = X_train,
        y = y_train,
        method = "rf",
        #tuneLength = 10,
        trControl = trainControl(method = "cv", number = n_folds)
      )
      
      model_id <- paste(unique(group$Country), unique(group$Client), unique(group$EAN), sep = "_")
      saveRDS(self$rf_model,   file = file.path("data/models", paste0("RF_", model_id, ".rds")))
      
    },
    test_baseline_rf = function(test){
      
      test <- test[,vars_to_predict]
      baseline <- predict(self$rf_model, newdata = test)[1]
      r2 <- 0.5#summary(self$tree_model)$r.squared
      return(list(baseline, r2))
      
    },
    
    # Function to run baseline calculations
    run_baseline = function() {
      
      model_save_path = "models"
      dir.create(model_save_path, showWarnings = FALSE)
      
      #self$data$Date <- ymd(self$data$Date)
      self$data$WeekOfMonth <- ((day(self$data$Date) - 1) %/% 7) + 1
      self$data$DayOfWeek <- wday(self$data$Date)
      self$data$DayOfMonth <- day(self$data$Date)
      
      # Determine if the date is a payment day
      self$data$IsPaymentDay <- self$data$DayOfMonth %in% c(14, 15, 16, 28, 30, 31)
      
      # Handle February (only allow 28 as a payment day in February)
      self$data$IsPaymentDay <- ifelse(
        month(self$data$Date) == 2 & self$data$DayOfMonth == 28,
        TRUE,
        self$data$IsPaymentDay
      )
      
      filtered_data <- self$data
      
      all_vars <- names(filtered_data) %>% 
        as.data.frame() %>% 
        unlist()
      
      all_vars <- all_vars %>% 
        as.data.frame()
      names(all_vars) <- "vars"
      
      filtered_vars <- all_vars %>%
        filter(str_starts(vars, "PROMO_NAME")) %>% 
        unlist()
      
      attributes(filtered_vars) <- NULL
      
      vars_fill <- c("Client", filtered_vars, "Discount", "price",
                     "DayOfMonth","DayOfWeek","WeekOfMonth","IsPaymentDay") %>% unlist()
      
      filtered_data_merged <- filtered_data %>% 
        dplyr::select(vars_fill,"EAN","Date","Country")
      
      
      min_date <- min(filtered_data$Date)
      max_date <- max(filtered_data$Date)
      date_range <- seq(min_date, max_date, by = "day")
      date_df <- data.frame(Date = date_range)
      
      merged_data <- left_join(date_df, filtered_data, by = "Date")
      
      merged_data <- merged_data %>%
        group_by(Country, Client, EAN, Date) %>%
        summarize(Units = sum(Units, na.rm = TRUE)) %>%
        inner_join(., filtered_data_merged, by = c("Country", "Client", "EAN", "Date")) %>% 
        mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
        mutate_if(is.character, ~replace(., is.na(.), ""))
      
      
      merged_data <- merged_data %>% 
        mutate(Month = floor_date(Date, "month")) %>%  # Extract the month from the Date
        group_by(Country, Client, EAN, Month) %>%
        mutate(
          Mean_Units = mean(Units, na.rm = TRUE),      # Calculate the mean of Units
          SD_Units = sd(Units, na.rm = TRUE),          # Calculate the standard deviation of Units
          Above_SD = ifelse(Units > Mean_Units + SD_Units, TRUE, FALSE),  # Check if Units > Mean + SD
          Below_SD = ifelse(Units < Mean_Units - SD_Units, TRUE, FALSE)   # Check if Units < Mean - SD
        ) %>% 
        dplyr::select(-c(Month, Mean_Units, SD_Units))
      
      group <- merged_data %>% 
        group_by(Country, Client, EAN, Date)
      
      
      # Create a complete date sequence
      date_range <- seq(min(group$Date), max(group$Date), by = "day")
      
      
      # Generate all combinations of Client, EAN, and Date
      complete_data <- expand.grid(
        Country = unique(group$Country),
        Date = date_range,
        Client = unique(group$Client),
        EAN = unique(group$EAN)
      )
      complete_data$Date = as.Date(complete_data$Date)
      
      # Merge with original data and fill missing Units with 0
      group <- complete_data %>%
        left_join(group, by = c("Country", "Client", "EAN", "Date")) %>% 
        mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
        mutate_if(is.character, ~replace(., is.na(.), ""))
      
      group$WeekOfMonth <- ((day(group$Date) - 1) %/% 7) + 1
      group$DayOfWeek <- wday(group$Date)
      group$DayOfMonth <- day(group$Date)
      
      # Determine if the date is a payment day
      group$IsPaymentDay <- group$DayOfMonth %in% c(14, 15, 16, 28, 29, 30, 31, 01)
      
      # Handle February (only allow 28 as a payment day in February)
      group$IsPaymentDay <- ifelse(
        month(group$Date) == 2 & group$DayOfMonth %in% c(14, 15, 16, 27, 28, 01),
        TRUE,
        group$IsPaymentDay
      )
      
      group_test <- group
      
      set.seed(123)  # For reproducibility
      train_index <- createDataPartition(combo_data$Units, p = 0.68, list = FALSE)
      combo_data <- combo_data[train_index, ]
      
      group <- group %>%
        filter(Units > 0) %>%
        #filter(Discount == 0) %>% 
        mutate(d2 = as.numeric(paste0(year(floor_date(Date, "month")), month(floor_date(Date, "month")))),
               d3 = as.numeric(year(floor_date(Date, "month"))),
               Units = log(Units)) %>%
        #dplyr::select(-Month) %>% 
        group_by(Country, Client)
      
      # Model object creator
      
      results_lr <- group %>% do(data.frame(as.list(self$calculate_baseline(.))))
      results_tree <- group %>% do(data.frame(as.list(self$calculate_baseline_tree(.))))
      results_xgb <- group %>% do(data.frame(as.list(self$calculate_baseline_xgb(.))))
      results_rf <- group %>% do(data.frame(as.list(self$calculate_baseline_rf(.))))
      
      
      # Prepare testing data
      group_test <- group_test %>%
        filter(Units > 0) %>%
        mutate(d2 = as.numeric(paste0(year(floor_date(Date, "month")), month(floor_date(Date, "month")))),
               d3 = as.numeric(year(floor_date(Date, "month"))),
               Units = log(Units)) %>%
        dplyr::select(-Month) %>% 
        group_by(Country, Client, EAN, Date)
      
      
      results_lr <- group_test %>% do(data.frame(t(unlist(self$test_baseline(.))))) 
      results_tree <- group_test %>% do(data.frame(t(unlist(self$test_baseline_tree(.))))) 
      results_xgb <- group_test %>% do(data.frame(t(unlist(self$test_baseline_xgb(.))))) 
      results_rf <- group_test %>% do(data.frame(t(unlist(self$test_baseline_rf(.)))))  # Add Random Forest results
      
      results_lr <- as.data.frame(results_lr)
      results_lr <- results_lr[,1:6]
      
      results_tree <- as.data.frame(results_tree)
      results_tree <- results_tree[,1:6]
      
      results_xgb <- as.data.frame(results_xgb)
      results_xgb <- results_xgb[,1:6]
      
      results_rf <- as.data.frame(results_rf)
      results_rf <- results_rf[,1:6]
      
      
      names(results_lr) <- c("Country", "Client", "EAN", "Date", "Baseline_LR", "r2_LR")
      names(results_tree) <- c("Country", "Client", "EAN", "Date", "Baseline_DT", "r2_DT")
      names(results_xgb) <- c("Country", "Client", "EAN", "Date", "Baseline_XGB", "r2_XGB")
      names(results_rf) <- c("Country", "Client", "EAN", "Date", "Baseline_RF", "r2_RF")  # Add Random Forest column names
      
      
      compare_models <- left_join(results_lr, results_tree, by = c("Country", "Client", "EAN", "Date"))
      compare_models <- left_join(compare_models, results_xgb, by = c("Country", "Client", "EAN", "Date"))
      compare_models <- left_join(compare_models, results_rf, by = c("Country", "Client", "EAN", "Date"))
      
      compare_models$Baseline_LR <- exp(compare_models$Baseline_LR)
      compare_models$Baseline_DT <- exp(compare_models$Baseline_DT)
      compare_models$Baseline_XGB <- exp(compare_models$Baseline_XGB)
      compare_models$Baseline_RF <- exp(compare_models$Baseline_RF)
      
      compare_models <- left_join(compare_models, merged_data, by = c("Country", "Client", "EAN", "Date"))
      compare_models <- compare_models %>% 
        filter(is.na(Units) == F)
      
      return(list(compare_models, merged_data))
    },
    
    # Function to extract feature importance from models
    get_feature_importance = function(model, model_type) {
      if (model_type == "LR") {
        # Extract coefficients for Linear Regression
        importance <- summary(model$finalModel)$coefficients
        importance <- as.data.frame(importance)
        importance$Variable <- rownames(importance)
        importance <- importance %>% dplyr::select(Variable, Estimate = Estimate)
        print("LR IMP")
        print(importance)
      } else if (model_type == "DT") {
        # Extract variable importance for Decision Tree
        importance <- varImp(model, scale = FALSE)$importance
        importance <- as.data.frame(importance)
        importance$Variable <- rownames(importance)
        importance <- importance %>% dplyr::select(Variable, Importance = Overall)
        print("DT IMP")
        print(importance)
      } else if (model_type == "XGB") {
        # Extract variable importance for XGBoost
        importance <- xgb.importance(model = model$finalModel)
        importance <- as.data.frame(importance)
        importance <- importance %>% dplyr::select(Variable = Feature, Importance = Gain)
        print("XGB IMP")
        print(importance)
      } else if (model_type == "RF") {
        # Extract variable importance for Random Forest
        importance <- varImp(model, scale = FALSE)$importance
        importance <- as.data.frame(importance)
        importance$Variable <- rownames(importance)
        importance <- importance %>% dplyr::select(Variable, Importance = Overall)
        print("RF IMP")
        print(importance)
      } else {
        stop("Unsupported model type")
      }
      
      # Add model type to the importance data
      importance$Model <- model_type
      return(importance)
    },
    # Function to get feature importance for all models
    get_all_feature_importance = function() {
      importance_lr <- self$get_feature_importance(self$lr_model, "LR")
      importance_dt <- self$get_feature_importance(self$tree_model, "DT")
      importance_xgb <- self$get_feature_importance(self$xgb_model, "XGB")
      importance_rf <- self$get_feature_importance(self$rf_model, "RF")  # Add RF feature importance
      
      # Combine all feature importance data
      feature_importance <- bind_rows(importance_lr, importance_dt, importance_xgb, importance_rf)
      return(feature_importance)
    }
    
  )
)


#######################################################################


get_best_baseline = function(group) {
  # Helper to calculate R²
  calculate_r2 <- function(actual, predicted) {
    rss <- sum((actual - predicted)^2, na.rm = TRUE)
    tss <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
    r2 <- 1 - (rss / tss)
    return(r2)
  }
  
  
  # Calculate R² for each baseline
  group$Units <- group$Units * (1-group$Discount)
  r2_LR   <- calculate_r2(group$Units, group$Baseline_LR)
  r2_DT   <- calculate_r2(group$Units, group$Baseline_DT)
  r2_XGB  <- calculate_r2(group$Units, group$Baseline_XGB)
  r2_RF   <- calculate_r2(group$Units, group$Baseline_RF)
  
  r2_vec <- c(LR = r2_LR, DT = r2_DT, XGB = r2_XGB, RF = r2_RF)
  best_model <- names(r2_vec)[which.max(r2_vec)]
  value_col <- paste0("Baseline_", best_model)
  
  # Return the vector of best baseline predictions and R² values
  return(list(
    best_baseline = group[[value_col]],
    best_model = best_model,
    r2_LR = r2_LR,
    r2_DT = r2_DT,
    r2_XGB = r2_XGB,
    r2_RF = r2_RF
  ))
}

# Get unique combinations of Country, Client, and EAN
unique_combinations <- market_df %>%
  ungroup() %>% 
  dplyr::select(Country, Client, EAN) %>% 
  unique()

table(unique_combinations$Client, unique_combinations$Country)
# Initialize a flag to check if the table is created
table_created <- condition_write_table

t <- Sys.time()
i <- 1
system('pg_ctl -D "C:/Users/jahir.briones/AppData/Roaming/pgsql/RGM" -l archivo_de_registro start')
source("lib/con_pg.R")

# Loop through each unique combination
for (row in seq_len(nrow(unique_combinations))) {
  country <- unique_combinations$Country[row]
  client  <- unique_combinations$Client[row]
  ean     <- unique_combinations$EAN[row]
  
  
  # country <- 'Peru'
  # client  <- 'Inkafarma'
  # ean     <- '7509552843620'
  cat("Country: ", country, "\n", "Client: ", client, "\n")
  cat("Progress: ", i, "/", nrow(unique_combinations), " (", i/nrow(unique_combinations)*100, "%)\n")
  
  # Filter data for the current combination
  combo_data <- market_df %>%
    filter(Country == country, Client == client, EAN == ean)
  
  # Repeat data for Panama until nrow > 180
  if (country == "Panama") {
    while (nrow(combo_data) > 0 && nrow(combo_data) < 300) {
      combo_data <- rbind(combo_data, combo_data)
    }
  }
  
  if (nrow(combo_data) > 180) {
    # Initialize the BaselineModel for the current combination
    
    baseline_model <- BaselineModel$new(combo_data)
    
    # Run the baseline calculations
    # if you need filter decoment
    #baseline_model$data_profiling()l
    results <- baseline_model$run_baseline()
    
    # Extract the compare_models and merged_data
    compare_models <- results[[1]]
    merged_data <- results[[2]]
    
    # Only one combination per loop, but keep the structure for clarity
    filtered_data <- compare_models %>%
      filter(Country == country, Client == client, EAN == ean)
    
    if (nrow(filtered_data) == 0) next
    
    best_baseline_result <- get_best_baseline(filtered_data)
    
    
    filtered_data <- filtered_data %>%
      mutate(
        best_baseline = best_baseline_result$best_baseline,
        best_model = best_baseline_result$best_model,
        r2_LR = best_baseline_result$r2_LR,
        r2_DT = best_baseline_result$r2_DT,
        r2_XGB = best_baseline_result$r2_XGB,
        r2_RF = best_baseline_result$r2_RF
      ) %>% 
      mutate(d2 = as.numeric(paste0(year(floor_date(Date, "month")), month(floor_date(Date, "month")))),
             d3 = as.numeric(year(floor_date(Date, "month")))
      )
    
    baseline_selected <- filtered_data
    
    # Load the best model object
    model_id <- paste(unique(filtered_data$Country), unique(filtered_data$Client), unique(filtered_data$EAN), sep = "_")
    model_path <- file.path("data/models", paste0(best_baseline_result$best_model, "_", model_id, ".rds"))
    best_model_object <- readRDS(model_path)
    
    baseline_selected$price_old <- baseline_selected$price
    baseline_selected$price <- baseline_selected$price * (1 + baseline_selected$Discount)
    
    # Prepare predictors
    X_pred <- baseline_selected[, vars_to_predict, drop = FALSE]
    # Predict
    pred_units <- predict(best_model_object, newdata = X_pred)
    # Store predictions
    baseline_selected$baseline_no_discount <- exp(pred_units)
    
    
    compare_models <- baseline_selected# %>% unique()
    
    compare_models <- compare_models %>% 
      group_by(Country, Client, EAN) %>% 
      mutate(best_baseline = if_else(is.na(best_baseline), 
                                     mean(ifelse(Discount == 0, Units, 1)),
                                     best_baseline)
      )
    
    # Write the results to the database
    if (!table_created) {
      dbWriteTable(con, Id(schema = "ceran", table = TABLE_NAME_MODEL_RESULTS), compare_models, overwrite = TRUE)
      table_created <- TRUE
    } else {
      dbWriteTable(con, Id(schema = "ceran", table = TABLE_NAME_MODEL_RESULTS), compare_models, append = TRUE)
    }
  }
  gc()
  i <- i + 1
}
Sys.time() - t

cat("Processing complete. Results saved to the database.\n")
# DBI::dbExecute(con, "
#   DELETE FROM ceran.baseline_model_results
#   WHERE \"Client\" IN ('Metro', 'Plaza Vea');
# ")

ean_baseline <- dbGetQuery(con, glue("SELECT * FROM ceran.{TABLE_NAME_MODEL_RESULTS}"))
table(ean_baseline$Client,ean_baseline$best_model)

market_df_cat <- market_df %>% 
  ungroup() %>% 
  dplyr::select(Country, Client, Date, EAN, SKU, Sales) %>% 
  unique()
table(market_df_cat$Client, market_df_cat$Country)

baseline <- ean_baseline %>%
  #filter(Client %in% c('Exito','Kioskos')) %>% 
  left_join(., market_df_cat %>%
              mutate(date = as.Date(Date, format = "%Y-%m-%d")),
            by = c("Date"="Date","Client"="Client","EAN"="EAN","Country"="Country")) %>%
  mutate(
    year = as.character(year(Date)),
    baseline_no_discount = floor(baseline_no_discount),
    Sales = floor(Sales)
  ) %>% 
  dplyr::select(date = Date,
                year,
                month = Month,
                client = Client,
                country = Country,
                ean = EAN,
                sku = SKU.y,
                real_units = Units,
                real_sales = Sales,
                price = price,
                discount = Discount,
                baseline_units = baseline_no_discount) %>%
  mutate(baseline_sales = round((baseline_units*price)))

nrow(baseline)

table(baseline$client)


table(market_df$Client)
omit_combos <- baseline %>% dplyr::select(country, client, ean,date) %>% unique()

# Filter out those combinations from market_df
group <- market_df %>%
  anti_join(omit_combos, by = c("Country"="country",
                                "Client"="client",
                                "EAN"="ean",
                                "Date"="date"))

# Create a complete date sequence
date_range <- seq(min(group$Date), max(group$Date), by = "day")


# Generate all combinations of Client, EAN, and Date
complete_data <- expand.grid(
  Country = unique(group$Country),
  Date = date_range,
  Client = unique(group$Client),
  EAN = unique(group$EAN)
)
complete_data$Date = as.Date(complete_data$Date)

# Merge with original data and fill missing Units with 0
group <- complete_data %>%
  left_join(group, by = c("Country", "Client", "EAN", "Date"))
group[is.na(group)] <- 0


group <- group %>% 
  filter(Units > 0)

compare_models_no_model <- group %>% 
  mutate(Month = floor_date(Date, "month")) %>% 
  group_by(Country, Client, EAN, SKU, Month) %>% 
  mutate(best_baseline = median(ifelse(Discount == 0, Units, 1))
  )

compare_models_no <- compare_models_no_model %>%
  left_join(., market_df_cat %>%
              mutate(date = as.Date(Date, format = "%Y-%m-%d")),
            by = c("Date"="Date","Client"="Client","EAN"="EAN","Country"="Country")) %>%
  mutate(
    year = as.character(year(Date)),
    best_baseline = floor(best_baseline),
    Sales = floor(Sales.x)
  ) %>% 
  ungroup() %>% 
  dplyr::select(date = Date,
                year,
                month = Month,
                client = Client,
                country = Country,
                ean = EAN,
                sku = SKU.x,
                real_units = Units,
                real_sales = Sales,
                price = price,
                discount = Discount,
                baseline_units = best_baseline) %>%
  mutate(baseline_sales = round((baseline_units*price)))


dbWriteTable(con, Id(schema = "ceran", table = TABLE_NAME_NO_MODEL_RESULTS), compare_models_no, overwrite = TRUE)
dbWriteTable(con, Id(schema = "ceran", table = BASELINE), baseline, overwrite = TRUE)

base <- baseline %>% 
  dplyr::select(names(compare_models_no)) %>% 
  rbind(compare_models_no) %>% 
  filter(baseline_units > 0,
         country != 'Panama')

table(base$client,base$country)

write.csv(base,"data/-CO- PromoTool CERAN - Base R/Promotool/Baseline R/Baseline R full.csv", row.names = F)
dbWriteTable(con, Id(schema = "ceran", table = CONSOLIDATED_BASELINE), base, overwrite = TRUE)

base <- dbGetQuery(con, glue("SELECT * FROM ceran.{CONSOLIDATED_BASELINE}"))

dbExecute(con, 'DROP TABLE IF EXISTS ceran.full_sku_baseline;')
dbExecute(con, glue('
CREATE TABLE ceran.full_sku_baseline AS
SELECT 
    A.date, 
    A.year, 
    A.month, 
    A.client, 
    A.country, 
    A.ean, 
    A.sku, 
    B.brand, 
    B.franchise, 
    B.category, 
    B.sub_brand, 
    B.category_2, 
    B.sub_category, 
    A.real_units, 
    A.real_sales, 
    A.price, 
    A.discount, 
    A.baseline_units, 
    A.baseline_sales
FROM ceran.{CONSOLIDATED_BASELINE} AS A
LEFT JOIN ceran.maestra AS B
    ON A.ean = B.ean;
'))

#rm(list = ls())
#gc()
