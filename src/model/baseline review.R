library(RPostgres)
library(DBI)
library(dplyr)
library(lubridate)
library(openssl)
library(base64enc)
library(ggplot2)


# Define the key
key <- charToRaw("my_secret_key_12")  # 32-byte key for AES-256

# Read and decode the encrypted password from the file

encrypted_password_base64 <- readLines("encrypted_password.txt")
encrypted_password <- base64decode(encrypted_password_base64)

# Decrypt the password
decrypted_password <- rawToChar(aes_cbc_decrypt(encrypted_password, key = key, iv = NULL))


# Connect to the database
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "promotool",
  host = "127.0.0.1",
  port = 5432,  # Default PostgreSQL port
  user = "postgres",
  password = decrypted_password
)

dbGetQuery(con, 'SELECT * FROM ceran.baseline_model_results limit 10') #THE TEST VALUES

#ean_baseline <- dbGetQuery(con, 'SELECT * FROM ceran.baseline') THE OK VALUES
baseline <- dbGetQuery(con, 'SELECT DISTINCT * FROM ceran.consolidated_baseline') #THE TEST VALUES
# ean_baseline_no <- dbGetQuery(con, 'SELECT * FROM ceran.baseline_no_model_results')
# baseline_old <- dbGetQuery(con, "SELECT * FROM ceran.baseline_old
#                            WHERE client = 'Inkafarma'")
# baseline_old <- baseline_old %>% 
#   mutate(date = as.Date(date, format = "%Y-%m-%d"),
#         ean = as.character(ean))
#write.csv(ean_baseline,"bases/baseline_models.csv", row.names = F)
#write.csv(ean_baseline_no,"bases/baseline_no_models.csv", row.names = F)

# Example group for visualization 
#'7509552792287',7509552455557','3600524057336','6923700977561','3600542081160','7509552845884','7509552849516'
example_group <- baseline %>%
  filter(ean == '7509552844184'
         ,discount != 0# 7899706130899, 7509552455557 , 7509552792287, 7509552849516
         ) 

merged_data_for_viz <- example_group
merged_data_for_viz <- merged_data_for_viz %>%
  mutate(date = #Date
           floor_date(date, "week")
  ) %>%
  group_by(date,ean) %>%
  summarise(Units = sum(real_units),
            best_baseline = sum(baseline_units),
            #baseline_no_discount = sum(baseline_no_discount),
            #old_price = mean(price_old),
            price = mean(price)
            # ,Baseline_LR = sum(Baseline_LR)
            # ,Baseline_DT = sum(Baseline_DT)
            # ,Baseline_XGB = sum(Baseline_XGB)
            # ,Baseline_RF = sum(Baseline_RF)
  ) 
  #left_join(baseline_old, by = c("date","ean"))


# Visualization using ggplot2
ggplot(merged_data_for_viz, aes(x = date)) +
  geom_line(aes(y = Units, color = "Units Sales"), size = 1, linetype = "solid") +
  geom_line(aes(y = best_baseline, color = "Baseline No Discount"), size = 1, linetype = "dotdash") +
  geom_text(aes(y = Units, label = round(Units, 0)), 
            color = "black", vjust = -0.7, size = 3, show.legend = FALSE) +
  geom_text(aes(y = best_baseline, label = round(best_baseline, 0)), 
            color = "black", vjust = 1.5, size = 3, show.legend = FALSE) +
  labs(
    title = "Actual Sales vs Baseline",
    subtitle = "Acondicionador Hairfood Aloe 300Ml",
    x = "Date",
    y = "Units",
    color = "Legend"
  ) +
  scale_color_manual(
    values = c(
      "Units Sales" = "hotpink3",
      "Baseline No Discount" = "dodgerblue4"
    )
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  guides(color = guide_legend(override.aes = list(size = 2)))

table(ean_baseline$best_model)


# Calculate model counts and percentages
model_counts <- ean_baseline %>% count(best_model) %>% rename(Frequency = n)
model_counts$Percentage <- (model_counts$Frequency / sum(model_counts$Frequency)) * 100

# Ensure Frequency and Percentage are numeric
model_counts$Frequency <- as.numeric(model_counts$Frequency)
model_counts$Percentage <- as.numeric(model_counts$Percentage)

# # Plotting
ggplot(model_counts, aes(x = best_model, y = Frequency, fill = best_model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.01f%%", Percentage)), vjust = -0.8) +
  labs(title = "Distribution of Best Models", x = "Model", y = "Frequency") +
  theme_bw() +
  theme(legend.position = "none")


ean_baseline %>% 
  ungroup() %>% 
  filter(EAN == '7501027209627') %>% 
  group_by(Month,EAN) %>% 
  summarise(units = sum(Units),
            baseline = sum(baseline_no_discount))


sell_out %>% 
  filter(ean == '7501027209627') %>% 
  group_by(month, year, ean) %>% 
  summarise(units = sum(real_units))
            

