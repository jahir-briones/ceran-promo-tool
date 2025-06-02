library(RPostgres)
library(DBI)
library(readxl)
library(dplyr)
library(stringr)

# Create a connection object
con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "promotool",
  host     = "127.0.0.1",
  port     = 5432,  # Default PostgreSQL port
  user     = "postgres",
  password = "AdminRGM$LATAM$2025"
)

df <- read_xlsx("Colombia/Baseline/Baseline Inretail.xlsx", sheet = "Base Baseline")
names(df)
# [1] "Fecha"             "Año"               "Mes"               "Cadena"            "Pais"              "EAN"               "SKU"              
# [8] "Unidades Baseline" "Ventas Baseline" 

new_names <- c('date','year','month','client','contry','ean','sku','baseline_units','baseline_sales')
names(df) <- new_names

data <- df %>%
  mutate(date = format(as.Date(date, format="%Y-%m-%d")),
         ean = as.character(ean),
         month = str_pad(month, 2, pad = "0"),
         year = str_pad(year, 4, pad = "0"))


dbWriteTable(con, Id(schema = paste("ceran"), table = paste("baseline_old")),
             data, overwrite = TRUE)

dbGetQuery(con, 'SELECT DISTINCT client FROM ceran.baseline_old')

dbDisconnect(con)
