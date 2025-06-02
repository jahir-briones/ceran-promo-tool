library(RPostgres)
library(DBI)
library(readxl)
library(dplyr)
library(stringr)
library(openssl)
library(base64enc)

# Define the key
key <- charToRaw("my_secret_key_12")  # 32-byte key for AES-256

# Read and decode the encrypted password from the file

encrypted_password_base64 <- readLines("src/crypt/encrypted_password.txt")
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

df <- read_xlsx("data/Colombia/Baseline/Baseline Inretail.xlsx", sheet = "Base Baseline")
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

rm(list = ls())
gc()
