# Load libraries
library(forecast)
library(xgboost)
library(caret)
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)
library(zoo)
library(R6)
library(tidyr)
library(purrr)
library(readxl)
library(RPostgres)
library(DBI)
library(openssl)
library(base64enc)

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

# Fetch discounts data
discounts <- dbGetQuery(con, 'SELECT * FROM ceran.discounts')

df <- discounts %>%
  select(year, month, client, country) %>%
  group_by(year, month, client, country) %>% 
  summarise(N = n()) %>% 
  group_by(month, year) %>% 
  summarise(N_avg = median(N))


# Fetch sales data
sales <- dbGetQuery(con, 'SELECT * FROM ceran.sell_out')

df <- sales %>%
  select(year, month, client, country) %>%
  group_by(year, month, client, country) %>% 
  summarise(N = n()) %>% 
  group_by(month, year) %>% 
  summarise(N_avg = median(N))

# Fetch sales data
pl <- dbGetQuery(con, 'SELECT * FROM ceran.profit_loss')


df <- pl %>%
  select(year, month, client, country) %>%
  group_by(year, month, client, country) %>% 
  summarise(N = n()) %>% 
  group_by(month, year) %>% 
  summarise(N_avg = median(N))



