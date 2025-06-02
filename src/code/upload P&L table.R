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

df0 <- read_xlsx("data/Colombia/P&L/P&L D2D.xlsx", sheet = "Sheet1")
df0 <- df0 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))


df1 <- read_xlsx("data/Colombia/P&L/P&L Plaza Vea.xlsx", sheet = "Sheet1")
df1 <- df1 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))

df2 <- read_xlsx("data/Colombia/P&L/P&L Éxito.xlsx", sheet = "Sheet1")
df2 <- df2 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df3 <- read_xlsx("data/Colombia/P&L/P&L Inkafarma.xlsx", sheet = "Sheet1")
df3 <- df3 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df4 <- read_xlsx("data/Colombia/P&L/P&L Metro.xlsx", sheet = "Sheet1")
df4 <- df4 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df5 <- read_xlsx("data/Colombia/P&L/P&L Plaza Vea.xlsx", sheet = "Sheet1")
df5 <- df5 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df6 <- read_xlsx("data/Colombia/P&L/P&L Tottus.xlsx", sheet = "Sheet1")
df6 <- df6 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))


df7 <- read_xlsx("data/Colombia/P&L/P&L Farmatodo.xlsx", sheet = "Sheet1")
df7 <- df7 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))



df8 <- read_xlsx("data/Colombia/P&L/P&L Wong.xlsx", sheet = "Sheet1")
df8 <- df8 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df9 <- read_xlsx("data/Colombia/P&L/P&L.xlsx", sheet = "Sheet1")
df9 <- df9 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df10 <- read_xlsx("data/Colombia/P&L/P&L Mi Farma.xlsx", sheet = "Sheet1")
df10 <- df10 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df11 <- read_xlsx("data/Colombia/P&L/P&L Guatemala WM.xlsx", sheet = "Sheet1")
df11 <- df11 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))



df <- rbind(df0,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11)

rm(df0,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11)


names(df)
# [1] "customer"             "client"               "country"              "pl_product"           "product"              "compass product code"
# [7] "ean"                  "scenario"             "date"                 "year"                 "month"                "pl_total" 

new_names <- c('customer','client','country','pl_product','product','compass_product_code','ean','scenario','date','year','month','pl_total')
names(df) <- new_names

data <- df %>%
  mutate(customer_id = paste0(customer,"_",country),
         date = format(as.Date(date, format="%Y-%m-%d")),
         ean = as.character(ean),
         month = str_pad(month, 2, pad = "0"),
         year = str_pad(year, 4, pad = "0")) %>% 
         select(date,year,month,customer_id, customer,client,country,pl_product,product,compass_product_code,ean,scenario,pl_total)


dbWriteTable(con, Id(schema = paste("ceran"), table = paste("profit_loss")),
             data, overwrite = TRUE)

dbGetQuery(con, 'SELECT distinct client FROM ceran.profit_loss')

dbDisconnect(con)
