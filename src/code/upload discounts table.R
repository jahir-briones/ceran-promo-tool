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

df0 <- read_xlsx("data/Colombia/Descuento/Descuentos D2D.xlsx", sheet = "Hoja1")
df0 <- df0 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df1 <- read_xlsx("data/Colombia/Descuento/Descuentos Alkosto.xlsx", sheet = "Hoja1")
df1 <- df1 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df2 <- read_xlsx("data/Colombia/Descuento/Descuentos Éxito.xlsx", sheet = "Hoja1")
df2 <- df2 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df3 <- read_xlsx("data/Colombia/Descuento/Descuentos Inkafarma.xlsx", sheet = "Hoja1")
df3 <- df3 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df4 <- read_xlsx("data/Colombia/Descuento/Descuentos Metro.xlsx", sheet = "Hoja1")
df4 <- df4 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df5 <- read_xlsx("data/Colombia/Descuento/Descuentos Plaza Vea.xlsx", sheet = "Hoja1")
df5 <- df5 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df6 <- read_xlsx("data/Colombia/Descuento/Descuentos Tottus.xlsx", sheet = "Hoja1")
df6 <- df6 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df7 <- read_xlsx("data/Colombia/Descuento/Descuentos Vivanda.xlsx", sheet = "Hoja1")
df7 <- df7 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df8 <- read_xlsx("data/Colombia/Descuento/Descuentos Wong.xlsx", sheet = "Hoja1")
df8 <- df8 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df9 <- read_xlsx("data/Colombia/Descuento/Descuentos.xlsx", sheet = "Hoja1")
df9 <- df9 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df10 <- read_xlsx("data/Colombia/Descuento/Descuentos Mi Farma.xlsx", sheet = "Hoja1")
df10 <- df10 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
#df11 <- read_xlsx("Colombia/Descuento/Descuentos Farmatodo 3.0.xlsx", sheet = "Hoja1")
#df11 <- df11 %>% 
#  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df12 <- read_xlsx("data/Colombia/Descuento/Descuentos GT Walmart.xlsx", sheet = "Hoja1")
df12 <- df12 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))

df13 <- read_xlsx("data/Colombia/Descuento/Descuentos Costa Rica Walmart.xlsx", sheet = "Hoja1")
df13 <- df13 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df14 <- read_xlsx("data/Colombia/Descuento/Descuentos HN WM.xlsx", sheet = "Hoja1")
df14 <- df14 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df15 <- read_xlsx("data/Colombia/Descuento/Descuentos SV Walmart.xlsx", sheet = "Hoja1")
df15 <- df15 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df16 <- read_xlsx("data/Colombia/Descuento/Descuentos WM Nicaragua.xlsx", sheet = "Hoja1")
df16 <- df16 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df17 <- read_xlsx("data/Colombia/Descuento/Descuentos FarmT.xlsx", sheet = "Hoja1")
df17 <- df17 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))



df <- rbind(df0,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10#,df11
            ,df12,df13,df14,df15,df16,df17)

rm(df0,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10#,df11
   ,df12,df13,df14,df15,df16,df17)


names(df)
# [1] "Fecha"         "Año"           "Mes"           "Cadena"        "Pais"          "EAN"           "PLU_SAP"       "PLU_SICOL"     "SKU"          
# [10] "% descuento"   "Tipo de promo" "Nombre Promo" 

new_names <- c('date','year','month','client','country','ean','plu_sap','plu_sicol','sku','discount_pct','promo_type','promo_name')
names(df) <- new_names

data <- df %>%
  mutate(date = format(as.Date(date, format="%Y-%m-%d")),
         ean = as.character(ean),
         month = str_pad(month, 2, pad = "0"),
         year = str_pad(year, 4, pad = "0"))


dbWriteTable(con, Id(schema = paste("ceran"), table = paste("discounts")),
             data, overwrite = TRUE)
dbDisconnect(con)

rm(list = ls())
gc()
