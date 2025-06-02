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

df <- read_xlsx("data/Maestra/Maestra-promotool.xlsx", sheet = "MAESTRA DEFINITIVA")[,c(1,3:12)]
names(df)
#[1] "Fecha"             "Año"               "Mes"               "Cadena"            "EAN"               "SKU"               "Unidades Baseline" "Ventas Baseline"

new_names <- c("ean", "sku", "brand", "franchise", "manufacture", "category",
               "sub_brand","category_2","sub_category","sub_line","sub_direction"
               #,"direction","format","client","format_2","content","pack","pack_unit","segment","subaxis"
               )

names(df) <- new_names

data <- df %>%
  mutate(ean = as.character(EAN))


dbWriteTable(con, Id(schema = paste("ceran"), table = paste("maestra")),
             data, overwrite = TRUE)

dbGetQuery(con, 'SELECT * FROM ceran.maestra')

dbDisconnect(con)
