library(readxl)
library(dplyr)

source("lib/con_pg.R")

df <- read_xlsx("data/Maestra/Maestra-promotool.xlsx", sheet = "MAESTRA DEFINITIVA")[,c(1,3:12)]
names(df)
#[1] "Fecha"             "Año"               "Mes"               "Cadena"            "EAN"               "SKU"               "Unidades Baseline" "Ventas Baseline"

new_names <- c("ean", "sku", "brand", "franchise", "manufacture", "category",
               "sub_brand","category_2","sub_category","sub_line","sub_direction"
               #,"direction","format","client","format_2","content","pack","pack_unit","segment","subaxis"
               )

names(df) <- new_names

data <- df %>%
  mutate(ean = as.character(ean))


dbWriteTable(con, Id(schema = paste("ceran"), table = paste("maestra")),
             data, overwrite = TRUE)

dbGetQuery(con, 'SELECT * FROM ceran.maestra')

dbDisconnect(con)

rm(list = ls())
gc()

print("Maestra uploaded")
