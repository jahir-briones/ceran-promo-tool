library(readxl)
library(dplyr)

source("lib/con_pg.R")


df0 <- read_xlsx("data/Colombia/Ventas/Ventas D2D.xlsx", sheet = "Base")
df0 <- df0 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df1 <- read_xlsx("data/Colombia/Ventas/Ventas Éxito.xlsx", sheet = "Base")
df1 <- df1 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df2 <- read_xlsx("data/Colombia/Ventas/Ventas Inkafarma.xlsx", sheet = "Base")
df2 <- df2 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df3 <- read_xlsx("data/Colombia/Ventas/Ventas Kioskos.xlsx", sheet = "Base")
df3 <- df3 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df4 <- read_xlsx("data/Colombia/Ventas/Ventas Metro.xlsx", sheet = "Base")
df4 <- df4 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df5 <- read_xlsx("data/Colombia/Ventas/Ventas Mi Farma.xlsx", sheet = "Base")
df5 <- df5 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df6 <- read_xlsx("data/Colombia/Ventas/Ventas Plaza Vea.xlsx", sheet = "Base")
df6 <- df6 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df7 <- read_xlsx("data/Colombia/Ventas/Ventas Tottus.xlsx", sheet = "Base")
df7 <- df7[,1:9] %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df8 <- read_xlsx("data/Colombia/Ventas/Ventas Vivanda.xlsx", sheet = "Base")
df8 <- df8 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df9 <- read_xlsx("data/Colombia/Ventas/Ventas Wong.xlsx", sheet = "Base")
df9 <- df9[,1:9] %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))

df10 <- read_xlsx("data/Colombia/Ventas/Ventas Walmart Costa Rica.xlsx", sheet = "Base")
df10 <- df10 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df11 <- read_xlsx("data/Colombia/Ventas/Ventas WM HN.xlsx", sheet = "Base")
df11 <- df11 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df12 <- read_xlsx("data/Colombia/Ventas/Ventas WM Nicaragua.xlsx", sheet = "Base")
df12 <- df12 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df13 <- read_xlsx("data/Colombia/Ventas/Ventas Walmart El Salvador.xlsx", sheet = "Base")
df13 <- df13 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df14 <- read_xlsx("data/Colombia/Ventas/Ventas Walmart Guatemala.xlsx", sheet = "Base")
df14 <- df14 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))

df15 <- read_xlsx("data/Colombia/Ventas/Ventas Farmatodo 2.0.xlsx", sheet = "Base")
df15 <- df15 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))

df16 <- read_xlsx("data/Colombia/Ventas/Ventas Panama.xlsx", sheet = "Base")
df16 <- df16 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))



df <- rbind(df0,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15, df16)

rm(df0,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16)

names(df)
# [1] "Fecha"           "Año"             "Mes"             "Cadena"          "Pais"            "EAN"             "Descripción"     "Unidades Reales"
# [9] "Value Real" 

new_names <- c('date','year','month','client','country','ean','sku','real_units','real_sales')
names(df) <- new_names

data <- df %>% 
  mutate(date = format(as.Date(date, format="%Y-%m-%d")),
         ean = as.character(ean),
         month = str_pad(month, 2, pad = "0"),
         year = str_pad(year, 4, pad = "0"))

dbWriteTable(con, Id(schema = paste("ceran"), table = paste("sell_out")), data, overwrite = TRUE)

dbGetQuery(con, 'SELECT * FROM ceran.sell_out')

dbDisconnect(con)

rm(list = ls())
gc()

print("Sell Out uploaded")
