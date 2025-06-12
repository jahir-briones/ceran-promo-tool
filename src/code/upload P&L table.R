library(readxl)
library(dplyr)


df0 <- read_xlsx("data/Colombia/P&L/P&L D2D.xlsx", sheet = "Sheet1")[,1:12]
df0 <- df0 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))


df1 <- read_xlsx("data/Colombia/P&L/P&L Plaza Vea.xlsx", sheet = "Sheet1")[,1:12]
df1 <- df1 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))

df2 <- read_xlsx("data/Colombia/P&L/P&L Éxito.xlsx", sheet = "Sheet1")[,1:12]
df2 <- df2 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df3 <- read_xlsx("data/Colombia/P&L/P&L Inkafarma.xlsx", sheet = "Sheet1")[,1:12]
df3 <- df3 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df4 <- read_xlsx("data/Colombia/P&L/P&L Metro.xlsx", sheet = "Sheet1")[,1:12]
df4 <- df4 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))

df5 <- read_xlsx("data/Colombia/P&L/P&L Salvador WM.xlsx", sheet = "Sheet1")[,1:12]
df5 <- df5 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))

df6 <- read_xlsx("data/Colombia/P&L/P&L Tottus.xlsx", sheet = "Sheet1")[,1:12]
df6 <- df6 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))


df7 <- read_xlsx("data/Colombia/P&L/P&L Farmatodo.xlsx", sheet = "Sheet1")[,1:12]
df7 <- df7 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))


df8 <- read_xlsx("data/Colombia/P&L/P&L Wong.xlsx", sheet = "Sheet1")[,1:12]
df8 <- df8 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df9 <- read_xlsx("data/Colombia/P&L/P&L.xlsx", sheet = "Sheet1")[,1:12]
df9 <- df9 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df10 <- read_xlsx("data/Colombia/P&L/P&L Mi Farma.xlsx", sheet = "Sheet1")[,1:12]
df10 <- df10 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df11 <- read_xlsx("data/Colombia/P&L/P&L Guatemala WM.xlsx", sheet = "Sheet1")[,1:12]
df11 <- df11 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))

df12 <- read_xlsx("data/Colombia/P&L/P&L WM Costa Rica.xlsx", sheet = "Sheet1")[,1:12]
df12 <- df12 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))
df13 <- read_xlsx("data/Colombia/P&L/P&L HN.xlsx", sheet = "Sheet1")[,1:12]
df13 <- df13 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))

df14 <- read_xlsx("data/Colombia/P&L/P&L Nicaragua WM.xlsx", sheet = "Sheet1")[,1:12]
df14 <- df14 %>% 
  mutate(Fecha = format(as.Date(Fecha, format="%Y-%m-%d")))



df <- rbind(df0,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14)

rm(df0,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14)


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

source("lib/con_pg.R")

dbWriteTable(con, Id(schema = paste("ceran"), table = paste("profit_loss")),
             data, overwrite = TRUE)

dbGetQuery(con, 'SELECT distinct client FROM ceran.profit_loss')

dbDisconnect(con)

rm(list = ls())
gc()

print("P&L uploaded")
