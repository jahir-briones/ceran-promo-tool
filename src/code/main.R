system('pg_ctl -D "C:/Users/jahir.briones/AppData/Roaming/pgsql/RGM" -l archivo_de_registro start')

source("src/code/upload Maestra.R", encoding = 'utf8')
source("src/code/upload ventas table.R", encoding = 'utf8')
#source("src/code/upload baseline table.R", encoding = 'utf8')
source("src/code/upload discounts table.R", encoding = 'utf8')
source("src/code/upload P&L table.R", encoding = 'utf8')


### Vars to model
rm(list = ls())
gc()


filters = list('client' = c('Mi Farma','Inkafarma','Inkafarma','Mi Farma','WM-Bodega','WM-Descuento','Farmatodo','WM-Hipermercado','WM-Supermercado'
                           ,'Wong','Novaventa', 'Exito', 'Kioskos', 'Metro','Plaza Vea','Novaventa/Door to door')
               #'country' = c('Panama')
               )

condition_write_table <- TRUE # IF FALSE THEN WRITE NEW TABLE
TABLE_NAME_MODEL_RESULTS <- "baseline_model_results"
TABLE_NAME_NO_MODEL_RESULTS <- "baseline_no_model_results"
BASELINE <- "baseline"
CONSOLIDATED_BASELINE <- "consolidated_baseline"

source("src/model/baseline model.R", encoding = 'utf8')
#source("code/consolida tablas.R")  

system('pg_ctl -D "C:/Users/jahir.briones/AppData/Roaming/pgsql/RGM" -l archivo_de_registro stop')
