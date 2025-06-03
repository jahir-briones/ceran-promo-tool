

system('pg_ctl -D "C:/Users/jahir.briones/AppData/Roaming/pgsql/RGM" -l archivo_de_registro start')

source("src/code/upload Maestra.R", encoding = 'utf8')
source("src/code/upload ventas table.R", encoding = 'utf8')
source("src/code/upload discounts table.R", encoding = 'utf8')
source("src/code/upload P&L table.R", encoding = 'utf8')


### Vars to model
rm(list = ls())
gc()


filters = list( 'client' = c('WM-Bodega','WM-Descuento','WM-Hipermercado','WM-Supermercado'),
               'country' = 'Costa Rica')

condition_write_table <- TRUE # IF FALSE THEN WRITE NEW TABLE
TABLE_NAME_MODEL_RESULTS <- "baseline_model_results_test"
TABLE_NAME_NO_MODEL_RESULTS <- "baseline_no_model_results_test"
BASELINE <- "baseline_test"
CONSOLIDATED_BASELINE <- "consolidated_baseline_test"

source("src/model/baseline model.R", encoding = 'utf8')


system('pg_ctl -D "C:/Users/jahir.briones/AppData/Roaming/pgsql/RGM" -l archivo_de_registro stop')
