
arg <- "inicializar server"
system2('pg_ctl -D "C:/Users/jahir.briones/AppData/Roaming/pgsql/RGM" -l archivo_de_registro start', arg)

source("src/code/upload Maestra.R", encoding = 'utf8')
source("src/code/upload ventas table.R", encoding = 'utf8')
#source("src/code/upload baseline table.R", encoding = 'utf8')
source("src/code/upload discounts table.R", encoding = 'utf8')
source("src/code/upload P&L table.R", encoding = 'utf8')


### Vars to model
client <- "Inkafarma"
country <- "Peru"
condition_write_table <- FALSE # IF FALSE THEN WRITE NEW TABLE
TABLE_NAME_MODEL_RESULTS <- "baseline_model_results_TEST"
TABLE_NAME_NO_MODEL_RESULTS <- "baseline_no_model_results_TEST"
BASELINE <- "baseline_TEST"
CONSOLIDATED_BASELINE <- "consolidated_baseline_TEST"

source("src/model/baseline model.R", encoding = 'utf8')
#source("code/consolida tablas.R")  

arg <- "finalizar server"
system2('pg_ctl -D "C:/Users/jahir.briones/AppData/Roaming/pgsql/RGM" -l archivo_de_registro stop', arg)
