
arg <- "inicializar server"
system2('pg_ctl -D "C:/Users/jahir.briones/AppData/Roaming/pgsql/RGM" -l archivo_de_registro start', arg)

source("src/code/upload ventas table.R")
source("src/code/upload baseline table.R")
source("src/code/upload discounts table.R")
source("src/code/upload P&L table.R")
#source("code/consolida tablas.R")  

arg <- "finalizar server"
system2('pg_ctl -D "C:/Users/jahir.briones/AppData/Roaming/pgsql/RGM" -l archivo_de_registro stop', arg)
