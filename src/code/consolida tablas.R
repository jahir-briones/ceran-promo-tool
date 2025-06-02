library(RPostgres)
library(DBI)

# --- Main Script ---

# 1. Establish Connection
con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "promotool",
  host     = "127.0.0.1",
  port     = 5432,  # Default PostgreSQL port
  user     = "postgres",
  password = "AdminRGM$LATAM$2025"
)
# 2. Read SQL File
sql_commands <- paste(readLines("LATAM/2025/CERAN/sql/join tables.sql"), collapse = "\n")

# 3. Split SQL Commands (using robust regex)
sql_commands_list <- unlist(strsplit(sql_commands, split = "(?<=;)(?=[^'\"`]*['\"`][^'\"`]*['\"`][^'\"`]*$)", perl = TRUE))
sql_commands_list <- trimws(sql_commands_list)
sql_commands_list <- sql_commands_list[sql_commands_list != ""]

# 4. Execute SQL Commands
for (sql_command in sql_commands_list) {
  tryCatch({
    # Determine if the command is likely a SELECT statement
    if (grepl("^SELECT", toupper(sql_command))) {
      result <- dbGetQuery(con, sql_command)
      print(result) # Or process the result as needed
      cat("SELECT command executed successfully:\n", sql_command, "\n\n")
      
    } else {
      result <- dbGetQuery(con, sql_command)
      cat("Command executed successfully:\n", sql_command, "\n\n")
    }
  }, error = function(e) {
    cat("Error executing command:\n", sql_command, "\nError message:\n", e$message, "\n\n")
  })
}

# 5. Close Connection
dbDisconnect(con)

write.csv(result, "LATAM/2025/CERAN/bases/promo_tool_consolidated.csv", row.names = F)


