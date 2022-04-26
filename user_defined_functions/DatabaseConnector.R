database_connector <- function(
  server = config$sql_credentials$server,
  database = config$sql_credentials$database,
  port = config$sql_credentials$port,
  uid = config$sql_credentials$uid,
  pwd = config$sql_credentials$pwd
) {
  dbConnect(
    PostgreSQL(),
    dbname = database,
    user = uid,
    host = server,
    password = pwd,
    port = port
 )
}
