#' Local environment for sharing info
#'
#' @noRd
#' @export
mapic_env <- new.env(parent = emptyenv())


##### TODO: Change the lines below to be taken from YAML <---------------------------------|

#' Create the mapic DB configuration based on table name
#' 
#' @param table_name Name of the table to use
#'
#' @importFrom mapic database_configuration
#' @noRd
mapic_dbconf_tbl <- function(table_name) {
  mdb_local_db <- database_configuration("SQLite", table_name, "~/Downloads/shiny_testing.sqlite")
  assign("mdb_local_db", mdb_local_db, envir = mapic_env)
}


##### TODO: Change the lines below to be taken from YAML <---------------------------------|
#' Define the connection to the main DB based on YAML file
#'
#' @importFrom DBI dbConnect
#' @noRd
define_connection_to_main_db <- function() {
  generate_connection <- function() {
    con <- dbConnect(drv = RSQLite::SQLite(), dbname = "~/Downloads/shiny_dev_main.sqlite")
    assign("con_main_db", con, envir = mapic_env)
    return(con)
  }
  assign("main_db_connect", generate_connection, envir = mapic_env)
}

##### TODO: Change the lines below to be taken from YAML <---------------------------------|
#' Define the command to load the table in the main DB
#'
#' @noRd
define_table_main_db <- function(main_table_name = "main_table") {
  query_table <- paste0(
    "CREATE TABLE IF NOT EXISTS ", main_table_name,
    "(id TEXT,
       year_start INTEGER,
       year_end INTEGER,
       city TEXT,
       country TEXT,
       region TEXT,
       state TEXT,
       county TEXT,
       lon REAL,
       lat REAL,
       osm_name TEXT,
       UNIQUE(id, country));"
  )
  assign("query_table", query_table, envir = mapic_env)
  assign("main_table_name", main_table_name, envir = mapic_env)
}
