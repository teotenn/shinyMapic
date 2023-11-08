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
