#' Server
#' 
#' Core server function.
#' 
#' @param input,output Input and output list objects
#' containing said registered inputs and outputs.
#' @param session Shiny session.
#'
#' @importFrom DBI dbExecute dbDisconnect
#' @noRd 
server <- function(input, output, session){
  ## Load main data base
  define_connection_to_main_db()
  define_table_main_db()
  define_mapic_colors()

  ##### TODO: Wrap this part inside try-catch just to test if DB connection is possible <-----|
  mapic_env$main_db_connect()
  DBI::dbExecute(mapic_env$con_main_db, mapic_env$query_table)
  DBI::dbDisconnect(mapic_env$con_main_db)
  
  ## Modules
  mod_intro_server("intro")
  mod_session_server("session")
  mod_coords_server("coords")
  mod_maps_server("maps")
}
