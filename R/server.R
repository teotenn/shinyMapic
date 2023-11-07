#' Server
#' 
#' Core server function.
#' 
#' @param input,output Input and output list objects
#' containing said registered inputs and outputs.
#' @param session Shiny session.
#' 
#' @noRd 
server <- function(input, output, session){
  mod_intro_server("intro")
  mod_session_server("session")
  mod_coords_server("coords")
}
