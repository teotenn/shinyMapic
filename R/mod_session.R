#' Module session ui
#' 
#' @param id Unique id for module instance.
#'
#' @import shiny
#' @noRd
mod_session_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
      column(7, h6("Session No.")),
      column(5, p("123")))
  )
}


#' Module session Server Functions
#'
#' @noRd
mod_session_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}


## To be copied in the UI
# mod_session_ui("session")

## To be copied in the server
# mod_session_server("session")
