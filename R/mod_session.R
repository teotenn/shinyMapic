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
      column(5, tags$h6(textOutput(NS(id, "session_id"), inline = TRUE)))
    )
  )
}


#' Module session Server Functions
#'
#' @noRd
mod_session_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$session_id <- renderPrint({cat(paste(Sys.Date(), rnorm(1, 100), sep = "-"))})
  })
}


## To be copied in the UI
# mod_session_ui("session")

## To be copied in the server
# mod_session_server("session")

