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
      column(5, h6("Session No.")),
      column(7, tags$h6(textOutput(NS(id, "session_id"), inline = TRUE)))
    )
  )
}


#' Module session Server Functions
#'
#' @noRd
mod_session_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    react_f_intro <- get("reactive_from_intro", envir = mapic_env)
    output$session_id <- renderPrint({cat(react_f_intro$session_id)})
  })
}


## To be copied in the UI
# mod_session_ui("session")

## To be copied in the server
# mod_session_server("session")

