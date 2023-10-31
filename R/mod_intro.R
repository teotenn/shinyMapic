#' Module intro ui
#' 
#' @param id Unique id for module instance.
#'
#' @import shiny
#' @noRd
mod_intro_ui <- function(id){
  ns <- NS(id)

  tagList(
    h2("Intro")
  )
}


#' Module Intro Server Functions
#'
#' @noRd
mod_intro_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}


## To be copied in the UI
# mod_intro_ui("intro")

## To be copied in the server
# mod_intro_server("intro")
