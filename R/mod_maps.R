#' Module maps ui
#' 
#' @param id Unique id for module instance.
#'
#' @import shiny
#' @noRd
mod_maps_ui <- function(id){
  ns <- NS(id)

  tagList(
    h2("maps")
    
  )
}


#' Module maps Server Functions
#'
#' @noRd
mod_maps_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}


## To be copied in the UI
# mod_maps_ui("maps")

## To be copied in the server
# mod_maps_server("maps")

