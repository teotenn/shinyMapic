#' Module coords ui
#' 
#' @param id Unique id for module instance.
#'
#' @import shiny
#' @noRd
mod_coords_ui <- function(id){
  ns <- NS(id)

  tagList(
    h2("coords")
    
  )
}


#' Module coords Server Functions
#'
#' @noRd
mod_coords_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}


## To be copied in the UI
# mod_coords_ui("coords")

## To be copied in the server
# mod_coords_server("coords")
