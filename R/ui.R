#' Shiny UI
#' 
#' Core UI of package.
#' 
#' @param req The request object.
#' 
#' @import shiny
#' 
#' @noRd
ui <- function(req){
  navbarPage(
    ## theme = bs_theme(version = 5),
    ## if we use it, add @importFrom bslib bs_theme
    title = "Mapic",
    id = "main-menu",
    tabPanel(
      "Intro",
      mod_intro_ui("intro")
    ),
    tabPanel(
      "Coords",
      mod_coords_ui("coords")
    ),
    tabPanel(
      "Maps",
      mod_maps_ui("maps")
    )
  )
}
