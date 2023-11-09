#' Shiny UI
#' 
#' Core UI of package.
#' 
#' @param req The request object.
#' 
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' 
#' @noRd
ui <- function(req){
  fluidPage(
    includeCSS("www/styles.css"),
    includeScript("www/script.js"),
    shinyjs::useShinyjs(),
    
    fluidRow(
      column(10, h1("Maps")),
      column(2, mod_session_ui("session"))
    ),
    navbarPage(
      ## theme = bs_theme(version = 5),
      ## if we use it, add to doc above #' @importFrom bslib bs_theme
      title = "Maps",
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
  )
}
