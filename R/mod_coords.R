#' Module coords ui
#' 
#' @param id Unique id for module instance.
#'
#' @import shiny
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom DT DTOutput
#' @noRd
mod_coords_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(h2("Search Coords")),
    ## shinyjs::useShinyjs(),
    fluidRow( # First row, buttons
      column(4,
             column(6, actionButton(NS(id, "bt_search_coords"), "Search Coords")),
             column(6, actionButton(NS(id, "bt_remove_search"), "Clear Table"))
             )
    ),
    # Second row, search tables and logs/missing
    fluidRow(
      column(5,
             rHandsontableOutput(NS(id, "rhst_country_dat"))),
      column(7,
               verbatimTextOutput(NS(id, "txt_search_log")),
               #tags$head(tags$script(src='script.js'))
               DT::DTOutput(NS(id, "dt_not_found"))
             ),
      fluidRow( # Add missing manually
        column(3,
               fluidRow(actionButton(NS(id, "bt_add_manually"), "Add Data")),
               fluidRow(actionButton(NS(id, "bt_clear_bottom_table"), "Clear Data"))),
        column(9,
               rhandsontable::rHandsontableOutput(NS(id, "rhst_add_manually")))
      )
    )
  )
}


#' Module coords Server Functions
#'
#' @importFrom rhandsontable renderRHandsontable rhandsontable
#' @importFrom DT renderDT
#' @noRd
mod_coords_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ## Init data
    empty_coords <- data.frame(
      city = character(15),
      country = character(15),
      state = character(15),
      region = character(15),
      year_start = integer(15),
      year_end = integer(15))

    mapic_df <- data.frame(
      id = character(5),
      year_start = integer(5),
      year_end = integer(5),
      city = character(5),
      country = character(5),
      region = character(5),
      state = character(5),
      county = character(5),
      lon = numeric(5),
      lat = numeric(5),
      osm_name = character(5))

    ## Initialize empty info
    output$rhst_country_dat = renderRHandsontable(rhandsontable(empty_coords, readOnly = F))
    output$rhst_add_manually = renderRHandsontable(rhandsontable(mapic_df, readOnly = F))
    output$txt_search_log <- renderPrint({"Log Info"})
    output$dt_not_found <- DT::renderDT({mapic_df})


    ## ----- Reactive interactions -----
    ## Clear search data button 'Remove'
    observeEvent(input$bt_remove_search, {
      output$rhst_country_dat <- renderRHandsontable(rhandsontable(empty_coords, readOnly = F))
    })

    ## Search coords
    observeEvent(input$bt_search_coords, {
      output$txt_search_log <- renderPrint({
        for (i in 1:10) print(letters[i])
      })
      output$dt_not_found <- DT::renderDT({
        data.frame(
          id = paste("A", c(1:5)),
          year_start = 2001:2005,
          year_end = NA,
          city = "Fake",
          country = "MX",
          region = NA,
          state = "State",
          county = NA,
          lon = 1:5,
          lat = 11:15,
          osm_name = character(5))
      })
    })

    ## Add coords manually
     observeEvent(input$bt_clear_bottom_table, {
      output$rhst_add_manually = renderRHandsontable(rhandsontable(mapic_df, readOnly = F))
    })
    observeEvent(input$bt_add_manually, {
      print("added")
    })
  })
}


## To be copied in the UI
# mod_coords_ui("coords")

## To be copied in the server
# mod_coords_server("coords")
