#' Module maps ui
#' 
#' @param id Unique id for module instance.
#'
#' @import shiny
#' @importFrom DT DTOutput
#' @importFrom shinyWidgets noUiSliderInput
#' @importFrom ggplot2 map_data
#' @noRd
mod_maps_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(h2("Maps")),

    fluidRow( # First row, buttons
      column(4,
             column(6, actionButton(NS(id, "btn_render_map"), "Render Map"))
             ),
      column(6, offset = 2,
             fluidRow(
               sliderInput(NS(id, "slid_year"), "Year",
                           min = 1950,
                           max = as.numeric(format(Sys.time(), "%Y")),
                           value = as.numeric(format(Sys.time(), "%Y")),                          
                           animate = TRUE)
             ))
    ),
    
    ## Second row, table and map
    fluidRow(
      column(4,
             DT::DTOutput(NS(id, "dt_data_map"))),
      column(7, offset = 1,
             column(1,
                    br(),
                    br(),
                    noUiSliderInput(
                      inputId = NS(id, "sld_lat"), label = "Latitude",
                      min = min(map_data("world")$lat), max = max(map_data("world")$lat),
                      value = c(min(map_data("world")$lat), max(map_data("world")$lat)),
                      direction = "rtl",
                      margin = 1,
                      update_on = "end",
                      orientation = "vertical",
                      width = "1px", height = "400px")),
             column(11,
                    fluidRow(
                      column(10, offset = 2,
                      noUiSliderInput(
                        inputId = NS(id, "sld_lon"),
                        label = "Longitude",
                        min = min(map_data("world")$long), max = max(map_data("world")$long),
                        value = c(min(map_data("world")$long), max(map_data("world")$long)),
                        update_on = "end",
                        width = "600px",
                        margin = 1)),
                      plotOutput(NS(id, "plt_map"),
                                 dblclick = NS(id, "plt_map_dblclick"),
                                 brush = brushOpts(
                                   id = NS(id, "plt_map_selected"),
                                   resetOnNew = TRUE))
                    ))
             )
    ),

    ## Third row, saving and labels
    fluidRow(
      column(5, p("Reserved section")),
      column(7,
             column(6,
                    textInput(NS(id, "txi_year_lab"),
                              h5("Year Label"), 
                              value = "Year")),
             column(6,
                    textInput(NS(id, "txi_total_lab"),
                              h5("Total Label"), 
                              value = "Total"))
             )
    )
  )
}


#' Module maps Server Functions
#'
#' @import ggplot2
#' @importFrom shinyWidgets updateNoUiSliderInput
#' @noRd
mod_maps_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## Reactive values: coords
    ranges <- reactiveValues(
      xlon = c(min(map_data("world")$lon), max(map_data("world")$lon)),
      ylat = c(min(map_data("world")$lat), max(map_data("world")$lat)),
      x = NULL, y = NULL)

    observeEvent(input$sld_lon, {
      ranges$xlon <- input$sld_lon
    })
    observeEvent(input$sld_lat, {
      ranges$ylat <- input$sld_lat
    })
    observeEvent(input$plt_map_dblclick, {
      brush <- input$plt_map_selected
      if (!is.null(brush)) {
        ranges$xlon <- c(brush$xmin, brush$xmax)
        ranges$ylat <- c(brush$ymin, brush$ymax)
      } else {
        ranges$xlon <- c(min(map_data("world")$lon), max(map_data("world")$lon))
        ranges$ylat <- c(min(map_data("world")$lat), max(map_data("world")$lat))
      }
      shinyWidgets::updateNoUiSliderInput(inputId = "sld_lon", value = ranges$xlon)
      shinyWidgets::updateNoUiSliderInput(inputId = "sld_lat", value = ranges$ylat)
    })

    ## Initial plot and DT
    output$plt_map <- renderPlot({
      ggplot() +
        geom_polygon(data = map_data("world"),
                     aes(x = long, y = lat, group = group)) +
        coord_fixed(1.3,
                    xlim = ranges$xlon,
                    ylim = ranges$ylat)
    })

    ## call the data
    reactive_from_intro <- get("reactive_from_intro", envir = mapic_env)
    output$dt_data_map <- DT::renderDT({ reactive_from_intro$current_data })

    ## Render map

  })
}


## To be copied in the UI
# mod_maps_ui("maps")

## To be copied in the server
# mod_maps_server("maps")

