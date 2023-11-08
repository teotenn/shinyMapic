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
             )
             )
    ),
    
    ## Second row, table and map
    fluidRow(
      column(4,
             DT::DTOutput(NS(id, "dt_data_map"))),
      column(7, offset = 1,
             column(1,
                    br(),
                    br(),
                    br(),
                    br(),
                    noUiSliderInput(
                      inputId = NS(id, "sld_lat"), label = "Latitude",
                      min = min(map_data("world")$lat), max = max(map_data("world")$lat),
                      value = c(min(map_data("world")$lat), max(map_data("world")$lat)),
                      margin = 1,
                      update_on = "end",
                      orientation = "vertical",
                      width = "10px", height = "300px")),
             column(11,
                    fluidRow(
                      column(10, offset = 2,
                      noUiSliderInput(
                        inputId = NS(id, "sld_lon"),
                        label = "Longitude",
                        min = min(map_data("world")$long), max = max(map_data("world")$long),
                        value = c(min(map_data("world")$long), max(map_data("world")$long)),
                        update_on = "end",
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
#' @noRd
mod_maps_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    y_limits <- reactive(input$sld_lat)
    x_limits <- reactive(input$sld_lon)

    ## Init plot and DT
    output$plt_map <- renderPlot({
      ggplot() +
        geom_polygon(data = map_data("world"),
                     aes(x = long, y = lat, group = group)) +
        coord_fixed(1.3,
                    xlim = x_limits(),
                    ylim = y_limits())
    })
    
    output$dt_data_map <- DT::renderDT({
        data.frame(id = 1:200,
                   country = "MX",
                   city = letters[1:20],
                   state = NA,
                   county = NA,
                   region = NA,
                   start_year = c(1990, 1995, 2001, 2010, 2020),
                   end_year = NA)
    })

    ## Reactivity
    ranges <- reactiveValues(x = NULL, y = NULL)

    observeEvent(input$btn_render_map, {
      output$plt_map <- renderPlot({
        ggplot(mtcars, aes(wt, mpg)) +
          geom_point() +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
      })
    })

    ## When a double-click happens, check if there's a brush on the plot.
    ## If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$plt_map_dblclick, {
      brush <- input$plt_map_selected
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })

  })
}


## To be copied in the UI
# mod_maps_ui("maps")

## To be copied in the server
# mod_maps_server("maps")

