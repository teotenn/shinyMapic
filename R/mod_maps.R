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
    div(
      fluidRow( # First row, DT
        DT::DTOutput(NS(id, "dt_data_map"))
      )
    ),

    div(
      fluidRow( # 2nd row control btns
        br(),
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
      )
    ),
    
    div(
      fluidRow( # 3rd row map
        br(),
        column(
          1,
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
            width = "1px", height = "400px")
        ),
        column(
          11,
          fluidRow(
            column(10, offset = 2,
                   noUiSliderInput(
                     inputId = NS(id, "sld_lon"),
                     label = "Longitude",
                     min = min(map_data("world")$long), max = max(map_data("world")$long),
                     value = c(min(map_data("world")$long), max(map_data("world")$long)),
                     update_on = "end",
                     width = "800px",
                     margin = 1))
          ),
          div(
            fluidRow(
              ## Main map
              plotOutput(
                NS(id, "plt_map"),
                dblclick = NS(id, "plt_map_dblclick"),
                brush = brushOpts(
                  id = NS(id, "plt_map_selected"),
                  resetOnNew = TRUE)),
              ## Labels
              plotOutput(NS(id, "plt_labels"), height = "150px", width = "80%")
            )
          )
        )
      )
    ),

    div(
      fluidRow( # 4yh lab controls and save
        br(),
        column(3, p("Reserved section")),
        column(
          6,
          column(
            6,
            textInput(NS(id, "txi_year_lab"),
                      h5("Year Label"), 
                      value = "Year")),
          column(
            6,
            textInput(NS(id, "txi_total_lab"),
                      h5("Total Label"), 
                      value = "Total"))
        ),
        column(3, p("Saving section"))
      )
    )
    
  )
}


#' Module maps Server Functions
#'
#' @import ggplot2
#' @importFrom shinyWidgets updateNoUiSliderInput
#' @importFrom mapic base_map mapic_city_dots mapic_year_external mapic_totals_external mapic_city_names
#' @importFrom cowplot plot_grid
#' @noRd
mod_maps_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## Reactive values: coords
    ranges <- reactiveValues(
      xlon = c(min(map_data("world")$lon), max(map_data("world")$lon)),
      ylat = c(min(map_data("world")$lat), max(map_data("world")$lat)))

    ## Coords manipulation
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
    observeEvent(input$btn_render_map, {
      country_name <- na.omit( ## TODO: Add error handling when country not found <--------|
        with(countries_naming_conventions,
             ifelse(alpha.2 == reactive_from_intro$current_data$country, common_name, NA)))[1]

      the_map <- base_map(country_name,
                          ranges$xlon,
                          ranges$ylat) |>
        mapic_city_dots(reactive_from_intro$current_data,
                        year = input$slid_year) |>
        ## mapic_city_names(c("Ciudad de Mexico", "Guadalajara", "Tijuana")) |> ## TODO <------|
        mapic_year_external(year_label = input$txi_year_lab) |>
        mapic_totals_external(totals_label = input$txi_total_lab)

      the_labels <- plot_grid(the_map$mapic_year, the_map$mapic_totals, the_map$legend,
                              nrow = 1, rel_widths = c(0.5, 0.5, 1.26))

      output$plt_map <- renderPlot({ ## TODO: Bug with coords when using mouse event <--------|
        ## The bug could probably due to the message "Coordinate system already present..."
        ## because it changes the values of x,y from -119,82 (MX) to 0.213,0.315 <------------|
        the_map$mapic
      })

      output$plt_labels <- renderPlot({ the_labels })
    })

  })
}


## To be copied in the UI
# mod_maps_ui("maps")

## To be copied in the server
# mod_maps_server("maps")

