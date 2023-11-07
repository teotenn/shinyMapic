#' Module intro ui
#' 
#' @param id Unique id for module instance.
#'
#' @import shiny
#' @importFrom DT DTOutput
#' @noRd
mod_intro_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(h2("Intro")),
    fluidRow(
      column(6,
             h4("Filters"),
             selectInput(NS(id, "select_country"),
                         h5("Filter by Country"), 
                         choices = list("Choice 1" = 1, "Choice 2" = 2,
                                        "Choice 3" = 3), selected = 1)
             ),
      column(6,
             h4("Source"),
             column(6,
                    selectInput(
                      NS(id, "select_source"),
                      h5("Select the source"), 
                      choices = list("Data Base" = "db", "Session" = "sess"),
                      selected = 1)
                    ),
             column(6,
                    textInput(
                      NS(id, "in_session_id"),
                      h5("Session ID"), 
                      value = "ID no..."),
                    actionButton(NS(id, "bt_load_session"), "Load Session")
                    )
             ),
      actionButton(NS(id, "bt_load_filtered_data"), "Load Data"),
      DT::DTOutput(NS(id, "dt_main_view"))
    )
  )
}


#' Module Intro Server Functions
#'
#' @importFrom DT renderDT
#' @noRd
mod_intro_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$bt_load_filtered_data, {
      output$dt_main_view <- DT::renderDT({
        data.frame(id = 1:200,
                   country = "MX",
                   city = letters[1:20],
                   state = NA,
                   county = NA,
                   region = NA,
                   start_year = c(1990, 1995, 2001, 2010, 2020),
                   end_year = NA)
      })
    })
  })
}


## To be copied in the UI
# mod_intro_ui("intro")

## To be copied in the server
# mod_intro_server("intro")
