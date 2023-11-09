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
                    actionButton(NS(id, "btn_load_session"), "Load Session")
                    )
             ),
      actionButton(NS(id, "btn_load_filtered_data"), "Load Data"),
      DT::DTOutput(NS(id, "dt_main_view"))
    )
  )
}


#' Module Intro Server Functions
#'
#' @importFrom DT renderDT
#' @importFrom mapic db_load
#' @noRd
mod_intro_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    reactive_from_intro <- reactiveValues(current_data = data.frame())

    ## SESSION ID
    sID <- paste(
      "TBL",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      sample.int(999, 1), sep = "_")
    observe({reactive_from_intro$session_id <- sID})
    mapic_dbconf_tbl(sID)

    assign("reactive_from_intro", reactive_from_intro, envir = mapic_env)

    ## Change session ID 
    observeEvent(input$btn_load_session, {
      ##### TODO: Add validators/error handlers for input ID <---------------------------------|
      reactive_from_intro$session_id <- input$in_session_id
      assign("reactive_from_intro", reactive_from_intro, envir = mapic_env)
      mapic_dbconf_tbl(input$in_session_id)
      ##### TODO: The button needs notification to say that session was loaded <----------------|
    })

    observeEvent(input$btn_load_filtered_data, {
      ## Data from session
      if (input$select_source == "sess") {
        current_mdb <- get("mdb_local_db", envir = mapic_env)
        reactive_from_intro$current_data <- db_load(current_mdb)
        ## Data from DB
      } else if (input$select_source == "db") {
        reactive_from_intro$current_data <- data.frame(id = 1:200,
                   country = "MX",
                   city = letters[1:20],
                   state = NA,
                   county = NA,
                   region = NA,
                   start_year = c(1990, 1995, 2001, 2010, 2020),
                   end_year = NA)
        ## Else, safety net
      } else {
        reactive_from_intro$current_data <- data.frame()
      }

      output$dt_main_view <- DT::renderDT({
        reactive_from_intro$current_data
      })

      ## Place the currently used data in the mapic_env
      assign("reactive_from_intro", reactive_from_intro, envir = mapic_env)
    })
  })
}


## To be copied in the UI
# mod_intro_ui("intro")

## To be copied in the server
# mod_intro_server("intro")
