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
                         choices = list("All" = 1),
                         ## multiple = TRUE,
                         selected = 1)
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
      actionButton(NS(id, "btn_load_data"), "Load Data"),
      actionButton(NS(id, "btn_send_session_to_db"), "Send Data Session to DB"),
      DT::DTOutput(NS(id, "dt_main_view"))
    )
  )
}


#' Module Intro Server Functions
#'
#' @import shiny
#' @importFrom dplyr %>%
#' @importFrom DT renderDT
#' @importFrom mapic db_load
#' @importFrom DBI dbExecute dbDisconnect dbWriteTable dbReadTable
#' @noRd
mod_intro_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    reactive_from_intro <- reactiveValues(current_data = data.frame())

    ## Load countries options
    mapic_env$main_db_connect()
    countries_list <- dplyr::tbl(mapic_env$con_main_db, mapic_env$main_table_name) %>%
      dplyr::group_by(country) %>%
      dplyr::summarize(mean(lon)) %>%
      dplyr::select(country) %>%
      dplyr::collect()
    DBI::dbDisconnect(mapic_env$con_main_db)
    ## Update list of countries
    observe({
      updateSelectInput(
        session,
        "select_country",
        choices = append("All", countries_list[1]$country))
    })

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
      if (input$in_session_id != "ID no..." & input$in_session_id != "") {
        ##### TODO: Add validators/error handlers for input ID <---------------|
        reactive_from_intro$session_id <- input$in_session_id
        assign("reactive_from_intro", reactive_from_intro, envir = mapic_env)
      }
      mapic_dbconf_tbl(reactive_from_intro$session_id)
      ## TODO: The button needs notification to say that session was loaded <--|
    })

    observeEvent(input$btn_load_data, {
      ## Data from session
      if (input$select_source == "sess") {
        ##### TODO: Add error handlers for non existent table <---------------|
        current_mdb <- get("mdb_local_db", envir = mapic_env)
        reactive_from_intro$current_data <- db_load(current_mdb)
        
        ## Filtered data from DB
      } else if (input$select_source == "db") {
        mapic_env$main_db_connect()
        if ("All" %in% input$select_country) {
          loaded_data <- dbReadTable(mapic_env$con_main_db, mapic_env$main_table_name)
        } else {
          list_countries_to_load <- input$select_country         
          loaded_data <- dplyr::tbl(mapic_env$con_main_db, mapic_env$main_table_name) %>%
            dplyr::filter(country == list_countries_to_load) %>%
            dplyr::collect()
        }
        DBI::dbDisconnect(mapic_env$con_main_db)        
        reactive_from_intro$current_data <- loaded_data
        
        ## Else, safety net
      } else {
        reactive_from_intro$current_data <- data.frame()
      }

      ## Render the data as DT
      output$dt_main_view <- DT::renderDT({
        reactive_from_intro$current_data
      })

      ## Place the currently used data in the mapic_env
      assign("reactive_from_intro", reactive_from_intro, envir = mapic_env)
    })

    ## Send session data to the main DB
    observeEvent(input$btn_send_session_to_db, {
      if (input$select_source == "sess") {
        ## TODO: Send notification that data was sent <--------------------|
        mapic_env$main_db_connect()
        DBI::dbWriteTable(
          mapic_env$con_main_db,
          mapic_env$main_table_name,
          mapic_env$reactive_from_intro$current_data,
          append = TRUE)
        DBI::dbDisconnect(mapic_env$con_main_db)
      }
      ## TODO: Send warning if not in session option <----------------------|
    })
  })
}


## To be copied in the UI
# mod_intro_ui("intro")

## To be copied in the server
# mod_intro_server("intro")
