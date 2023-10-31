#' Run app
#' 
#' Run application
#' 
#' @param ... Additional parameters to pass to [shiny::shinyApp].
#' 
#' @importFrom shiny shinyApp
#' 
#' @export 
run_app <- function(...){
  shinyApp(
    ui = ui,
    server = server,
    ...
  )
}
