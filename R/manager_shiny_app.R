#' function to launch shiny app
#' 
#' @export
manager  <- function(){
  shiny::runApp(system.file('app/manager', package = 'manager'))
}
