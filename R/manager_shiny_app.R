#' Function to launch shiny app for MANAGER
#' 
#' @export

manager  <- function() {
  
  shiny::runApp(system.file('app/manager', package = 'manager'))
  
}