#' funciton to launch shiny app
#' 
groundwater_app  <- function(){
  
  shiny::runApp(system.file('app', package='groundwater'))
  
}