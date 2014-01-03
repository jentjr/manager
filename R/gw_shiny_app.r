#' function to launch shiny groundwater app
#' 
#' @export
groundwater_app  <- function(){
  shiny::runApp(system.file('app/groundwater_plots', package='groundwater'))
}

#' function to launch shiny geochemistry plot app
#' 
#' @export
geochemistry_app <- function(){
  shiny::runApp(system.file('app/geochemistry_plots', package='groundwater'))
}