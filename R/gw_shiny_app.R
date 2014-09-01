#' function to launch shiny app
#' 
#' @export
gwstats  <- function(){
  shiny::runApp(system.file('app/gwstats', package='gwstats'))
}
