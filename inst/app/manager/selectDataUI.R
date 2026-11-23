selectDataUI <- function(id, multiple = TRUE) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("selectSites")),
    shiny::uiOutput(ns("selectLocations")),
    shiny::uiOutput(ns("selectConstituents"))
  )
}
