selectDataUI <- function(id, multiple) {
  
  ns <- NS(id)
  
  tagList(
     
    uiOutput(ns("selectSites")),
     
    uiOutput(ns("selectLocations")),

    uiOutput(ns("selectConstituents"))
    
  )

  
}