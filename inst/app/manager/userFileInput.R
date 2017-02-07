# Module UI Function
userFileInput <- function(id, label = "File Input") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    
    selectInput(
      ns('fileInputType'), "Data Input Type", 
      choices = c(MANAGES = "manages",
                  csv = "csv")
      ),
    
    conditionalPanel(
      paste0("input['", ns("fileInputType"), "'] == 'manages' "),
      fileInput(ns("file"), label)
    ),

    conditionalPanel(
      paste0("input['", ns("fileInputType"), "'] == 'csv' "),
      checkboxInput(ns("heading"), "Has heading", value = TRUE),
      selectInput(ns("quote"), "Quote",
                  c("Double quote" = "\"", "Single quote" = "'")
                  ),
      fileInput(ns("file"), label)
    )
  )
}