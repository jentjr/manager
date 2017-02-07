# Module UI Function
userFileInput <- function(id, label = "File Input") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    
    selectInput(
      ns('fileInputType'), "Data Input Type", 
      choices = c(manages = "manages",
                  csv = "csv")
    ),
    
    conditionalPanel(
      sprintf("input['%s'] == 'manages'", ns("fileInputType")),
      fileInput(ns("managesfile"), label)
    ),

    conditionalPanel(
      sprintf("input['%s'] == 'csv'", ns("fileInputType")),
      fileInput(ns("csvfile"), label)
    )
  )
}