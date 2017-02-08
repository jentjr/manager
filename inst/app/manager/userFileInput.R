# Module UI Function
userFileInput <- function(id, label = "File Input") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    
    selectInput(
      ns('fileInputType'), "Data Input Type", 
      choices = c(csv = "csv", 
                  manages = "manages")
    ),
    
    conditionalPanel(
      sprintf("input['%s'] == 'csv'", ns("fileInputType")),
      textInput(ns("csv_date"), label = "Date format", value = "mdy"),
      fileInput(ns("csvfile"), label)
    ),
    
    conditionalPanel(
      sprintf("input['%s'] == 'manages'", ns("fileInputType")),
      fileInput(ns("managesfile"), label)
    )
  )
}