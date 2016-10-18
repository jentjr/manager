# Module UI Function
csvFileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Has heading", value = TRUE),
    selectInput(ns("quote"), "Quote", c(
      "Double quote" = "\"",
      "Single quote" = "'"
    ))
  )
}