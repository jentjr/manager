# Module UI Function
managesSiteFileInput <- function(id, label = "Manages Site file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label)
  )
}