# Module UI Function
userFileInput <- function(id, label = "Choose MANAGES 3.x Site.mdb file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
      fileInput(ns("managesfile"), label,
                accept = ".mdb")
  )
}