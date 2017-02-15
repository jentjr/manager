# Module UI Function
boxplotInput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("selectBoxplot"))
}