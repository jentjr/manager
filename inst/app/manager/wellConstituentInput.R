# Module UI Function
wellConstituentInput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("selectWellConstituent"))
}