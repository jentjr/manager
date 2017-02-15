# Module UI Function
timeSeriesInput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("selectTimeSeries"))
}