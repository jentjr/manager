timeSeriesInput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("time_series"))
}