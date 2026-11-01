#' Attach nested group data
#'
#' @param result Result tibble.
#' @param data Nested group data.
#'
#' @return Tibble with data list-column.
#'
#' @noRd
.attach_nested_data <- function(result, data) {
  result$data <- list(data)
  
  result
}
