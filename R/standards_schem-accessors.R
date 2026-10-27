#' Test if object is a standards schema
#'
#' @param x Object to test.
#'
#' @returns Logical scalar.
#'
#' @keywords internal
is_standards_schema <- function(x) {
  inherits(x, "standards_schema")
}
