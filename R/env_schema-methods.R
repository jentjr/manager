#' @export
print.env_schema <- function(x, ...) {
  cat("<env_schema>\n")

  cat(
    nrow(x),
    "rows x",
    ncol(x),
    "columns\n"
  )

  cat(
    "Locations:",
    dplyr::n_distinct(x$.location),
    "\n"
  )

  cat(
    "Analytes:",
    dplyr::n_distinct(x$.analyte),
    "\n"
  )

  cat(
    "Version:",
    schema_version(x),
    "\n"
  )

  invisible(x)
}
