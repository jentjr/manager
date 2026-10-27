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
    dplyr::n_distinct(location(x)),
    "\n"
  )

  cat(
    "Analytes:",
    dplyr::n_distinct(analyte(x)),
    "\n"
  )

  cat(
    "Valid:",
    has_env_schema_structure(x),
    "\n"
  )

  cat(
    "Version:",
    attr(
      x,
      "env_schema_version",
      exact = TRUE
    ),
    "\n"
  )

  invisible(x)
}
