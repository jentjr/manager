#' @export
print.env_schema <- function(x, ...) {
  cat("<env_schema>\n")

  cat(
    nrow(x),
    "rows x",
    ncol(x),
    "columns\n"
  )

  if (".location" %in% names(x)) {
    cat(
      "Locations:",
      dplyr::n_distinct(x$.location),
      "\n"
    )
  }

  if (".analyte" %in% names(x)) {
    cat(
      "Analytes:",
      dplyr::n_distinct(x$.analyte),
      "\n"
    )
  }

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
