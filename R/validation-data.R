#' Validate est_dist Output
#'
#' Checks that a data frame contains the required structure produced by
#' `est_dist()`, including:
#'
#' * a `distribution` column
#' * a `data` list-column
#' * nested data frames containing `analysis_result`
#' * supported distribution values
#'
#' This validator ensures compatibility with downstream functions such
#' as:
#'
#' * `pred_int()`
#' * `tol_int()`
#' * control chart calculations
#'
#' @param df A data frame returned by `est_dist()`.
#'
#' @return Invisibly returns `df`.
#'
#' @keywords internal

.validate_est_dist_output <- function(df) {
  required_cols <- c(
    "distribution",
    "n",
    "data"
  )

  missing_cols <- setdiff(
    required_cols,
    names(df)
  )

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "Input must be the output of {.fn est_dist}.",
        "x" = "Missing column(s): {.val {missing_cols}}."
      )
    )
  }

  if (!inherits(df$data, "list")) {
    cli::cli_abort(
      c(
        "Input must be the output of {.fn est_dist}.",
        "x" = "Column {.field data} must be a list-column."
      )
    )
  }

  empty_data <- purrr::map_lgl(
    df$data,
    is.null
  )

  if (any(empty_data)) {
    cli::cli_abort(
      c(
        "Invalid nested data detected.",
        "x" = "{sum(empty_data)} row(s) contain a NULL {.field data} object."
      )
    )
  }

  has_analysis_result <- purrr::map_lgl(
    df$data,
    ~ "analysis_result" %in% names(.x)
  )

  if (!all(has_analysis_result)) {
    cli::cli_abort(
      c(
        "Invalid nested data detected.",
        "x" = paste(
          "Each nested data frame must contain",
          "{.field analysis_result}."
        )
      )
    )
  }

  valid_distributions <- c(
    "Normal",
    "Lognormal",
    "Nonparametric"
  )

  invalid_distributions <- setdiff(
    unique(stats::na.omit(df$distribution)),
    valid_distributions
  )

  if (length(invalid_distributions) > 0) {
    cli::cli_abort(
      c(
        "Unsupported distribution detected.",
        "x" = "Found {.val {invalid_distributions}}.",
        "i" = "Valid values are {.val {valid_distributions}}."
      )
    )
  }

  invisible(df)
}
