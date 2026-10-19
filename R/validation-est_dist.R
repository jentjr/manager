#' Validate est_dist arguments
#'
#' Internal helper for validating distribution estimation arguments.
#'
#' @param alpha Significance level.
#' @param method Goodness-of-fit test method.
#' @param choices Candidate distributions.
#' @param return_result Logical indicating whether to return the
#' EnvStats result object.
#'
#' @return Invisibly returns TRUE.
#'
#' @noRd
.validate_est_dist_args <- function(
  alpha,
  method,
  choices,
  return_result
) {
  if (!is.logical(return_result) || length(return_result) != 1) {
    rlang::abort(
      "return_result must be TRUE or FALSE."
    )
  }

  if (
    !is.numeric(alpha) ||
      length(alpha) != 1 ||
      alpha <= 0 ||
      alpha >= 1
  ) {
    rlang::abort(
      "alpha must be a single numeric value between 0 and 1."
    )
  }

  if (!is.character(method) || length(method) != 1) {
    rlang::abort(
      "method must be a single character value."
    )
  }

  if (!is.character(choices) || length(choices) < 1) {
    rlang::abort(
      "choices must be a non-empty character vector."
    )
  }

  invisible(TRUE)
}
