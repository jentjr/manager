#' Construct distribution estimation result
#'
#' Internal helper used by est_dist() and est_dist_censored().
#'
#' @param distribution Selected distribution.
#' @param n Number of observations used in estimation.
#' @param result EnvStats result object.
#' @param return_result Logical indicating whether to include the
#' result object in the output.
#'
#' @return A tibble with distribution metadata.
#'
#' @noRd
.make_dist_result <- function(
  distribution,
  n,
  result = NULL,
  return_result = FALSE
) {
  out <- tibble::tibble(
    distribution = distribution,
    n = n
  )

  if (return_result) {
    out$result <- list(result)
  }

  out
}

#' Helper function for est_dist() and est_dist_censored()
#'
#' @noRd
.handle_dist_result <- function(
  result,
  n,
  return_result
) {
  if (is.null(result)) {
    return(
      .make_dist_result(
        "estimation_failed",
        n,
        return_result = return_result
      )
    )
  }

  .make_dist_result(
    distribution = paste(
      result$decision,
      collapse = ", "
    ),
    n = n,
    result = result,
    return_result = return_result
  )
}

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
