#' Fit candidate distributions
#'
#' Internal wrapper around EnvStats::distChoose().
#'
#' @inheritParams est_dist
#' @param x Numeric vector.
#'
#' @return An EnvStats distChoose object.
#'
#' @noRd
.fit_dist <- function(
  x,
  alpha,
  method,
  choices
) {
  EnvStats::distChoose(
    y = x,
    alpha = alpha,
    method = method,
    choices = choices
  )
}

#' Fit candidate distributions to censored data
#'
#' Internal wrapper around EnvStats::distChooseCensored().
#'
#' @param x Numeric vector.
#' @param censored Logical censoring indicator.
#' @param alpha Significance level.
#' @param method Goodness-of-fit test method.
#' @param choices Candidate distributions.
#'
#' @return An EnvStats distChooseCensored object.
#'
#' @noRd
.fit_dist_censored <- function(
  x,
  censored,
  alpha,
  method,
  choices
) {
  EnvStats::distChooseCensored(
    x = x,
    censored = censored,
    censoring.side = "left",
    alpha = alpha,
    method = method,
    choices = choices
  )
}

#' Estimate distributions by group
#'
#' Internal worker for est_dist().
#'
#' @inheritParams est_dist
#' @param min_n Minimum sample size required for estimation.
#'
#' @return A tibble with one row per group.
#'
#' @noRd
#' @noRd
.est_dist_worker <- function(
  df,
  return_result,
  min_n,
  alpha,
  method,
  choices
) {
  df %>%
    dplyr::group_modify(
      ~ {
        group_data <- .x

        x <- stats::na.omit(
          .x$analysis_result
        )

        n <- length(x)

        if (n < min_n) {
          result <- .make_dist_result(
            distribution = "insufficient_data",
            n = n,
            return_result = return_result
          )

          return(
            .attach_nested_data(
              result = result,
              data = group_data
            )
          )
        }

        fit_result <- purrr::possibly(
          .fit_dist,
          otherwise = NULL
        )(
          x = x,
          alpha = alpha,
          method = method,
          choices = choices
        )

        result <- .handle_dist_result(
          result = fit_result,
          n = n,
          return_result = return_result
        )

        .attach_nested_data(
          result = result,
          data = group_data
        )
      }
    )
}

#' Estimate distributions for censored data by group
#'
#' Internal worker for est_dist_censored().
#'
#' @inheritParams .est_dist_worker
#'
#' @return A tibble with one row per group.
#'
#' @noRd
#' @noRd
.est_dist_censored_worker <- function(
  df,
  return_result,
  min_n,
  alpha,
  method,
  choices
) {
  df %>%
    dplyr::group_modify(
      ~ {
        group_data <- .x

        x <- .x$analysis_result
        censored <- .x$left_censored

        keep <- !is.na(x)

        x <- x[keep]
        censored <- censored[keep]

        n <- length(x)

        if (n < min_n) {
          result <- .make_dist_result(
            distribution = "insufficient_data",
            n = n,
            return_result = return_result
          )

          return(
            .attach_nested_data(
              result = result,
              data = group_data
            )
          )
        }

        if (any(is.na(censored))) {
          rlang::abort(
            "left_censored cannot contain missing values."
          )
        }

        fit_result <- if (!any(censored)) {
          purrr::possibly(
            .fit_dist,
            otherwise = NULL
          )(
            x = x,
            alpha = alpha,
            method = method,
            choices = choices
          )
        } else {
          purrr::possibly(
            .fit_dist_censored,
            otherwise = NULL
          )(
            x = x,
            censored = censored,
            alpha = alpha,
            method = method,
            choices = choices
          )
        }

        result <- .handle_dist_result(
          result = fit_result,
          n = n,
          return_result = return_result
        )

        .attach_nested_data(
          result = result,
          data = group_data
        )
      }
    )
}
