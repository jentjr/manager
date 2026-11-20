#' Internal helper: validate a guidance profile.
#' @keywords internal
.validate_guidance_profile <- function(profile) {
  required <- c(
    "low_threshold",
    "moderate_threshold",
    "high_threshold"
  )

  if (!is.list(profile) || !all(required %in% names(profile))) {
    rlang::abort(
      paste(
        "`profile` must contain:",
        paste(required, collapse = ", ")
      )
    )
  }

  thresholds <- unlist(profile[required], use.names = FALSE)

  if (
    !is.numeric(thresholds) ||
      length(thresholds) != 3L ||
      anyNA(thresholds) ||
      any(thresholds < 0 | thresholds > 100) ||
      is.unsorted(thresholds, strictly = TRUE)
  ) {
    rlang::abort(
      paste0(
        "Guidance thresholds must be three strictly increasing ",
        "percentages between 0 and 100."
      )
    )
  }

  invisible(TRUE)
}


#' Internal helper: classify censoring percentages.
#'
#' Boundary behavior:
#'   [0, low_threshold)                 low_censoring
#'   [low_threshold, moderate_threshold) moderate_censoring
#'   [moderate_threshold, high_threshold) high_censoring
#'   [high_threshold, 100]              extreme_censoring
#'
#' Missing percentages remain NA.
#' @keywords internal
.classify_censoring <- function(
  percent_censored,
  profile = guidance_options()
) {
  .validate_guidance_profile(profile)

  if (!is.numeric(percent_censored)) {
    rlang::abort("`percent_censored` must be numeric.")
  }

  invalid <- !is.na(percent_censored) &
    (percent_censored < 0 | percent_censored > 100)

  if (any(invalid)) {
    rlang::abort(
      "`percent_censored` must contain percentages from 0 to 100."
    )
  }

  low <- profile$low_threshold
  moderate <- profile$moderate_threshold
  high <- profile$high_threshold

  dplyr::case_when(
    is.na(percent_censored) ~ NA_character_,
    percent_censored < low ~ "low_censoring",
    percent_censored < moderate ~ "moderate_censoring",
    percent_censored < high ~ "high_censoring",
    TRUE ~ "extreme_censoring"
  )
}
