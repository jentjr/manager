#' Statistical Decision Guidance
#'
#' Selects recommended statistical methods based on sample size and
#' the degree of censoring.
#'
#' This function implements a rule-based statistical decision framework
#' for environmental monitoring data. Recommendations are based on the
#' percentage of censored observations and the number of observations
#' available for evaluation.
#'
#' Returned guidance can be used to select:
#'
#' \itemize{
#'   \item distribution estimation methods
#'   \item trend analysis approaches
#'   \item censored-data handling methods
#'   \item interval estimation settings
#' }
#'
#' The default profile follows EPA-style guidance in which
#' parametric methods are preferred for low censoring, regression-on-order
#' statistics (ROS) is preferred for moderate censoring, and
#' nonparametric methods are preferred when censoring becomes extensive.
#'
#' @param percent_censored Numeric percentage of censored observations.
#' @param n_used Number of non-missing observations available for
#'   statistical evaluation.
#' @param n_detect Number of detected (non-censored) observations.
#' @param profile A guidance profile created by
#'   [guidance_profile()].
#'
#' @returns
#' A tibble with class `"statistical_decision"`.
#'
#' The returned object contains:
#'
#' \describe{
#'   \item{n_used}{
#'     Number of analytical results used in evaluation.
#'   }
#'
#'   \item{n_detect}{
#'     Number of uncensored observations.
#'   }
#'
#'   \item{percent_censored}{
#'     Percentage of observations classified as censored.
#'   }
#'
#'   \item{censoring_class}{
#'     Censoring category:
#'     `"low_censoring"`,
#'     `"moderate_censoring"`,
#'     `"high_censoring"`,
#'     or `"extreme_censoring"`.
#'   }
#'
#'   \item{distribution_method}{
#'     Recommended distributional analysis approach.
#'   }
#'
#'   \item{trend_method}{
#'     Recommended trend evaluation method.
#'   }
#'
#'   \item{trend_guidance}{
#'     Recommended interpretation of trend results.
#'   }
#'
#'   \item{censored_method}{
#'     Recommended censored-data treatment method for trend analyses.
#'   }
#'
#'   \item{distribution_config}{
#'     Configuration list suitable for
#'     [estimate_distribution()].
#'   }
#'
#'   \item{interval_config}{
#'     Configuration list suitable for interval estimation
#'     functions.
#'   }
#'   \item{interval_method}{
#'     Recommended interval method.
#'   }
#'   \item{decision_basis}{
#'     Primary basis for the recommendation.
#'   }
#'   \item{comparison_method}{
#'     Recommended multiple-group comparison procedure.
#'   }
#' }
#'
#' @details
#' Distribution recommendations are assigned as follows:
#'
#' \tabular{lll}{
#' Condition \tab Recommendation \tab Method \cr
#' n_used < 4 \tab insufficient_data \tab no distribution fit \cr
#' low censoring \tab parametric \tab normal/lognormal evaluation \cr
#' moderate censoring \tab ros_censored \tab censored distribution fitting \cr
#' high censoring \tab nonparametric \tab nonparametric methods \cr
#' extreme censoring \tab descriptive_only \tab descriptive summaries only
#' }
#'
#' Trend recommendations always use a Mann-Kendall framework,
#' although interpretation guidance may recommend screening-level
#' evaluation when censoring is extensive.
#'
#' @seealso
#' [derive_decision()],
#' [estimate_distribution()],
#' [guidance_profile()],
#' [guidance_options()]
#'
#' @examples
#' statistical_decision(
#'   percent_censored = 12,
#'   n_used = 20,
#'   n_detect = 18
#' )
#'
#' statistical_decision(
#'   percent_censored = 75,
#'   n_used = 16,
#'   n_detect = 4
#' )
#'
#' statistical_decision(
#'   percent_censored = c(5, 25, 80),
#'   n_used = c(10, 10, 10),
#'   n_detect = c(9, 7, 2)
#' )
#'
#' @export
statistical_decision <- function(
  percent_censored,
  n_used,
  n_detect,
  profile = guidance_options()
) {
  .validate_guidance_profile(profile)

  .validate_count_vector(n_used, "n_used")
  .validate_count_vector(n_detect, "n_detect")

  if (any(n_detect > n_used)) {
    rlang::abort("`n_detect` cannot exceed `n_used`.")
  }

  if (
    !is.numeric(percent_censored) ||
      any(
        !is.na(percent_censored) &
          (!is.finite(percent_censored) |
            percent_censored < 0 |
            percent_censored > 100)
      )
  ) {
    rlang::abort(
      "`percent_censored` must contain values between 0 and 100."
    )
  }

  lengths <- c(
    length(percent_censored),
    length(n_used),
    length(n_detect)
  )

  if (length(unique(lengths)) != 1L) {
    rlang::abort(
      "`percent_censored`, `n_used`, and `n_detect` must have equal lengths."
    )
  }

  censoring_class <- .classify_censoring(
    percent_censored = percent_censored,
    profile = profile
  )

  tibble::tibble(
    n_used = n_used,
    n_detect = n_detect,
    percent_censored = percent_censored,
    censoring_class = censoring_class,

    distribution_method = dplyr::case_when(
      is.na(censoring_class) ~ NA_character_,
      is.na(n_used) ~ NA_character_,
      n_used < 4L ~ "insufficient_data",
      censoring_class == "low_censoring" ~ "parametric",
      censoring_class == "moderate_censoring" ~ "ros_censored",
      censoring_class == "high_censoring" ~ "nonparametric",
      censoring_class == "extreme_censoring" ~ "descriptive_only"
    ),

    trend_method = dplyr::case_when(
      is.na(censoring_class) ~ NA_character_,
      n_used < 5L ~ "insufficient_data",
      TRUE ~ "mann_kendall"
    ),

    trend_guidance = dplyr::case_when(
      is.na(censoring_class) ~ NA_character_,
      n_used < 5L ~ "insufficient_data",
      censoring_class %in%
        c(
          "low_censoring",
          "moderate_censoring"
        ) ~ "formal_trend_test",
      TRUE ~ "screening_only"
    ),

    censored_method = dplyr::case_when(
      is.na(censoring_class) ~ NA_character_,
      censoring_class == "low_censoring" ~ "standard",
      censoring_class == "moderate_censoring" ~ "ros",
      censoring_class == "high_censoring" ~ "nonparametric",
      censoring_class == "extreme_censoring" ~ "descriptive_only"
    ),

    distribution_config = purrr::map(
      distribution_method,
      \(method) {
        if (identical(method, "parametric")) {
          list(
            method = "sf",
            choices = c("norm", "lnorm")
          )
        } else {
          list()
        }
      }
    ),

    interval_config = purrr::map(
      distribution_method,
      \(method) {
        if (
          identical(method, "parametric") ||
            identical(method, "ros_censored") ||
            identical(method, "nonparametric")
        ) {
          list(
            conf_level = 0.95,
            method = "Bonferroni",
            k = 1,
            n_mean = 1
          )
        } else {
          list()
        }
      }
    ),

    interval_method = dplyr::case_when(
      is.na(censoring_class) ~ NA_character_,
      is.na(n_used) ~ NA_character_,
      n_used < 4L ~ "insufficient_data",
      censoring_class == "low_censoring" ~ "parametric",
      censoring_class == "moderate_censoring" ~ "ros_censored",
      censoring_class == "high_censoring" ~ "nonparametric",
      censoring_class == "extreme_censoring" ~ "descriptive_only"
    ),

    comparison_method = dplyr::case_when(
      is.na(censoring_class) |
        is.na(n_used) |
        is.na(n_detect) ~ NA_character_,

      n_used < 4L ~ "insufficient_data",

      n_detect < 4L ~ "insufficient_data",

      censoring_class == "extreme_censoring" ~ "insufficient_data",

      censoring_class == "low_censoring" ~ "anova",

      censoring_class %in%
        c(
          "moderate_censoring",
          "high_censoring"
        ) ~ "censored_nonparametric",

      TRUE ~ "insufficient_data"
    ),

    decision_basis = dplyr::case_when(
      is.na(censoring_class) ~ NA_character_,
      n_used < 4L ~ "fewer_than_4_observations",
      TRUE ~ censoring_class
    )
  ) |>
    structure(
      class = c(
        "statistical_decision",
        "tbl_df",
        "tbl",
        "data.frame"
      )
    )
}

#' Derive Statistical Decision Guidance from Data
#'
#' Calculates censoring metrics from a dataset and returns a
#' corresponding [statistical_decision()] object.
#'
#' @param df Data frame containing analytical results.
#' @param ... Additional arguments passed to methods.
#'
#' @export
derive_decision <- function(df, ...) {
  UseMethod("derive_decision")
}

#' @rdname derive_decision
#'
#' @export
derive_decision.data.frame <- function(
  df,
  result_col = NULL,
  censored_col = NULL,
  ...
) {
  columns <- .resolve_analysis_columns(
    df,
    result_col,
    censored_col
  )

  .derive_decision_core(
    df = df,
    result_col = columns$result_col,
    censored_col = columns$censored_col
  )
}

#' @rdname derive_decision
#'
#' @export
derive_decision.grouped_df <- function(
  df,
  result_col = NULL,
  censored_col = NULL,
  ...
) {
  if (is.null(result_col)) {
    result_col <- if (inherits(df, "env_schema")) {
      analysis_result_var(df)
    } else {
      "analysis_result"
    }
  }

  if (is.null(censored_col)) {
    censored_col <- if (inherits(df, "env_schema")) {
      censored_var(df)
    } else {
      "censored"
    }
  }

  out <- dplyr::group_modify(
    .data = df,
    .f = ~ .derive_decision_core(
      df = .x,
      result_col = result_col,
      censored_col = censored_col
    )
  )

  out
}

#' @rdname derive_decision
#'
#' @section env_schema Method:
#'
#' For `env_schema` objects, censoring metrics are computed using
#' the standardized analytical fields:
#'
#' \itemize{
#'   \item `.analysis_result`
#'   \item `.censored`
#' }
#'
#' @export
derive_decision.env_schema <- function(
  df,
  result_col = NULL,
  censored_col = NULL,
  ...
) {
  if (is.null(result_col)) {
    result_col <- analysis_result_var(df)
  }

  if (is.null(censored_col)) {
    censored_col <- censored_var(df)
  }

  .derive_decision_core(
    df = df,
    result_col = result_col,
    censored_col = censored_col
  )
}

#' @keywords internal
.derive_decision_core <- function(
  df,
  result_col,
  censored_col
) {
  required_cols <- c(
    result_col,
    censored_col
  )

  missing_cols <- setdiff(
    required_cols,
    names(df)
  )

  if (length(missing_cols) > 0) {
    rlang::abort(
      paste(
        "Missing required columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  results <- df[[result_col]]
  censored <- df[[censored_col]]

  if (!is.numeric(results)) {
    rlang::abort(
      sprintf(
        "`%s` must be numeric.",
        result_col
      )
    )
  }

  if (!is.logical(censored)) {
    rlang::abort(
      sprintf(
        "`%s` must be a logical column.",
        censored_col
      )
    )
  }

  usable <- !is.na(results) & !is.na(censored)

  n_used <- sum(usable)

  n_detect <- sum(!censored[usable])

  percent_censored <- if (n_used == 0L) {
    NA_real_
  } else {
    mean(censored[usable]) * 100
  }

  statistical_decision(
    percent_censored = percent_censored,
    n_used = n_used,
    n_detect = n_detect
  )
}

#' Print Statistical Decision Object
#'
#' Prints a compact summary of a
#' `"statistical_decision"` object.
#'
#' @param x A `"statistical_decision"` object.
#' @param ... Unused.
#'
#' @returns
#' The input object, invisibly.
#'
#' @examples
#' decision <- statistical_decision(
#'   percent_censored = 25,
#'   n_used = 12,
#'   n_detect = 9
#' )
#'
#' print(decision)
#'
#' @export
print.statistical_decision <- function(
  x,
  ...
) {
  cat(
    "<statistical_decision>\n"
  )

  print(
    tibble::as_tibble(x)
  )

  invisible(x)
}

#' Guidance Options
#'
#' Retrieves the active statistical guidance options used to classify
#' censoring severity and select recommended analytical methods.
#'
#' Guidance options are read from the global package option
#' `"manager.guidance"`. If no option has been set, EPA-style default
#' thresholds are returned.
#'
#' These thresholds are used by [statistical_decision()] to classify
#' censoring as low, moderate, high, or extreme and to determine
#' recommended distribution fitting, trend analysis, and censored-data
#' handling approaches.
#'
#' @returns
#' A named list containing:
#'
#' \describe{
#'   \item{low_threshold}{
#'     Threshold separating low and moderate censoring.
#'   }
#'
#'   \item{moderate_threshold}{
#'     Threshold separating moderate and high censoring.
#'   }
#'
#'   \item{high_threshold}{
#'     Threshold separating high and extreme censoring.
#'   }
#' }
#'
#' @details
#' The default values are:
#'
#' \itemize{
#'   \item `low_threshold = 15`
#'   \item `moderate_threshold = 50`
#'   \item `high_threshold = 90`
#' }
#'
#' These defaults correspond to the thresholds used by the
#' `"epa_unified"` [guidance_profile()].
#'
#' Users may override the defaults using [options()]:
#'
#' \preformatted{
#' options(
#'   manager.guidance = list(
#'     low_threshold = 10,
#'     moderate_threshold = 25,
#'     high_threshold = 75
#'   )
#' )
#' }
#'
#' @examples
#' guidance_options()
#'
#' options(
#'   manager.guidance = list(
#'     low_threshold = 10,
#'     moderate_threshold = 25,
#'     high_threshold = 75
#'   )
#' )
#'
#' guidance_options()
#'
#' @seealso
#' [guidance_profile()],
#' [statistical_decision()]
#'
#' @export
guidance_options <- function() {
  getOption(
    "manager.guidance",
    guidance_profile("epa_unified")
  )
}

#' Guidance Profiles
#'
#' Returns predefined statistical decision thresholds used by
#' [statistical_decision()].
#'
#' Guidance profiles control the censoring levels used to distinguish
#' between low, moderate, high, and extreme censoring conditions.
#'
#' @param profile Character value specifying the profile.
#' One of:
#'
#' \itemize{
#'   \item `"epa_unified"`
#'   \item `"conservative"`
#'   \item `"site_specific"`
#' }
#'
#' @returns
#' A named list containing:
#'
#' \describe{
#'   \item{low_threshold}{
#'     Boundary between low and moderate censoring.
#'   }
#'
#'   \item{moderate_threshold}{
#'     Boundary between moderate and high censoring.
#'   }
#'
#'   \item{high_threshold}{
#'     Boundary between high and extreme censoring.
#'   }
#' }
#'
#' @details
#' The built-in profiles are:
#'
#' \describe{
#'   \item{epa_unified}{
#'     15%, 50%, 90%
#'   }
#'
#'   \item{conservative}{
#'     10%, 25%, 75%
#'   }
#'
#'   \item{site_specific}{
#'     20%, 60%, 90%
#'   }
#' }
#'
#' These profiles influence recommended distribution fitting,
#' trend interpretation, and censored-data handling methods.
#'
#' @seealso
#' [guidance_options()],
#' [statistical_decision()]
#'
#' @examples
#' guidance_profile()
#'
#' guidance_profile("conservative")
#'
#' @export
guidance_profile <- function(
  profile = c(
    "epa_unified",
    "conservative",
    "site_specific"
  )
) {
  profile <- match.arg(profile)

  switch(
    profile,

    epa_unified = list(
      low_threshold = 15,
      moderate_threshold = 50,
      high_threshold = 90
    ),

    conservative = list(
      low_threshold = 10,
      moderate_threshold = 25,
      high_threshold = 75
    ),

    site_specific = list(
      low_threshold = 20,
      moderate_threshold = 60,
      high_threshold = 90
    )
  )
}


#' Create a statistical analysis plan
#'
#' @param objective Analysis objective:
#'   "distribution", "trend", "comparison", "interval", or "control_chart".
#' @param response Name of the numeric response column.
#' @param censored Name of the logical censoring-indicator column.
#' @param date Optional date/time column used for trend analyses.
#' @param group Optional grouping column.
#' @param comparison Optional comparison column.
#' @param method Optional pre-specified method. If NULL, the method is selected
#'   using the guidance rules.
#' @param alpha Type I error rate.
#' @param confidence_level Confidence level for intervals.
#' @param censoring_method Explicit censored-data method, if required.
#' @param missing_policy Missing-data policy.
#' @param guidance Guidance profile returned by guidance_profile().
#' @param parameters Additional method-specific parameters.
#'
#' @return An object of class "statistical_analysis_plan".
#' @export
analysis_plan <- function(
  objective = c(
    "distribution",
    "trend",
    "comparison",
    "interval",
    "control_chart"
  ),
  response = NULL,
  censored = NULL,
  date = NULL,
  group = NULL,
  comparison = NULL,
  method = NULL,
  alpha = 0.05,
  confidence_level = 0.95,
  censoring_method = NULL,
  missing_policy = c("exclude_required_fields", "error"),
  guidance = guidance_options(),
  parameters = list()
) {
  objective <- match.arg(objective)
  missing_policy <- match.arg(missing_policy)

  plan <- list(
    objective = objective,
    response = response,
    censored = censored,
    date = date,
    group = group,
    comparison = comparison,
    method = method,
    alpha = alpha,
    confidence_level = confidence_level,
    censoring_method = censoring_method,
    missing_policy = missing_policy,
    guidance = guidance,
    parameters = parameters
  )

  class(plan) <- "statistical_analysis_plan"
  validate_analysis_plan(plan)

  plan
}


#' Validate a statistical analysis plan
#'
#' @param plan A statistical_analysis_plan object or compatible list.
#'
#' @return Invisibly returns TRUE.
#' @export
validate_analysis_plan <- function(plan) {
  if (!is.list(plan)) {
    rlang::abort("`plan` must be a list.")
  }

  required <- c(
    "objective",
    "response",
    "censored",
    "date",
    "group",
    "comparison",
    "method",
    "alpha",
    "confidence_level",
    "censoring_method",
    "missing_policy",
    "guidance",
    "parameters"
  )

  missing_fields <- setdiff(required, names(plan))

  if (length(missing_fields) > 0L) {
    rlang::abort(
      paste(
        "The analysis plan is missing:",
        paste(missing_fields, collapse = ", ")
      )
    )
  }

  valid_objectives <- c(
    "distribution",
    "trend",
    "comparison",
    "interval",
    "control_chart"
  )

  if (
    !is.character(plan$objective) ||
      length(plan$objective) != 1L ||
      !plan$objective %in% valid_objectives
  ) {
    rlang::abort(
      paste(
        "`objective` must be one of:",
        paste(valid_objectives, collapse = ", ")
      )
    )
  }

  if (
    !is.character(plan$response) ||
      length(plan$response) != 1L ||
      is.na(plan$response)
  ) {
    rlang::abort("`response` must be one column name.")
  }

  if (
    !is.numeric(plan$alpha) ||
      length(plan$alpha) != 1L ||
      is.na(plan$alpha) ||
      plan$alpha <= 0 ||
      plan$alpha >= 1
  ) {
    rlang::abort("`alpha` must be a single number between 0 and 1.")
  }

  if (
    !is.numeric(plan$confidence_level) ||
      length(plan$confidence_level) != 1L ||
      is.na(plan$confidence_level) ||
      plan$confidence_level <= 0 ||
      plan$confidence_level >= 1
  ) {
    rlang::abort(
      "`confidence_level` must be a single number between 0 and 1."
    )
  }

  if (!is.list(plan$guidance)) {
    rlang::abort("`guidance` must be a guidance profile list.")
  }

  required_guidance <- c(
    "low_threshold",
    "moderate_threshold",
    "high_threshold"
  )

  if (!all(required_guidance %in% names(plan$guidance))) {
    rlang::abort(
      paste(
        "`guidance` must contain:",
        paste(required_guidance, collapse = ", ")
      )
    )
  }

  thresholds <- unlist(plan$guidance[required_guidance], use.names = FALSE)

  if (
    !is.numeric(thresholds) ||
      anyNA(thresholds) ||
      any(thresholds < 0 | thresholds > 100) ||
      is.unsorted(thresholds, strictly = TRUE)
  ) {
    rlang::abort(
      "Guidance thresholds must be strictly increasing percentages from 0 to 100."
    )
  }

  if (plan$objective == "trend" && is.null(plan$date)) {
    rlang::abort("Trend analyses require `date`.")
  }

  if (plan$objective == "comparison" && is.null(plan$comparison)) {
    rlang::abort("Comparison analyses require `comparison`.")
  }

  if (
    plan$objective == "control_chart" &&
      is.null(plan$group) &&
      is.null(plan$date)
  ) {
    rlang::abort(
      "Control-chart analyses require either `group` or `date`."
    )
  }

  invisible(TRUE)
}


#' Validate data against an analysis plan
#'
#' @param data A data frame or env_schema object.
#' @param plan A statistical_analysis_plan object.
#'
#' @return A validated data frame.
#' @export
validate_analysis_data <- function(data, plan) {
  validate_analysis_plan(plan)

  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame or env_schema object.")
  }

  response_col <- plan$response

  if (
    inherits(data, "env_schema") &&
      identical(response_col, ".analysis_result")
  ) {
    response_col <- analysis_result_var(data)
  }

  censored_col <- plan$censored

  if (
    inherits(data, "env_schema") &&
      identical(censored_col, ".censored")
  ) {
    censored_col <- censored_var(data)
  }

  required <- unique(
    c(
      response_col,
      censored_col,
      plan$date,
      plan$group,
      plan$comparison
    )
  )

  required <- required[!is.null(required)]

  missing_columns <- setdiff(required, names(data))

  if (length(missing_columns) > 0L) {
    rlang::abort(
      paste(
        "Required columns are missing:",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  if (!is.numeric(data[[response_col]])) {
    rlang::abort(
      sprintf("`%s` must be numeric.", response_col)
    )
  }

  if (!is.logical(data[[censored_col]])) {
    rlang::abort(
      sprintf("`%s` must be logical.", censored_col)
    )
  }

  if (
    any(
      data[[response_col]][!is.na(data[[response_col]])] < 0,
      na.rm = TRUE
    )
  ) {
    rlang::abort("Negative analytical results are not permitted.")
  }

  data
}


#' Select a statistical method
#'
#' @param data Validated analysis data.
#' @param plan A statistical_analysis_plan object.
#' @param allow_override Whether an explicitly supplied method may override
#'   the recommendation.
#'
#' @return A list with selected method, decision basis, diagnostics, and
#'   configuration.
#' @export
select_statistical_method <- function(
  data,
  plan,
  allow_override = TRUE
) {
  data <- validate_analysis_data(data, plan)

  response_col <- plan$response
  censored_col <- plan$censored

  if (inherits(data, "env_schema")) {
    if (identical(response_col, ".analysis_result")) {
      response_col <- analysis_result_var(data)
    }

    if (identical(censored_col, ".censored")) {
      censored_col <- censored_var(data)
    }
  }

  usable <- !is.na(data[[response_col]]) &
    !is.na(data[[censored_col]])

  n_used <- sum(usable)
  n_detect <- sum(!data[[censored_col]][usable])

  percent_censored <- if (n_used == 0L) {
    NA_real_
  } else {
    mean(data[[censored_col]][usable]) * 100
  }

  censoring_class <- .classify_censoring(
    percent_censored = percent_censored,
    profile = plan$guidance
  )

  if (n_used < 4L) {
    recommendation <- "insufficient_data"
    decision_basis <- "fewer_than_4_observations"
  } else if (is.na(censoring_class)) {
    recommendation <- "insufficient_data"
    decision_basis <- "no_usable_observations"
  } else {
    recommendation <- censoring_class
    decision_basis <- censoring_class
  }

  method <- .recommended_method(
    objective = plan$objective,
    censoring_class = recommendation,
    n_used = n_used,
    percent_censored = percent_censored,
    plan = plan
  )

  if (!is.null(plan$method)) {
    if (!allow_override) {
      rlang::abort(
        "An explicit method was supplied, but method overrides are disabled."
      )
    }

    method <- plan$method
    decision_basis <- "pre_specified_in_analysis_plan"
  }

  list(
    method = method,
    objective = plan$objective,
    censoring_class = recommendation,
    n_used = n_used,
    n_detect = n_detect,
    percent_censored = percent_censored,
    decision_basis = decision_basis,
    alpha = plan$alpha,
    confidence_level = plan$confidence_level,
    guidance = plan$guidance,
    parameters = plan$parameters
  )
}


.recommended_method <- function(
  objective,
  censoring_class,
  n_used,
  percent_censored,
  plan
) {
  if (
    censoring_class %in%
      c(
        "insufficient_data",
        "extreme_censoring"
      )
  ) {
    return("descriptive_only")
  }

  switch(
    objective,

    distribution = switch(
      censoring_class,
      low_censoring = "parametric_distribution",
      moderate_censoring = "ros_censored_distribution",
      high_censoring = "nonparametric_distribution"
    ),

    trend = if (n_used < 5L) {
      "trend_screening_only"
    } else if (censoring_class == "low_censoring") {
      "mann_kendall"
    } else if (censoring_class == "moderate_censoring") {
      "censored_kendall"
    } else {
      "trend_screening_only"
    },

    comparison = switch(
      censoring_class,
      low_censoring = "anova_or_welch",
      moderate_censoring = "kruskal_wallis",
      high_censoring = "kruskal_wallis"
    ),

    interval = switch(
      censoring_class,
      low_censoring = "parametric_prediction_interval",
      moderate_censoring = "ros_prediction_interval",
      high_censoring = "nonparametric_prediction_interval"
    ),

    control_chart = if (
      censoring_class == "low_censoring" &&
        percent_censored <= 50
    ) {
      "parametric_control_chart"
    } else {
      "unsupported_control_chart"
    }
  )
}


#' Run an analysis from an analysis plan
#'
#' Method execution is intentionally delegated to handler functions. This
#' prevents run_analysis() from silently substituting a different statistical
#' method when the requested method is unavailable or assumptions fail.
#'
#' @param data Analysis data.
#' @param plan A statistical_analysis_plan object.
#' @param handlers Named list of analysis handler functions. Each handler must
#'   accept `data`, `plan`, and `selection`.
#' @param allow_override Whether an explicit plan method may override guidance.
#'
#' @return An object of class "environmental_analysis_result".
#' @export
run_analysis <- function(
  data,
  plan,
  handlers = list(),
  allow_override = TRUE
) {
  started_at <- Sys.time()

  validate_analysis_plan(plan)
  data <- validate_analysis_data(data, plan)

  selection <- select_statistical_method(
    data = data,
    plan = plan,
    allow_override = allow_override
  )

  method <- selection$method
  handler <- handlers[[method]]

  if (is.null(handler)) {
    rlang::abort(
      paste0(
        "No execution handler is registered for method `",
        method,
        "`. ",
        "Register it in `handlers` or specify a supported method."
      )
    )
  }

  if (!is.function(handler)) {
    rlang::abort(
      sprintf("Handler for method `%s` must be a function.", method)
    )
  }

  result <- handler(
    data = data,
    plan = plan,
    selection = selection
  )

  structure(
    list(
      result = result,
      selection = selection,
      plan = plan,
      n_input = nrow(data),
      started_at = started_at,
      completed_at = Sys.time(),
      provenance = list(
        r_version = R.version.string,
        packages = utils::sessionInfo()$otherPkgs
      )
    ),
    class = "environmental_analysis_result"
  )
}


#' Print an environmental analysis result
#'
#' @export
print.environmental_analysis_result <- function(x, ...) {
  cat("<environmental_analysis_result>\n")
  cat("Objective: ", x$selection$objective, "\n", sep = "")
  cat("Method: ", x$selection$method, "\n", sep = "")
  cat("Observations used: ", x$selection$n_used, "\n", sep = "")
  cat(
    "Censored: ",
    sprintf("%.1f%%", x$selection$percent_censored),
    "\n",
    sep = ""
  )
  cat("Decision basis: ", x$selection$decision_basis, "\n", sep = "")

  invisible(x)
}
