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
#'
#'   \item{decision_basis}{
#'     Primary basis for the recommendation.
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
  profile = guidance_profile("epa_unified")
) {
  low_threshold <- profile$low_threshold
  moderate_threshold <- profile$moderate_threshold
  high_threshold <- profile$high_threshold

  out <- tibble::tibble(
    n_used = n_used,
    n_detect = n_detect,
    percent_censored = percent_censored,

    censoring_class = dplyr::case_when(
      is.na(percent_censored) ~ NA_character_,
      percent_censored < low_threshold ~
        "low_censoring",
      percent_censored < moderate_threshold ~
        "moderate_censoring",
      percent_censored < high_threshold ~
        "high_censoring",
      TRUE ~
        "extreme_censoring"
    ),

    distribution_method = dplyr::case_when(
      n_used < 4 ~
        "insufficient_data",

      percent_censored < low_threshold ~
        "parametric",

      percent_censored < moderate_threshold ~
        "ros_censored",

      percent_censored < high_threshold ~
        "nonparametric",

      TRUE ~
        "descriptive_only"
    ),

    trend_method = "mann_kendall",

    trend_guidance = dplyr::case_when(
      is.na(percent_censored) ~
        NA_character_,

      n_detect < 4 ~
        "insufficient_detects",

      percent_censored < high_threshold ~
        "formal_trend_test",

      TRUE ~
        "screening_only"
    ),

    censored_method = dplyr::case_when(
      is.na(percent_censored) ~ NA_character_,
      percent_censored < low_threshold ~
        "as_reported",
      TRUE ~
        "cenken"
    ),

    distribution_config = purrr::map(
      percent_censored,
      ~ list(
        method = "sf",
        choices = c("norm", "lnorm")
      )
    ),

    interval_config = purrr::map(
      percent_censored,
      ~ list(
        conf_level = 0.95,
        method = "Bonferroni",
        k = 1,
        n_mean = 1
      )
    ),
    decision_basis = dplyr::case_when(
      is.na(percent_censored) ~ NA_character_,
      n_used < 4 ~
        "fewer_than_4_observations",

      percent_censored < low_threshold ~
        "low_censoring",

      percent_censored < moderate_threshold ~
        "moderate_censoring",

      percent_censored < high_threshold ~
        "high_censoring",

      TRUE ~
        "extreme_censoring"
    )
  )

  class(out) <- c(
    "statistical_decision",
    class(out)
  )

  out
}

#' Derive Statistical Decision Guidance from Data
#'
#' Calculates censoring metrics from a dataset and returns a
#' corresponding [statistical_decision()] object.
#'
#' The input must contain standardized analytical fields:
#'
#' \itemize{
#'   \item `.analysis_result`
#'   \item `.censored`
#' }
#'
#' @param df Data frame containing analytical results.
#'
#' @returns
#' A `"statistical_decision"` object.
#'
#' @details
#' The following quantities are calculated automatically:
#'
#' \describe{
#'   \item{n_used}{
#'     Number of non-missing analytical results.
#'   }
#'
#'   \item{n_detect}{
#'     Number of detected observations.
#'   }
#'
#'   \item{percent_censored}{
#'     Percentage of observations flagged as censored.
#'   }
#' }
#'
#' These values are supplied to [statistical_decision()]
#' to obtain method-selection guidance.
#'
#' @seealso
#' [statistical_decision()],
#' [estimate_distribution()]
#'
#' @examples
#' derive_decision(gw_schema)
#'
#' gw_schema |>
#'   dplyr::filter(param_name == "Arsenic, dissolved") |>
#'   derive_decision()
#'
#' @export
derive_decision <- function(
  df,
  result_col = analysis_result_var(),
  censored_col = ".censored"
) {
  n_used <- sum(
    !is.na(df[[result_col]])
  )

  n_detect <- sum(
    !df[[censored_col]] &
      !is.na(df[[result_col]]),
    na.rm = TRUE
  )

  pct <- mean(
    df[[censored_col]],
    na.rm = TRUE
  ) *
    100

  statistical_decision(
    percent_censored = pct,
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

#' Estimate Distributions Using Statistical Decision Guidance
#'
#' Estimates an appropriate distributional model for each group using
#' censoring-aware statistical decision rules.
#'
#' `estimate_distribution()` automatically evaluates the percentage of
#' censored observations and available sample size using
#' [derive_decision()]. A [statistical_decision()] object is then used
#' to select the recommended distribution estimation pathway for each
#' group.
#'
#' Depending on the degree of censoring, groups are evaluated using:
#'
#' \itemize{
#'   \item `"parametric"` distribution fitting
#'   \item `"ros_censored"` censored distribution fitting
#'   \item `"nonparametric"` distribution classification
#'   \item `"descriptive_only"` evaluation
#'   \item `"insufficient_data"` when too few observations are available
#' }
#'
#' Distribution fitting is performed independently within each group and
#' requires the input data to already be grouped using
#' [dplyr::group_by()].
#'
#' @param df Grouped data frame or `env_schema` object.
#'
#' @param distribution_config Configuration list supplied to
#' distribution fitting functions.
#'
#' If `"auto"` (the default), configuration settings are obtained from
#' the group's [statistical_decision()] object.
#'
#' @returns
#' A tibble containing one row per group.
#'
#' The returned table includes statistical decision metadata together
#' with the distribution evaluation result.
#'
#' \describe{
#'   \item{n_detect}{
#'     Number of non-censored observations.
#'   }
#'
#'   \item{percent_censored}{
#'     Percentage of observations classified as censored.
#'   }
#'
#'   \item{censoring_class}{
#'     Censoring classification used for method selection.
#'   }
#'
#'   \item{distribution_method}{
#'     Recommended distribution estimation approach.
#'   }
#'
#'   \item{trend_method}{
#'     Recommended trend evaluation method.
#'   }
#'
#'   \item{trend_guidance}{
#'     Recommended interpretation of trend analyses.
#'   }
#'
#'   \item{censored_method}{
#'     Recommended censored-data handling method.
#'   }
#'
#'   \item{distribution_config}{
#'     Distribution-fitting configuration used during evaluation.
#'     This is a named list of arguments passed to
#'     [EnvStats::distChoose()] or
#'     [EnvStats::distChooseCensored()].
#'   }
#'
#'   \item{interval_config}{
#'     Recommended interval estimation settings.
#'   }
#'
#'   \item{decision_basis}{
#'     Basis used to select the statistical approach.
#'   }
#'
#'   \item{distribution}{
#'     Selected distribution or recommended distributional approach.
#'   }
#'
#'   \item{n_used}{
#'     Number of observations used during evaluation.
#'   }
#'
#'   \item{result}{
#'     Distribution fitting result object.
#'   }
#' }
#'
#' @details
#' Method selection follows the active guidance profile used by
#' [statistical_decision()].
#'
#' Under the default EPA-style guidance:
#'
#' \tabular{lll}{
#' Condition \tab Recommendation \tab Action \cr
#' n_used < 4 \tab insufficient_data \tab no distribution fitting \cr
#' <15\% censored \tab parametric \tab fit candidate distributions \cr
#' 15-50\% censored \tab ros_censored \tab censored distribution fitting \cr
#' 50-90\% censored \tab nonparametric \tab classify as nonparametric \cr
#' >=90\% censored \tab descriptive_only \tab descriptive evaluation only
#' }
#'
#' Parametric evaluations use candidate distributions specified by
#' `distribution_config`, typically through
#' [EnvStats::distChoose()]. By default, distribution fitting evaluates
#' normal and lognormal candidate distributions using the `"sf"` selection method.
#'
#' Censored evaluations use
#' [EnvStats::distChooseCensored()] with left-censoring assumptions.
#'
#' Nonparametric and descriptive-only groups do not perform formal
#' distribution fitting and instead return recommendation metadata for
#' downstream analyses.
#'
#' @examples
#' gw |>
#'   dplyr::group_by(
#'     location_id,
#'     param_name,
#'     default_unit
#'   ) |>
#'   estimate_distribution()
#'
#' gw |>
#'   dplyr::group_by(param_name) |>
#'   estimate_distribution()
#'
#' estimate_distribution(
#'   gw |>
#'     dplyr::group_by(
#'       location_id,
#'       param_name
#'     )
#' )
#'
#' @seealso
#' [statistical_decision()],
#' [derive_decision()],
#' [guidance_profile()],
#' [guidance_options()]
#'
#' @export
estimate_distribution <- function(
  df,
  distribution_config = "auto",
  result_col = analysis_result_var()
) {
  if (!dplyr::is_grouped_df(df)) {
    rlang::abort(
      paste(
        "estimate_distribution() requires grouped data.",
        "Use group_by() before calling this function."
      )
    )
  }

  df |>
    dplyr::group_modify(
      ~ .estimate_distribution_group(
        .x,
        distribution_config = distribution_config,
        result_col = result_col
      )
    ) |>
    dplyr::ungroup()
}

#' @keywords internal
.estimate_distribution_group <- function(
  df,
  distribution_config,
  result_col
) {
  decision_obj <- derive_decision(df, result_col = result_col)

  if (identical(distribution_config, "auto")) {
    distribution_config <-
      decision_obj$distribution_config[[1]]
  }

  method <-
    decision_obj$distribution_method[[1]]

  out <- switch(
    method,

    insufficient_data = {
      .attach_nested_data(
        result = .make_dist_result(
          distribution = "insufficient_data",
          n_used = sum(
            !is.na(df[[result_col]])
          ),
          return_result = TRUE
        ),
        data = df
      )
    },

    parametric = {
      .est_dist_group(
        df,
        return_result = TRUE,
        distribution_config = distribution_config,
        result_col = result_col
      )
    },

    ros_censored = {
      .est_dist_censored_group(
        df,
        return_result = TRUE,
        distribution_config = distribution_config,
        result_col = result_col
      )
    },

    nonparametric = {
      .attach_nested_data(
        result = .make_dist_result(
          distribution = "Nonparametric",
          n_used = sum(
            !is.na(df[[result_col]])
          ),
          return_result = TRUE
        ),
        data = df
      )
    },

    descriptive_only = {
      .attach_nested_data(
        result = .make_dist_result(
          distribution = "Descriptive Only",
          n_used = sum(
            !is.na(df[[result_col]])
          ),
          return_result = TRUE
        ),
        data = df
      )
    }
  )

  dplyr::bind_cols(
    dplyr::select(
      decision_obj,
      -n_used
    ),
    out
  )
}

#' Summary Statistics
#'
#' Calculates descriptive statistics for environmental monitoring
#' data.
#'
#' For `env_schema` objects, calculations are performed using the
#' standardized `.analysis_result` and `.censored` columns rather
#' than the original source data columns. This ensures summary
#' statistics are based on the processed analytical values used
#' throughout the package.
#'
#' If the input is grouped with [dplyr::group_by()], summary
#' statistics are calculated within the existing groups. For ungrouped
#' inputs, summaries are calculated separately for each
#' location, analyte, and unit.
#'
#' @param df A data frame or `env_schema` object.
#' @param na.rm Logical indicating whether missing values should
#'   be removed when calculating summary statistics.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' A tibble containing one row per summary group.
#'
#' The returned table includes:
#' \describe{
#'   \item{n}{Number of records in the group, including records with missing result values.}
#'   \item{n_used}{ Number of non-missing analytical results used in statistical
#'   evaluations and decision selection.}
#'   \item{n_detect}{Number of non-censored analytical results.}
#'   \item{percent_censored}{Percentage of records in the group classified as censored.}
#'   \item{mean}{Arithmetic mean concentration.}
#'   \item{median}{Median concentration.}
#'   \item{sd}{Sample standard deviation.}
#'   \item{var}{Sample variance.}
#'   \item{min}{Minimum concentration.}
#'   \item{max}{Maximum concentration.}
#'   \item{cv}{Coefficient of variation.}
#'   \item{25\%}{25th percentile.}
#'   \item{75\%}{75th percentile.}
#'   \item{IQR}{Interquartile range.}
#'   \item{mad}{Median absolute deviation.}
#'   \item{censoring_class}{
#'         EPA Unified Guidance style censoring classification:
#'        low_censoring, moderate_censoring, high_censoring, or extreme_censoring.
#' }
#'    \item{distribution_method}{
#'          Recommended distributional approach based on the degree of censoring.
#' }
#'     \item{trend_method}{
#'          Recommended trend method.
#' }
#'     \item{trend_guidance}{
#'          Recommended interpretation of trend results.
#'            "formal_trend_test" indicates trend inference is generally
#'             supported. "screening_only" indicates trend results should
#'             be considered exploratory because of extensive censoring.
#' }
#'      \item{censored_method}{
#'          Recommended censored-data handling method for trend analyses.
#' }
#'      \item{decision_basis}{
#'          Basis for the statistical recommendation.
#'            Examples include:
#'              fewer_than_4_observations,
#'              low_censoring,
#'              moderate_censoring,
#'              high_censoring,
#'              and extreme_censoring.
#'}
#' }
#'
#' @details
#' For ungrouped `data.frame` inputs, summaries are calculated by:
#'
#' \itemize{
#'   \item location
#'   \item analyte
#'   \item reporting unit
#' }
#'
#' For ungrouped `env_schema` inputs, summaries are calculated by:
#'
#' \itemize{
#'   \item the mapped location variable returned by `location_var()`
#'   \item the mapped analyte variable returned by `analyte_var()`
#'   \item the standardized analysis unit (`.analysis_unit`)
#' }
#'
#' Missing result values are excluded from statistical calculations
#' when `na.rm = TRUE`, but still contribute to the record count
#' (`n`), which represents the number of records in each summary
#' group.
#'
#' Empty inputs return an empty tibble with the standard summary columns.
#'
#' @examples
#' stats_summary(gw)
#'
#' gw |>
#'   dplyr::group_by(location_id) |>
#'   stats_summary()
#'
#' x <- env_schema(
#'   raw_data,
#'   location = site_id,
#'   date = sample_date,
#'   analyte = parameter,
#'   result = result_value
#' )
#'
#' stats_summary(x)
#'
#' @export
stats_summary <- function(df, ...) {
  UseMethod("stats_summary")
}

#' @rdname stats_summary
#' @param location_id Location column name.
#' @param param_name Analyte column name.
#' @param censored Censoring indicator column name.
#' @param analysis_result Analytical result column name.
#' @param default_unit Reporting unit column name.
#' @param result_col Numerical column on which stats_summary is computed
#' @export
stats_summary.data.frame <- function(
  df,
  location_id = "location_id",
  param_name = "param_name",
  censored = "censored",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  result_col = analysis_result,
  na.rm = TRUE
) {
  group_cols <- if (dplyr::is_grouped_df(df)) {
    dplyr::group_vars(df)
  } else {
    c(
      location_id,
      param_name,
      default_unit
    )
  }

  .stats_summary_core(
    df = df,
    group_cols = group_cols,
    result_col = result_col,
    censored_col = censored,
    na.rm = na.rm
  )
}

#' @rdname stats_summary
#'
#' @section env_schema Method:
#' For `env_schema` objects, `stats_summary()` automatically uses
#' the schema mappings and standardized analytical fields:
#'
#' \itemize{
#'   \item `.analysis_result`
#'   \item `.censored`
#'   \item `.analysis_unit`
#' }
#'
#' This allows summaries to remain consistent regardless of the
#' original source column names or schema mappings used by the input data.
#'
#' @export
stats_summary.env_schema <- function(
  df,
  result_col = analysis_result_var(),
  na.rm = TRUE,
  ...
) {
  group_cols <- if (dplyr::is_grouped_df(df)) {
    dplyr::group_vars(df)
  } else {
    c(
      location_var(df),
      analyte_var(df),
      ".analysis_unit"
    )
  }

  .stats_summary_core(
    df = df,
    group_cols = group_cols,
    result_col = result_col,
    censored_col = ".censored",
    na.rm = na.rm
  )
}

#' Summary statistics core
#'
#' @keywords internal
.stats_summary_core <- function(
  df,
  group_cols,
  result_col,
  censored_col,
  na.rm = TRUE
) {
  required_cols <- c(
    group_cols,
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

  if (length(group_cols) > 0) {
    df <- dplyr::group_by(
      df,
      dplyr::across(
        dplyr::all_of(group_cols)
      ),
      .add = FALSE
    )
  }

  if (nrow(df) == 0) {
    return(
      tibble::tibble(
        n = integer(),
        n_used = integer(),
        n_detect = integer(),
        percent_censored = numeric(),
        mean = numeric(),
        median = numeric(),
        sd = numeric(),
        var = numeric(),
        min = numeric(),
        max = numeric(),
        cv = numeric(),
        `25%` = numeric(),
        `75%` = numeric(),
        IQR = numeric(),
        mad = numeric(),
        censoring_class = character(),
        distribution_method = character(),
        trend_method = character(),
        trend_guidance = character(),
        censored_method = character(),
        distribution_config = list(),
        interval_config = list(),
        decision_basis = character()
      )
    )
  }

  df |>
    dplyr::summarise(
      n = dplyr::n(),

      n_used = sum(
        !is.na(.data[[result_col]])
      ),

      n_detect = sum(
        !.data[[censored_col]] &
          !is.na(.data[[result_col]]),
        na.rm = TRUE
      ),

      percent_censored = mean(
        .data[[censored_col]],
        na.rm = na.rm
      ) *
        100,

      mean = mean(
        .data[[result_col]],
        na.rm = na.rm
      ),

      median = stats::median(
        .data[[result_col]],
        na.rm = na.rm
      ),

      sd = stats::sd(
        .data[[result_col]],
        na.rm = na.rm
      ),

      var = stats::var(
        .data[[result_col]],
        na.rm = na.rm
      ),

      min = min(
        .data[[result_col]],
        na.rm = na.rm
      ),

      max = max(
        .data[[result_col]],
        na.rm = na.rm
      ),

      cv = cv(
        .data[[result_col]],
        na.rm = na.rm
      ),

      `25%` = unname(
        stats::quantile(
          .data[[result_col]],
          probs = 0.25,
          na.rm = na.rm
        )
      ),

      `75%` = unname(
        stats::quantile(
          .data[[result_col]],
          probs = 0.75,
          na.rm = na.rm
        )
      ),

      IQR = stats::IQR(
        .data[[result_col]],
        na.rm = na.rm
      ),

      mad = stats::mad(
        .data[[result_col]],
        na.rm = na.rm
      ),

      .groups = "drop"
    ) |>
    (\(x) {
      decision <- statistical_decision(
        n_used = x$n_used,
        n_detect = x$n_detect,
        percent_censored = x$percent_censored
      )

      dplyr::bind_cols(
        x,
        dplyr::select(
          decision,
          -c(n_used, n_detect, percent_censored)
        )
      )
    })()
}

#' Trend Summary Statistics
#'
#' Computes trend statistics and monitoring-period summary metrics
#' for environmental time-series data.
#'
#' For `env_schema` objects, trend calculations are performed using
#' the standardized `.analysis_result` values and schema mappings.
#' This ensures trends are evaluated consistently regardless of the
#' original source column names.
#'
#' If the input is grouped with [dplyr::group_by()], trend summaries
#' are calculated within the existing groups. For ungrouped inputs,
#' summaries are calculated separately for each location and analyte.
#'
#' @param df A data frame or `env_schema` object.
#' @param method Trend method. One of:
#'   \itemize{
#'   \item `"lm"` Linear regression
#'   \item `"loess"` Local regression smoother
#'   \item `"gam"` Generalized additive model
#'   \item `"mann_kendall"` Mann-Kendall trend test
#'   }
#' @param censored_method Method used to handle censored data.
#'   One of:
#'   \itemize{
#'   \item `"cenken"` Censored Kendall trend estimator
#'   \item `"as_reported"` Use analytical results as reported
#'   }
#' @param conf.level Confidence level used for trend estimates.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' A tibble containing one row per trend group.
#'
#' The returned table includes:
#' \describe{
#'   \item{n}{Number of records in the group.}
#'   \item{first_date}{Earliest sample date.}
#'   \item{last_date}{Latest sample date.}
#'   \item{date_range_days}{Monitoring period length in days.}
#'   \item{date_range_years}{Monitoring period length in years.}
#'   \item{sampling_frequency}{Approximate samples collected per year.}
#'   \item{minimum_concentration}{Minimum analytical result.}
#'   \item{median_concentration}{Median analytical result.}
#'   \item{mean_concentration}{Mean analytical result.}
#'   \item{maximum_concentration}{Maximum analytical result.}
#'   \item{standard_deviation}{Sample standard deviation.}
#'   \item{coefficient_of_variation}{Standard deviation divided by mean.}
#'   \item{trend_method}{Trend estimation method used.}
#'   \item{censored_method}{Censored-data handling method used.}
#'   \item{slope}{Estimated trend slope.}
#'   \item{p_value}{Trend test p-value.}
#'   \item{conf_low}{Lower confidence limit for the slope estimate.}
#'   \item{conf_high}{Upper confidence limit for the slope estimate.}
#'   \item{significant}{TRUE when the trend is statistically significant
#'   at the specified confidence level.}
#'   \item{trend_direction}{Increasing, Decreasing, or Stable.}
#' }
#'
#' @details
#' Trend calculations require at least three non-missing observations
#' and at least two distinct analytical result values. Groups that do not meet
#' these requirements return summary metrics, but trend-specific fields
#' such as `slope`, `p_value`, `conf_low`, and `conf_high` are returned as `NA`.
#'
#' Slope estimates and significance statistics are only available for
#' trend methods that support formal trend estimation. For smoothing
#' methods such as `"loess"` and `"gam"`, these fields are returned as `NA`.
#'
#' For Mann-Kendall analyses using `censored_method = "cenken"`,
#' confidence limits for the Sen slope are not currently available
#' and are returned as `NA`.
#'
#' For ungrouped `env_schema` inputs, trends are calculated by:
#'
#' \itemize{
#'   \item the mapped location variable returned by `location_var()`
#'   \item the mapped analyte variable returned by `analyte_var()`
#' }
#'
#' Confidence intervals are available for some trend methods. When a
#' confidence interval cannot be estimated, `conf_low` and `conf_high`
#' are returned as `NA`.
#'
#' @examples
#' trend_summary(gw)
#'
#' gw |>
#'   dplyr::group_by(location_id) |>
#'   trend_summary()
#'
#' trend_summary(
#'   gw,
#'   method = "mann_kendall",
#'   censored_method = "cenken"
#' )
#'
#' @export
trend_summary <- function(df, ...) {
  UseMethod("trend_summary")
}

#' @rdname trend_summary
#' @param location_id Location column name.
#' @param param_name Analyte column name.
#' @param sample_date Sample date column name.
#' @param analysis_result Analytical result column name.
#' @export
trend_summary.data.frame <- function(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  method = "mann_kendall",
  censored_method = "auto",
  conf.level = 0.95
) {
  env_schema(
    df,
    location = !!rlang::sym(location_id),
    sample_date = !!rlang::sym(sample_date),
    analyte = !!rlang::sym(param_name),
    result = !!rlang::sym(analysis_result)
  ) |>
    trend_summary(
      method = method,
      censored_method = censored_method,
      conf.level = conf.level
    )
}

#' @rdname trend_summary
#'
#' @section env_schema Method:
#' For `env_schema` objects, `trend_summary()` automatically uses
#' the schema mappings and standardized analytical fields:
#'
#' \itemize{
#'   \item `.analysis_result`
#'   \item `.censored`
#'   \item mapped location variable
#'   \item mapped analyte variable
#' }
#'
#' This allows trend summaries to remain consistent regardless
#' of the original source column names or schema mappings used by
#' the input data.
#'
#' @export
trend_summary.env_schema <- function(
  df,
  result_col = analysis_result_var(),
  method = "mann_kendall",
  censored_method = "auto",
  conf.level = 0.95
) {
  validate_env_schema(df)

  alpha <- 1 - conf.level

  sample_date_col <- sample_date_var(df)

  group_cols <- .get_trend_group_cols(df)

  df |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(group_cols)
      )
    ) |>
    dplyr::group_modify(
      ~ .trend_summary_group(
        .x,
        sample_date_col = sample_date_col,
        result_col = result_col,
        method = method,
        censored_method = censored_method,
        alpha = alpha,
        conf.level = conf.level
      )
    ) |>
    dplyr::ungroup()
}

#' @keywords internal
.trend_summary_group <- function(
  df,
  sample_date_col,
  result_col,
  method,
  censored_method,
  alpha,
  conf.level
) {
  n <- nrow(df)

  sample_date_values <- stats::na.omit(
    df[[sample_date_col]]
  )

  if (length(sample_date_values) > 0) {
    first_date <- min(sample_date_values)

    last_date <- max(sample_date_values)

    date_range_days <- as.numeric(
      difftime(
        last_date,
        first_date,
        units = "days"
      )
    )

    date_range_years <- date_range_days / 365.25
  } else {
    first_date <- as.Date(NA)

    last_date <- as.Date(NA)

    date_range_days <- NA_real_

    date_range_years <- NA_real_
  }

  sampling_frequency <- dplyr::case_when(
    is.na(date_range_years) ~ NA_real_,
    date_range_years <= 0 ~ NA_real_,
    TRUE ~ n / date_range_years
  )

  values <- stats::na.omit(
    df[[result_col]]
  )

  if (length(values) > 0) {
    minimum_concentration <- min(values)

    median_concentration <- stats::median(values)

    mean_concentration <- mean(values)

    maximum_concentration <- max(values)

    standard_deviation <- stats::sd(values)

    coefficient_of_variation <- dplyr::case_when(
      is.na(mean_concentration) ~ NA_real_,
      mean_concentration == 0 ~ NA_real_,
      TRUE ~ standard_deviation / mean_concentration
    )
  } else {
    minimum_concentration <- NA_real_
    median_concentration <- NA_real_
    mean_concentration <- NA_real_
    maximum_concentration <- NA_real_
    standard_deviation <- NA_real_
    coefficient_of_variation <- NA_real_
  }

  tr <- .compute_trend(
    df = df,
    sample_date_col = sample_date_col,
    result_col = result_col,
    method = method,
    censored_method = censored_method,
    conf.level = conf.level
  )

  if (is.null(tr)) {
    return(
      tibble::tibble(
        n = n,
        first_date = first_date,
        last_date = last_date,
        date_range_days = date_range_days,
        date_range_years = date_range_years,
        sampling_frequency = sampling_frequency,
        minimum_concentration = minimum_concentration,
        median_concentration = median_concentration,
        mean_concentration = mean_concentration,
        maximum_concentration = maximum_concentration,
        standard_deviation = standard_deviation,
        coefficient_of_variation = coefficient_of_variation,
        trend_method = method,
        censored_method = censored_method,
        slope = NA_real_,
        p_value = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_,
        significant = NA,
        trend_direction = NA_character_
      )
    )
  }

  tibble::tibble(
    n = n,
    first_date = first_date,
    last_date = last_date,
    date_range_days = date_range_days,
    date_range_years = date_range_years,
    sampling_frequency = sampling_frequency,
    minimum_concentration = minimum_concentration,
    median_concentration = median_concentration,
    mean_concentration = mean_concentration,
    maximum_concentration = maximum_concentration,
    standard_deviation = standard_deviation,
    coefficient_of_variation = coefficient_of_variation,
    trend_method = method,
    censored_method = tr$censored_method,
    slope = tr$slope,
    p_value = tr$p_value,
    conf_low = tr$conf_low,
    conf_high = tr$conf_high,
    significant = dplyr::case_when(
      is.na(tr$p_value) ~ NA,
      TRUE ~ tr$p_value <= alpha
    ),
    trend_direction = dplyr::case_when(
      is.na(tr$slope) ~ NA_character_,
      tr$p_value > alpha ~ "Stable",
      tr$slope > 0 ~ "Increasing",
      tr$slope < 0 ~ "Decreasing",
      TRUE ~ "Stable"
    )
  )
}

#' @keywords internal
#' @noRd
.prepare_trend_data <- function(
  df,
  sample_date_col,
  result_col,
  censored_method = "cenken"
) {
  out <- df |>
    dplyr::filter(
      !is.na(.data[[sample_date_col]]),
      !is.na(.data[[result_col]])
    ) |>
    dplyr::arrange(
      .data[[sample_date_col]]
    )

  if (nrow(out) < 3) {
    return(NULL)
  }

  if (
    dplyr::n_distinct(
      out[[result_col]]
    ) <
      2
  ) {
    return(NULL)
  }

  out
}

#' Mann-Kendall Trend Test
#'
#' Performs a Mann-Kendall monotonic trend test using
#' [EnvStats::kendallTrendTest()].
#'
#' For `env_schema` objects, the standardized analytical results
#' stored in `.analysis_result` are used together with the mapped
#' date variable.
#'
#' @param df A data frame or `env_schema` object.
#' @param conf.level Confidence level for confidence interval
#'   calculations.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' An object returned by [EnvStats::kendallTrendTest()].
#'
#' The result includes:
#' \itemize{
#'   \item trend test statistics
#'   \item p-values
#'   \item Sen slope estimate
#'   \item confidence intervals when available
#' }
#'
#' @details
#' The Mann-Kendall test is a nonparametric method commonly used
#' to evaluate monotonic trends in environmental monitoring data.
#'
#' The test estimates the Sen slope and evaluates whether a monotonic
#' increasing or decreasing trend is present through time.
#'
#' Confidence intervals may not be available for small sample sizes.
#'
#' @examples
#' kendall_trend(gw)
#'
#' kendall_trend(
#'   gw,
#'   conf.level = 0.90
#' )
#'
#' @export
kendall_trend <- function(df, ...) {
  UseMethod("kendall_trend")
}

#' @rdname kendall_trend
#' @param location_id Location column name.
#' @param param_name Analyte column name.
#' @param sample_date Sample date column name.
#' @param analysis_result Analytical result column name.
#' @export
kendall_trend.data.frame <- function(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  conf.level = 0.95
) {
  env_schema(
    df,
    location = !!rlang::sym(location_id),
    sample_date = !!rlang::sym(sample_date),
    analyte = !!rlang::sym(param_name),
    result = !!rlang::sym(analysis_result)
  ) |>
    kendall_trend(
      conf.level = conf.level
    )
}


#' @rdname kendall_trend
#'
#' @section env_schema Method:
#' For `env_schema` objects, the test is performed using:
#'
#' \itemize{
#'   \item the mapped date variable returned by `date_var()`
#'   \item standardized analytical results stored in `.analysis_result`
#' }
#'
#' @export
kendall_trend.env_schema <- function(
  df,
  result_col = analysis_result_var(),
  conf.level = 0.95
) {
  validate_env_schema(df)

  sample_date_col <- sample_date_var(df)

  EnvStats::kendallTrendTest(
    x = df[[result_col]],
    y = df[[sample_date_col]],
    conf.level = conf.level
  )
}


#' Extract lm stats information
#'
#' Internal helper used by plotting functions.
#'
#' @keywords internal
#'
#' @noRd
.extract_lm_stats <- function(
  fit,
  conf.level = 0.95
) {
  fit_summary <- suppressWarnings(
    base::summary(fit)
  )

  coef_tbl <- fit_summary$coefficients

  slope <- NA_real_
  p_value <- NA_real_
  conf_low <- NA_real_
  conf_high <- NA_real_

  if (
    !is.null(coef_tbl) &&
      nrow(coef_tbl) >= 2 &&
      length(stats::coef(fit)) >= 2
  ) {
    slope <- unname(stats::coef(fit)[2])

    p_value <- coef_tbl[2, 4]

    slope_ci <- tryCatch(
      suppressWarnings(
        stats::confint(
          fit,
          level = conf.level
        )
      ),
      error = function(e) NULL
    )

    if (
      !is.null(slope_ci) &&
        nrow(slope_ci) >= 2
    ) {
      conf_low <- slope_ci[2, 1]
      conf_high <- slope_ci[2, 2]
    }
  }

  list(
    slope = slope,
    p_value = p_value,
    conf_low = conf_low,
    conf_high = conf_high
  )
}

#' Compute Trend Information
#'
#' Internal helper used by plotting functions.
#'
#' @param df Data frame
#' @param method Trend method
#' @param conf.level Confidence level
#'
#' @keywords internal
#'
#' @noRd
.compute_trend <- function(
  df,
  sample_date_col,
  result_col = analysis_result_var(),
  method = "mann_kendall",
  censored_method = "auto",
  conf.level = 0.95
) {
  df <- .prepare_trend_data(
    df = df,
    sample_date_col = sample_date_col,
    result_col = result_col,
    censored_method = censored_method
  )

  if (is.null(df)) {
    return(NULL)
  }

  method <- rlang::arg_match(
    method,
    c(
      "none",
      "lm",
      "loess",
      "gam",
      "mann_kendall"
    )
  )

  censored_method <- rlang::arg_match(
    censored_method,
    c(
      "auto",
      "cenken",
      "as_reported"
    )
  )
  if (censored_method == "auto") {
    decision <- derive_decision(
      df,
      result_col = result_col
    )

    censored_method <-
      decision$censored_method[[1]]
  }

  if (method == "none") {
    return(NULL)
  }

  z <- stats::qnorm(
    1 - (1 - conf.level) / 2
  )

  if (method == "lm") {
    df$.date_num <- as.numeric(
      df[[sample_date_col]]
    )

    fit <- stats::lm(
      stats::reformulate(
        ".date_num",
        response = result_col
      ),
      data = df
    )

    pred <- stats::predict(
      fit,
      interval = "confidence",
      level = conf.level
    )

    trend_df <- dplyr::bind_cols(
      df,
      as.data.frame(pred)
    )

    trend_stats <- .extract_lm_stats(
      fit,
      conf.level
    )

    return(
      list(
        method = method,
        censored_method = censored_method,
        model = fit,
        slope = trend_stats$slope,
        p_value = trend_stats$p_value,
        conf_low = trend_stats$conf_low,
        conf_high = trend_stats$conf_high,
        trend_df = trend_df
      )
    )
  }

  if (method == "loess") {
    df$.date_num <- as.numeric(
      df[[sample_date_col]]
    )

    fit <- stats::loess(
      stats::reformulate(
        ".date_num",
        response = result_col
      ),
      data = df
    )

    pred <- stats::predict(
      fit,
      se = TRUE
    )

    trend_df <- df |>
      dplyr::mutate(
        fit = pred$fit,
        se = pred$se.fit,
        lwr = fit - z * se,
        upr = fit + z * se
      )

    return(
      list(
        method = method,
        censored_method = censored_method,
        model = fit,
        trend_df = trend_df
      )
    )
  }

  if (method == "gam") {
    df$.date_num <- as.numeric(
      df[[sample_date_col]]
    )

    fit <- tryCatch(
      mgcv::gam(
        stats::as.formula(
          paste(result_col, "~ s(.date_num)")
        ),
        data = df
      ),
      error = function(e) NULL
    )

    if (is.null(fit)) {
      return(NULL)
    }

    pred <- stats::predict(
      fit,
      se.fit = TRUE
    )

    trend_df <- df |>
      dplyr::mutate(
        fit = pred$fit,
        se = pred$se.fit,
        lwr = fit - z * se,
        upr = fit + z * se
      )

    return(
      list(
        method = method,
        censored_method = censored_method,
        model = fit,
        trend_df = trend_df
      )
    )
  }

  if (
    method == "mann_kendall" &&
      censored_method == "as_reported"
  ) {
    fit <- EnvStats::kendallTrendTest(
      x = df[[result_col]],
      y = df[[sample_date_col]],
      conf.level = conf.level
    )

    x_num <- as.numeric(
      df[[sample_date_col]]
    )

    x0 <- min(x_num)

    slope <- unname(
      fit$estimate[["slope"]]
    )

    intercept <- stats::median(
      df[[result_col]] -
        slope * (x_num - x0),
      na.rm = TRUE
    )

    trend_df <- df |>
      dplyr::mutate(
        fit = intercept +
          slope * (x_num - x0)
      )

    conf_low <- NA_real_
    conf_high <- NA_real_

    if (
      !is.null(fit$conf.int) &&
        length(fit$conf.int) >= 2
    ) {
      conf_low <- fit$conf.int[1]
      conf_high <- fit$conf.int[2]
    }

    return(
      list(
        method = method,
        censored_method = censored_method,
        slope = slope,
        p_value = fit$p.value,
        conf_low = conf_low,
        conf_high = conf_high,
        trend_df = trend_df
      )
    )
  }

  if (
    method == "mann_kendall" &&
      censored_method == "cenken"
  ) {
    fit <- suppressWarnings(
      tryCatch(
        NADA2::cenken(
          y = df[[result_col]],
          ycen = df$.censored,
          x = as.numeric(df[[sample_date_col]])
        ),
        error = function(e) NULL
      )
    )

    if (is.null(fit)) {
      return(NULL)
    }

    slope <- fit$slope

    p_value <- fit$p

    x_num <- as.numeric(
      df[[sample_date_col]]
    )

    x0 <- min(x_num)

    intercept <- fit$intercept

    trend_df <- df |>
      dplyr::mutate(
        fit = intercept +
          slope * (x_num - x0)
      )

    return(
      list(
        method = method,
        censored_method = censored_method,
        slope = slope,
        p_value = p_value,
        conf_low = NA_real_,
        conf_high = NA_real_,
        trend_df = trend_df
      )
    )
  }
}

#' Add Trend Layer to Plots
#'
#' @noRd
.add_trend <- function(
  p,
  df,
  trend = "none",
  trend_ci = FALSE,
  trend_conf_level = 0.95,
  trend_label = "none",
  trend_label_sig_only = TRUE,
  censored_method = "cenken",
  plot_type = "split"
) {
  censored_method <- rlang::arg_match(
    censored_method,
    c(
      "cenken",
      "as_reported"
    )
  )

  sample_date_col <- sample_date_var(df)

  location_col <- location_var(df)

  analyte_col <- analyte_var(df)

  if (trend == "none") {
    return(p)
  }

  if (trend %in% c("loess", "gam")) {
    trend_label <- "none"
  }

  alpha <- 1 - trend_conf_level

  if (plot_type == "split") {
    tr <- .compute_trend(
      df = df,
      sample_date_col = sample_date_col,
      method = trend,
      censored_method = censored_method,
      conf.level = trend_conf_level
    )

    if (is.null(tr)) {
      return(p)
    }

    if (
      trend_ci &&
        all(c("lwr", "upr") %in% names(tr$trend_df))
    ) {
      p <- p +
        ggplot2::geom_ribbon(
          data = tr$trend_df,
          ggplot2::aes(
            x = .data[[sample_date_col]],
            ymin = lwr,
            ymax = upr
          ),
          fill = "red",
          alpha = 0.15,
          inherit.aes = FALSE
        )
    }

    p <- p +
      ggplot2::geom_line(
        data = tr$trend_df,
        ggplot2::aes(
          x = .data[[sample_date_col]],
          y = fit
        ),
        colour = "red",
        linewidth = 1,
        inherit.aes = FALSE
      )

    if (trend_label != "none") {
      label_df <- tr$trend_df |>
        dplyr::slice_tail(n = 1) |>
        dplyr::mutate(
          .trend_p_value = tr$p_value,
          .trend_slope = tr$slope,
          .trend_conf_low = tr$conf_low,
          .trend_conf_high = tr$conf_high,

          label = .format_trend_label(
            slope = tr$slope,
            p_value = tr$p_value,
            conf_low = tr$conf_low,
            conf_high = tr$conf_high,
            trend_label = trend_label,
            trend = trend
          )
        )

      keep_label <- TRUE

      if (
        trend_label_sig_only &&
          trend == "mann_kendall"
      ) {
        keep_label <-
          !is.na(tr$p_value) &&
          tr$p_value <= alpha
      }

      if (keep_label) {
        p <- p +
          ggplot2::geom_text(
            data = label_df,
            ggplot2::aes(
              x = .data[[sample_date_col]],
              y = fit,
              label = label
            ),
            hjust = -0.05,
            size = 3,
            inherit.aes = FALSE
          )
      }
    }

    return(p)
  }

  trend_df <- .compute_group_trends(
    df = df,
    method = trend,
    censored_method = censored_method,
    conf.level = trend_conf_level
  )

  if (nrow(trend_df) == 0) {
    return(p)
  }

  if (
    trend_ci &&
      all(
        c("lwr", "upr") %in% names(trend_df)
      )
  ) {
    p <- p +
      ggplot2::geom_ribbon(
        data = trend_df,
        ggplot2::aes(
          x = .data[[sample_date_col]],
          ymin = lwr,
          ymax = upr,
          group = interaction(
            .data[[location_col]],
            .data[[analyte_col]]
          ),
          fill = .data[[analyte_col]]
        ),
        alpha = 0.15,
        colour = NA,
        inherit.aes = FALSE,
        show.legend = FALSE
      )
  }

  p <- p +
    ggplot2::geom_line(
      data = trend_df,
      ggplot2::aes(
        x = .data[[sample_date_col]],
        y = fit,
        group = interaction(
          .data[[location_col]],
          .data[[analyte_col]]
        ),
        colour = .data[[analyte_col]]
      ),
      linewidth = 1,
      inherit.aes = FALSE
    )

  if (trend_label != "none") {
    labels <- .compute_group_labels(
      trend_df = trend_df,
      trend = trend,
      trend_label = trend_label,
      sample_date_col = sample_date_col,
      location_col = location_col,
      analyte_col = analyte_col
    )

    if (
      trend_label_sig_only &&
        trend == "mann_kendall"
    ) {
      labels <- labels |>
        dplyr::filter(
          !is.na(.trend_p_value),
          .trend_p_value <= alpha
        )
    }

    if (nrow(labels) > 0) {
      p <- p +
        ggplot2::geom_text(
          data = labels,
          ggplot2::aes(
            x = label_x,
            y = label_y,
            label = label,
            colour = .data[[analyte_col]]
          ),
          size = 3,
          hjust = -0.05,
          inherit.aes = FALSE,
          show.legend = FALSE
        )
    }
  }

  p
}

#' Compute trend models for grouped series
#'
#' @keywords internal
#' @noRd
.compute_group_trends <- function(
  df,
  method = "mann_kendall",
  censored_method = "cenken",
  conf.level = 0.95
) {
  sample_date_col <- sample_date_var(df)

  group_cols <- .get_trend_group_cols(df)

  df |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(group_cols)
      )
    ) |>
    dplyr::group_modify(
      ~ {
        tr <- .compute_trend(
          df = .x,
          sample_date_col = sample_date_col,
          method = method,
          censored_method = censored_method,
          conf.level = conf.level
        )

        if (is.null(tr)) {
          return(
            tibble::tibble()
          )
        }

        tr$trend_df |>
          dplyr::mutate(
            .trend_method = method,

            .trend_slope = if (is.null(tr$slope)) {
              NA_real_
            } else {
              tr$slope
            },

            .trend_p_value = if (is.null(tr$p_value)) {
              NA_real_
            } else {
              tr$p_value
            },

            .trend_conf_low = if (is.null(tr$conf_low)) {
              NA_real_
            } else {
              tr$conf_low
            },

            .trend_conf_high = if (is.null(tr$conf_high)) {
              NA_real_
            } else {
              tr$conf_high
            }
          )
      }
    ) |>
    dplyr::ungroup()
}

#' Compute trend labels
#' @keywords internal
#' @noRd
.compute_group_labels <- function(
  trend_df,
  trend = "mann_kendall",
  trend_label = "compact",
  sample_date_col,
  location_col,
  analyte_col
) {
  trend_df |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          c(
            location_col,
            analyte_col
          )
        )
      )
    ) |>
    dplyr::arrange(
      .data[[sample_date_col]],
      .by_group = TRUE
    ) |>
    dplyr::slice_tail(
      n = 1
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      label_x = .data[[sample_date_col]],

      label_y = fit,

      label = purrr::pmap_chr(
        list(
          .trend_slope,
          .trend_p_value,
          .trend_conf_low,
          .trend_conf_high
        ),
        ~ .format_trend_label(
          slope = ..1,
          p_value = ..2,
          conf_low = ..3,
          conf_high = ..4,
          trend_label = trend_label,
          trend = trend
        )
      )
    )
}

#' @keywords internal
#' @noRd
.format_trend_label <- function(
  slope,
  p_value,
  conf_low,
  conf_high,
  trend_label,
  trend
) {
  if (
    trend_label == "none" ||
      is.na(p_value)
  ) {
    return("")
  }

  direction <- dplyr::case_when(
    slope > 0 ~ "Increasing",
    slope < 0 ~ "Decreasing",
    TRUE ~ "Stable"
  )

  if (trend_label == "compact") {
    return(
      sprintf(
        "%s\np=%.3f",
        direction,
        p_value
      )
    )
  }

  if (
    trend == "mann_kendall" &&
      (is.na(conf_low) ||
        is.na(conf_high))
  ) {
    return(
      sprintf(
        paste(
          "Mann-Kendall",
          "p=%.3f",
          "Sen=%.3g",
          sep = "\n"
        ),
        p_value,
        slope
      )
    )
  }

  if (trend == "mann_kendall") {
    return(
      sprintf(
        paste(
          "Mann-Kendall",
          "p=%.3f",
          "Sen=%.3g",
          "CI=[%.3g, %.3g]",
          sep = "\n"
        ),
        p_value,
        slope,
        conf_low,
        conf_high
      )
    )
  }

  if (
    is.na(conf_low) ||
      is.na(conf_high)
  ) {
    return(
      sprintf(
        paste(
          "Slope=%.3g",
          "p=%.3f",
          sep = "\n"
        ),
        slope,
        p_value
      )
    )
  }

  sprintf(
    paste(
      "Slope=%.3g",
      "p=%.3f",
      "CI=[%.3g, %.3g]",
      sep = "\n"
    ),
    slope,
    p_value,
    conf_low,
    conf_high
  )
}

#' @keywords internal
#' @noRd
.get_trend_group_cols <- function(df) {
  if (dplyr::is_grouped_df(df)) {
    return(
      dplyr::group_vars(df)
    )
  }

  c(
    location_var(df),
    analyte_var(df)
  )
}

#' @keywords internal
#' @noRd
.get_outlier_group_cols <- function(df) {
  if (dplyr::is_grouped_df(df)) {
    return(
      dplyr::group_vars(df)
    )
  }

  c(
    location_var(df),
    analyte_var(df),
    ".analysis_unit"
  )
}

#' @keywords internal
#' @noRd
.build_outlier_groups <- function(df) {
  group_cols <- .get_outlier_group_cols(df)

  df |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(group_cols)
      ),
      .add = FALSE
    ) |>
    tidyr::nest()
}

#' @keywords internal
#' @noRd
.extract_htest_results <- function(
  df,
  test_col
) {
  df |>
    dplyr::mutate(
      alternative = purrr::map(
        .data[[test_col]],
        ~ {
          if (is.null(.x)) {
            NA_character_
          } else {
            .x$alternative
          }
        }
      ),

      p_value = purrr::map(
        .data[[test_col]],
        ~ {
          if (is.null(.x)) {
            NA_real_
          } else {
            .x$p.value
          }
        }
      )
    )
}

#' Dixon Test
#'
#' Performs Dixon's Q test for potential outliers in environmental
#' monitoring data.
#'
#' `dixon_test()` is an S3 generic that supports both raw
#' `data.frame` inputs and `env_schema` objects.
#'
#' If the input is grouped using [dplyr::group_by()], Dixon tests are
#' performed within the existing groups. For ungrouped `env_schema`
#' inputs, tests are performed separately for each:
#'
#' \itemize{
#'   \item location
#'   \item analyte
#'   \item analysis unit
#' }
#'
#' Dixon's test is intended for relatively small sample sizes and is
#' generally appropriate for groups containing between three and
#' thirty observations.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' A tibble containing one row per outlier evaluation group.
#'
#' @details
#' Dixon's test evaluates whether the most extreme observation is
#' statistically inconsistent with the remaining observations in the
#' group.
#'
#' The method is most appropriate for small datasets and should be
#' interpreted cautiously for environmental monitoring programs with
#' larger sample sizes.
#'
#' @section Groundwater Monitoring:
#'
#' Dixon's and Grubbs' tests are generally most appropriate
#' for relatively small datasets and may be sensitive to
#' distributional assumptions.
#'
#' For larger groundwater monitoring datasets,
#' `rosner_test()` is often preferred.
#'
#' @examples
#' dixon_test(gw_data)
#'
#' schema <- env_schema(
#'   gw_data,
#'   location = location_id,
#'   sample_date = sample_date,
#'   analyte = param_name,
#'   result = analysis_result,
#'   unit = default_unit
#' )
#'
#' dixon_test(schema)
#'
#' schema |>
#'   dplyr::group_by(location_id) |>
#'   dixon_test()
#'
#' @export
dixon_test <- function(df, ...) {
  UseMethod("dixon_test")
}

#' @rdname dixon_test
#'
#' @param location_id Location column name.
#' @param param_name Analyte column name.
#' @param sample_date Sample date column name.
#' @param analysis_result Analytical result column name.
#' @param default_unit Reporting unit column name.
#'
#' @export
dixon_test.data.frame <- function(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  ...
) {
  df <- env_schema(
    df,
    location = !!rlang::sym(location_id),
    analyte = !!rlang::sym(param_name),
    sample_date = !!rlang::sym(sample_date),
    result = !!rlang::sym(analysis_result),
    unit = !!rlang::sym(default_unit)
  )

  dixon_test(
    df,
    ...
  )
}

#' @rdname dixon_test
#'
#' @param type Variant of Dixon's test.
#' @param opposite Logical indicating whether the lowest value
#' should be tested rather than the highest value.
#' @param two_sided Logical indicating whether a two-sided test
#' should be performed.
#' @param keep_data_object Logical indicating whether the complete
#' test object should be retained in the output.
#'
#' @section env_schema Method:
#'
#' For `env_schema` objects, Dixon tests are performed using
#' standardized analytical results stored in `.analysis_result`.
#'
#' Grouping is determined automatically from the current
#' `dplyr::group_by()` structure or, for ungrouped data, uses:
#'
#' \itemize{
#'   \item location
#'   \item analyte
#'   \item analysis unit
#' }
#'
#' @export
dixon_test.env_schema <- function(
  df,
  type = 0,
  opposite = FALSE,
  two_sided = TRUE,
  keep_data_object = FALSE
) {
  validate_env_schema(df)

  df <- df |>
    .build_outlier_groups() |>
    dplyr::mutate(
      dixon = purrr::map(
        data,
        ~ {
          x <- stats::na.omit(
            .x$.analysis_result
          )

          x <- x[
            is.finite(x)
          ]

          if (
            length(x) < 3 ||
              length(x) > 30 ||
              dplyr::n_distinct(x) < 3
          ) {
            return(NULL)
          }

          outliers::dixon.test(
            x,
            type = type,
            opposite = opposite,
            two.sided = two_sided
          )
        }
      )
    )

  if (isTRUE(keep_data_object)) {
    outliers <- df |>
      .extract_htest_results(
        test_col = "dixon"
      ) |>
      dplyr::select(-data)
  } else {
    outliers <- df |>
      .extract_htest_results(
        test_col = "dixon"
      ) |>
      dplyr::select(-data, -dixon) |>
      tidyr::unnest(
        cols = c(
          alternative,
          p_value
        )
      )
  }

  return(outliers)
}

#' Grubbs Test
#'
#' Performs Grubbs' test for potential outliers in environmental
#' monitoring data.
#'
#' `grubbs_test()` is an S3 generic supporting both `data.frame`
#' and `env_schema` objects.
#'
#' If the input is grouped using [dplyr::group_by()], Grubbs'
#' tests are performed within the existing groups.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' A tibble containing one row per outlier evaluation group.
#'
#' @details
#' Grubbs' test evaluates whether the most extreme value differs
#' significantly from the remaining observations.
#'
#' Grubbs' test assumes approximately normally distributed data.
#'
#' For many environmental datasets, Rosner's test may be more
#' appropriate when larger sample sizes are available.
#'
#' @section Groundwater Monitoring:
#'
#' Dixon's and Grubbs' tests are generally most appropriate
#' for relatively small datasets and may be sensitive to
#' distributional assumptions.
#'
#' For larger groundwater monitoring datasets,
#' `rosner_test()` is often preferred.
#'
#' @examples
#' grubbs_test(gw_data)
#'
#' @export
grubbs_test <- function(df, ...) {
  UseMethod("grubbs_test")
}

#' @rdname grubbs_test
#'
#' @param location_id Location column name.
#' @param param_name Analyte column name.
#' @param sample_date Sample date column.
#' @param analysis_result Analytical result column name.
#' @param default_unit Reporting unit column name.
#'
#' @export
grubbs_test.data.frame <- function(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  ...
) {
  df <- env_schema(
    df,
    location = !!rlang::sym(location_id),
    analyte = !!rlang::sym(param_name),
    sample_date = !!rlang::sym(sample_date),
    result = !!rlang::sym(analysis_result),
    unit = !!rlang::sym(default_unit)
  )

  grubbs_test(
    df,
    ...
  )
}

#' @rdname grubbs_test
#'
#' @param type Grubbs test variant.
#' @param opposite Logical indicating whether the lowest value
#' should be tested rather than the highest value.
#' @param two_sided Logical indicating whether a two-sided test
#' should be performed.
#' @param keep_data_object Logical indicating whether the complete
#' test object should be retained.
#'
#' @section env_schema Method:
#'
#' For `env_schema` objects, tests are performed using
#' `.analysis_result` values and respect existing
#' `dplyr::group_by()` groups.
#'
#' @export
grubbs_test.env_schema <- function(
  df,
  type = 10,
  opposite = FALSE,
  two_sided = FALSE,
  keep_data_object = FALSE
) {
  validate_env_schema(df)

  df <- df |>
    .build_outlier_groups() |>
    dplyr::mutate(
      grubbs = purrr::map(
        .x = data,
        ~ outliers::grubbs.test(
          .x$.analysis_result,
          type = type,
          opposite = opposite,
          two.sided = two_sided
        )
      )
    )

  if (isTRUE(keep_data_object)) {
    outliers <- df |>
      .extract_htest_results(
        test_col = "grubbs"
      ) |>
      dplyr::select(-data)
  } else {
    outliers <- df |>
      .extract_htest_results(
        test_col = "grubbs"
      ) |>
      dplyr::select(-data, -grubbs) |>
      tidyr::unnest(
        cols = c(
          alternative,
          p_value
        )
      )
  }

  return(outliers)
}

#' Rosner Test
#'
#' Performs Rosner's test for one or more potential outliers in
#' environmental monitoring data.
#'
#' `rosner_test()` is an S3 generic supporting both `data.frame`
#' and `env_schema` objects.
#'
#' If the input is grouped using [dplyr::group_by()], Rosner tests
#' are performed within the existing groups.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' A tibble containing one row per outlier evaluation group.
#'
#' @details
#' Rosner's test evaluates multiple candidate outliers
#' simultaneously and is commonly used in environmental and
#' groundwater statistical evaluations.
#'
#' When groups contain insufficient data, no outlier evaluation is
#' performed and missing values are returned.
#'
#' @section Groundwater Monitoring:
#'
#' Rosner's test is often preferred for groundwater monitoring
#' datasets because it can evaluate multiple potential outliers and
#' generally performs better than Dixon's or Grubbs' tests when
#' sample sizes are sufficiently large.
#'
#' @examples
#' schema <- env_schema(
#'   gw_data,
#'   location = location_id,
#'   sample_date = sample_date,
#'   analyte = param_name,
#'   result = analysis_result,
#'   unit = default_unit
#' )
#'
#' rosner_test(schema)
#'
#' schema |>
#'   dplyr::group_by(location_id) |>
#'   rosner_test()
#'
#' @export
rosner_test <- function(df, ...) {
  UseMethod("rosner_test")
}

#' @rdname rosner_test
#'
#' @param location_id Location column name.
#' @param param_name Analyte column name.
#' @param sample_date Sample date column.
#' @param analysis_result Analytical result column name.
#' @param default_unit Reporting unit column name.
#'
#' @export
rosner_test.data.frame <- function(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  ...
) {
  df <- env_schema(
    df,
    location = !!rlang::sym(location_id),
    analyte = !!rlang::sym(param_name),
    sample_date = !!rlang::sym(sample_date),
    result = !!rlang::sym(analysis_result),
    unit = !!rlang::sym(default_unit)
  )

  rosner_test(
    df,
    ...
  )
}

#' @rdname rosner_test
#'
#' @param k Maximum number of candidate outliers evaluated.
#' @param alpha Significance level.
#' @param warn Logical indicating whether Rosner test warnings
#' should be returned.
#' @param keep_data_object Logical indicating whether the complete
#' Rosner result object should be retained.
#'
#' @section env_schema Method:
#'
#' For `env_schema` objects, Rosner tests use standardized
#' `.analysis_result` values.
#'
#' Existing `dplyr::group_by()` groups are preserved. When the
#' input is not grouped, tests are performed by:
#'
#' \itemize{
#'   \item location
#'   \item analyte
#'   \item analysis unit
#' }
#'
#' @returns
#' A tibble containing one row per outlier evaluation group.
#'
#' The returned table includes:
#' \describe{
#'   \item{Value}{Observation evaluated by Rosner's procedure.}
#'   \item{Outlier}{Logical indicator identifying observations
#'   classified as outliers.}
#' }
#'
#' @export
rosner_test.env_schema <- function(
  df,
  k = 3,
  alpha = 0.05,
  warn = TRUE,
  keep_data_object = FALSE
) {
  validate_env_schema(df)

  df <- df |>
    .build_outlier_groups() |>
    dplyr::mutate(
      rosner = purrr::map(
        data,
        purrr::possibly(
          function(.x) {
            x <- stats::na.omit(
              .x$.analysis_result
            )

            x <- x[
              is.finite(x)
            ]

            if (length(x) < 3) {
              return(NULL)
            }

            if (dplyr::n_distinct(x) < 2) {
              return(NULL)
            }

            k_use <- min(
              k,
              length(x) - 2
            )

            if (k_use < 1) {
              return(NULL)
            }

            suppressWarnings(
              EnvStats::rosnerTest(
                x,
                k = k_use,
                alpha = alpha,
                warn = warn
              )
            )
          },
          otherwise = NULL
        )
      )
    )

  if (isTRUE(keep_data_object)) {
    outliers <- df |>
      dplyr::mutate(
        value = purrr::map(.x = rosner, ~ .x$all.stats[c("Value", "Outlier")])
      ) |>
      dplyr::select(-data)
  } else {
    outliers <- df |>
      dplyr::mutate(
        value = purrr::map(.x = rosner, ~ .x$all.stats[c("Value", "Outlier")])
      ) |>
      dplyr::select(-data, -rosner) |>
      tidyr::unnest(
        cols = c(value)
      )
  }

  return(outliers)
}

#' Tukey Outlier Screening
#'
#' Identifies potential outliers using Tukey's interquartile range
#' (IQR) method.
#'
#' Observations greater than:
#'
#' \deqn{
#' Q3 + k \times IQR
#' }
#'
#' or less than:
#'
#' \deqn{
#' Q1 - k \times IQR
#' }
#'
#' are flagged as potential outliers.
#'
#' `tukey_outlier()` supports both `data.frame`
#' and `env_schema` objects.
#'
#' If the input is grouped using [dplyr::group_by()], screening is
#' performed within each group.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' The original observations with an added logical
#' `outlier` column identifying observations that exceed
#' the specified Tukey IQR thresholds.
#'
#' @section Groundwater Monitoring:
#'
#' Tukey's method should generally be considered a screening tool
#' rather than a formal hypothesis test. Potential outliers
#' identified using this method should be evaluated further before
#' exclusion from statistical analyses.
#'
#' @export
tukey_outlier <- function(df, ...) {
  UseMethod("tukey_outlier")
}

#' @rdname tukey_outlier
#' @export
tukey_outlier.data.frame <- function(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  ...
) {
  df <- env_schema(
    df,
    location = !!rlang::sym(location_id),
    analyte = !!rlang::sym(param_name),
    sample_date = !!rlang::sym(sample_date),
    result = !!rlang::sym(analysis_result),
    unit = !!rlang::sym(default_unit)
  )

  tukey_outlier(
    df,
    ...
  )
}

#' @rdname tukey_outlier
#'
#' @param k IQR multiplier.
#' @param na.rm Logical indicating whether missing values should be
#' removed.
#' @param type Quantile algorithm used in the calculation.
#'
#' @section env_schema Method:
#'
#' For `env_schema` objects, screening is performed using
#' standardized `.analysis_result` values and respects existing
#' `dplyr::group_by()` groups.
#'
#' @export
tukey_outlier.env_schema <- function(
  df,
  k = 3,
  na.rm = FALSE,
  type = 6
) {
  validate_env_schema(df)

  df <- df |>
    .build_outlier_groups() |>
    dplyr::mutate(
      outlier = purrr::map(
        .x = data,
        ~ dplyr::case_when(
          .x$.analysis_result >
            .tukey_high_cutoff(
              .x$.analysis_result,
              k = k,
              na.rm = na.rm,
              type = type
            ) ~ TRUE,
          .x$.analysis_result <
            .tukey_low_cutoff(
              .x$.analysis_result,
              k = k,
              na.rm = na.rm,
              type = type
            ) ~ TRUE,
          TRUE ~ FALSE
        )
      )
    ) |>
    tidyr::unnest(cols = c(data, outlier))

  return(df)
}

#' @keywords internal
.tukey_low_cutoff <- function(x, k = 3, na.rm = FALSE, type = 6) {
  low <- quantile(x, na.rm = na.rm, type = type)[["25%"]] -
    k * IQR(x, na.rm = na.rm, type = type)
  return(low)
}

#' @keywords internal
.tukey_high_cutoff <- function(x, k = 3, na.rm = FALSE, type = 6, ...) {
  high <- quantile(x, na.rm = na.rm, type = type)[["75%"]] +
    k * IQR(x, na.rm = na.rm, type = type)
  return(high)
}

#' Box-Cox Transformation
#'
#' Applies a Box-Cox transformation to analytical results within groups.
#'
#' Transformation parameters are estimated independently for each group
#' using [EnvStats::boxcox()]. Transformed values are calculated using
#' [EnvStats::boxcoxTransform()].
#'
#' Existing analytical results are preserved. Transformed values are
#' returned in a new `.boxcox_transform` column together with the
#' estimated transformation parameter `.boxcox_lambda`.
#'
#' If the input is grouped using [dplyr::group_by()], transformations
#' are performed within the existing groups.
#'
#' For ungrouped `env_schema` objects, transformations are performed by:
#'
#' \itemize{
#'   \item location
#'   \item analyte
#'   \item analysis unit
#' }
#'
#' For ungrouped `data.frame` objects, the entire dataset is treated as
#' a single group.
#'
#' @param x A data frame or `env_schema` object.
#' @param objective_name Character string specifying the optimization
#'   criterion passed to [EnvStats::boxcox()].
#' @param keep_data_object Logical indicating whether the complete
#'   Box-Cox result object should be retained.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' The original data with additional columns:
#'
#' \describe{
#'   \item{.boxcox_transform}{
#'   Box-Cox transformed analytical result.
#'   }
#'
#'   \item{.boxcox_lambda}{
#'   Estimated Box-Cox transformation parameter.
#'   }
#'
#'   \item{boxcox}{
#'   Full Box-Cox optimization result object when
#'   `keep_data_object = TRUE`.
#'   }
#' }
#'
#' @examples
#' gw |>
#'   dplyr::group_by(
#'     location_id,
#'     param_name
#'   ) |>
#'   boxcox_transform()
#'
#' gw |>
#'   boxcox_transform()
#'
#' @export
boxcox_transform <- function(x, ...) {
  UseMethod("boxcox_transform")
}

#' @rdname boxcox_transform
#' @export
boxcox_transform.data.frame <- function(
  x,
  objective_name = "PPCC",
  keep_data_object = FALSE,
  ...
) {
  .boxcox_transform_core(
    x = x,
    result_col = "analysis_result",
    objective_name = objective_name,
    keep_data_object = keep_data_object
  )
}

#' @rdname boxcox_transform
#'
#' @section env_schema Method:
#'
#' For `env_schema` objects, Box-Cox transformations are performed using
#' standardized analytical results stored in `.analysis_result`.
#'
#' Existing `dplyr::group_by()` groups are preserved.
#'
#' When the input is not grouped, transformations are performed by:
#'
#' \itemize{
#'   \item location
#'   \item analyte
#'   \item analysis unit
#' }
#'
#' Schema metadata are preserved.
#'
#' @export
boxcox_transform.env_schema <- function(
  x,
  objective_name = "PPCC",
  keep_data_object = FALSE,
  ...
) {
  validate_env_schema(x)

  if (!dplyr::is_grouped_df(x)) {
    x <- x |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(
            c(
              location_var(x),
              analyte_var(x),
              analysis_unit_var()
            )
          )
        )
      )
  }

  out <- .boxcox_transform_core(
    x = x,
    result_col = analysis_result_var(),
    objective_name = objective_name,
    keep_data_object = keep_data_object
  )

  restore_env_schema(
    out,
    x,
    validate = FALSE
  )
}

#' @keywords internal
.boxcox_transform_core <- function(
  x,
  result_col,
  objective_name = "PPCC",
  keep_data_object = FALSE
) {
  grouped <- dplyr::is_grouped_df(x)

  out <- x |>
    tidyr::nest() |>
    dplyr::mutate(
      boxcox = purrr::map(
        data,
        ~ EnvStats::boxcox(
          .x[[result_col]],
          optimize = TRUE,
          objective.name = objective_name
        )
      ),

      .boxcox_lambda = purrr::map_dbl(
        boxcox,
        "lambda"
      ),

      data = purrr::map2(
        data,
        .boxcox_lambda,
        ~ dplyr::mutate(
          .x,
          .boxcox_transform = EnvStats::boxcoxTransform(
            .x[[result_col]],
            lambda = .y
          )
        )
      )
    )

  if (!keep_data_object) {
    out <- dplyr::select(
      out,
      -boxcox
    )
  }

  out <- out |>
    tidyr::unnest(
      cols = data
    )

  if (!grouped) {
    out <- dplyr::ungroup(out)
  }

  out
}

#' Analysis of Variance
#'
#' @param df data frame
#' @param keep_data_object logical to keep objects from aov and TukeyHSD
#'
#' @examples
#' data(gw_data)
#'
#' wells <- c("MW-1", "MW-2", "MW-3", "MW-4")
#'
#' params <- c("Calcium, dissolved", "Sodium, dissolved", "Sulfate, total")
#'
#' gw_data |>
#'   filter(location_id %in% wells, param_name %in% params) |>
#'   anova()
#'
#' anova_object <- gw_data |>
#'   filter(location_id %in% wells, param_name %in% params) |>
#'   anova(., keep_data_object = TRUE)
#'
#' plot(anova_object$anova[[1]], 1)
#'
#' @export

anova <- function(df, keep_data_object = FALSE) {
  df <- df |>
    mutate(location_id = fct_recode(location_id))

  aov_result <- df |>
    group_by(param_name, default_unit) |>
    nest() |>
    mutate(
      anova = map(
        .x = data,
        ~ aov(analysis_result ~ location_id, data = .x)
      ),
      tukey = map(.x = anova, ~ TukeyHSD(.x))
    ) |>
    ungroup() |>
    select(-data)

  if (isTRUE(keep_data_object)) {
    aov_result |>
      mutate(
        pairwise = map(
          .x = tukey,
          ~ rownames_to_column(data.frame(.x$location_id))
        )
      ) |>
      select(-default_unit)
  } else {
    aov_result |>
      mutate(
        pairwise = map(
          .x = tukey,
          ~ rownames_to_column(data.frame(.x$location_id))
        )
      ) |>
      select(-anova, -tukey, -default_unit) |>
      unnest()
  }
}


#' @keywords internal
.est_dist_group <- function(
  df,
  return_result = FALSE,
  distribution_config,
  result_col
) {
  x <- stats::na.omit(
    df[[result_col]]
  )

  n <- length(x)

  fit_result <- purrr::possibly(
    .fit_dist,
    otherwise = NULL
  )(
    x = x,
    distribution_config = distribution_config
  )

  result <- .handle_dist_result(
    result = fit_result,
    n = n,
    return_result = return_result
  )

  .attach_nested_data(
    result = result,
    data = df
  )
}

#' @keywords internal
.est_dist_worker <- function(
  df,
  return_result,
  distribution_config,
  result_col
) {
  df |>
    dplyr::group_modify(
      ~ .est_dist_group(
        df = .x,
        return_result = return_result,
        distribution_config = distribution_config,
        result_col = result_col
      )
    )
}

#' @keywords internal
.est_dist_censored_group <- function(
  df,
  return_result = FALSE,
  distribution_config,
  result_col
) {
  x <- df[[result_col]]
  censored <- df$.censored

  keep <- !is.na(x)

  x <- x[keep]
  censored <- censored[keep]

  n <- length(x)

  if (any(is.na(censored))) {
    rlang::abort(
      ".censored cannot contain missing values."
    )
  }

  fit_result <- if (!any(censored)) {
    purrr::possibly(
      .fit_dist,
      otherwise = NULL
    )(
      x = x,
      distribution_config = distribution_config
    )
  } else {
    purrr::possibly(
      .fit_dist_censored,
      otherwise = NULL
    )(
      x = x,
      censored = censored,
      distribution_config = distribution_config
    )
  }

  result <- .handle_dist_result(
    result = fit_result,
    n = n,
    return_result = return_result
  )

  .attach_nested_data(
    result = result,
    data = df
  )
}

#' @keywords internal
.est_dist_censored_worker <- function(
  df,
  return_result,
  distribution_config,
  result_col
) {
  df |>
    dplyr::group_modify(
      ~ .est_dist_censored_group(
        df = .x,
        return_result = return_result,
        distribution_config = distribution_config
      )
    )
}

#' Fit candidate distributions
#'
#' Internal wrapper around EnvStats::distChoose().
#'
#' @inheritParams est_dist
#' @param x Numeric vector.
#'
#' @return An EnvStats distChoose object.
#'
#' @keywords internal
#' @noRd
.fit_dist <- function(
  x,
  distribution_config
) {
  do.call(
    EnvStats::distChoose,
    c(
      list(y = x),
      distribution_config
    )
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
#' @keywords internal
#'
#' @noRd
.fit_dist_censored <- function(
  x,
  censored,
  distribution_config
) {
  do.call(
    EnvStats::distChooseCensored,
    c(
      list(
        x = x,
        censored = censored,
        censoring.side = "left"
      ),
      distribution_config
    )
  )
}

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
#' @keywords internal
#'
#' @noRd
.make_dist_result <- function(
  distribution,
  n_used,
  result = NULL,
  return_result = FALSE
) {
  out <- tibble::tibble(
    distribution = distribution,
    n_used = n_used
  )

  if (return_result) {
    out$result <- list(result)
  }

  out
}

#' Helper function for est_dist() and est_dist_censored()
#'
#' @keywords internal
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
        n_used = n,
        return_result = return_result
      )
    )
  }

  .make_dist_result(
    distribution = paste(
      result$decision,
      collapse = ", "
    ),
    n_used = n,
    result = result,
    return_result = return_result
  )
}

#' Compute prediction intervals
#'
#' @keywords internal
#'
#' @noRd
.compute_pred_int <- function(
  df,
  simultaneous = FALSE,
  method = "Bonferroni",
  n_mean = 1,
  k = 1,
  pi_type = "upper",
  conf_level = 0.95
) {
  .validate_interval_distribution(df$distribution)
  .validate_est_dist_output(df)

  rlang::arg_match0(
    pi_type,
    c("upper", "lower", "two-sided")
  )

  checkmate::assert_int(n_mean, lower = 1)
  checkmate::assert_int(k, lower = 1)
  checkmate::assert_number(
    conf_level,
    lower = 0,
    upper = 1
  )

  pred_fun <- function(data, distribution) {
    values <- data$.analysis_result

    switch(
      distribution,

      "Normal" = {
        if (simultaneous) {
          EnvStats::predIntNormSimultaneous(
            x = values,
            n.mean = n_mean,
            k = k,
            pi.type = pi_type,
            conf.level = conf_level
          )
        } else {
          EnvStats::predIntNorm(
            x = values,
            method = method,
            n.mean = n_mean,
            k = k,
            pi.type = pi_type,
            conf.level = conf_level
          )
        }
      },

      "Lognormal" = {
        if (simultaneous) {
          EnvStats::predIntLnormSimultaneous(
            x = values,
            n.geomean = n_mean,
            k = k,
            pi.type = pi_type,
            conf.level = conf_level
          )
        } else {
          EnvStats::predIntLnorm(
            x = values,
            method = method,
            n.geomean = n_mean,
            k = k,
            pi.type = pi_type,
            conf.level = conf_level
          )
        }
      },

      "Nonparametric" = {
        if (simultaneous) {
          EnvStats::predIntNparSimultaneous(
            x = values,
            pi.type = pi_type
          )
        } else {
          EnvStats::predIntNpar(
            x = values,
            pi.type = pi_type
          )
        }
      }
    )
  }

  df |>
    dplyr::mutate(
      pred_int = purrr::map2(
        data,
        distribution,
        pred_fun
      )
    ) |>
    dplyr::mutate(
      sample_size = purrr::map_dbl(
        pred_int,
        "sample.size"
      ),
      conf_level = purrr::map_dbl(
        pred_int,
        ~ .x$interval$conf.level
      ),
      lpl = purrr::map_dbl(
        pred_int,
        ~ unname(.x$interval$limits["LPL"])
      ),
      upl = purrr::map_dbl(
        pred_int,
        ~ unname(.x$interval$limits["UPL"])
      )
    ) |>
    dplyr::select(-data, -pred_int)
}

#' Function to calculate confidence intervals
#'
#' @param df df data frame of groundwater data in tidy format
#' @param analysis_result the analysis result column
#' @param distribution the distribution column
#' @param ci_type character string indicating what kind of confidence interval
#' to compute. The possible values are ci_type="two-sided" (the default),
#' ci_type="lower", and ci_type = "upper
#' @param conf_level a scalar between 0 and 1 indicating the confidence level
#' of the prediction interval. The default value is conf_level = 0.95
#' @param method character string specifying the method of estimation.
#' Possible values are "mvue" (minimum variance unbiased; the default),
#' and "mle/mme" (maximum likelihood/method of moments).
#'
#' @examples
#' data("gw_data")

#' wells <- c("MW-1", "MW-2", "MW-3", "MW-4")

#' params <- c("Sulfate, total",
#'             "Arsenic, dissolved",
#'             "Boron, dissolved")
#'
#' background <- lubridate::ymd(c("2007-12-20", "2012-01-01"), tz = "UTC")

#' # first group data by location, param, and background
#' # estimate percent less than and distribution
#' background_data <- gw_data |>
#'  filter(location_id %in% wells, param_name %in% params,
#'          sample_date >= background[1] & sample_date <= background[2]) |>
#'   group_by(location_id, param_name, default_unit) |>
#'   percent_lt() |>
#'   est_dist(keep_data_object = TRUE) |>
#'   arrange(location_id, param_name)
#'
#' background_data |>
#' conf_int(., ci_type = "lower", conf_level = 0.99)
#'
#' @export

conf_int <- function(
  df,
  analysis_result = "analysis_result",
  distribution = "distribution",
  method = "mvue",
  ci_type = "two-sided",
  conf_level = 0.95
) {
  .validate_interval_distribution(
    df$distribution
  )

  conf_int <- df |>
    mutate(
      conf_int = case_when(
        distribution == "Normal" ~ map(
          .x = data,
          ~ EnvStats::enorm(
            x = .x$analysis_result,
            method = method,
            ci = TRUE,
            ci.type = ci_type,
            conf.level = conf_level,
            ci.param = "mean"
          )
        ),
        distribution == "Lognormal" ~ map(
          .x = data,
          ~ EnvStats::elnormAlt(
            x = .x$analysis_result,
            ci = TRUE,
            ci.type = ci_type,
            ci.method = "land",
            conf.level = conf_level
          )
        ),
        distribution == "Nonparametric" ~ map(
          .x = data,
          ~ EnvStats::eqnpar(
            x = .x$analysis_result,
            ci = TRUE,
            ci.type = ci_type,
            ci.method = "interpolate",
            approx.conf.level = conf_level
          )
        )
      )
    )

  conf_int |>
    mutate(
      distribution = distribution,
      sample_size = map(.x = conf_int, ~ .x$sample.size),
      lcl = map(.x = conf_int, ~ round(.x$interval$limits["LCL"], 3)),
      ucl = map(.x = conf_int, ~ .x$interval$limits["UCL"]),
      conf_level = map(.x = conf_int, ~ .x$interval$conf.level)
    ) |>
    select(-data, -conf_int) |>
    unnest()
}

#' Function to calculate prediction interval
#'
#' @param df df data frame of groundwater data in tidy format
#' @param analysis_result the analysis result column
#' @param distribution the distribution column
#' @param n_mean n.mean positive integer specifying the sample size associated
#' with the future averages.
#' The default value is n.mean=1 (i.e., individual observations).
#' Note that all future averages must be based on the same sample size.
#' @param k k positive integer specifying the number of future observations or
#' averages the prediction interval should contain with confidence level
#' conf.level. The default value is k=1.
#' @param method default is "Bonferroni"
#' @param pi_type character string indicating what kind of prediction interval
#' to compute. The possible values are pi_type="two-sided" (the default),
#' pi_type="upper", and pi_type="lower".
#' @param conf_level a scalar between 0 and 1 indicating the confidence level
#' of the prediction interval. The default value is conf.level=0.95
#'
#' @examples
#' data("gw_data")
#'
#' wells <- c("MW-1", "MW-2", "MW-3", "MW-4")
#'
#' params <- c("Sulfate, total",
#'             "Arsenic, dissolved",
#'             "Boron, dissolved")
#'
#' background <- lubridate::ymd(c("2007-12-20", "2012-01-01"), tz = "UTC")
#'
#' # first group data by location, param, and background
#' # estimate percent less than and distribution
#' background_data <- gw_data |>
#'  filter(location_id %in% wells, param_name %in% params,
#'          sample_date >= background[1] & sample_date <= background[2]) |>
#'   group_by(location_id, param_name, default_unit) |>
#'   percent_lt() |>
#'   est_dist(keep_data_object = TRUE) |>
#'   arrange(location_id, param_name)
#'
#' background_data |>
#' pred_int(., pi_type = "upper", conf_level = 0.99)
#'
#'
#' @export
pred_int <- function(
  df,
  method = "Bonferroni",
  n_mean = 1,
  k = 1,
  pi_type = "upper",
  conf_level = 0.95
) {
  .validate_interval_distribution(
    df$distribution
  )

  .compute_pred_int(
    df = df,
    simultaneous = FALSE,
    method = method,
    n_mean = n_mean,
    k = k,
    pi_type = pi_type,
    conf_level = conf_level
  )
}

#' Function to calculate simultaneous prediction interval
#'
#' @param df df data frame of groundwater data in tidy format
#' @param analysis_result the analysis result column
#' @param distribution the distribution column
#' @param n_mean n.mean positive integer specifying the sample size associated
#' with the future averages.
#' The default value is n.mean=1 (i.e., individual observations).
#' Note that all future averages must be based on the same sample size.
#' @param k k positive integer specifying the number of future observations or
#' averages the prediction interval should contain with confidence level
#' conf.level. The default value is k=1.
#' @param m m if m = 1, then same results as predInt
#' @param r r
#' @param rule rule "k.of.m"
#' @param pi_type character string indicating what kind of prediction interval
#' to compute. The possible values are pi_type="upper" (the default),
#' and pi_type="lower".
#' @param conf_level a scalar between 0 and 1 indicating the confidence level
#' of the prediction interval. The default value is conf.level=0.95
#'
#' @examples
#' data("gw_data")
#'
#' wells <- c("MW-1", "MW-2", "MW-3", "MW-4")
#'
#' params <- c("Sulfate, total",
#'             "Arsenic, dissolved",
#'             "Boron, dissolved")
#'
#' background <- lubridate::ymd(c("2007-12-20", "2012-01-01"), tz = "UTC")
#'
#' # first group data by location, param, and background
#' # estimate percent less than and distribution
#' background_data <- gw_data |>
#'  filter(location_id %in% wells, param_name %in% params,
#'          sample_date >= background[1] & sample_date <= background[2]) |>
#'   group_by(location_id, param_name, default_unit) |>
#'   percent_lt() |>
#'   est_dist(., keep_data_object = TRUE) |>
#'   arrange(location_id, param_name)
#'
#' background_data |>
#' pred_int_sim(pi_type = "upper", conf_level = 0.99)
#'
#' @export

pred_int_sim <- function(
  df,
  n_mean = 1,
  k = 1,
  pi_type = "upper",
  conf_level = 0.95
) {
  .validate_interval_distribution(
    df$distribution
  )

  .compute_pred_int(
    df = df,
    simultaneous = TRUE,
    n_mean = n_mean,
    k = k,
    pi_type = pi_type,
    conf_level = conf_level
  )
}

#' Function to calculate tolernace intervals
#'
#' @param df df data frame of groundwater data in tidy format
#' @param analysis_result the analysis result column
#' @param distribution the distribution column
#' @param coverage a scalar between 0 and 1 indicating the desired coverage of
#' the tolerance interval. The default value is coverage=0.95.
#' If cov.type="expectation", this argument is ignored
#' @param cov_type character string specifying the coverage type for the
#' tolerance interval. The possible values are "content"
#' (β-content; the default), and "expectation" (β-expectation).
#' @param method for the case of a two-sided tolerance interval, a character
#' string specifying the method for constructing the tolerance interval.
#' This argument is ignored if ti.type="lower" or ti.type="upper".
#' The possible values are "exact" (the default) and
#' "wald.wolfowitz" (the Wald-Wolfowitz approximation).
#' @param ti_type character string indicating what kind of tolerance interval
#' to compute. The possible values are ti_type="two-sided",
#' ti_type="upper", and ti_type="lower" (the default).
#' @param conf_level a scalar between 0 and 1 indicating the confidence level
#' of the tolerance interval. The default value is conf.level=0.95
#' @param est_method for tolIntLnormAlt, a character string specifying the
#' method of estimating the mean and coefficient of variation.
#' This argument has no effect on the method of constructing the
#' tolerance interval. Possible values are "mvue" (minimum variance unbiased;
#' the default), "qmle" (quasi maximum likelihood), "mle" (maximum likelihood),
#'  "mme" (method of moments), and "mmue" (method of moments based on the
#'  unbiased estimate of variance).
#'
#' @export

tol_int <- function(
  df,
  analysis_result = "analysis_result",
  distribution = "distribution",
  coverage = 0.95,
  cov_type = "content",
  ti_type = "two-sided",
  method = "exact"
) {
  .validate_interval_distribution(
    df$distribution
  )

  tol_int <- df |>
    mutate(
      tol_int = case_when(
        distribution == "Normal" ~ map(
          .x = data,
          ~ EnvStats::tolIntNorm(
            x = .x$analysis_result,
            coverage = coverage,
            cov.type = cov_type,
            ti.type = ti_type,
            method = method
          )
        ),
        distribution == "Lognormal" ~ map(
          .x = data,
          ~ EnvStats::tolIntLnorm(
            x = .x$analysis_result,
            coverage = coverage,
            cov.type = cov_type,
            ti.type = ti_type,
            method = method
          )
        ),
        distribution == "Nonparametric" ~ map(
          .x = data,
          ~ EnvStats::tolIntNpar(
            x = .x$analysis_result,
            coverage = coverage,
            cov.type = cov_type,
            ti.type = ti_type
          )
        )
      )
    )

  tol_int |>
    mutate(
      distribution = distribution,
      sample_size = map(.x = tol_int, ~ .x$sample.size),
      ltl = map(.x = tol_int, ~ .x$interval$limits["LTL"]),
      utl = map(.x = tol_int, ~ .x$interval$limits["UTL"]),
      conf_level = map(.x = tol_int, ~ .x$interval$conf.level)
    ) |>
    select(-data, -tol_int) |>
    unnest()
}

#' Function to update background data
#'
#' @param df data frame of groundwater data in tidy format
#' @param well the location to update
#' @param param the constituent to update
#' @param start the start date of the background data. IF not provide the
#' start of the data will be found using @seealso [start_date()]
#' @param n_end the number of samples in current background. The date will be
#' found from this sample.
#' @param n_prop the number of samples to include in the proposed background
#'
#' @export

update_background <- function(
  df,
  well,
  param,
  start = NULL,
  n_end = 8,
  n_prop = 4
) {
  tmp <- df |>
    filter_(~ location_id == well, ~ param_name == param)

  if (is.null(start)) {
    start <- tmp |>
      start_date()
  }

  end <- tmp |>
    nth_date(start = start, n = n_end - 1)

  max <- tmp |>
    select_(~sample_date) |>
    first() |>
    max()

  bkgd <- tmp |>
    set_background(start = start, end = end)

  next_date <- tmp |>
    nth_date(start = end, n = n_prop)

  next_bkgd <- tmp |>
    set_proposed_background(start = end, end = next_date)

  outlier <- EnvStats::rosnerTest(bkgd$analysis_result, k = 1, alpha = 0.01)

  trend <- EnvStats::kendallTrendTest(
    bkgd$analysis_result ~ bkgd$sample_date,
    conf.level = 0.99
  )

  two_sample <- EnvStats::twoSampleLinearRankTest(
    bkgd$analysis_result,
    next_bkgd$analysis_result,
    test = "wilcoxon"
  )

  output <- list(
    rosner_test = outlier,
    kendall_trend = trend,
    two_sample = two_sample
  )

  output
}

#' @keywords internal
.validate_interval_distribution <- function(distribution) {
  supported <- c(
    "Normal",
    "Lognormal",
    "Nonparametric"
  )

  bad <- setdiff(
    unique(stats::na.omit(distribution)),
    supported
  )

  if (length(bad) > 0) {
    rlang::abort(
      paste(
        "Intervals are only available for:",
        paste(supported, collapse = ", "),
        "\nUnsupported distribution(s):",
        paste(bad, collapse = ", ")
      )
    )
  }

  invisible(distribution)
}
