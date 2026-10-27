#' Summary statistics
#'
#' @param df Data object.
#' @param ... Additional arguments.
#'
#' @export
summary_stats <- function(df, ...) {
  UseMethod("summary_stats")
}

#' @export
summary_stats.data.frame <- function(
  df,
  location_id = "location_id",
  param_name = "param_name",
  lt_measure = "lt_measure",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  na.rm = TRUE
) {
  if (dplyr::is_grouped_df(df)) {
    group_cols <- dplyr::group_vars(df)
  } else {
    group_cols <- c(
      location_id,
      param_name,
      default_unit
    )
  }

  .summary_stats_core(
    df = df,
    group_cols = group_cols,
    result_col = analysis_result,
    lt_measure_col = lt_measure,
    na.rm = na.rm
  )
}

#' @export
summary_stats.env_schema <- function(
  df,
  na.rm = TRUE,
  ...
) {
  analysis_df <- .as_analysis_tbl(df)

  if (dplyr::is_grouped_df(analysis_df)) {
    group_cols <- dplyr::group_vars(analysis_df)
  } else {
    group_cols <- c(
      "location",
      "analyte",
      "unit"
    )
  }

  .summary_stats_core(
    df = analysis_df,
    group_cols = group_cols,
    result_col = "result",
    censored_col = "censored",
    na.rm = na.rm
  )
}

#' Summary statistics core
#'
#' @keywords internal
.summary_stats_core <- function(
  df,
  group_cols,
  result_col,
  na.rm = TRUE,
  censored_col = NULL,
  lt_measure_col = NULL
) {
  missing_cols <- setdiff(
    c(
      group_cols,
      result_col,
      censored_col,
      lt_measure_col
    ),
    names(df)
  )

  missing_cols <- missing_cols[!is.na(missing_cols)]

  if (length(missing_cols) > 0) {
    rlang::abort(
      paste(
        "Missing required columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  if (length(group_cols) > 0) {
    df <- df %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(group_cols)
        ),
        .add = FALSE
      )
  }

  df %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(group_cols)
      ),
      .add = FALSE
    ) %>%
    dplyr::summarise(
      n = dplyr::n(),

      percent_lt = if (!is.null(censored_col)) {
        mean(
          .data[[censored_col]],
          na.rm = na.rm
        ) *
          100
      } else {
        sum(
          .data[[lt_measure_col]] == "<",
          na.rm = na.rm
        ) /
          dplyr::n() *
          100
      },

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
    )
}
