#' Summary function
#'
#' @param df dataframe of groundwater data in tidy format
#' @param location_id column of sample locations
#' @param param_name column of constituents
#' @param lt_measure column of less than symbols for non-detect data
#' @param analysis_result column for numerical result
#' @param default_unit column of units
#' @param combine_locations logical. Summarize by individual sample location
#' or by grouping all sample locations. Default is FALSE.
#' @param na.rm TRUE/FALSE for missing values
#'
#' @export

summary_stats <- function(
  df,
  location_id = "location_id",
  param_name = "param_name",
  lt_measure = "lt_measure",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  combine_locations = FALSE,
  na.rm = TRUE
) {
  if (!combine_locations) {
    df %>%
      dplyr::group_by(
        .data[[location_id]],
        .data[[param_name]],
        .data[[default_unit]]
      ) %>%
      dplyr::summarise(
        n = dplyr::n(),
        percent_lt = sum(
          .data[[lt_measure]] == "<",
          na.rm = na.rm
        ) /
          dplyr::n() *
          100,
        mean = mean(
          .data[[analysis_result]],
          na.rm = na.rm
        ),
        median = stats::median(
          .data[[analysis_result]],
          na.rm = na.rm
        ),
        sd = stats::sd(
          .data[[analysis_result]],
          na.rm = na.rm
        ),
        var = stats::var(
          .data[[analysis_result]],
          na.rm = na.rm
        ),
        min = min(
          .data[[analysis_result]],
          na.rm = na.rm
        ),
        max = max(
          .data[[analysis_result]],
          na.rm = na.rm
        ),
        cv = cv(
          .data[[analysis_result]],
          na.rm = na.rm
        ),
        `25%` = stats::quantile(
          .data[[analysis_result]],
          na.rm = na.rm
        )[2],
        `75%` = stats::quantile(
          .data[[analysis_result]],
          na.rm = na.rm
        )[4],
        IQR = stats::IQR(
          .data[[analysis_result]],
          na.rm = na.rm
        ),
        mad = stats::mad(
          .data[[analysis_result]],
          na.rm = na.rm
        ),
        .groups = "drop"
      )
  } else {
    df %>%
      dplyr::group_by(
        .data[[param_name]],
        .data[[default_unit]]
      ) %>%
      dplyr::summarise(
        n = dplyr::n(),
        percent_lt = sum(
          .data[[lt_measure]] == "<",
          na.rm = na.rm
        ) /
          dplyr::n() *
          100,
        mean = mean(
          .data[[analysis_result]],
          na.rm = na.rm
        ),
        median = stats::median(
          .data[[analysis_result]],
          na.rm = na.rm
        ),
        sd = stats::sd(
          .data[[analysis_result]],
          na.rm = na.rm
        ),
        var = stats::var(
          .data[[analysis_result]],
          na.rm = na.rm
        ),
        min = min(
          .data[[analysis_result]],
          na.rm = na.rm
        ),
        max = max(
          .data[[analysis_result]],
          na.rm = na.rm
        ),
        cv = cv(
          .data[[analysis_result]],
          na.rm = na.rm
        ),
        `25%` = stats::quantile(
          .data[[analysis_result]],
          na.rm = na.rm
        )[2],
        `75%` = stats::quantile(
          .data[[analysis_result]],
          na.rm = na.rm
        )[4],
        IQR = stats::IQR(
          .data[[analysis_result]],
          na.rm = na.rm
        ),
        mad = stats::mad(
          .data[[analysis_result]],
          na.rm = na.rm
        ),
        .groups = "drop"
      )
  }
}
