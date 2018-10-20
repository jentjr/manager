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

update_background <- function(df, well, param, start = NULL,
                              n_end = 8, n_prop = 4) {

  tmp <- df %>%
    filter_(~location_id == well, ~param_name == param)

  if (is.null(start)) {
    start <- tmp %>%
      start_date()
  }

  end <- tmp %>%
    nth_date(start = start, n = n_end - 1)

  max <- tmp %>%
    select_(~sample_date) %>%
    first() %>%
    max()

  bkgd <- tmp %>%
    set_background(start = start, end = end)

  next_date <- tmp %>%
    nth_date(start = end, n = n_prop)

  next_bkgd <- tmp %>%
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

  output <- list(rosner_test = outlier,
                 kendall_trend = trend,
                 two_sample = two_sample)

  output

}