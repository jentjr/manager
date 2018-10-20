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
#' gw_data %>%
#'   filter(location_id %in% wells, param_name %in% params) %>%
#'   anova()
#'
#' anova_object <- gw_data %>%
#'   filter(location_id %in% wells, param_name %in% params) %>%
#'   anova(., keep_data_object = TRUE)
#'
#' plot(anova_object$anova[[1]], 1)
#'
#' @export

anova <- function(df, keep_data_object = FALSE) {

  df <- df %>%
    mutate(location_id = fct_recode(location_id))

  aov_result <- df %>%
    group_by(param_name, default_unit) %>%
    nest() %>%
    mutate(
      anova = map(
        .x = data, ~aov(analysis_result ~ location_id, data = .x)),
      tukey = map(.x = anova, ~TukeyHSD(.x))
    ) %>%
    ungroup() %>%
    select(-data)

  if (isTRUE(keep_data_object)) {
    aov_result %>%
      mutate(pairwise = map(.x = tukey,
                          ~rownames_to_column(data.frame(.x$location_id)))) %>%
      select(-default_unit)
  } else {
    aov_result %>%
      mutate(pairwise = map(.x = tukey,
                          ~rownames_to_column(data.frame(.x$location_id)))) %>%
      select(-anova, -tukey, -default_unit) %>%
      unnest()
  }

}
