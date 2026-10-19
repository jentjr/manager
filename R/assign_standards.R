#' Function to assign Standards
#'
#' @param df dataframe of data in tidy format
#' @param column column containing the constituent names
#'
#' @export

assign_standards <- function(
  df,
  column = "param_name"
) {
  df %>%
    dplyr::mutate(
      param_base = sub(
        ",.*$",
        "",
        .data[[column]]
      )
    ) %>%
    dplyr::left_join(
      limits_tbl,
      by = c(
        "param_base" = "param_name"
      )
    ) %>%
    .convert_standard_units(
      data_unit_col = "default_unit"
    ) %>%
    dplyr::select(
      -param_base
    )
}
