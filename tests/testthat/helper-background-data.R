background_data <- gw_data %>%
  dplyr::filter(
    location_id %in% c("MW-1", "MW-2", "MW-3", "MW-4"),
    param_name %in%
      c(
        "Sulfate, total",
        "Arsenic, dissolved",
        "Boron, dissolved"
      )
  ) %>%
  dplyr::group_by(
    location_id,
    param_name,
    default_unit
  ) %>%
  percent_lt() %>%
  est_dist()
