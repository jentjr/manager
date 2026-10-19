test_that("default Piper plot remains stable", {
  vdiffr::expect_doppelganger(
    "default-piper-plot",
    piper_plot(gw_data)
  )
})

test_that("grouped Piper plot remains stable", {
  group_df <- gw_data %>%
    dplyr::distinct(location_id) %>%
    dplyr::mutate(group = "A")

  df <- gw_data %>%
    dplyr::left_join(
      group_df,
      by = "location_id"
    )

  vdiffr::expect_doppelganger(
    "grouped-piper-plot",
    piper_plot(
      df,
      group_col = "group"
    )
  )
})

test_that("scaled Piper plot remains stable", {
  vdiffr::expect_doppelganger(
    "scaled-piper-plot",
    piper_plot(
      gw_data,
      scale_analyte = "Total Dissolved Solids"
    )
  )
})
