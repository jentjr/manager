test_that("piper_plot returns a ggplot object", {
  p <- piper_plot(gw_data)

  expect_s3_class(
    p,
    "ggplot"
  )
})

test_that("piper_plot errors when required columns are missing", {
  bad_df <- dplyr::select(
    gw_data,
    -default_unit
  )

  expect_error(
    piper_plot(bad_df),
    "Missing required columns"
  )
})

test_that("piper_plot validates analyte_matching", {
  expect_error(
    piper_plot(
      gw_data,
      analyte_matching = "invalid"
    )
  )
})

test_that("piper_plot accepts exact matching", {
  expect_no_error(
    piper_plot(
      gw_data,
      analyte_matching = "exact"
    )
  )
})

test_that("piper_plot accepts strict matching", {
  expect_no_error(
    piper_plot(
      gw_data,
      analyte_matching = "strict"
    )
  )
})

test_that("piper_plot accepts normalized matching", {
  expect_no_error(
    piper_plot(
      gw_data,
      analyte_matching = "normalized"
    )
  )
})

test_that("piper_plot accepts scale_analyte", {
  expect_no_error(
    piper_plot(
      gw_data,
      scale_analyte = "Total Dissolved Solids"
    )
  )
})

test_that("piper_plot warns when scale analyte cannot be resolved", {
  expect_warning(
    piper_plot(
      gw_data,
      scale_analyte = "This Analyte Does Not Exist"
    ),
    "Size scaling analyte not available"
  )
})

test_that("piper_plot applies title", {
  p <- piper_plot(
    gw_data,
    title = "Test Piper Plot"
  )

  expect_equal(
    p$labels$title,
    "Test Piper Plot"
  )
})

test_that("piper_plot accepts plotmath labels", {
  expect_no_error(
    piper_plot(
      gw_data,
      x_cation_label = "Ca~phantom()^+2",
      y_cation_label = "Mg~phantom()^+2",
      z_anion_label = "SO[4]^-2"
    )
  )
})

test_that("piper_plot accepts group_col with one value per location", {
  group_df <- gw_data %>%
    dplyr::distinct(location_id) %>%
    dplyr::mutate(
      group = ifelse(
        seq_len(dplyr::n()) %% 2 == 0,
        "A",
        "B"
      )
    )

  df <- gw_data %>%
    dplyr::left_join(
      group_df,
      by = "location_id"
    )

  expect_no_error(
    piper_plot(
      df,
      group_col = "group"
    )
  )
})

test_that("piper_plot errors when a location has multiple group values", {
  df <- gw_data

  first_location <- unique(df$location_id)[1]

  df$group <- "A"

  df$group[
    df$location_id == first_location
  ] <- rep(
    c("A", "B"),
    length.out = sum(
      df$location_id == first_location
    )
  )

  expect_error(
    piper_plot(
      df,
      group_col = "group"
    ),
    "must contain only one value per location"
  )
})

test_that("piper_plot returns ggplot when scale_analyte is used", {
  p <- piper_plot(
    gw_data,
    scale_analyte = "Total Dissolved Solids"
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})
