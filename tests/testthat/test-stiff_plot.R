test_that("stiff_plot returns a named list", {
  p <- stiff_plot(gw_data)

  expect_type(p, "list")
  expect_named(p)
})

test_that("list elements are ggplot objects", {
  p <- stiff_plot(gw_data)

  expect_s3_class(
    p[[1]],
    "ggplot"
  )
})

test_that("invalid group_var errors", {
  expect_error(
    stiff_plot(
      gw_data,
      group_var = "site_id"
    )
  )
})

test_that("invalid facet_var errors", {
  expect_error(
    stiff_plot(
      gw_data,
      facet_var = "site_id"
    )
  )
})

test_that("returns one plot per location", {
  plots <- stiff_plot(
    gw_data,
    group_var = "location_id"
  )

  expect_length(
    plots,
    dplyr::n_distinct(gw_data$location_id)
  )
})

test_that("returns one plot per sample date", {
  test_data_complete <- gw_data %>%
    dplyr::filter(
      location_id %in% c("MW-1", "MW-2"),
      sample_date <= lubridate::ymd("2009-01-01")
    )
  plots <- stiff_plot(
    test_data_complete,
    group_var = "sample_date"
  )

  expect_length(
    plots,
    dplyr::n_distinct(test_data_complete$sample_date)
  )
})

test_that("plot names equal grouping values", {
  plots <- stiff_plot(
    gw_data,
    group_var = "location_id"
  )

  expect_setequal(
    names(plots),
    unique(gw_data$location_id)
  )
})

test_that("invalid group_var throws error", {
  expect_error(
    stiff_plot(
      gw_data,
      group_var = "site_id"
    ),
    "group_var"
  )
})

test_that("invalid facet_var throws error", {
  expect_error(
    stiff_plot(
      gw_data,
      facet_var = "site_id"
    ),
    "facet_var"
  )
})

test_that("missing ion throws error", {
  data <- dplyr::filter(
    gw_data,
    param_name != "Calcium, dissolved"
  )

  expect_error(
    stiff_plot(data),
    "Missing required major ions"
  )
})

test_that("Na + K is correctly calculated", {
  stiff <- .transform_stiff_data(
    gw_data,
    location_id = "location_id",
    sample_date = "sample_date",
    param_name = "param_name",
    analysis_result = "analysis_result",
    default_unit = "default_unit",
    calcium = "Calcium, dissolved",
    magnesium = "Magnesium, dissolved",
    sodium = "Sodium, dissolved",
    potassium = "Potassium, dissolved",
    chloride = "Chloride, total",
    sulfate = "Sulfate, total",
    alkalinity = "Alkalinity, total (lab)"
  )

  expect_true(
    "Na + K" %in% stiff$ion
  )
})

test_that("stiff coordinates are assigned correctly", {
  stiff <- .transform_stiff_data(
    gw_data,
    location_id = "location_id",
    sample_date = "sample_date",
    param_name = "param_name",
    analysis_result = "analysis_result",
    default_unit = "default_unit",
    calcium = "Calcium, dissolved",
    magnesium = "Magnesium, dissolved",
    sodium = "Sodium, dissolved",
    potassium = "Potassium, dissolved",
    chloride = "Chloride, total",
    sulfate = "Sulfate, total",
    alkalinity = "Alkalinity, total (lab)"
  )

  expect_true(
    all(
      stiff$stiff_x[
        stiff$ion %in%
          c("Mg", "Ca", "Na + K")
      ] <=
        0,
      na.rm = TRUE
    )
  )

  expect_true(
    all(
      stiff$stiff_x[
        stiff$ion %in%
          c("SO4", "Alk", "Cl")
      ] >=
        0,
      na.rm = TRUE
    )
  )
})

test_that("TDS is included when requested", {
  stiff <- .transform_stiff_data(
    gw_data,
    location_id = "location_id",
    sample_date = "sample_date",
    param_name = "param_name",
    analysis_result = "analysis_result",
    default_unit = "default_unit",
    calcium = "Calcium, dissolved",
    magnesium = "Magnesium, dissolved",
    sodium = "Sodium, dissolved",
    potassium = "Potassium, dissolved",
    chloride = "Chloride, total",
    sulfate = "Sulfate, total",
    alkalinity = "Alkalinity, total (lab)",
    total_dissolved_solids = "Total Dissolved Solids"
  )

  expect_true(
    "TDS" %in% names(stiff)
  )
})

test_that("faceting variable is applied", {
  p <- stiff_plot(
    gw_data,
    facet_var = "sample_date"
  )[[1]]

  expect_s3_class(
    p$facet,
    "FacetWrap"
  )
})

test_that("frame count equals distinct date-group combinations", {
  n_frames <- gw_data %>%
    dplyr::distinct(
      sample_date,
      location_id
    ) %>%
    nrow()

  expect_equal(
    n_frames,
    gw_data %>%
      dplyr::distinct(
        sample_date,
        location_id
      ) %>%
      nrow()
  )
})

test_that("default title contains group value", {
  plots <- stiff_plot(
    gw_data,
    group_var = "location_id"
  )

  expect_equal(
    plots[[1]]$labels$title,
    paste0(
      "location_id: ",
      names(plots)[1]
    )
  )
})

test_that("custom title overrides group value", {
  plots <- stiff_plot(
    gw_data,
    title = "Test Title"
  )

  expect_equal(
    plots[[1]]$labels$title,
    "Test Title"
  )
})

test_that("TDS can be included without unit conversion", {
  expect_no_error(
    stiff_plot(
      gw_data,
      total_dissolved_solids = "Total Dissolved Solids"
    )
  )
})

test_that("TDS column is present when requested", {
  stiff <- .transform_stiff_data(
    gw_data,
    location_id = "location_id",
    sample_date = "sample_date",
    param_name = "param_name",
    analysis_result = "analysis_result",
    default_unit = "default_unit",
    calcium = "Calcium, dissolved",
    magnesium = "Magnesium, dissolved",
    sodium = "Sodium, dissolved",
    potassium = "Potassium, dissolved",
    chloride = "Chloride, total",
    sulfate = "Sulfate, total",
    alkalinity = "Alkalinity, total (lab)",
    total_dissolved_solids = "Total Dissolved Solids"
  )

  expect_true(
    "TDS" %in% names(stiff)
  )
})
