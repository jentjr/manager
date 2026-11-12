# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

build_stiff_schema <- function(df = gw_data) {
  suppressWarnings(
    env_schema(
      df,
      location = location_id,
      analyte = param_name,
      result = analysis_result,
      unit = default_unit,
      sample_date = sample_date
    )
  )
}

# ------------------------------------------------------------------------------
# stiff_plot()
# ------------------------------------------------------------------------------

test_that("stiff_plot returns a named list", {
  schema <- build_stiff_schema()

  p <- stiff_plot(schema)

  expect_type(p, "list")
  expect_named(p)
})

test_that("list elements are ggplot objects", {
  schema <- build_stiff_schema()

  p <- stiff_plot(schema)

  expect_s3_class(
    p[[1]],
    "ggplot"
  )
})

test_that("returns one plot per location", {
  schema <- build_stiff_schema()

  plots <- stiff_plot(schema)

  expect_length(
    plots,
    dplyr::n_distinct(
      schema[[location_var(schema)]]
    )
  )
})

test_that("returns one plot per sample date", {
  schema <- build_stiff_schema()

  plots <- suppressWarnings(
    stiff_plot(
      schema,
      group_var = sample_date_var(schema)
    )
  )

  expect_length(
    plots,
    dplyr::n_distinct(
      schema[[sample_date_var(schema)]]
    )
  )
})

test_that("plot names equal grouping values", {
  schema <- build_stiff_schema()

  plots <- stiff_plot(schema)

  expect_setequal(
    names(plots),
    as.character(
      unique(
        schema[[location_var(schema)]]
      )
    )
  )
})

test_that("invalid group_var throws error", {
  schema <- build_stiff_schema()

  expect_error(
    stiff_plot(
      schema,
      group_var = "site_id"
    ),
    "site_id"
  )
})

test_that("invalid facet_var throws error", {
  schema <- build_stiff_schema()

  expect_error(
    stiff_plot(
      schema,
      facet_var = "site_id"
    ),
    "not found"
  )
})

test_that("missing major ions are handled gracefully", {
  schema <- build_stiff_schema(
    dplyr::filter(
      gw_data,
      param_name != "Calcium, dissolved"
    )
  )

  expect_no_error(
    stiff_plot(schema)
  )
})

# ------------------------------------------------------------------------------
# stiff_data()
# ------------------------------------------------------------------------------

test_that("Na + K is correctly calculated", {
  schema <- build_stiff_schema()

  stiff <- stiff_data(schema)

  expect_true(
    "Na + K" %in% stiff$ion
  )
})

test_that("stiff coordinates are assigned correctly", {
  schema <- build_stiff_schema()

  stiff <- stiff_data(schema)

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
          c("SO[4]", "HCO[3]", "Cl")
      ] >=
        0,
      na.rm = TRUE
    )
  )
})

test_that("scale_analyte works", {
  schema <- build_stiff_schema()

  expect_no_error(
    stiff_data(
      schema,
      scale_analyte = "Total Dissolved Solids"
    )
  )
})

test_that("scale_value column is present", {
  schema <- build_stiff_schema()

  stiff <- stiff_data(
    schema,
    scale_analyte = "Total Dissolved Solids"
  )

  expect_true(
    "scale_value" %in% names(stiff)
  )
})

test_that("faceting variable is applied", {
  schema <- build_stiff_schema()

  p <- stiff_plot(
    schema,
    facet_var = sample_date_var(schema)
  )[[1]]

  expect_s3_class(
    p$facet,
    "FacetWrap"
  )
})

test_that("custom title overrides default", {
  schema <- build_stiff_schema()

  plots <- stiff_plot(
    schema,
    title = "Test Title"
  )

  expect_equal(
    plots[[1]]$labels$title,
    "Test Title"
  )
})
