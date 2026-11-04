# test-piper_plot.R

library(testthat)
library(dplyr)

# ------------------------------------------------------------------------------
# Test fixture
# ------------------------------------------------------------------------------

gw_piper <- tibble::tribble(
  ~location_id , ~sample_date          , ~param_name               , ~analysis_result , ~default_unit ,

  "MW-1"       , as.Date("2020-01-01") , "Calcium, dissolved"      ,               40 , "mg/L"        ,
  "MW-1"       , as.Date("2020-01-01") , "Magnesium, dissolved"    ,               12 , "mg/L"        ,
  "MW-1"       , as.Date("2020-01-01") , "Sodium, dissolved"       ,               25 , "mg/L"        ,
  "MW-1"       , as.Date("2020-01-01") , "Potassium, dissolved"    ,                3 , "mg/L"        ,
  "MW-1"       , as.Date("2020-01-01") , "Chloride, total"         ,               18 , "mg/L"        ,
  "MW-1"       , as.Date("2020-01-01") , "Sulfate, total"          ,               30 , "mg/L"        ,
  "MW-1"       , as.Date("2020-01-01") , "Alkalinity, total (lab)" ,              110 , "mg/L"        ,

  "MW-2"       , as.Date("2020-01-01") , "Calcium, dissolved"      ,               55 , "mg/L"        ,
  "MW-2"       , as.Date("2020-01-01") , "Magnesium, dissolved"    ,               14 , "mg/L"        ,
  "MW-2"       , as.Date("2020-01-01") , "Sodium, dissolved"       ,               20 , "mg/L"        ,
  "MW-2"       , as.Date("2020-01-01") , "Potassium, dissolved"    ,                2 , "mg/L"        ,
  "MW-2"       , as.Date("2020-01-01") , "Chloride, total"         ,               12 , "mg/L"        ,
  "MW-2"       , as.Date("2020-01-01") , "Sulfate, total"          ,               22 , "mg/L"        ,
  "MW-2"       , as.Date("2020-01-01") , "Alkalinity, total (lab)" ,              135 , "mg/L"
)

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

build_schema <- function(df = gw_piper) {
  env_schema(
    df,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  )
}

piper_plot_df <- function(df = gw_piper, ...) {
  piper_plot(
    df,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    ...
  )
}

piper_plot_schema <- function(df = gw_piper, ...) {
  piper_plot(
    build_schema(df),
    ...
  )
}

piper_scale_df <- function() {
  bind_rows(
    gw_piper,
    tibble::tribble(
      ~location_id             , ~sample_date          , ~param_name ,
      ~analysis_result         , ~default_unit         ,

      "MW-1"                   , as.Date("2020-01-01") ,
      "Total Dissolved Solids" ,                   250 , "mg/L"      ,

      "MW-2"                   , as.Date("2020-01-01") ,
      "Total Dissolved Solids" ,                   325 , "mg/L"
    )
  )
}

# ------------------------------------------------------------------------------
# S3 dispatch
# ------------------------------------------------------------------------------

test_that("data.frame method dispatches correctly", {
  p <- piper_plot_df()

  expect_s3_class(
    p,
    "ggplot"
  )
})

test_that("data.frame method requires mappings", {
  expect_error(
    piper_plot(gw_piper),
    "location.*date.*analyte.*result"
  )
})

# ------------------------------------------------------------------------------
# env_schema method
# ------------------------------------------------------------------------------

test_that("env_schema method returns ggplot", {
  p <- piper_plot_schema()

  expect_s3_class(
    p,
    "ggplot"
  )
})

test_that("invalid analyte matching throws error", {
  expect_error(
    piper_plot_schema(
      analyte_matching = "invalid"
    )
  )
})

test_that("all supported analyte matching methods work", {
  methods <- c(
    "exact",
    "strict",
    "normalized"
  )

  purrr::walk(
    methods,
    ~ expect_no_error(
      piper_plot_schema(
        analyte_matching = .x
      )
    )
  )
})

# ------------------------------------------------------------------------------
# Plot metadata
# ------------------------------------------------------------------------------

test_that("title is applied", {
  p <- piper_plot_schema(
    title = "Test Piper Plot"
  )

  expect_equal(
    p$labels$title,
    "Test Piper Plot"
  )
})

test_that("plotmath labels are accepted", {
  expect_no_error(
    piper_plot_schema(
      x_cation_label = "Ca~phantom()^+2",
      y_cation_label = "Mg~phantom()^+2",
      z_anion_label = "SO[4]^-2"
    )
  )
})

# ------------------------------------------------------------------------------
# Grouping validation
# ------------------------------------------------------------------------------

test_that("valid group assignments are accepted", {
  group_df <- gw_piper %>%
    distinct(location_id) %>%
    mutate(group = c("A", "B"))

  df <- left_join(
    gw_piper,
    group_df,
    by = "location_id"
  )

  expect_no_error(
    piper_plot_schema(
      df,
      group_col = "group"
    )
  )
})

test_that("multiple groups per location error", {
  df <- gw_piper

  df$group <- "A"

  first_location <- unique(df$location_id)[1]

  df$group[
    df$location_id == first_location
  ] <- rep(
    c("A", "B"),
    length.out = sum(
      df$location_id == first_location
    )
  )

  expect_error(
    piper_plot_schema(
      df,
      group_col = "group"
    ),

    "must contain only one value per location"
  )
})

# ------------------------------------------------------------------------------
# Scale analyte support
# ------------------------------------------------------------------------------

test_that("scale_analyte is supported", {
  expect_no_error(
    piper_plot_schema(
      piper_scale_df(),
      scale_analyte = "Total Dissolved Solids"
    )
  )
})

test_that("scale_analyte returns a ggplot", {
  p <- piper_plot_schema(
    piper_scale_df(),
    scale_analyte = "Total Dissolved Solids"
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})

test_that("unavailable scale analyte warns", {
  expect_warning(
    piper_plot_schema(
      scale_analyte = "Does Not Exist"
    )
  )
})

# ------------------------------------------------------------------------------
# Plot structure
# ------------------------------------------------------------------------------

test_that("plot contains layers", {
  p <- piper_plot_schema()

  expect_gt(
    length(p$layers),
    0
  )
})

test_that("plot uses a coordinate system", {
  p <- piper_plot_schema()

  expect_true(
    inherits(
      p$coordinates,
      "Coord"
    )
  )
})

test_that("plot contains labels list", {
  p <- piper_plot_schema()

  expect_true(
    is.list(p$labels)
  )
})

# ------------------------------------------------------------------------------
# Internal transform smoke test
# ------------------------------------------------------------------------------

test_that("piper transformation returns coordinate columns", {
  piper_df <- .transform_piper_data(
    build_schema(),
    analyte_matching = "strict",
    x_cation = "Calcium, dissolved",
    y_cation = "Magnesium, dissolved",
    z_cation = c(
      "Sodium, dissolved",
      "Potassium, dissolved"
    ),
    x_anion = c(
      "Chloride, total",
      "Fluoride, total"
    ),
    y_anion = "Alkalinity, total (lab)",
    z_anion = "Sulfate, total",
    scale_analyte = NULL
  )

  expect_true(
    all(
      c(
        "cation_x",
        "cation_y",
        "anion_x",
        "anion_y",
        "diamond_x",
        "diamond_y"
      ) %in%
        names(piper_df)
    )
  )
})

# ------------------------------------------------------------------------------
# Regression guard
# ------------------------------------------------------------------------------

test_that("piper_plot remains constructible with major ions only", {
  expect_no_error({
    p <- piper_plot_schema()

    expect_s3_class(
      p,
      "ggplot"
    )
  })
})
