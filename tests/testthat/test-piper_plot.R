# test-piper_plot.R

library(testthat)
library(dplyr)

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

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

test_that("env_schema method returns ggplot", {
  p <- piper_plot_schema()

  expect_s3_class(
    p,
    "ggplot"
  )
})

# ------------------------------------------------------------------------------
# analyte matching
# ------------------------------------------------------------------------------

test_that("invalid analyte matching throws error", {
  expect_error(
    piper_plot_schema(
      analyte_matching = "invalid"
    )
  )
})

test_that("all supported analyte matching methods work", {
  purrr::walk(
    c(
      "exact",
      "strict",
      "normalized"
    ),

    ~ expect_no_error(
      piper_plot_schema(
        analyte_matching = .x
      )
    )
  )
})

# ------------------------------------------------------------------------------
# plot metadata
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
# grouping validation
# ------------------------------------------------------------------------------

test_that("valid group assignments are accepted", {
  group_df <- gw_piper |>
    distinct(location_id) |>
    mutate(
      group = c("A", "B")
    )

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

  first_location <- unique(
    df$location_id
  )[1]

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
# scale analyte
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
# plot structure
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

# ------------------------------------------------------------------------------
# piper data generation
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
    z_anion = "Sulfate, total"
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
        names(
          piper_df
        )
    )
  )
})

test_that("strict matching produces valid Piper coordinates", {
  out <- piper_data(
    gw,
    analyte_matching = "strict"
  )

  expect_gt(
    sum(
      stats::complete.cases(
        out[,
          c(
            "cation_x",
            "cation_y",
            "anion_x",
            "anion_y",
            "diamond_x",
            "diamond_y"
          )
        ]
      )
    ),
    0
  )
})

test_that("normalized matching produces valid Piper coordinates", {
  out <- piper_data(
    gw,
    analyte_matching = "normalized"
  )

  expect_gt(
    sum(
      stats::complete.cases(
        out[,
          c(
            "cation_x",
            "cation_y",
            "anion_x",
            "anion_y",
            "diamond_x",
            "diamond_y"
          )
        ]
      )
    ),
    0
  )
})

test_that("strict and normalized matching give equivalent Piper coordinates", {
  strict <- piper_data(
    gw,
    analyte_matching = "strict"
  )

  normalized <- piper_data(
    gw,
    analyte_matching = "normalized"
  )

  expect_equal(
    strict$cation_x,
    normalized$cation_x
  )

  expect_equal(
    strict$cation_y,
    normalized$cation_y
  )

  expect_equal(
    strict$anion_x,
    normalized$anion_x
  )

  expect_equal(
    strict$anion_y,
    normalized$anion_y
  )

  expect_equal(
    strict$diamond_x,
    normalized$diamond_x
  )

  expect_equal(
    strict$diamond_y,
    normalized$diamond_y
  )
})

test_that("normalized matching aggregates through analyte_id", {
  chemistry_df <- .prepare_major_ion_chemistry(
    gw,
    analytes = c(
      "Calcium",
      "Magnesium",
      "Sodium",
      "Potassium",
      "Chloride",
      "Bicarbonate",
      "Sulfate"
    ),
    analyte_matching = "normalized"
  )

  components <- .create_geochemical_components(
    chemistry_df,
    .major_ion_components()
  )

  expect_gt(
    sum(!is.na(components$calcium)),
    0
  )

  expect_gt(
    sum(!is.na(components$magnesium)),
    0
  )

  expect_gt(
    sum(!is.na(components$chloride)),
    0
  )

  expect_gt(
    sum(!is.na(components$alkalinity)),
    0
  )
})

test_that("piper_data supports custom analyte selections", {
  out <- piper_data(
    gw,
    analyte_matching = "strict",
    x_anion = "Chloride, total"
  )

  expect_gt(
    sum(!is.na(out$anion_x)),
    0
  )
})

test_that("custom analytes are included during Piper preparation", {
  out <- piper_data(
    gw,
    analyte_matching = "strict",
    x_anion = c(
      "Chloride, total",
      "Fluoride, total"
    )
  )

  expect_gt(
    sum(!is.na(out$anion_x)),
    0
  )
})

# ------------------------------------------------------------------------------
# regression guard
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

test_that("strict matching produces non-missing Piper components", {
  components <- .prepare_major_ion_chemistry(
    build_schema(),
    analyte_matching = "strict"
  ) |>
    .create_geochemical_components(
      .major_ion_components()
    )

  expect_gt(
    sum(!is.na(components$calcium)),
    0
  )

  expect_gt(
    sum(!is.na(components$alkalinity)),
    0
  )
})
