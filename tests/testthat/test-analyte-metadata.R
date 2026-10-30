# tests/testthat/test-analyte-metadata.R

test_that("analyte ids are unique", {
  expect_equal(
    anyDuplicated(
      analyte_tbl$analyte_id
    ),
    0L
  )
})

test_that("canonical analyte names are unique", {
  expect_equal(
    anyDuplicated(
      analyte_tbl$analyte
    ),
    0L
  )
})

test_that("normalized analyte names are unique", {
  expect_equal(
    anyDuplicated(
      analyte_tbl$normalized_analyte
    ),
    0L
  )
})

test_that("standard units are populated", {
  expect_false(
    any(
      is.na(
        analyte_tbl$std_unit
      )
    )
  )
})

test_that("standard units normalize successfully", {
  normalized <- .normalize_unit(
    analyte_tbl$std_unit
  )

  expect_false(
    any(
      is.na(
        normalized
      )
    )
  )
})

test_that("major ions are flagged correctly", {
  major_ids <- c(
    "calcium",
    "magnesium",
    "sodium",
    "potassium",
    "chloride",
    "fluoride",
    "bromide",
    "iodide",
    "sulfate",
    "nitrate",
    "nitrite",
    "phosphate",
    "bicarbonate",
    "carbonate"
  )

  expect_true(
    all(
      analyte_tbl$is_major_ion[
        analyte_tbl$analyte_id %in%
          major_ids
      ]
    )
  )
})

test_that("major ions have molecular weights", {
  major <- analyte_tbl %>%
    dplyr::filter(
      is_major_ion
    )

  expect_false(
    any(
      is.na(
        major$molecular_weight
      )
    )
  )
})

test_that("major ions have charges", {
  major <- analyte_tbl %>%
    dplyr::filter(
      is_major_ion
    )

  expect_false(
    any(
      is.na(
        major$charge
      )
    )
  )
})

test_that("major ions have valid ion classes", {
  major <- analyte_tbl %>%
    dplyr::filter(
      is_major_ion
    )

  expect_true(
    all(
      major$ion_class %in%
        c(
          "cation",
          "anion"
        )
    )
  )
})

test_that("major ions have meq factors", {
  major <- analyte_tbl %>%
    dplyr::filter(
      is_major_ion
    )

  expect_false(
    any(
      is.na(
        major$meq_factor
      )
    )
  )
})

test_that("major ion meq factors are positive", {
  major <- analyte_tbl %>%
    dplyr::filter(
      is_major_ion
    )

  expect_true(
    all(
      major$meq_factor > 0
    )
  )
})

test_that("major ion meq factors are chemically consistent", {
  major <- analyte_tbl %>%
    dplyr::filter(
      is_major_ion
    )

  expected <-
    abs(major$charge) /
    major$molecular_weight

  expect_equal(
    major$meq_factor,
    expected,
    tolerance = 1e-10
  )
})

test_that("trace metals are flagged correctly", {
  metals <- analyte_tbl %>%
    dplyr::filter(
      analyte_group == "Trace Metals"
    )

  expect_true(
    all(
      metals$is_trace_metal
    )
  )
})

test_that("radionuclides are flagged correctly", {
  radionuclides <- analyte_tbl %>%
    dplyr::filter(
      analyte_group == "Radionuclides"
    )

  expect_true(
    all(
      radionuclides$is_radionuclide
    )
  )
})

test_that("field parameters are flagged correctly", {
  field <- analyte_tbl %>%
    dplyr::filter(
      analyte_group == "Field Parameters"
    )

  expect_true(
    all(
      field$is_field_parameter
    )
  )
})

test_that("cas numbers are unique when present", {
  x <- analyte_tbl %>%
    dplyr::filter(
      !is.na(casrn)
    )

  expect_equal(
    anyDuplicated(
      x$casrn
    ),
    0L
  )
})

test_that("meq lookup returns unique analyte ids", {
  lookup <-
    .get_meq_conversion_factors()

  expect_equal(
    anyDuplicated(
      lookup$analyte_id
    ),
    0L
  )
})

test_that("meq lookup analyte ids exist in analyte metadata", {
  lookup <-
    .get_meq_conversion_factors()

  expect_true(
    all(
      lookup$analyte_id %in%
        analyte_tbl$analyte_id
    )
  )
})

test_that("default major ion analytes resolve", {
  required <- c(
    "Calcium",
    "Magnesium",
    "Sodium",
    "Potassium",
    "Chloride",
    "Sulfate"
  )

  resolved <-
    .resolve_geochemical_analytes(
      required
    )

  expect_equal(
    length(resolved),
    length(required)
  )
})

test_that("all major ions resolve successfully", {
  major <- analyte_tbl %>%
    dplyr::filter(
      is_major_ion
    )

  resolved <-
    .resolve_geochemical_analytes(
      major$analyte
    )

  expect_equal(
    length(resolved),
    nrow(major)
  )
})
