test_analyte_tbl <- tibble::tibble(
  analyte = c(
    "calcium",
    "magnesium",
    "sodium",
    "bicarbonate",
    "benzene"
  ),

  aliases = list(
    c("ca", "calcium"),
    c("mg", "magnesium"),
    c("na", "sodium"),
    c("alkalinity", "hco3"),
    "benzene"
  ),

  std_unit = c(
    "mg/L",
    "mg/L",
    "mg/L",
    "mg/L as CaCO3",
    "ug/L"
  ),

  ion_class = c(
    "cation",
    "cation",
    "cation",
    "anion",
    NA_character_
  ),

  charge = c(
    2,
    2,
    1,
    -1,
    NA
  ),

  molecular_weight = c(
    40.078,
    24.305,
    22.990,
    61.017,
    NA
  ),

  meq_factor = c(
    0.0499,
    0.0823,
    0.0435,
    0.0200,
    NA_real_
  )
)

make_schema <- function() {
  env_schema(
    tibble::tibble(
      location = c(
        "MW-1",
        "MW-1",
        "MW-2",
        "MW-2"
      ),
      date = as.Date(c(
        "2024-01-01",
        "2024-01-02",
        "2024-01-03",
        "2024-01-04"
      )),
      analyte = c(
        "Arsenic",
        "Arsenic",
        "Arsenic",
        "Arsenic"
      ),
      result = c(
        1,
        2,
        3,
        4
      )
    ),
    location = location,
    date = date,
    analyte = analyte,
    result = result
  )
}
