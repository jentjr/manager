source("data-raw/analyte_tbl.R")
source("data-raw/standards_tbl.R")
source("data-raw/build-lookups.R")
source("data-raw/gw.R")

validate_analyte_tbl(analyte_tbl)

validate_env_schema(gw)

validate_standards_schema(standards_tbl)

usethis::use_data(
  gw,
  analyte_tbl,
  analyte_alias_lookup,
  standard_units,
  internal = TRUE,
  overwrite = TRUE
)

usethis::use_data(
  standards_tbl,
  overwrite = TRUE
)
