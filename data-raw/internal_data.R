source("data-raw/limits_tbl.R")
source("data-raw/build-lookups.R")
source("data-raw/analyte_tbl.R")


usethis::use_data(
  limits_tbl,
  analyte_tbl,
  analyte_alias_lookup,
  standard_units,
  internal = TRUE,
  overwrite = TRUE
)
