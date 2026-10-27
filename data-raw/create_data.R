source("data-raw/analyte_tbl.R")
source("data-raw/analyte_alias_tbl.R")
source("data-raw/standards_tbl.R")
source("data-raw/build-lookups.R")
source("data-raw/data-gw.R")

validate_analyte_tbl(analyte_tbl)

validate_analyte_alias_tbl(
  analyte_alias_tbl,
  analyte_tbl
)

analyte_alias_lookup <- .build_alias_lookup(
  analyte_tbl,
  analyte_alias_tbl
)

.validate_alias_lookup(
  analyte_alias_lookup,
  analyte_tbl
)

validate_env_schema(gw)

validate_standards_schema(standards_tbl)

# ------------------------------------------------------------------
# Exported datasets
# ------------------------------------------------------------------

usethis::use_data(
  gw,
  analyte_tbl,
  analyte_alias_tbl,
  standards_tbl,
  overwrite = TRUE
)

# ------------------------------------------------------------------
# Internal package data
# ------------------------------------------------------------------

usethis::use_data(
  analyte_alias_lookup,
  internal = TRUE,
  overwrite = TRUE
)
