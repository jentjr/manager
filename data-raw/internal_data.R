source("data-raw/analyte_tbl.R")
source("data-raw/analyte_alias_tbl.R")
source("data-raw/standards_tbl.R")
source("data-raw/build-lookups.R")
source("data-raw/gw.R")

validate_analyte_tbl(analyte_tbl)

validate_analyte_alias_tbl(
  analyte_alias_tbl,
  analyte_tbl
)

analyte_alias_lookup <- .build_alias_lookup(analyte_tbl, analyte_alias_tbl)

.validate_alias_lookup(
  analyte_alias_lookup,
  analyte_tbl
)

validate_env_schema(gw)

validate_standards_schema(standards_tbl)

usethis::use_data(
  gw,
  internal = TRUE,
  overwrite = TRUE
)

usethis::use_data(
  analyte_tbl,
  analyte_alias_tbl,
  analyte_alias_lookup,
  standards_tbl,
  overwrite = TRUE
)
