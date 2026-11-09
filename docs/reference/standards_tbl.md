# Environmental Standards Lookup Table

Default environmental standards used by functions such as
\[\`assign_standards\`()\].

## Format

A tibble with the following columns:

- .analyte:

  Analyte or constituent name.

- .standard_type:

  Standard category or criterion name.

- .standard_value:

  Numeric standard value.

- .standard_unit:

  Unit associated with the standard value.

- .location:

  Optional location identifier for location-specific standards.

- .medium:

  Environmental medium associated with the standard (for example
  groundwater, soil, sediment, or surface water).

- .source:

  Source authority, regulation, permit, guidance document, or
  project-specific basis.

- .priority:

  Numeric conflict-resolution priority. Lower values take precedence
  over higher values.

## Details

The dataset is a standardized \[\`standards_schema\`()\] object
containing regulatory, screening, permit, background, and
project-specific environmental standards.

By default, records included with the package represent groundwater
screening criteria and drinking water standards. Users may extend or
replace this table with custom standards and supply them to
\[assign_standards()\] through the \`standards\` argument.

Standards are selected according to \`.priority\`, where lower values
indicate higher priority. When multiple standards match a single
observation, the standard with the lowest \`.priority\` value is used.

Standards are stored in a schema-based framework that supports
location-specific, medium-specific, permit-specific, and project-
specific criteria.

The \`.priority\` field is used solely to resolve situations where
multiple standards apply to the same observation. Lower values represent
higher precedence.

Recommended priority ranges are:

- 1-9 = Site- or project-specific objectives

- 10-19 = Permit limits and compliance criteria

- 20-49 = Regulatory standards

- 50-99 = Screening levels and guidance values

- 100+ = Background, reference, or informational values

These ranges are recommendations only. Users may define any priority
system appropriate for their project provided that lower values
represent higher precedence.

## Examples

``` r
data("standards_tbl", package = "manager")

standards_tbl
#> # A tibble: 15 × 12
#>    .analyte      .standard_type .standard_value .standard_unit .location .medium
#>    <chr>         <chr>                    <dbl> <chr>          <chr>     <chr>  
#>  1 Antimony      Maximum Conta…               6 ug/L           NA        ground…
#>  2 Arsenic       Maximum Conta…              10 ug/L           NA        ground…
#>  3 Barium        Maximum Conta…            2000 ug/L           NA        ground…
#>  4 Beryllium     Maximum Conta…               4 ug/L           NA        ground…
#>  5 Cadmium       Maximum Conta…               5 ug/L           NA        ground…
#>  6 Chromium      Maximum Conta…             100 ug/L           NA        ground…
#>  7 Cobalt        Regional Scre…               6 ug/L           NA        ground…
#>  8 Fluoride      Maximum Conta…               4 mg/L           NA        ground…
#>  9 Lead          Regional Scre…              15 ug/L           NA        ground…
#> 10 Lithium       Regional Scre…              40 ug/L           NA        ground…
#> 11 Mercury       Maximum Conta…               2 ug/L           NA        ground…
#> 12 Molybdenum    Regional Scre…             100 ug/L           NA        ground…
#> 13 Selenium      Maximum Conta…              50 ug/L           NA        ground…
#> 14 Thallium      Maximum Conta…               2 ug/L           NA        ground…
#> 15 Radium-226 a… Maximum Conta…               5 pCi/L          NA        ground…
#> # ℹ 6 more variables: .source <chr>, .provenance <chr>, .reference <chr>,
#> #   .effective_date <date>, .notes <chr>, .priority <int>

# Add a custom site-specific standard
custom_standards <- dplyr::bind_rows(
  standards_tbl,
  tibble::tibble(
    .analyte = "Arsenic",
    .standard_type = "Site Cleanup Standard",
    .standard_value = 5,
    .standard_unit = "ug/L",
    .location = "MW-01",
    .medium = "groundwater",
    .source = "Project",
    .priority = 1L
  )
)
```
