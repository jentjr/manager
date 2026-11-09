# Example Groundwater Raw Data

A groundwater monitoring dataset used throughout the package to
demonstrate creation of an \[\`env_schema\`\] object.

## Usage

``` r
gw_data
```

## Format

A tibble with 5,878 rows and 8 variables:

- location_id:

  Monitoring well or sampling location identifier.

- sample_date:

  Sample collection date.

- param_name:

  Laboratory-reported analyte name.

- analysis_result:

  Reported analytical result.

- default_unit:

  Reported analytical unit.

- lt_measure:

  Non-detect indicator.

- sample_id:

  Sample identifier.

- method:

  Analytical method.

## Source

Example groundwater monitoring dataset.

## Details

This dataset serves as the source data for \[gw\], the corresponding
processed \`env_schema\` example dataset.

## See also

\[gw\], \[env_schema()\]

## Examples

``` r
data(gw_data)

head(gw_data)
#> # A tibble: 6 × 6
#>   location_id sample_date         analysis_result lt_measure default_unit
#>   <chr>       <dttm>                        <dbl> <chr>      <chr>       
#> 1 MW-2        2008-10-28 00:00:00           36.3  NA         mg/L        
#> 2 MW-2        2008-10-28 00:00:00           18.9  NA         mg/L        
#> 3 MW-2        2008-10-28 00:00:00            0.5  NA         mg/L        
#> 4 MW-2        2008-10-28 00:00:00           24.9  NA         mg/L        
#> 5 MW-2        2008-10-28 00:00:00          170    NA         mg/L        
#> 6 MW-2        2008-10-28 00:00:00            0.29 NA         mg/L        
#> # ℹ 1 more variable: param_name <chr>

gw <- env_schema(
  gw_data,
  location = location_id,
  date = sample_date,
  analyte = param_name,
  result = analysis_result,
  unit = default_unit,
  reporting_flag = lt_measure,
  analyte_lookup = analyte_tbl
)
#> Error in alias_lookup %>% dplyr::count(alias) %>% dplyr::filter(n > 1): could not find function "%>%"
```
