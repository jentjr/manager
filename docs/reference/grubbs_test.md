# Grubbs Test

Performs Grubbs' test for potential outliers in environmental monitoring
data.

## Usage

``` r
grubbs_test(df, ...)

# S3 method for class 'data.frame'
grubbs_test(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  ...
)

# S3 method for class 'env_schema'
grubbs_test(
  df,
  type = 10,
  opposite = FALSE,
  two_sided = FALSE,
  keep_data_object = FALSE,
  ...
)
```

## Arguments

- df:

  A data frame or \`env_schema\` object.

- ...:

  Additional arguments passed to methods.

- location_id:

  Location column name.

- param_name:

  Analyte column name.

- sample_date:

  Sample date column.

- analysis_result:

  Analytical result column name.

- default_unit:

  Reporting unit column name.

- type:

  Grubbs test variant.

- opposite:

  Logical indicating whether the lowest value should be tested rather
  than the highest value.

- two_sided:

  Logical indicating whether a two-sided test should be performed.

- keep_data_object:

  Logical indicating whether the complete test object should be
  retained.

## Value

A tibble containing one row per outlier evaluation group.

## Details

\`grubbs_test()\` is an S3 generic supporting both \`data.frame\` and
\`env_schema\` objects.

If the input is grouped using \[dplyr::group_by()\], Grubbs' tests are
performed within the existing groups.

Grubbs' test evaluates whether the most extreme value differs
significantly from the remaining observations.

Grubbs' test assumes approximately normally distributed data.

For many environmental datasets, Rosner's test may be more appropriate
when larger sample sizes are available.

## Groundwater Monitoring

Dixon's and Grubbs' tests are generally most appropriate for relatively
small datasets and may be sensitive to distributional assumptions.

For larger groundwater monitoring datasets, \`rosner_test()\` is often
preferred.

## env_schema Method

For \`env_schema\` objects, tests are performed using
\`.analysis_result\` values and respect existing \`dplyr::group_by()\`
groups.

## Examples

``` r
grubbs_test(gw_data)
#> Warning: 13 analytes were not found in analyte metadata.
#> Aluminum, dissolved
#> Chromium, hexavalent
#> Cobalt, Dis
#> Groundwater Elevation (ref MSL)
#> Groundwater, depth from TOC
#> Nitrite + Nitrate, total
#> SiO2
#> Strontium, dissolved
#> Temperature (Celsius)
#> Tin, dissolved
#> ,
#> Warning: There were 11 warnings in `dplyr::mutate()`.
#> The first warning was:
#> ℹ In argument: `grubbs = purrr::map(...)`.
#> ℹ In group 6: `location_id = "MW-1"`, `param_name = "Beryllium, dissolved"`,
#>   `.analysis_unit = "ug/l"`.
#> Caused by warning in `sqrt()`:
#> ! NaNs produced
#> ℹ Run dplyr::last_dplyr_warnings() to see the 10 remaining warnings.
#> # A tibble: 387 × 5
#> # Groups:   location_id, param_name, .analysis_unit [387]
#>    location_id param_name           .analysis_unit alternative           p_value
#>    <chr>       <chr>                <chr>          <chr>                   <dbl>
#>  1 MW-2        Magnesium, dissolved mg/l           lowest value 26.7 is… 0.335  
#>  2 MW-2        Sodium, dissolved    mg/l           highest value 27.4 i… 0.00724
#>  3 MW-2        Potassium, dissolved mg/l           highest value 0.8 is… 0.154  
#>  4 MW-2        Chloride, total      mg/l           highest value 31.6 i… 1      
#>  5 MW-2        Sulfate, total       mg/l           highest value 200 is… 0.00876
#>  6 MW-2        Fluoride, total      mg/l           highest value 1 is a… 0.00857
#>  7 MW-2        Arsenic, dissolved   ug/l           highest value 10 is … 0.425  
#>  8 MW-2        Barium, dissolved    ug/l           highest value 134 is… 0.0440 
#>  9 MW-2        Beryllium, dissolved ug/l           highest value 1 is a… 0      
#> 10 MW-2        Boron, dissolved     ug/l           lowest value 1200 is… 0.125  
#> # ℹ 377 more rows
```
