# Summary Statistics

Calculates descriptive statistics for environmental monitoring data.

## Usage

``` r
stats_summary(df, ...)

# S3 method for class 'data.frame'
stats_summary(
  df,
  location_id = "location_id",
  param_name = "param_name",
  censored = "censored",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  na.rm = TRUE
)

# S3 method for class 'env_schema'
stats_summary(df, na.rm = TRUE, ...)
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

- censored:

  Censoring indicator column name.

- analysis_result:

  Analytical result column name.

- default_unit:

  Reporting unit column name.

- na.rm:

  Logical indicating whether missing values should be removed when
  calculating summary statistics.

## Value

A tibble containing one row per summary group.

The returned table includes:

- n:

  Number of records in the group, including records with missing result
  values.

- percent_censored:

  Percentage of records in the group classified as censored.

- mean:

  Arithmetic mean concentration.

- median:

  Median concentration.

- sd:

  Sample standard deviation.

- var:

  Sample variance.

- min:

  Minimum concentration.

- max:

  Maximum concentration.

- cv:

  Coefficient of variation.

- 25%:

  25th percentile.

- 75%:

  75th percentile.

- IQR:

  Interquartile range.

- mad:

  Median absolute deviation.

- censoring_class:

  EPA Unified Guidance style censoring classification: low_censoring,
  moderate_censoring, high_censoring, or extreme_censoring.

- recommended_distribution_method:

  Recommended distributional approach based on the degree of censoring.

- recommended_trend_method:

  Recommended trend test based on censoring.

## Details

For \`env_schema\` objects, calculations are performed using the
standardized \`.analysis_result\` and \`.censored\` columns rather than
the original source data columns. This ensures summary statistics are
based on the processed analytical values used throughout the package.

If the input is grouped with \[dplyr::group_by()\], summary statistics
are calculated within the existing groups. For ungrouped inputs,
summaries are calculated separately for each location, analyte, and
unit.

For ungrouped \`data.frame\` inputs, summaries are calculated by:

- location

- analyte

- reporting unit

For ungrouped \`env_schema\` inputs, summaries are calculated by:

- the mapped location variable returned by \`location_var()\`

- the mapped analyte variable returned by \`analyte_var()\`

- the standardized analysis unit (\`.analysis_unit\`)

Missing result values are excluded from statistical calculations when
\`na.rm = TRUE\`, but still contribute to the record count (\`n\`),
which represents the number of records in each summary group.

Empty inputs return an empty tibble with the standard summary columns.

## env_schema Method

For \`env_schema\` objects, \`stats_summary()\` automatically uses the
schema mappings and standardized analytical fields:

- \`.analysis_result\`

- \`.censored\`

- \`.analysis_unit\`

This allows summaries to remain consistent regardless of the original
source column names or schema mappings used by the input data.

## Examples

``` r
stats_summary(gw)
#> # A tibble: 387 × 16
#>    location_id param_name  .analysis_unit     n percent_censored    mean  median
#>    <chr>       <chr>       <chr>          <int>            <dbl>   <dbl>   <dbl>
#>  1 MW-1        Alkalinity… mg/l              20              0   290.     338.  
#>  2 MW-1        Aluminum, … ug/l              19             68.4  33.1     20   
#>  3 MW-1        Antimony, … ug/l              20             85     5.76     5   
#>  4 MW-1        Arsenic, d… ug/l              21             19.0   3.69     1.93
#>  5 MW-1        Barium, di… ug/l              20              0   124.     115   
#>  6 MW-1        Beryllium,… ug/l              20            100     0.525    0.5 
#>  7 MW-1        Boron, dis… ug/l              20              0   905.    1115   
#>  8 MW-1        Cadmium, d… ug/l              20             95     0.992    1   
#>  9 MW-1        Calcium, d… mg/l              20              0   106.     116.  
#> 10 MW-1        Chloride, … mg/l              20              0    22.0     21.9 
#> # ℹ 377 more rows
#> # ℹ 9 more variables: sd <dbl>, var <dbl>, min <dbl>, max <dbl>, cv <dbl>,
#> #   `25%` <dbl>, `75%` <dbl>, IQR <dbl>, mad <dbl>

gw |>
  dplyr::group_by(location_id) |>
  stats_summary()
#> Error in .stats_summary_core(df = df, group_cols = group_cols, result_col = analysis_result,     censored_col = censored, na.rm = na.rm): Missing required columns: censored

x <- env_schema(
  raw_data,
  location = site_id,
  date = sample_date,
  analyte = parameter,
  result = result_value
)
#> Error: object 'raw_data' not found

stats_summary(x)
#> Error: object 'x' not found
```
