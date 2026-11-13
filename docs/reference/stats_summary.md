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
  result_col = analysis_result,
  censored_col = censored,
  na.rm = TRUE
)

# S3 method for class 'env_schema'
stats_summary(
  df,
  result_col = analysis_result_var(df),
  censored_col = censored_var(df),
  na.rm = TRUE,
  ...
)

# S3 method for class 'grouped_df'
stats_summary(df, ...)
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

- result_col:

  Numerical column on which stats_summary is computed.

- censored_col:

  Censoring indicator column name used to calculate detection counts and
  censoring percentages.

- na.rm:

  Logical indicating whether missing values should be removed when
  calculating summary statistics.

## Value

A tibble containing one row per summary group.

The returned table includes:

- n:

  Number of records in the group, including records with missing result
  values.

- n_used:

  Number of non-missing analytical results used in statistical
  evaluations and decision selection.

- n_detect:

  Number of non-censored analytical results.

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

- distribution_method:

  Recommended distributional approach based on the degree of censoring.

- trend_method:

  Recommended trend method.

- trend_guidance:

  Recommended interpretation of trend results. "formal_trend_test"
  indicates trend inference is generally supported. "screening_only"
  indicates trend results should be considered exploratory because of
  extensive censoring.

- censored_method:

  Recommended censored-data handling method for trend analyses.

- decision_basis:

  Basis for the statistical recommendation. Examples include:
  fewer_than_4_observations, low_censoring, moderate_censoring,
  high_censoring, and extreme_censoring.

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
#> Warning: There were 16 warnings in `dplyr::summarise()`.
#> The first warning was:
#> ℹ In argument: `min = .safe_stat(min(.data[[".analysis_result"]], na.rm =
#>   na.rm))`.
#> ℹ In group 38: `location_id = "MW-1"`, `param_name = "Specific Conductance @
#>   25C (field)"`, `.analysis_unit = "us/cm"`.
#> Caused by warning in `min()`:
#> ! no non-missing arguments to min; returning Inf
#> ℹ Run dplyr::last_dplyr_warnings() to see the 15 remaining warnings.
#> # A tibble: 387 × 27
#>    location_id param_name  .analysis_unit     n n_used n_detect percent_censored
#>    <chr>       <chr>       <chr>          <int>  <int>    <int>            <dbl>
#>  1 MW-1        Alkalinity… mg/l              20     20       20              0  
#>  2 MW-1        Aluminum, … ug/l              19     19        6             68.4
#>  3 MW-1        Antimony, … ug/l              20     20        3             85  
#>  4 MW-1        Arsenic, d… ug/l              21     21       17             19.0
#>  5 MW-1        Barium, di… ug/l              20     20       20              0  
#>  6 MW-1        Beryllium,… ug/l              20     20        0            100  
#>  7 MW-1        Boron, dis… ug/l              20     20       20              0  
#>  8 MW-1        Cadmium, d… ug/l              20     20        1             95  
#>  9 MW-1        Calcium, d… mg/l              20     20       20              0  
#> 10 MW-1        Chloride, … mg/l              20     20       20              0  
#> # ℹ 377 more rows
#> # ℹ 20 more variables: mean <dbl>, median <dbl>, sd <dbl>, var <dbl>,
#> #   min <dbl>, max <dbl>, cv <dbl>, `25%` <dbl>, `75%` <dbl>, IQR <dbl>,
#> #   mad <dbl>, censoring_class <chr>, distribution_method <chr>,
#> #   trend_method <chr>, trend_guidance <chr>, censored_method <chr>,
#> #   distribution_config <list>, interval_config <list>, decision_basis <chr>,
#> #   comparison_method <chr>

gw |>
  dplyr::group_by(location_id) |>
  stats_summary()
#> # A tibble: 8 × 25
#>   location_id     n n_used n_detect percent_censored  mean median    sd      var
#>   <chr>       <int>  <int>    <int>            <dbl> <dbl>  <dbl> <dbl>    <dbl>
#> 1 MW-1          791    772      491             35.5 143.    6.94  398.   1.58e5
#> 2 MW-2          791    772      497             34.8 130.    5     348.   1.21e5
#> 3 MW-3          790    771      489             35.7  84.7   5     252.   6.33e4
#> 4 MW-4          715    697      444             35.4 168.    6.8   536.   2.87e5
#> 5 MW-5          715    697      427             37.8 181.    6.95  829.   6.87e5
#> 6 MW-6          715    697      412             39.9 757.    5.35 4233.   1.79e7
#> 7 MW-7          646    630      384             38.1 481.    5    2354.   5.54e6
#> 8 MW-8          715    697      441             35.8 151.    7.57  511.   2.61e5
#> # ℹ 16 more variables: min <dbl>, max <dbl>, cv <dbl>, `25%` <dbl>,
#> #   `75%` <dbl>, IQR <dbl>, mad <dbl>, censoring_class <chr>,
#> #   distribution_method <chr>, trend_method <chr>, trend_guidance <chr>,
#> #   censored_method <chr>, distribution_config <list>, interval_config <list>,
#> #   decision_basis <chr>, comparison_method <chr>

x <- env_schema(
  raw_data,
  location = site_id,
  date = sample_date,
  analyte = parameter,
  result = result_value
)
#> Error in env_schema(raw_data, location = site_id, date = sample_date,     analyte = parameter, result = result_value): unused argument (date = sample_date)

stats_summary(x)
#> Error: object 'x' not found
```
