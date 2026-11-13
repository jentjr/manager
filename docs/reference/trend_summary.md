# Trend Summary Statistics

Computes trend statistics and monitoring-period summary metrics for
environmental time-series data.

## Usage

``` r
trend_summary(df, ...)

# S3 method for class 'data.frame'
trend_summary(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  method = "mann_kendall",
  censored_method = "auto",
  conf.level = 0.95
)

# S3 method for class 'env_schema'
trend_summary(
  df,
  result_col = analysis_result_var(df),
  method = "mann_kendall",
  censored_method = "auto",
  conf.level = 0.95,
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

  Sample date column name.

- analysis_result:

  Analytical result column name.

- method:

  Trend method. One of:

  - \`"lm"\` Linear regression

  - \`"loess"\` Local regression smoother

  - \`"gam"\` Generalized additive model

  - \`"mann_kendall"\` Mann-Kendall trend test

- censored_method:

  Method used to handle censored data. One of:

  - \`"cenken"\` Censored Kendall trend estimator

  - \`"as_reported"\` Use analytical results as reported

- conf.level:

  Confidence level used for trend estimates.

## Value

A tibble containing one row per trend group.

The returned table includes:

- n:

  Number of records in the group.

- first_date:

  Earliest sample date.

- last_date:

  Latest sample date.

- date_range_days:

  Monitoring period length in days.

- date_range_years:

  Monitoring period length in years.

- sampling_frequency:

  Approximate samples collected per year.

- minimum_concentration:

  Minimum analytical result.

- median_concentration:

  Median analytical result.

- mean_concentration:

  Mean analytical result.

- maximum_concentration:

  Maximum analytical result.

- standard_deviation:

  Sample standard deviation.

- coefficient_of_variation:

  Standard deviation divided by mean.

- trend_method:

  Trend estimation method used.

- censored_method:

  Censored-data handling method used.

- slope:

  Estimated trend slope.

- p_value:

  Trend test p-value.

- conf_low:

  Lower confidence limit for the slope estimate.

- conf_high:

  Upper confidence limit for the slope estimate.

- significant:

  TRUE when the trend is statistically significant at the specified
  confidence level.

- trend_direction:

  Increasing, Decreasing, or Stable.

## Details

For \`env_schema\` objects, trend calculations are performed using the
standardized \`.analysis_result\` values and schema mappings. This
ensures trends are evaluated consistently regardless of the original
source column names.

If the input is grouped with \[dplyr::group_by()\], trend summaries are
calculated within the existing groups. For ungrouped inputs, summaries
are calculated separately for each location and analyte.

Trend calculations require at least three non-missing observations and
at least two distinct analytical result values. Groups that do not meet
these requirements return summary metrics, but trend-specific fields
such as \`slope\`, \`p_value\`, \`conf_low\`, and \`conf_high\` are
returned as \`NA\`.

Slope estimates and significance statistics are only available for trend
methods that support formal trend estimation. For smoothing methods such
as \`"loess"\` and \`"gam"\`, these fields are returned as \`NA\`.

For Mann-Kendall analyses using \`censored_method = "cenken"\`,
confidence limits for the Sen slope are not currently available and are
returned as \`NA\`.

For ungrouped \`env_schema\` inputs, trends are calculated by:

- the mapped location variable returned by \`location_var()\`

- the mapped analyte variable returned by \`analyte_var()\`

Confidence intervals are available for some trend methods. When a
confidence interval cannot be estimated, \`conf_low\` and \`conf_high\`
are returned as \`NA\`.

## env_schema Method

For \`env_schema\` objects, \`trend_summary()\` automatically uses the
schema mappings and standardized analytical fields:

- \`.analysis_result\`

- \`.censored\`

- mapped location variable

- mapped analyte variable

This allows trend summaries to remain consistent regardless of the
original source column names or schema mappings used by the input data.

## Examples

``` r
trend_summary(gw)
#> # A tibble: 387 × 22
#>    location_id param_name              n first_date          last_date          
#>    <chr>       <chr>               <int> <dttm>              <dttm>             
#>  1 MW-1        Alkalinity, total …    20 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  2 MW-1        Aluminum, dissolved    19 2008-02-20 00:00:00 2013-10-02 00:00:00
#>  3 MW-1        Antimony, dissolved    20 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  4 MW-1        Arsenic, dissolved     21 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  5 MW-1        Barium, dissolved      20 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  6 MW-1        Beryllium, dissolv…    20 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  7 MW-1        Boron, dissolved       20 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  8 MW-1        Cadmium, dissolved     20 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  9 MW-1        Calcium, dissolved     20 2007-12-20 00:00:00 2013-10-02 00:00:00
#> 10 MW-1        Chloride, total        20 2007-12-20 00:00:00 2013-10-02 00:00:00
#> # ℹ 377 more rows
#> # ℹ 17 more variables: date_range_days <dbl>, date_range_years <dbl>,
#> #   sampling_frequency <dbl>, minimum_concentration <dbl>,
#> #   median_concentration <dbl>, mean_concentration <dbl>,
#> #   maximum_concentration <dbl>, standard_deviation <dbl>,
#> #   coefficient_of_variation <dbl>, trend_method <chr>, censored_method <chr>,
#> #   slope <dbl>, p_value <dbl>, conf_low <dbl>, conf_high <dbl>, …

gw |>
  dplyr::group_by(location_id) |>
  trend_summary()
#> # A tibble: 8 × 21
#>   location_id     n first_date          last_date           date_range_days
#>   <chr>       <int> <dttm>              <dttm>                        <dbl>
#> 1 MW-1          791 2007-12-20 00:00:00 2013-10-02 00:00:00            2113
#> 2 MW-2          791 2007-12-20 00:00:00 2013-10-02 00:00:00            2113
#> 3 MW-3          790 2007-12-20 00:00:00 2013-10-02 00:00:00            2113
#> 4 MW-4          715 2007-12-20 00:00:00 2013-10-02 00:00:00            2113
#> 5 MW-5          715 2007-12-20 00:00:00 2013-10-02 00:00:00            2113
#> 6 MW-6          715 2007-12-20 00:00:00 2013-10-02 00:00:00            2113
#> 7 MW-7          646 2008-04-24 00:00:00 2013-10-02 00:00:00            1987
#> 8 MW-8          715 2007-12-20 00:00:00 2013-10-02 00:00:00            2113
#> # ℹ 16 more variables: date_range_years <dbl>, sampling_frequency <dbl>,
#> #   minimum_concentration <dbl>, median_concentration <dbl>,
#> #   mean_concentration <dbl>, maximum_concentration <dbl>,
#> #   standard_deviation <dbl>, coefficient_of_variation <dbl>,
#> #   trend_method <chr>, censored_method <chr>, slope <dbl>, p_value <dbl>,
#> #   conf_low <dbl>, conf_high <dbl>, significant <lgl>, trend_direction <chr>

trend_summary(
  gw,
  method = "mann_kendall",
  censored_method = "cenken"
)
#> # A tibble: 387 × 22
#>    location_id param_name              n first_date          last_date          
#>    <chr>       <chr>               <int> <dttm>              <dttm>             
#>  1 MW-1        Alkalinity, total …    20 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  2 MW-1        Aluminum, dissolved    19 2008-02-20 00:00:00 2013-10-02 00:00:00
#>  3 MW-1        Antimony, dissolved    20 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  4 MW-1        Arsenic, dissolved     21 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  5 MW-1        Barium, dissolved      20 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  6 MW-1        Beryllium, dissolv…    20 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  7 MW-1        Boron, dissolved       20 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  8 MW-1        Cadmium, dissolved     20 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  9 MW-1        Calcium, dissolved     20 2007-12-20 00:00:00 2013-10-02 00:00:00
#> 10 MW-1        Chloride, total        20 2007-12-20 00:00:00 2013-10-02 00:00:00
#> # ℹ 377 more rows
#> # ℹ 17 more variables: date_range_days <dbl>, date_range_years <dbl>,
#> #   sampling_frequency <dbl>, minimum_concentration <dbl>,
#> #   median_concentration <dbl>, mean_concentration <dbl>,
#> #   maximum_concentration <dbl>, standard_deviation <dbl>,
#> #   coefficient_of_variation <dbl>, trend_method <chr>, censored_method <chr>,
#> #   slope <dbl>, p_value <dbl>, conf_low <dbl>, conf_high <dbl>, …
```
