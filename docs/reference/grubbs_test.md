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
#> Warning: 1 analytes were not found in analyte metadata.
#> ℹ Nitrite + Nitrate, total
#> ,
#> Warning: Skipped incompatible unit conversion: degrees c -> degC
#> Warning: Skipped incompatible unit conversion: std -> ph
#> Error in dplyr::mutate(.build_outlier_groups(df), grubbs = purrr::map(.x = data,     ~outliers::grubbs.test(.x$.analysis_result, type = type,         opposite = opposite, two.sided = two_sided))): ℹ In argument: `grubbs = purrr::map(...)`.
#> ℹ In group 41: `location_id = "MW-1"`, `param_name = "Temperature (Celsius)"`,
#>   `.analysis_unit = "degC"`.
#> Caused by error in `purrr::map()`:
#> ℹ In index: 1.
#> Caused by error in `if (xor(((x[n] - mean(x)) < (mean(x) - x[1])), opposite)) ...`:
#> ! argument is of length zero
```
