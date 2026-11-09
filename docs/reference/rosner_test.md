# Rosner Test

Performs Rosner's test for one or more potential outliers in
environmental monitoring data.

## Usage

``` r
rosner_test(df, ...)

# S3 method for class 'data.frame'
rosner_test(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  ...
)

# S3 method for class 'env_schema'
rosner_test(df, k = 3, alpha = 0.05, warn = TRUE, keep_data_object = FALSE)
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

- k:

  Maximum number of candidate outliers evaluated.

- alpha:

  Significance level.

- warn:

  Logical indicating whether Rosner test warnings should be returned.

- keep_data_object:

  Logical indicating whether the complete Rosner result object should be
  retained.

## Value

A tibble containing one row per outlier evaluation group.

## Details

\`rosner_test()\` is an S3 generic supporting both \`data.frame\` and
\`env_schema\` objects.

If the input is grouped using \[dplyr::group_by()\], Rosner tests are
performed within the existing groups.

Rosner's test evaluates multiple candidate outliers simultaneously and
is commonly used in environmental and groundwater statistical
evaluations.

When groups contain insufficient data, no outlier evaluation is
performed and missing values are returned.

## Groundwater Monitoring

Rosner's test is often preferred for groundwater monitoring datasets
because it can evaluate multiple potential outliers and generally
performs better than Dixon's or Grubbs' tests when sample sizes are
sufficiently large.

## env_schema Method

For \`env_schema\` objects, Rosner tests use standardized
\`.analysis_result\` values.

Existing \`dplyr::group_by()\` groups are preserved. When the input is
not grouped, tests are performed by:

- location

- analyte

- analysis unit

\#' @returns A tibble containing one row per outlier evaluation group.

The returned table includes:

- Value:

  Observation evaluated by Rosner's procedure.

- Outlier:

  Logical indicator identifying observations classified as outliers.

## Examples

``` r
schema <- env_schema(
  gw_data,
  location = location_id,
  date = sample_date,
  analyte = param_name,
  result = analysis_result,
  unit = default_unit
)
#> Error in alias_lookup %>% dplyr::count(alias) %>% dplyr::filter(n > 1): could not find function "%>%"

rosner_test(schema)
#> Error: object 'schema' not found

schema |>
  dplyr::group_by(location_id) |>
  rosner_test()
#> Error: object 'schema' not found
```
