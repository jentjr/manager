# Dixon Test

Performs Dixon's Q test for potential outliers in environmental
monitoring data.

## Usage

``` r
dixon_test(df, ...)

# S3 method for class 'data.frame'
dixon_test(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  ...
)

# S3 method for class 'env_schema'
dixon_test(
  df,
  type = 0,
  opposite = FALSE,
  two_sided = TRUE,
  keep_data_object = FALSE
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

- default_unit:

  Reporting unit column name.

- type:

  Variant of Dixon's test.

- opposite:

  Logical indicating whether the lowest value should be tested rather
  than the highest value.

- two_sided:

  Logical indicating whether a two-sided test should be performed.

- keep_data_object:

  Logical indicating whether the complete test object should be retained
  in the output.

## Value

A tibble containing one row per outlier evaluation group.

## Details

\`dixon_test()\` is an S3 generic that supports both raw \`data.frame\`
inputs and \`env_schema\` objects.

If the input is grouped using \[dplyr::group_by()\], Dixon tests are
performed within the existing groups. For ungrouped \`env_schema\`
inputs, tests are performed separately for each:

- location

- analyte

- analysis unit

Dixon's test is intended for relatively small sample sizes and is
generally appropriate for groups containing between three and thirty
observations.

Dixon's test evaluates whether the most extreme observation is
statistically inconsistent with the remaining observations in the group.

The method is most appropriate for small datasets and should be
interpreted cautiously for environmental monitoring programs with larger
sample sizes.

## Groundwater Monitoring

Dixon's and Grubbs' tests are generally most appropriate for relatively
small datasets and may be sensitive to distributional assumptions.

For larger groundwater monitoring datasets, \`rosner_test()\` is often
preferred.

## env_schema Method

For \`env_schema\` objects, Dixon tests are performed using standardized
analytical results stored in \`.analysis_result\`.

Grouping is determined automatically from the current
\`dplyr::group_by()\` structure or, for ungrouped data, uses:

- location

- analyte

- analysis unit

## Examples

``` r
dixon_test(gw_data)
#> Error in alias_lookup %>% dplyr::count(alias) %>% dplyr::filter(n > 1): could not find function "%>%"

schema <- env_schema(
  gw_data,
  location = location_id,
  date = sample_date,
  analyte = param_name,
  result = analysis_result,
  unit = default_unit
)
#> Error in alias_lookup %>% dplyr::count(alias) %>% dplyr::filter(n > 1): could not find function "%>%"

dixon_test(schema)
#> Error: object 'schema' not found

schema |>
  dplyr::group_by(location_id) |>
  dixon_test()
#> Error: object 'schema' not found
```
