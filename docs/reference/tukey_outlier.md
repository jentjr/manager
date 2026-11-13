# Tukey Outlier Screening

Identifies potential outliers using Tukey's interquartile range (IQR)
method.

## Usage

``` r
tukey_outlier(df, ...)

# S3 method for class 'data.frame'
tukey_outlier(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  ...
)

# S3 method for class 'env_schema'
tukey_outlier(df, k = 3, na.rm = FALSE, type = 6, ...)
```

## Arguments

- df:

  A data frame or \`env_schema\` object.

- ...:

  Additional arguments passed to methods.

- k:

  IQR multiplier.

- na.rm:

  Logical indicating whether missing values should be removed.

- type:

  Quantile algorithm used in the calculation.

## Value

The original observations with an added logical \`outlier\` column
identifying observations that exceed the specified Tukey IQR thresholds.

## Details

Observations greater than:

\$\$ Q3 + k \times IQR \$\$

or less than:

\$\$ Q1 - k \times IQR \$\$

are flagged as potential outliers.

\`tukey_outlier()\` supports both \`data.frame\` and \`env_schema\`
objects.

If the input is grouped using \[dplyr::group_by()\], screening is
performed within each group.

## Groundwater Monitoring

Tukey's method should generally be considered a screening tool rather
than a formal hypothesis test. Potential outliers identified using this
method should be evaluated further before exclusion from statistical
analyses.

## env_schema Method

For \`env_schema\` objects, screening is performed using standardized
\`.analysis_result\` values and respects existing \`dplyr::group_by()\`
groups.
