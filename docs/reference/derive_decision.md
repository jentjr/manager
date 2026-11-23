# Derive Statistical Decision Guidance from Data

Calculates censoring metrics from a dataset and returns a corresponding
\[statistical_decision()\] object.

## Usage

``` r
derive_decision(df, ...)

# S3 method for class 'data.frame'
derive_decision(df, result_col = NULL, censored_col = NULL, ...)

# S3 method for class 'grouped_df'
derive_decision(df, result_col = NULL, censored_col = NULL, ...)

# S3 method for class 'env_schema'
derive_decision(df, result_col = NULL, censored_col = NULL, ...)
```

## Arguments

- df:

  Data frame containing analytical results.

- ...:

  Additional arguments passed to methods.

## env_schema Method

For \`env_schema\` objects, censoring metrics are computed using the
standardized analytical fields:

- \`.analysis_result\`

- \`.censored\`
