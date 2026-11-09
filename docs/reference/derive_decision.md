# Derive Statistical Decision Guidance from Data

Calculates censoring metrics from a dataset and returns a corresponding
\[statistical_decision()\] object.

## Usage

``` r
derive_decision(df)
```

## Arguments

- df:

  Data frame containing analytical results.

## Value

A \`"statistical_decision"\` object.

## Details

The input must contain standardized analytical fields:

- \`.analysis_result\`

- \`.censored\`

The following quantities are calculated automatically:

- n_used:

  Number of non-missing analytical results.

- n_detect:

  Number of detected observations.

- percent_censored:

  Percentage of observations flagged as censored.

These values are supplied to \[statistical_decision()\] to obtain
method-selection guidance.

## See also

\[statistical_decision()\], \[estimate_distribution()\]

## Examples

``` r
derive_decision(gw_schema)
#> Error: object 'gw_schema' not found

gw_schema |>
  dplyr::filter(param_name == "Arsenic, dissolved") |>
  derive_decision()
#> Error: object 'gw_schema' not found
```
