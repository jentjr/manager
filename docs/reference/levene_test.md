# Levene's Test for Homogeneity of Variance

Levene's Test for Homogeneity of Variance

## Usage

``` r
levene_test(
  df,
  result_col = NULL,
  group_col = NULL,
  center = "median",
  alpha = 0.05
)
```

## Arguments

- df:

  A data frame, grouped data frame, or \`env_schema\` object.

- result_col:

  Name of the numeric analytical-result column.

- group_col:

  Optional grouping column or columns. If omitted for a grouped data
  frame, the columns created by \`dplyr::group_by()\` are used.

- center:

  Centering method. \`"median"\` performs the Brown-Forsythe version of
  Levene's test and is generally more robust.

## Value

A tibble containing the Levene test results.
