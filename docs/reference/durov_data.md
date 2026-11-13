# Durov Diagram Coordinates

Creates Durov-diagram coordinate data from major-ion chemistry.

## Usage

``` r
durov_data(df, ...)

# S3 method for class 'data.frame'
durov_data(df, location, analyte, result, unit, sample_date, ...)

# S3 method for class 'env_schema'
durov_data(df, analyte_matching = c("normalized", "strict", "exact"), ...)
```

## Arguments

- df:

  A data frame or \`env_schema\` object.

- ...:

  Additional arguments passed to methods.

- location:

  Location identifier column.

- analyte:

  Analyte column.

- result:

  Analytical result column.

- unit:

  Reporting unit column.

- sample_date:

  Sample date column.

- analyte_matching:

  Analyte matching strategy. One of \`"exact"\`, \`"strict"\`, or
  \`"normalized"\`.

## Value

A data frame containing Durov plotting coordinates.

## Details

\`durov_data()\` is an S3 generic that supports both raw data frames and
\`env_schema\` objects.

Major-ion concentrations are converted to normalized ionic percentages
and transformed into Durov projection coordinates.

## Output Variables

Returned data include:

- major-ion component totals;

- cation-triangle coordinates;

- anion-triangle coordinates;

- \`durov_x\`;

- \`durov_y\`;

- \`durov_x_plot\`;

- \`durov_y_plot\`.

## Examples

``` r
data(gw)

durov_data(gw)
#> Warning: 2 sample(s) were omitted because insufficient major-ion chemistry was
#> available.
#> # A tibble: 150 × 24
#>    location_id sample_date         calcium magnesium sodium potassium
#>    <chr>       <dttm>                <dbl>     <dbl>  <dbl>     <dbl>
#>  1 MW-1        2007-12-20 00:00:00    3.23      1.73  0.779   0.0831 
#>  2 MW-1        2008-02-20 00:00:00    3.56      1.62  0.609   0.0864 
#>  3 MW-1        2008-04-24 00:00:00    5.59      2.50  0.605   0.00256
#>  4 MW-1        2008-06-18 00:00:00    6.04      3.13  0.492   0.0767 
#>  5 MW-1        2008-08-26 00:00:00    6.24      3.45  0.518   0.0435 
#>  6 MW-1        2008-10-28 00:00:00    6.49      3.66  0.474   0.0281 
#>  7 MW-1        2008-12-16 00:00:00    5.74      3.18  0.406   0.0179 
#>  8 MW-1        2009-02-25 00:00:00    6.29      3.49  0.439   0.0179 
#>  9 MW-1        2009-04-29 00:00:00    5.99      3.28  0.439   0.0153 
#> 10 MW-1        2009-10-21 00:00:00    5.69      3.14  0.478   0.0128 
#> # ℹ 140 more rows
#> # ℹ 18 more variables: sodium_potassium <dbl>, chloride <dbl>, sulfate <dbl>,
#> #   alkalinity <dbl>, cation_left_pct <dbl>, cation_top_pct <dbl>,
#> #   cation_right_pct <dbl>, cation_x <dbl>, cation_y <dbl>,
#> #   anion_left_pct <dbl>, anion_top_pct <dbl>, anion_right_pct <dbl>,
#> #   anion_x <dbl>, anion_y <dbl>, durov_x <dbl>, durov_y <dbl>,
#> #   durov_x_plot <dbl>, durov_y_plot <dbl>
```
