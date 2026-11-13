# Major Ion Components

Calculates aggregated major-ion component concentrations for
hydrogeochemical evaluation.

## Usage

``` r
major_ions(df, ...)

# S3 method for class 'data.frame'
major_ions(df, location, analyte, result, unit, sample_date, ...)

# S3 method for class 'env_schema'
major_ions(df, analyte_matching = c("exact", "strict", "normalized"), ...)
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

A data frame containing one record per location and sample-date
combination.

## Details

\`major_ions()\` is an S3 generic that supports both raw data frames and
\`env_schema\` objects.

For data frames, \`major_ions()\` first converts the supplied data to an
\`env_schema\` object and then dispatches to
\[major_ions.env_schema()\].

Major-ion concentrations are calculated from analytical results
converted to milliequivalents per liter (meq/L).

## Output Variables

Returned variables include:

- \`calcium\`

- \`magnesium\`

- \`sodium\`

- \`potassium\`

- \`sodium_potassium\`

- \`chloride\`

- \`sulfate\`

- \`alkalinity\`

All concentrations are expressed in milliequivalents per liter (meq/L).

## Examples

``` r
data(gw)

major_ions(gw)
#> # A tibble: 150 × 10
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
#> # ℹ 4 more variables: sodium_potassium <dbl>, chloride <dbl>, sulfate <dbl>,
#> #   alkalinity <dbl>
```
