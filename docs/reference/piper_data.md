# Piper Diagram Coordinates

Creates Piper-diagram coordinate data from major-ion chemistry.

## Usage

``` r
piper_data(df, ...)

# S3 method for class 'data.frame'
piper_data(df, location, analyte, result, unit, sample_date, ...)

# S3 method for class 'env_schema'
piper_data(
  df,
  analyte_matching = c("strict", "exact", "normalized"),
  x_cation = "Calcium, dissolved",
  y_cation = "Magnesium, dissolved",
  z_cation = c("Sodium, dissolved", "Potassium, dissolved"),
  x_anion = c("Chloride, total", "Fluoride, total"),
  y_anion = "Alkalinity, total (lab)",
  z_anion = "Sulfate, total",
  scale_analyte = NULL,
  ...
)
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

  Analyte matching strategy used to resolve analyte names. One of
  \`"exact"\`, \`"strict"\`, or \`"normalized"\`.

- x_cation:

  Character vector identifying analytes representing calcium.

- y_cation:

  Character vector identifying analytes representing magnesium.

- z_cation:

  Character vector identifying analytes representing sodium and
  potassium.

- x_anion:

  Character vector identifying analytes representing chloride and
  fluoride.

- y_anion:

  Character vector identifying analytes representing alkalinity.

- z_anion:

  Character vector identifying analytes representing sulfate.

- scale_analyte:

  Optional analyte used to scale point size.

## Value

A data frame containing Piper plotting coordinates.

## Details

\`piper_data()\` is an S3 generic that supports both raw data frames and
\`env_schema\` objects.

Major-ion concentrations are converted to milliequivalents per liter
(meq/L), normalized to ionic percentages, and transformed into Piper
plotting coordinates.

## Output Variables

- \`cation_x\`

- \`cation_y\`

- \`anion_x\`

- \`anion_y\`

- \`diamond_x\`

- \`diamond_y\`

One row is returned for each unique location and sample-date
combination.

## Examples

``` r
data(gw)

piper_data(gw)
#> # A tibble: 150 × 8
#>    location_id sample_date         cation_x cation_y anion_x anion_y diamond_x
#>  * <chr>       <dttm>                 <dbl>    <dbl>   <dbl>   <dbl>     <dbl>
#>  1 MW-1        2007-12-20 00:00:00     29.7     25.7    149.   19.0       87.2
#>  2 MW-1        2008-02-20 00:00:00     25.6     23.9    151.   22.5       87.9
#>  3 MW-1        2008-04-24 00:00:00     21.4     24.9    146.   27.2       84.1
#>  4 MW-1        2008-06-18 00:00:00     21.9     27.8    148.   31.2       85.8
#>  5 MW-1        2008-08-26 00:00:00     22.3     29.1    147.   31.5       85.5
#>  6 MW-1        2008-10-28 00:00:00     21.9     29.8    137.    5.79      72.7
#>  7 MW-1        2008-12-16 00:00:00     21.5     29.5    147.   29.9       84.5
#>  8 MW-1        2009-02-25 00:00:00     21.5     29.5    141.   21.2       78.6
#>  9 MW-1        2009-04-29 00:00:00     21.5     29.2    139.   21.3       78.2
#> 10 MW-1        2009-10-21 00:00:00     22.1     29.2    141.   23.0       79.7
#> # ℹ 140 more rows
#> # ℹ 1 more variable: diamond_y <dbl>
```
