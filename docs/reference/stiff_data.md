# Stiff Diagram Coordinates

Creates Stiff-diagram polygon coordinates from major-ion chemistry.

## Usage

``` r
stiff_data(df, ...)

# S3 method for class 'data.frame'
stiff_data(df, location, analyte, result, unit, sample_date, ...)

# S3 method for class 'env_schema'
stiff_data(
  df,
  analyte_matching = c("exact", "strict", "normalized"),
  calcium = "Calcium, dissolved",
  magnesium = "Magnesium, dissolved",
  sodium = "Sodium, dissolved",
  potassium = "Potassium, dissolved",
  chloride = "Chloride, total",
  sulfate = "Sulfate, total",
  alkalinity = "Alkalinity, total (lab)",
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

  Analyte matching strategy. One of \`"exact"\`, \`"strict"\`, or
  \`"normalized"\`.

- calcium:

  Calcium analyte.

- magnesium:

  Magnesium analyte.

- sodium:

  Sodium analyte.

- potassium:

  Potassium analyte.

- chloride:

  Chloride analyte.

- sulfate:

  Sulfate analyte.

- alkalinity:

  Alkalinity analyte.

- scale_analyte:

  Optional analyte used to scale polygon fill.

## Value

A data frame containing Stiff-diagram polygon vertices.

## Details

\`stiff_data()\` is an S3 generic that supports both raw data frames and
\`env_schema\` objects.

Major-ion chemistry is converted to milliequivalent concentrations and
transformed into Stiff-diagram polygon coordinates.

Unlike most geochemical summary functions, multiple rows are returned
for each sample because each location/date combination is expanded into
a sequence of polygon vertices.

## Output Variables

Returned variables include:

- \`vertex\`

- \`ion\`

- \`stiff_x\`

- \`stiff_y\`

- \`label_side\`

When \`scale_analyte\` is supplied, a \`scale_value\` column is also
returned.

## Examples

``` r
data(gw)

stiff_data(gw)
#> # A tibble: 1,050 × 7
#>    location_id sample_date         vertex ion    stiff_x stiff_y label_side
#>    <chr>       <dttm>               <dbl> <chr>    <dbl>   <dbl> <chr>     
#>  1 MW-1        2007-12-20 00:00:00      1 Na + K  -0.862       3 left      
#>  2 MW-1        2007-12-20 00:00:00      2 Cl       0.846       3 right     
#>  3 MW-1        2007-12-20 00:00:00      3 HCO[3]   2.93        2 right     
#>  4 MW-1        2007-12-20 00:00:00      4 SO[4]    1.06        1 right     
#>  5 MW-1        2007-12-20 00:00:00      5 Mg      -1.73        1 left      
#>  6 MW-1        2007-12-20 00:00:00      6 Ca      -3.23        2 left      
#>  7 MW-1        2007-12-20 00:00:00      7 Na + K  -0.862       3 left      
#>  8 MW-1        2008-02-20 00:00:00      1 Na + K  -0.695       3 left      
#>  9 MW-1        2008-02-20 00:00:00      2 Cl       0.705       3 right     
#> 10 MW-1        2008-02-20 00:00:00      3 HCO[3]   2.25        2 right     
#> # ℹ 1,040 more rows
```
