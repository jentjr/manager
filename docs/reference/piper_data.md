# Piper Diagram Coordinates

Creates Piper-diagram coordinate data from major-ion chemistry.

## Usage

``` r
piper_data(df, ...)

# S3 method for class 'data.frame'
piper_data(df, location, analyte, result, unit, date, ...)

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
  scale_analyte = NULL
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

- date:

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
#> Error in tibble::tibble(source_analyte = source_analyte, alias = normalized) %>%     dplyr::left_join(alias_lookup, by = "alias", relationship = "many-to-one") %>%     dplyr::mutate(fraction = dplyr::coalesce(fraction, .extract_fraction(source_analyte))) %>%     dplyr::transmute(source_analyte, normalized_input = normalized,         is_known_analyte = !is.na(analyte_id), analyte_id = dplyr::coalesce(analyte_id,             normalized), analyte = dplyr::coalesce(analyte, source_analyte),         normalized_analyte = dplyr::coalesce(normalized_analyte,             analyte_id, normalized), fraction): could not find function "%>%"
```
