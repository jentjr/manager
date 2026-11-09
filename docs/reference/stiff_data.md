# Stiff Diagram Coordinates

Creates Stiff-diagram polygon coordinates from major-ion chemistry.

## Usage

``` r
stiff_data(df, ...)

# S3 method for class 'data.frame'
stiff_data(df, location, analyte, result, unit, date, ...)

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
#> Error in tibble::tibble(source_analyte = source_analyte, alias = normalized) %>%     dplyr::left_join(alias_lookup, by = "alias", relationship = "many-to-one") %>%     dplyr::mutate(fraction = dplyr::coalesce(fraction, .extract_fraction(source_analyte))) %>%     dplyr::transmute(source_analyte, normalized_input = normalized,         is_known_analyte = !is.na(analyte_id), analyte_id = dplyr::coalesce(analyte_id,             normalized), analyte = dplyr::coalesce(analyte, source_analyte),         normalized_analyte = dplyr::coalesce(normalized_analyte,             analyte_id, normalized), fraction): could not find function "%>%"
```
