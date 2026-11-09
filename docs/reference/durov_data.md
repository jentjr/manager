# Durov Diagram Coordinates

Creates Durov-diagram coordinate data from major-ion chemistry.

## Usage

``` r
durov_data(df, ...)

# S3 method for class 'data.frame'
durov_data(df, location, analyte, result, unit, date, ...)

# S3 method for class 'env_schema'
durov_data(df, analyte_matching = c("normalized", "strict", "exact"))
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
#> Error in tibble::tibble(source_analyte = source_analyte, alias = normalized) %>%     dplyr::left_join(alias_lookup, by = "alias", relationship = "many-to-one") %>%     dplyr::mutate(fraction = dplyr::coalesce(fraction, .extract_fraction(source_analyte))) %>%     dplyr::transmute(source_analyte, normalized_input = normalized,         is_known_analyte = !is.na(analyte_id), analyte_id = dplyr::coalesce(analyte_id,             normalized), analyte = dplyr::coalesce(analyte, source_analyte),         normalized_analyte = dplyr::coalesce(normalized_analyte,             analyte_id, normalized), fraction): could not find function "%>%"
```
