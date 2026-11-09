# Schoeller Diagram Data

Creates Schoeller-diagram plotting data from major-ion chemistry.

## Usage

``` r
schoeller_data(df, ...)

# S3 method for class 'data.frame'
schoeller_data(df, location, analyte, result, unit, date, ...)

# S3 method for class 'env_schema'
schoeller_data(df, analyte_matching = c("exact", "strict", "normalized"))
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

A tidy data frame containing Schoeller-diagram plotting data.

## Details

\`schoeller_data()\` is an S3 generic that supports both raw data frames
and \`env_schema\` objects.

For data frames, \`schoeller_data()\` first converts the supplied data
to an \`env_schema\` object and then dispatches to
\[schoeller_data.env_schema()\].

Major-ion concentrations are converted to milliequivalents per liter
(meq/L) and transformed into the categorical structure required for
Schoeller diagrams.

Sodium and potassium are combined into a single \`"Na + K"\` category
consistent with standard Schoeller-diagram conventions.

## Output Variables

Returned variables include:

- location identifier;

- sample date;

- \`schoeller_axis\`;

- \`analysis_result\`.

Schoeller axis levels are returned in the following order:

- \`Ca\`

- \`Mg\`

- \`Na + K\`

- \`Cl\`

- \`SO4\`

- \`Alk\`

The returned \`analysis_result\` values are expressed in
milliequivalents per liter (meq/L).

## Output Structure

Multiple rows are returned for each sample. Each row represents a single
Schoeller-axis category for a given location and sample date.

## Examples

``` r
data(gw)

schoeller <- schoeller_data(gw)
#> Error in tibble::tibble(source_analyte = source_analyte, alias = normalized) %>%     dplyr::left_join(alias_lookup, by = "alias", relationship = "many-to-one") %>%     dplyr::mutate(fraction = dplyr::coalesce(fraction, .extract_fraction(source_analyte))) %>%     dplyr::transmute(source_analyte, normalized_input = normalized,         is_known_analyte = !is.na(analyte_id), analyte_id = dplyr::coalesce(analyte_id,             normalized), analyte = dplyr::coalesce(analyte, source_analyte),         normalized_analyte = dplyr::coalesce(normalized_analyte,             analyte_id, normalized), fraction): could not find function "%>%"

schoeller
#> Error: object 'schoeller' not found
```
