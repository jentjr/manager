# Major Ion Components

Calculates aggregated major-ion component concentrations for
hydrogeochemical evaluation.

## Usage

``` r
major_ions(df, ...)

# S3 method for class 'data.frame'
major_ions(df, location, analyte, result, unit, date, ...)

# S3 method for class 'env_schema'
major_ions(df, analyte_matching = c("exact", "strict", "normalized"))
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
#> Error in tibble::tibble(source_analyte = source_analyte, alias = normalized) %>%     dplyr::left_join(alias_lookup, by = "alias", relationship = "many-to-one") %>%     dplyr::mutate(fraction = dplyr::coalesce(fraction, .extract_fraction(source_analyte))) %>%     dplyr::transmute(source_analyte, normalized_input = normalized,         is_known_analyte = !is.na(analyte_id), analyte_id = dplyr::coalesce(analyte_id,             normalized), analyte = dplyr::coalesce(analyte, source_analyte),         normalized_analyte = dplyr::coalesce(normalized_analyte,             analyte_id, normalized), fraction): could not find function "%>%"
```
