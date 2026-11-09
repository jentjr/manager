# Schoeller Diagram

Creates a Schoeller diagram for comparing major-ion chemistry among
groundwater or surface water samples.

## Usage

``` r
schoeller_plot(df, ...)

# S3 method for class 'data.frame'
schoeller_plot(df, location, analyte, result, unit, date, ...)

# S3 method for class 'env_schema'
schoeller_plot(
  df,
  calcium = "Calcium, dissolved",
  magnesium = "Magnesium, dissolved",
  sodium = "Sodium, dissolved",
  potassium = "Potassium, dissolved",
  chloride = "Chloride, total",
  sulfate = "Sulfate, total",
  alkalinity = "Alkalinity, total (lab)",
  analyte_matching = c("exact", "strict", "normalized"),
  facet_var = c("location", "date"),
  title = NULL,
  lwt = 1
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

- analyte_matching:

  Analyte matching strategy. One of \`"exact"\`, \`"strict"\`, or
  \`"normalized"\`.

- facet_var:

  Faceting variable. One of \`"location"\` or \`"date"\`.

- title:

  Optional plot title.

- lwt:

  Line width.

## Value

A ggplot object.

## Details

\`schoeller_plot()\` is an S3 generic that supports both raw data frames
and \`env_schema\` objects.

For data frames, environmental column mappings must be supplied. For
\`env_schema\` objects, mapped schema fields are used automatically.

For data frames, \`schoeller_plot()\` first converts the supplied data
to an \`env_schema\` object using the specified column mappings and then
dispatches to \[schoeller_plot.env_schema()\].

Schoeller diagrams display major-ion concentrations on a logarithmic
scale and are commonly used to compare water chemistry among locations
and sampling events.

Major-ion concentrations are converted to milliequivalents per liter
(meq/L) and plotted as connected profiles across standard hydrochemical
ion categories.

Sodium and potassium are combined into a single \`"Na + K"\` axis
following conventional Schoeller diagram practice.

Missing analytes are permitted and omitted from the resulting profile.

For \`env_schema\` objects, analyte, result, unit, date, and location
fields are determined from schema mappings.

## Analyte Matching

- exact:

  Uses analyte names exactly as reported.

- strict:

  Applies text normalization while preserving analytical distinctions
  such as dissolved versus total fractions.

- normalized:

  Resolves analyte aliases to canonical analyte names using package
  analyte metadata.

## Faceting

If \`facet_var = "location"\`, each location is displayed in a separate
panel and sample events are drawn as individual profiles.

If \`facet_var = "date"\`, each sampling event is displayed in a
separate panel and locations are drawn as individual profiles.

## Validation

Input data are validated for:

- analyte resolution;

- supported analytical units;

- major-ion chemistry availability;

- successful conversion to milliequivalents.

## Examples

``` r
data(gw)

schoeller_plot(gw)
#> Error in tibble::tibble(source_analyte = source_analyte, alias = normalized) %>%     dplyr::left_join(alias_lookup, by = "alias", relationship = "many-to-one") %>%     dplyr::mutate(fraction = dplyr::coalesce(fraction, .extract_fraction(source_analyte))) %>%     dplyr::transmute(source_analyte, normalized_input = normalized,         is_known_analyte = !is.na(analyte_id), analyte_id = dplyr::coalesce(analyte_id,             normalized), analyte = dplyr::coalesce(analyte, source_analyte),         normalized_analyte = dplyr::coalesce(normalized_analyte,             analyte_id, normalized), fraction): could not find function "%>%"

schoeller_plot(
  gw,
  facet_var = "date"
)
#> Error in tibble::tibble(source_analyte = source_analyte, alias = normalized) %>%     dplyr::left_join(alias_lookup, by = "alias", relationship = "many-to-one") %>%     dplyr::mutate(fraction = dplyr::coalesce(fraction, .extract_fraction(source_analyte))) %>%     dplyr::transmute(source_analyte, normalized_input = normalized,         is_known_analyte = !is.na(analyte_id), analyte_id = dplyr::coalesce(analyte_id,             normalized), analyte = dplyr::coalesce(analyte, source_analyte),         normalized_analyte = dplyr::coalesce(normalized_analyte,             analyte_id, normalized), fraction): could not find function "%>%"

schoeller_plot(
  gw,
  analyte_matching = "normalized"
)
#> Error in tibble::tibble(source_analyte = source_analyte, alias = normalized) %>%     dplyr::left_join(alias_lookup, by = "alias", relationship = "many-to-one") %>%     dplyr::mutate(fraction = dplyr::coalesce(fraction, .extract_fraction(source_analyte))) %>%     dplyr::transmute(source_analyte, normalized_input = normalized,         is_known_analyte = !is.na(analyte_id), analyte_id = dplyr::coalesce(analyte_id,             normalized), analyte = dplyr::coalesce(analyte, source_analyte),         normalized_analyte = dplyr::coalesce(normalized_analyte,             analyte_id, normalized), fraction): could not find function "%>%"

schoeller_plot(
  gw,
  title = "Major Ion Chemistry",
  lwt = 1.5
)
#> Error in tibble::tibble(source_analyte = source_analyte, alias = normalized) %>%     dplyr::left_join(alias_lookup, by = "alias", relationship = "many-to-one") %>%     dplyr::mutate(fraction = dplyr::coalesce(fraction, .extract_fraction(source_analyte))) %>%     dplyr::transmute(source_analyte, normalized_input = normalized,         is_known_analyte = !is.na(analyte_id), analyte_id = dplyr::coalesce(analyte_id,             normalized), analyte = dplyr::coalesce(analyte, source_analyte),         normalized_analyte = dplyr::coalesce(normalized_analyte,             analyte_id, normalized), fraction): could not find function "%>%"
```
