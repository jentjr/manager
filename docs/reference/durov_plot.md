# Durov Diagram

Creates a Durov diagram for interpretation of hydrochemical facies,
mixing processes, ion exchange, and geochemical evolution.

## Usage

``` r
durov_plot(df, ...)

# S3 method for class 'data.frame'
durov_plot(df, location, analyte, result, unit, date, ...)

# S3 method for class 'env_schema'
durov_plot(
  df,
  analyte_matching = c("normalized", "strict", "exact"),
  title = NULL
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

- title:

  Optional plot title.

## Value

A ggplot object.

## Details

\`durov_plot()\` is an S3 generic that supports both raw data frames and
\`env_schema\` objects.

For data frames, environmental column mappings must be supplied. For
\`env_schema\` objects, mapped schema fields are used automatically.

For data frames, \`durov_plot()\` first converts the supplied data to an
\`env_schema\` object using the specified column mappings and then
dispatches to \[durov_plot.env_schema()\].

Durov diagrams combine cation and anion ternary plots with a central
projection field to aid interpretation of hydrochemical facies, mixing
trends, ion exchange processes, and geochemical evolution.

Major-ion concentrations are converted to milliequivalents per liter
(meq/L), normalized to ionic percentages, and transformed into Durov
coordinates before plotting.

The central Durov field may be used to evaluate:

- groundwater recharge processes;

- mixing between water types;

- ion exchange reactions;

- salinization trends;

- hydrochemical evolution.

For \`env_schema\` objects, analyte, result, unit, date, and location
fields are obtained directly from schema mappings.

## Analyte Matching

- exact:

  Uses analyte names exactly as reported.

- strict:

  Applies text normalization while preserving analytical distinctions
  such as dissolved versus total fractions.

- normalized:

  Resolves analyte aliases to canonical analyte names using package
  analyte metadata.

## Interpretation

The Durov projection field is divided into nine regions representing
characteristic hydrochemical facies and geochemical processes.

These fields should be interpreted as general hydrochemical indicators
and evaluated alongside site-specific geological, hydrological, and
geochemical information.

## Validation

Input data are validated for:

- major-ion chemistry availability;

- analyte resolution;

- supported analytical units;

- successful conversion to milliequivalents;

- completeness of major-ion chemistry.

## Examples

``` r
data(gw)

durov_plot(gw)
#> Error in tibble::tibble(source_analyte = source_analyte, alias = normalized) %>%     dplyr::left_join(alias_lookup, by = "alias", relationship = "many-to-one") %>%     dplyr::mutate(fraction = dplyr::coalesce(fraction, .extract_fraction(source_analyte))) %>%     dplyr::transmute(source_analyte, normalized_input = normalized,         is_known_analyte = !is.na(analyte_id), analyte_id = dplyr::coalesce(analyte_id,             normalized), analyte = dplyr::coalesce(analyte, source_analyte),         normalized_analyte = dplyr::coalesce(normalized_analyte,             analyte_id, normalized), fraction): could not find function "%>%"

durov_plot(
  gw,
  analyte_matching = "normalized"
)
#> Error in tibble::tibble(source_analyte = source_analyte, alias = normalized) %>%     dplyr::left_join(alias_lookup, by = "alias", relationship = "many-to-one") %>%     dplyr::mutate(fraction = dplyr::coalesce(fraction, .extract_fraction(source_analyte))) %>%     dplyr::transmute(source_analyte, normalized_input = normalized,         is_known_analyte = !is.na(analyte_id), analyte_id = dplyr::coalesce(analyte_id,             normalized), analyte = dplyr::coalesce(analyte, source_analyte),         normalized_analyte = dplyr::coalesce(normalized_analyte,             analyte_id, normalized), fraction): could not find function "%>%"

durov_plot(
  gw,
  title = "Hydrochemical Facies"
)
#> Error in tibble::tibble(source_analyte = source_analyte, alias = normalized) %>%     dplyr::left_join(alias_lookup, by = "alias", relationship = "many-to-one") %>%     dplyr::mutate(fraction = dplyr::coalesce(fraction, .extract_fraction(source_analyte))) %>%     dplyr::transmute(source_analyte, normalized_input = normalized,         is_known_analyte = !is.na(analyte_id), analyte_id = dplyr::coalesce(analyte_id,             normalized), analyte = dplyr::coalesce(analyte, source_analyte),         normalized_analyte = dplyr::coalesce(normalized_analyte,             analyte_id, normalized), fraction): could not find function "%>%"
```
