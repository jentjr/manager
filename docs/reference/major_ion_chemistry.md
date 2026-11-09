# Major Ion Chemistry

Extracts, standardizes, and converts major-ion chemistry data for
hydrogeochemical analysis.

## Usage

``` r
major_ion_chemistry(df, ...)

# S3 method for class 'data.frame'
major_ion_chemistry(df, location, analyte, result, unit, date, ...)

# S3 method for class 'env_schema'
major_ion_chemistry(
  df,
  analytes = c("Calcium, dissolved", "Magnesium, dissolved", "Sodium, dissolved",
    "Potassium, dissolved", "Chloride, total", "Sulfate, total",
    "Alkalinity, total (lab)"),
  analyte_matching = c("exact", "strict", "normalized")
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

- analytes:

  Major-ion analytes to retain.

- analyte_matching:

  Analyte matching strategy. One of \`"exact"\`, \`"strict"\`, or
  \`"normalized"\`.

## Value

An \`env_schema\` object containing standardized major-ion chemistry
data.

## Details

\`major_ion_chemistry()\` is an S3 generic that supports both raw data
frames and \`env_schema\` objects.

For data frames, environmental column mappings must be supplied. For
\`env_schema\` objects, mapped schema fields are used automatically.

Major-ion analytes are standardized, duplicate measurements are
consolidated, and analytical results are converted to milliequivalents
per liter (meq/L).

For data frames, \`major_ion_chemistry()\` first converts the supplied
data to an \`env_schema\` object using the specified column mappings and
then dispatches to \[major_ion_chemistry.env_schema()\].

\`major_ion_chemistry()\` serves as the primary preprocessing function
for geochemical analyses and diagrams.

The function:

- resolves analyte names;

- filters requested analytes;

- consolidates duplicate measurements;

- standardizes analytical metadata;

- converts concentrations to milliequivalents per liter (meq/L).

Duplicate measurements within a location and sample-date combination are
averaged prior to conversion.

## Analyte Matching

- exact:

  Uses analyte names exactly as reported.

- strict:

  Applies text normalization while preserving analytical distinctions
  such as dissolved versus total fractions.

- normalized:

  Resolves analyte aliases to canonical analyte names using package
  analyte metadata.

## Output Structure

One record is returned for each unique analyte, location, and
sample-date combination.

Analytical concentrations are available in both native units and
standardized milliequivalent units suitable for geochemical
calculations.

## Examples

``` r
data(gw)

chem <- major_ion_chemistry(gw)
#> Error in tibble::tibble(source_analyte = source_analyte, alias = normalized) %>%     dplyr::left_join(alias_lookup, by = "alias", relationship = "many-to-one") %>%     dplyr::mutate(fraction = dplyr::coalesce(fraction, .extract_fraction(source_analyte))) %>%     dplyr::transmute(source_analyte, normalized_input = normalized,         is_known_analyte = !is.na(analyte_id), analyte_id = dplyr::coalesce(analyte_id,             normalized), analyte = dplyr::coalesce(analyte, source_analyte),         normalized_analyte = dplyr::coalesce(normalized_analyte,             analyte_id, normalized), fraction): could not find function "%>%"

chem
#> Error: object 'chem' not found

data(gw)

major_ion_chemistry(gw)
#> Error in tibble::tibble(source_analyte = source_analyte, alias = normalized) %>%     dplyr::left_join(alias_lookup, by = "alias", relationship = "many-to-one") %>%     dplyr::mutate(fraction = dplyr::coalesce(fraction, .extract_fraction(source_analyte))) %>%     dplyr::transmute(source_analyte, normalized_input = normalized,         is_known_analyte = !is.na(analyte_id), analyte_id = dplyr::coalesce(analyte_id,             normalized), analyte = dplyr::coalesce(analyte, source_analyte),         normalized_analyte = dplyr::coalesce(normalized_analyte,             analyte_id, normalized), fraction): could not find function "%>%"

major_ion_chemistry(
  gw,
  analyte_matching = "normalized"
)
#> Error in tibble::tibble(source_analyte = source_analyte, alias = normalized) %>%     dplyr::left_join(alias_lookup, by = "alias", relationship = "many-to-one") %>%     dplyr::mutate(fraction = dplyr::coalesce(fraction, .extract_fraction(source_analyte))) %>%     dplyr::transmute(source_analyte, normalized_input = normalized,         is_known_analyte = !is.na(analyte_id), analyte_id = dplyr::coalesce(analyte_id,             normalized), analyte = dplyr::coalesce(analyte, source_analyte),         normalized_analyte = dplyr::coalesce(normalized_analyte,             analyte_id, normalized), fraction): could not find function "%>%"
```
