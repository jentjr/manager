# Piper Diagram

Creates a Piper diagram for hydrochemical facies interpretation using
major-ion chemistry data.

## Usage

``` r
piper_plot(df, ...)

# S3 method for class 'data.frame'
piper_plot(df, location, analyte, result, unit, date, ...)

# S3 method for class 'env_schema'
piper_plot(
  df,
  analyte_matching = c("strict", "exact", "normalized"),
  x_cation = "Calcium, dissolved",
  x_cation_label = "Ca",
  y_cation = "Magnesium, dissolved",
  y_cation_label = "Mg",
  z_cation = c("Sodium, dissolved", "Potassium, dissolved"),
  z_cation_label = "Na + K",
  x_anion = c("Chloride, total", "Fluoride, total"),
  x_anion_label = "Cl + F",
  y_anion = "Alkalinity, total (lab)",
  y_anion_label = "HCO[3] + CO[3]",
  z_anion = "Sulfate, total",
  z_anion_label = "SO4",
  x_z_anion_label = "SO4 + Cl + F",
  x_y_cation_label = "Ca + Mg",
  scale_analyte = NULL,
  group_col = NULL,
  transparency = 0.7,
  pnt_size = 3,
  label_size = 3,
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

  Analyte matching strategy used to resolve analyte names. One of
  \`"exact"\`, \`"strict"\`, or \`"normalized"\`.

- x_cation:

  Character vector identifying analytes representing calcium.

- x_cation_label:

  Label for the calcium axis.

- y_cation:

  Character vector identifying analytes representing magnesium.

- y_cation_label:

  Label for the magnesium axis.

- z_cation:

  Character vector identifying analytes representing sodium and
  potassium.

- z_cation_label:

  Label for the sodium plus potassium axis.

- x_anion:

  Character vector identifying analytes representing chloride and
  fluoride.

- x_anion_label:

  Label for the chloride plus fluoride axis.

- y_anion:

  Character vector identifying analytes representing alkalinity.

- y_anion_label:

  Label for the bicarbonate/carbonate axis.

- z_anion:

  Character vector identifying analytes representing sulfate.

- z_anion_label:

  Label for the sulfate axis.

- x_z_anion_label:

  Label displayed on the upper-left side of the Piper diamond.

- x_y_cation_label:

  Label displayed on the upper-right side of the Piper diamond.

- scale_analyte:

  Optional analyte used to scale point size.

- group_col:

  Optional column used to assign point shape.

- transparency:

  Point transparency.

- pnt_size:

  Point size when size scaling is not used.

- label_size:

  Axis label size.

- title:

  Optional plot title.

## Value

A ggplot object containing a Piper diagram.

## Details

\`piper_plot()\` is an S3 generic that supports both raw data frames and
\`env_schema\` objects.

For data frames, environmental column mappings must be supplied. For
\`env_schema\` objects, mapped schema fields are used automatically.

For data frames, \`piper_plot()\` first converts the supplied data to an
\`env_schema\` object using the specified column mappings and then
dispatches to \[piper_plot.env_schema()\].

Piper diagrams display normalized major-ion chemistry within cation and
anion ternary plots and a central hydrochemical facies diamond.

Major ions are converted to milliequivalents per liter (meq/L),
normalized to ionic percentages, and transformed into Piper coordinates
before plotting.

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

## Validation

Input data are validated for:

- required major ions;

- analyte resolution;

- supported analytical units;

- duplicate analyte measurements;

- completeness of major-ion chemistry.

## Examples

``` r
data(gw)

piper_plot(gw)
#> Error in df %>% .transform_piper_data(analyte_matching = analyte_matching,     x_cation = x_cation, y_cation = y_cation, z_cation = z_cation,     x_anion = x_anion, y_anion = y_anion, z_anion = z_anion,     scale_analyte = scale_analyte): could not find function "%>%"

piper_plot(
  gw,
  analyte_matching = "normalized"
)
#> Error in df %>% .transform_piper_data(analyte_matching = analyte_matching,     x_cation = x_cation, y_cation = y_cation, z_cation = z_cation,     x_anion = x_anion, y_anion = y_anion, z_anion = z_anion,     scale_analyte = scale_analyte): could not find function "%>%"

piper_plot(
  gw,
  scale_analyte = "Total Dissolved Solids"
)
#> Error in tibble::tibble(source_analyte = source_analyte, alias = normalized) %>%     dplyr::left_join(alias_lookup, by = "alias", relationship = "many-to-one") %>%     dplyr::mutate(fraction = dplyr::coalesce(fraction, .extract_fraction(source_analyte))) %>%     dplyr::transmute(source_analyte, normalized_input = normalized,         is_known_analyte = !is.na(analyte_id), analyte_id = dplyr::coalesce(analyte_id,             normalized), analyte = dplyr::coalesce(analyte, source_analyte),         normalized_analyte = dplyr::coalesce(normalized_analyte,             analyte_id, normalized), fraction): could not find function "%>%"

piper_plot(
  gw,
  x_cation_label = "Ca~phantom()^+2"
)
#> Error in df %>% .transform_piper_data(analyte_matching = analyte_matching,     x_cation = x_cation, y_cation = y_cation, z_cation = z_cation,     x_anion = x_anion, y_anion = y_anion, z_anion = z_anion,     scale_analyte = scale_analyte): could not find function "%>%"
```
