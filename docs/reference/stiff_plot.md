# Stiff Diagram

Creates Stiff diagrams for visualization and comparison of major-ion
water chemistry.

## Usage

``` r
stiff_plot(df, ...)

# S3 method for class 'data.frame'
stiff_plot(df, location, analyte, result, unit, date, ...)

# S3 method for class 'env_schema'
stiff_plot(
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
  group_var = NULL,
  facet_var = NULL,
  stiff_limit = NULL,
  stiff_breaks = 1,
  lines = FALSE,
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

- group_var:

  Variable used to split the data into individual Stiff diagrams.

- facet_var:

  Variable used for faceting within each plot.

- stiff_limit:

  Maximum absolute x-axis extent expressed in milliequivalents per liter
  (meq/L). If \`NULL\`, the limit is determined automatically from the
  data.

- stiff_breaks:

  Grid spacing used for x-axis and reference lines.

- lines:

  Logical. If \`TRUE\`, horizontal reference lines are drawn through
  each ion category.

- title:

  Optional plot title.

## Value

A named list of ggplot objects. One Stiff diagram is produced for each
value of \`group_var\`.

A named list of ggplot objects. One Stiff diagram is produced for each
value of \`group_var\`.

## Details

\`stiff_plot()\` is an S3 generic that supports both raw data frames and
\`env_schema\` objects.

For data frames, environmental column mappings must be supplied. For
\`env_schema\` objects, mapped schema fields are used automatically.

For data frames, \`stiff_plot()\` first converts the supplied data to an
\`env_schema\` object using the specified column mappings and then
dispatches to \[stiff_plot.env_schema()\].

Stiff diagrams display major-ion chemistry as a polygon whose shape
reflects relative ionic composition.

Major-ion analytes are converted to milliequivalents per liter (meq/L)
and transformed into standard Stiff-diagram coordinates.

Cations are plotted on the left side of the diagram and anions are
plotted on the right side.

Similar polygon shapes generally indicate similar hydrochemical
characteristics, making Stiff diagrams useful for evaluating:

- spatial variability;

- temporal trends;

- groundwater mixing;

- hydrochemical facies;

- geochemical evolution.

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

## Grouping

\`group_var\` controls how data are divided into separate Stiff
diagrams.

By default, one plot is generated per location.

If \`group_var\` is set to the mapped date field, one plot is generated
per sampling event.

## Faceting

\`facet_var\` controls the panels displayed within each Stiff diagram
plot.

By default, individual sampling events are shown as separate facets.

## Scaling

When \`scale_analyte\` is supplied, polygon fill colour is scaled using
values of the selected analyte.

This is often useful for displaying Total Dissolved Solids (TDS) or
another indicator parameter alongside hydrochemical facies.

## Examples

``` r
data(gw)

stiff_plot(gw)
#> Error in split(df, df[[group_var]]) %>% purrr::imap(~.stiff_plot(df = .x,     group_name = .y, location_col = location_col, date_col = date_col,     analyte_matching = analyte_matching, calcium = calcium, magnesium = magnesium,     sodium = sodium, potassium = potassium, chloride = chloride,     sulfate = sulfate, alkalinity = alkalinity, scale_analyte = scale_analyte,     facet_var = facet_var, group_var = group_var, stiff_limit = stiff_limit,     stiff_breaks = stiff_breaks, lines = lines, title = title)): could not find function "%>%"

stiff_plot(
  gw,
  lines = TRUE
)
#> Error in split(df, df[[group_var]]) %>% purrr::imap(~.stiff_plot(df = .x,     group_name = .y, location_col = location_col, date_col = date_col,     analyte_matching = analyte_matching, calcium = calcium, magnesium = magnesium,     sodium = sodium, potassium = potassium, chloride = chloride,     sulfate = sulfate, alkalinity = alkalinity, scale_analyte = scale_analyte,     facet_var = facet_var, group_var = group_var, stiff_limit = stiff_limit,     stiff_breaks = stiff_breaks, lines = lines, title = title)): could not find function "%>%"

stiff_plot(
  gw,
  scale_analyte = "Total Dissolved Solids"
)
#> Error in split(df, df[[group_var]]) %>% purrr::imap(~.stiff_plot(df = .x,     group_name = .y, location_col = location_col, date_col = date_col,     analyte_matching = analyte_matching, calcium = calcium, magnesium = magnesium,     sodium = sodium, potassium = potassium, chloride = chloride,     sulfate = sulfate, alkalinity = alkalinity, scale_analyte = scale_analyte,     facet_var = facet_var, group_var = group_var, stiff_limit = stiff_limit,     stiff_breaks = stiff_breaks, lines = lines, title = title)): could not find function "%>%"

stiff_plot(
  gw,
  group_var = "sample_date"
)
#> Error in split(df, df[[group_var]]) %>% purrr::imap(~.stiff_plot(df = .x,     group_name = .y, location_col = location_col, date_col = date_col,     analyte_matching = analyte_matching, calcium = calcium, magnesium = magnesium,     sodium = sodium, potassium = potassium, chloride = chloride,     sulfate = sulfate, alkalinity = alkalinity, scale_analyte = scale_analyte,     facet_var = facet_var, group_var = group_var, stiff_limit = stiff_limit,     stiff_breaks = stiff_breaks, lines = lines, title = title)): could not find function "%>%"
```
