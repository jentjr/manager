# Stiff Diagram

Creates Stiff diagrams for visualization and comparison of major-ion
water chemistry.

## Usage

``` r
stiff_plot(df, ...)

# S3 method for class 'data.frame'
stiff_plot(df, location, analyte, result, unit, sample_date, ...)

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
  title = NULL,
  ...
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

- sample_date:

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

For \`env_schema\` objects, analyte, result, unit, sample_date, and
location fields are obtained directly from schema mappings.

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

If \`group_var\` is set to the mapped sample_date field, one plot is
generated per sampling event.

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
#> $`MW-1`

#> 
#> $`MW-2`

#> 
#> $`MW-3`

#> 
#> $`MW-4`

#> 
#> $`MW-5`

#> 
#> $`MW-6`

#> 
#> $`MW-7`

#> 
#> $`MW-8`

#> 

stiff_plot(
  gw,
  lines = TRUE
)
#> $`MW-1`

#> 
#> $`MW-2`

#> 
#> $`MW-3`

#> 
#> $`MW-4`

#> 
#> $`MW-5`

#> 
#> $`MW-6`

#> 
#> $`MW-7`

#> 
#> $`MW-8`

#> 

stiff_plot(
  gw,
  scale_analyte = "Total Dissolved Solids"
)
#> $`MW-1`

#> 
#> $`MW-2`

#> 
#> $`MW-3`

#> 
#> $`MW-4`

#> 
#> $`MW-5`

#> 
#> $`MW-6`

#> 
#> $`MW-7`

#> 
#> $`MW-8`

#> 

stiff_plot(
  gw,
  group_var = sample_date_var(gw)
)
#> Warning: No valid major-ion chemistry available for plotting.
#> $`2007-12-20`

#> 
#> $`2008-02-20`

#> 
#> $`2008-04-24`

#> 
#> $`2008-06-18`

#> 
#> $`2008-08-26`

#> 
#> $`2008-10-28`

#> 
#> $`2008-12-16`

#> 
#> $`2009-02-25`

#> 
#> $`2009-04-29`

#> 
#> $`2009-10-21`

#> 
#> $`2009-10-22`

#> 
#> $`2010-04-13`

#> 
#> $`2010-11-02`

#> 
#> $`2011-05-17`

#> 
#> $`2011-05-18`

#> 
#> $`2011-10-05`

#> 
#> $`2012-04-03`

#> 
#> $`2012-06-20`

#> 
#> $`2012-08-22`

#> 
#> $`2012-10-01`
#> NULL
#> 
#> $`2012-10-03`

#> 
#> $`2012-11-14`

#> 
#> $`2013-04-10`

#> 
#> $`2013-10-02`

#> 
```
