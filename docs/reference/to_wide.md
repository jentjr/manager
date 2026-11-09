# Convert Long Data to Wide Format

Converts environmental monitoring data from long format to wide format
using analytes as columns.

## Usage

``` r
to_wide(df, ...)

# S3 method for class 'data.frame'
to_wide(
  df,
  location_id = "location_id",
  sample_date = "sample_date",
  param_name = "param_name",
  analysis_result = "analysis_result",
  lab_id = NULL
)

# S3 method for class 'env_schema'
to_wide(df, label = TRUE, result = c("analysis", "display"))
```

## Arguments

- df:

  A data frame or \`env_schema\` object.

- ...:

  Additional information.

- location_id:

  Location column name.

- sample_date:

  Sample date column name.

- param_name:

  Analyte column name.

- analysis_result:

  Analytical result column name.

- lab_id:

  Optional laboratory identifier column.

- label:

  Use analyte display labels as column names.

- result:

  Result representation to use. Defaults to \`"analysis"\`.

  "analysis"

  : Uses standardized analytical values stored in \`.analysis_result\`.

  "display"

  : Uses display-ready reporting values stored in \`.display_result\`.

## Value

A data frame in wide format.

## Details

\`to_wide()\` is an S3 generic supporting both \`data.frame\` and
\`env_schema\` objects.

When \`result = "display"\`, analyte columns contain display-ready
character values derived from \`.display_result\`, making them suitable
for reporting tables and exports.

When \`result = "analysis"\`, analyte columns contain standardized
numeric values from \`.analysis_result\` suitable for statistical and
geochemical calculations.

## env_schema Method

For \`env_schema\` objects, location, date, analyte, and result fields
are obtained automatically from schema mappings.
