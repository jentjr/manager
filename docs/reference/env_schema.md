# Construct an environmental data schema

Creates an \`env_schema\` object by attaching environmental column
mappings and schema metadata to a data frame.

## Usage

``` r
env_schema(
  df,
  site = NULL,
  location,
  date,
  analyte,
  result,
  unit = NULL,
  sample_id = NULL,
  reporting_flag = NULL,
  reporting_limit = NULL,
  detection_limit = NULL,
  quantitation_limit = NULL,
  achieved_mdc = NULL,
  measurement_uncertainty = NULL,
  method = NULL,
  analysis_date = NULL,
  matrix = NULL,
  lab_name = NULL,
  metadata = NULL,
  qualifier_table = NULL,
  display_options = default_display_options(),
  analyte_lookup = analyte_tbl
)
```

## Arguments

- df:

  Data frame.

- site:

  Site identifier column.

- location:

  Location identifier column.

- date:

  Sample date column.

- analyte:

  Analyte column.

- result:

  Analytical result column.

- unit:

  Unit column.

- sample_id:

  Sample identifier column.

- reporting_flag:

  Laboratory qualifier column.

- reporting_limit:

  Reporting limit column.

- detection_limit:

  Detection limit column.

- quantitation_limit:

  Quantitation limit column (PQL, LOQ, SQL, etc.).

- achieved_mdc:

  Achieved minimum detectable concentration (radiochemistry).

- measurement_uncertainty:

  Measurement uncertainty column.

- method:

  Analytical method column.

- analysis_date:

  Analysis date column.

- matrix:

  Matrix column.

- lab_name:

  Laboratory column.

- metadata:

  Optional tidyselect specification identifying columns containing
  method-specific metadata.

- qualifier_table:

  Optional qualifier lookup table.

- analyte_lookup:

  Optional analyte metadata lookup table. Defaults to `analyte_tbl`.
  Used to standardize analyte names, assign analyte identifiers, and
  attach analyte metadata.

## Value

A tibble with class \`env_schema\`.

The returned object retains all original source columns and stores
environmental column mappings in object metadata.

Additional manager-generated columns may include:

- .analysis_result:

  Standardized analytical value used for calculations.

- .censored:

  Logical censoring indicator.

- .display_threshold:

  Threshold used when constructing display-ready censored values.
  Reporting limits are preferred, followed by quantitation limits,
  detection limits, and finally the analytical result itself.

- .display_threshold_type:

  Source of the display threshold. One of \`"reporting_limit"\`,
  \`"quantitation_limit"\`, \`"detection_limit"\`, or
  \`"analysis_result"\`.

- .display_result:

  Display-ready reporting result suitable for tables and exports.

## Details

Source column names are preserved and their environmental meaning is
stored in schema metadata, allowing downstream plotting, reporting,
trend analysis, geochemical calculations, censored-data methods, and
radiochemistry workflows to operate on a consistent environmental data
model.

\`env_schema()\` does not rename or modify source data columns.

Instead, the environmental meaning of each source column is stored as
schema metadata through an internal column-mapping structure.

For example:

    env_schema(
      data,
      location = location_id,
      date = sample_date,
      analyte = parameter,
      result = concentration
    )

records that \`location_id\` contains location identifiers,
\`sample_date\` contains sample dates, \`parameter\` contains analyte
names, and \`concentration\` contains analytical results.

The constructor may also create standardized derived columns used
internally by manager for result classification, analyte metadata,
reporting workflows, plotting, and statistical calculations.

Stored schema mappings can be inspected using \`schema_mapping()\`.

## Examples

``` r
if (FALSE) { # \dontrun{
gw <- env_schema(
  my_data,
  location = location_id,
  date = sample_date,
  analyte = param_name,
  result = analysis_result,
  unit = default_unit
)

schema_mapping(gw)
} # }
```
