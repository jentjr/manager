# Import environmental data from Excel

Reads an Excel worksheet and converts it directly to an \`env_schema\`
object.

## Usage

``` r
from_excel(
  path,
  sheet = 1,
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
  analyte_lookup = analyte_tbl
)
```

## Arguments

- path:

  Path to workbook.

- sheet:

  Worksheet name.

- location:

  Location identifier column.

- date:

  Sample date column.

- analyte:

  Analyte column.

- result:

  Result column.

- unit:

  Unit column.

- sample_id:

  Sample identifier column.

- reporting_flag:

  Reporting qualifier column.

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

  Laboratory name column.

- metadata:

  Additional metadata columns.

- qualifier_table:

  Optional qualifier lookup.

- analyte_lookup:

  Optional analyte lookup.

## Value

env_schema
