# Convert standardized analytes to milliequivalents per liter

Converts standardized analytical concentrations to milliequivalents per
liter (meq/L) using analyte-specific conversion factors stored in
\[analyte_tbl\].

## Usage

``` r
convert_analytes_to_meq(
  df,
  analyte_col = ".analyte_id",
  value_col = ".analysis_result",
  unit_col = ".analysis_unit",
  value_out_col = ".analysis_result_meq",
  unit_out_col = ".analysis_unit_meq"
)
```

## Arguments

- df:

  Data frame containing analytical results.

- analyte_col:

  Character string identifying the canonical analyte column. Default is
  \`"analyte"\`.

- value_col:

  Character string identifying the analytical concentration column.
  Default is \`".analysis_result"\`.

- unit_col:

  Character string identifying the reporting unit column. Default is
  \`".analysis_unit"\`.

- value_out_col:

  Character string identifying the output meq concentration column.
  Default is \`"analysis_result_meq"\`.

- unit_out_col:

  Character string identifying the output meq reporting unit column.
  Default is \`"default_unit_meq"\`.

## Value

A data frame containing all original columns plus:

- analysis_result_meq:

  Concentration expressed as meq/L.

- default_unit_meq:

  Reporting unit, always \`"meq/L"\` for supported analytes.

## Details

For env_schema objects, analytical concentrations are standardized
during schema construction. This function converts those standardized
concentrations to meq/L.

The function validates that analytes eligible for milliequivalent
conversion are reported in their expected standard unit.

Conversion factors are obtained from
\[\`.get_meq_conversion_factors()\`\].

The function validates that all convertible analytes are reported in
their expected standard unit before conversion.

Only analytes with a defined \`meq_factor\` in \[analyte_tbl\] are
converted.

Unsupported analytes receive \`NA_real\_\` in the output concentration
column.

## Workflow

The following workflows are equivalent:

    df |>
      convert_analytes_to_meq()

## Validation

The function validates:

- required columns exist;

- analytes eligible for meq conversion are reported in the expected
  standard unit;

- analyte metadata are internally consistent.

## Notes

Milliequivalent concentrations are commonly used in hydrogeochemical
calculations and visualizations, including:

- Piper diagrams;

- Durov diagrams;

- Stiff diagrams;

- Schoeller diagrams;

- charge-balance calculations; and

- sodium adsorption ratio (SAR) calculations.

## See also

\[piper_plot()\], \[durov_plot()\], \[stiff_plot()\],
\[schoeller_plot()\], \[charge_balance()\]

## Examples

``` r
gw_data |>
  convert_analytes_to_meq()
#> Error in .validate_required_cols(df, c(analyte_col, value_col, unit_col)): Missing required column(s): .analyte_id, .analysis_result, .analysis_unit

gw_data |>
  convert_analytes_to_meq(
    analyte_col = "analyte",
    value_col = "result_std",
    unit_col = "unit_std"
  )
#> Error in .validate_required_cols(df, c(analyte_col, value_col, unit_col)): Missing required column(s): analyte, result_std, unit_std
```
