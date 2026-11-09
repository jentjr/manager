# Validate analytes and units

Validates analyte and unit fields within an analytical dataset.

## Usage

``` r
.validate_analyte_units(
  df,
  param_name = "param_name",
  default_unit = "default_unit"
)
```

## Arguments

- df:

  A data frame containing analytical results.

- param_name:

  Character string identifying the analyte column.

- default_unit:

  Character string identifying the unit column.

## Value

A data frame containing resolved analyte metadata and normalized unit
values.

## Details

The function verifies required columns, resolves analytes using the
package analyte registry, warns on unsupported analytes, and
standardizes unit values.

Validation steps include:

1.  verification of required columns;

2.  validation of the analyte registry;

3.  validation of the alias lookup table;

4.  analyte resolution;

5.  unsupported analyte checks;

6.  unit normalization.

This helper is primarily used during environmental chemistry data import
and schema creation workflows.

## See also

\[\`.resolve_analyte_column\`()\], \[\`.normalize_unit\`()\]
