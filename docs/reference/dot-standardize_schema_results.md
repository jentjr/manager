# Standardize env_schema analytical results

Converts .analysis_result values to analyte-specific standard reporting
units and updates .analysis_unit.

## Usage

``` r
.standardize_schema_results(df)
```

## Arguments

- df:

  Data frame containing schema analyte metadata.

## Value

Data frame.

## Details

Original laboratory values are preserved in .original_lab_result and
.original_lab_unit.
