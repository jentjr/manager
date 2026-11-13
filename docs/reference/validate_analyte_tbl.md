# Validate analyte metadata table

Validates the structure and integrity of an analyte metadata table.

## Usage

``` r
validate_analyte_tbl(x)
```

## Arguments

- x:

  An analyte metadata table, typically \[analyte_tbl\].

## Value

Invisibly returns \`TRUE\` if validation succeeds.

## Details

An analyte metadata table provides the canonical analyte registry used
throughout the package for analyte resolution, metadata assignment,
standards matching, plotting, unit conversion, and environmental data
classification workflows.

The table must contain the following required columns:

- \`analyte_id\`

- \`analyte\`

- \`normalized_analyte\`

- \`short_name\`

- \`std_unit\`

- \`analyte_group\`

Validation includes:

- presence of required columns;

- non-missing analyte identifiers;

- uniqueness of canonical analyte names;

## See also

\[analyte_tbl\], \[\`.validate_alias_lookup\`()\]
