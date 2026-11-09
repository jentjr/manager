# Validate alias lookup table

Validates the structure and integrity of an analyte alias lookup table.

## Usage

``` r
.validate_alias_lookup(
  alias_lookup = analyte_alias_lookup,
  lookup = analyte_tbl
)
```

## Arguments

- alias_lookup:

  Alias lookup table, typically \[analyte_alias_lookup\].

- lookup:

  An analyte metadata table, typically \[analyte_tbl\].

## Value

Invisibly returns the validated alias lookup table.

## Details

The alias lookup table is used during analyte resolution to map
normalized analyte aliases to canonical analyte identifiers and
metadata.

Required columns are:

- \`alias\`

- \`analyte_id\`

- \`analyte\`

- \`normalized_analyte\`

- \`fraction\`

Validation includes:

- required column checks;

- referential integrity against \`analyte_tbl\`;

- duplicate alias detection;

- character type validation;

- missing value checks;

- fraction validation;

- alias normalization checks.

Valid fraction values are:

- \`"dissolved"\`

- \`"total"\`

- \`"filtered"\`

- \`"unfiltered"\`

All aliases are expected to have already been normalized using
\[\`.normalize_analyte_text\`()\].

## See also

\[analyte_tbl\], \[analyte_alias_lookup\],
\[\`.normalize_analyte_text\`()\]
