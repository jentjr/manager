# Validate supported analytes

Resolves analyte names using the configured analyte registry and
validates that analytes can be matched to known analyte metadata.

## Usage

``` r
.validate_supported_analytes(
  analytes,
  alias_lookup = analyte_alias_lookup,
  lookup = analyte_tbl
)
```

## Arguments

- analytes:

  Character vector of analyte names.

- alias_lookup:

  Alias lookup table, typically \[analyte_alias_lookup\].

- lookup:

  An analyte metadata table, typically \[analyte_tbl\].

## Value

Invisibly returns the resolved analyte metadata produced by
\[\`.resolve_analyte\`()\].

## Details

This function performs validation of both the analyte metadata table and
alias lookup table before attempting analyte resolution.

Unsupported analytes are reported using
\[\`.warn_unsupported_analytes\`()\].

The returned object includes:

- resolved analyte identifiers;

- canonical analyte names;

- normalized analyte names;

- fraction assignments;

- analyte metadata;

- analyte match status.

Unknown analytes are not discarded. Instead, they are flagged and
reported through a warning.

## See also

\[\`.resolve_analyte\`()\], \[\`.warn_unsupported_analytes\`()\]
