# Warn on unsupported analytes

Identifies analytes that could not be matched to the analyte registry.

## Usage

``` r
.warn_unsupported_analytes(x)
```

## Arguments

- x:

  Output from \[\`.resolve_analyte\`()\] or
  \[\`.resolve_analyte_column\`()\] containing \`source_analyte\` and
  \`is_known_analyte\` columns.

## Value

Invisibly returns the supplied object.

## Details

Unsupported analytes are determined using the \`is_known_analyte\` flag
generated during analyte resolution.

When unsupported analytes are present, a warning is issued describing
the affected analytes and any analyte-dependent functionality that will
be skipped, including metadata enrichment and standard-unit conversion.

This function is intended for quality assurance during data import and
schema construction workflows.

## See also

\[\`.resolve_analyte\`()\], \[\`.validate_supported_analytes\`()\]
