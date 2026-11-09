# Add resolved analyte metadata

Resolves analyte names and attaches analyte metadata to a data frame.

## Usage

``` r
.resolve_analyte_metadata(
  df,
  analyte_col = "param_name",
  alias_lookup = analyte_alias_lookup,
  analyte_lookup = analyte_tbl
)
```

## Arguments

- df:

  A data frame containing analytical results.

- analyte_col:

  Column containing analyte names.

## Value

A data frame enriched with analyte metadata.

## Details

This function serves as the primary analyte enrichment step used during
schema construction. Raw analyte names are standardized, matched to the
analyte registry, assigned metadata, and flagged when no registry match
is available.

The following schema columns are added:

- \`.source_analyte\`

- \`.analyte\`

- \`.analyte_id\`

- \`.normalized_analyte\`

- \`.fraction\`

- \`.is_known_analyte\`

Additional analyte metadata is attached from \[analyte_tbl\], including
analyte classifications, standard units, ion information, molecular
properties, and reporting metadata.

Unknown analytes are retained in the output and reported through
\[\`.warn_unsupported_analytes\`()\].

The resulting data frame is suitable for downstream workflows including:

- standards assignment;

- unit conversion;

- summary statistics;

- trend analysis;

- hydrochemical calculations;

- plotting and reporting.

## See also

\[\`.resolve_analyte_column\`()\], \[\`.resolve_analyte\`()\],
\[analyte_tbl\]
