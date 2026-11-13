# Build analyte alias lookup table

Expands analyte aliases into a normalized lookup table used for analyte
standardization and metadata assignment.

## Usage

``` r
.build_alias_lookup(
  analyte_tbl = analyte_tbl,
  analyte_alias_tbl = analyte_alias_tbl
)
```

## Arguments

- analyte_tbl:

  An analyte metadata table.

- analyte_alias_tbl:

  An analyte alias table.

## Value

A tibble with columns:

- alias:

  Normalized analyte alias.

- analyte_id:

  Canonical analyte identifier.

- analyte:

  Canonical analyte name.

## Details

Each analyte alias is associated with a canonical \`analyte_id\` and
analyte name. Aliases are normalized using \`.normalize_analyte_text()\`
and validated to ensure that a single alias does not map to more than
one analyte.

The resulting lookup table is used internally by functions that
standardize analyte names and attach analyte metadata to environmental
chemistry data.

Aliases are expanded from the \`aliases\` \[analyte_alias_tbl\]. Aliases
are normalized using \`.normalize_analyte_text()\`, and duplicate
aliases mapping to multiple analytes are not permitted.

Alias conflicts generate an error to ensure that analyte standardization
remains deterministic.

The lookup includes both primary analyte names and user-facing aliases
such as abbreviations, common synonyms, and fraction- specific forms
(for example, "calcium dissolved", "calcium total", and "ca").

## See also

\[analyte_tbl\], \[\`.normalize_analyte_text\`()\]
