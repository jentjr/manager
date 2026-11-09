# Resolve analyte metadata

Resolves analyte aliases and attaches analyte metadata.

## Usage

``` r
.resolve_analyte(
  x,
  alias_lookup = analyte_alias_lookup,
  analyte_lookup = analyte_tbl
)
```

## Arguments

- x:

  Character vector of analyte names.

- alias_lookup:

  Alias lookup table.

- analyte_lookup:

  Analyte metadata table.

## Value

Tibble containing resolved analyte metadata.
