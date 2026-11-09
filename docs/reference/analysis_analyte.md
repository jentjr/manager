# Return resolved analyte values

Returns analyte identifiers according to the requested analyte matching
strategy.

## Usage

``` r
analysis_analyte(x, analyte_matching = c("normalized", "strict", "exact"))
```

## Arguments

- x:

  Object.

- analyte_matching:

  Matching strategy.

## Details

- exact:

  Returns resolved analyte names and preserves dissolved/total
  distinctions.

- strict:

  Returns analyte keys incorporating fraction information.

- normalized:

  Returns canonical analyte identifiers suitable for grouping related
  analytes.
