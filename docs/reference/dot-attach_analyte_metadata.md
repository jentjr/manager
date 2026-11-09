# Attach analyte metadata

Joins analyte metadata from an analyte lookup table onto an `env_schema`
object using the internal analyte identifier.

## Usage

``` r
.attach_analyte_metadata(df, lookup = analyte_tbl)
```

## Arguments

- df:

  An `env_schema` object containing a `.analyte_id` column.

- lookup:

  An analyte metadata table, typically `analyte_tbl`.

## Value

An enriched `env_schema` object containing all original observations
plus analyte metadata columns from `lookup`.

## Details

This helper enriches schema data with analyte-level attributes such as
canonical names, short names, categories, groups, fractions, regulatory
classifications, preferred plotting labels, and any future metadata
stored in `analyte_tbl`.

The join is performed on `.analyte_id`, allowing analyte naming
conventions to evolve independently of downstream analyses.

## See also

\[analyte_tbl\], \[validate_analyte_tbl()\]
