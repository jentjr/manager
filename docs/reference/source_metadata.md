# Return source metadata columns

Returns source-system-specific metadata columns attached to an
env_schema object.

## Usage

``` r
source_metadata(x)
```

## Arguments

- x:

  An env_schema object.

## Value

Character vector of source metadata column names.

## Details

These columns are not part of the core schema mapping and are typically
added by source-specific importers such as \`read_manages4()\`.
