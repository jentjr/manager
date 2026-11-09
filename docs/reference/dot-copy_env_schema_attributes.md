# Copy env_schema attributes

Copies schema metadata and custom attributes from one object to another
while preserving the target object's row names and column names.

## Usage

``` r
.copy_env_schema_attributes(from, to)
```

## Arguments

- from:

  Source object containing attributes to copy.

- to:

  Target object receiving attributes.

## Value

The `to` object with attributes inherited from `from`.

## Details

This helper is primarily used by schema transformation functions that
create new data frames internally but should retain the original
`env_schema` attributes, including schema mappings, version information,
and other metadata stored as attributes.

Standard data frame structural attributes such as `names` and
`row.names` are intentionally preserved from the target object.
