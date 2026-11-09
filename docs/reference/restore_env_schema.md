# Restore env_schema attributes

Reattaches all env_schema metadata from a template object to a modified
data frame.

## Usage

``` r
restore_env_schema(x, to, validate = TRUE)
```

## Arguments

- x:

  Data frame to restore.

- to:

  Template env_schema object.

- validate:

  Logical. Should the restored object be validated before returning?

## Value

An object of class \`env_schema\`.
