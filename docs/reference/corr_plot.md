# Create a correlation plot

\`corr_plot()\` is an S3 generic that supports both raw data frames and
\`env_schema\` objects.

## Usage

``` r
corr_plot(df, ...)

# S3 method for class 'data.frame'
corr_plot(df, location, analyte, result, unit, sample_date, ...)

# S3 method for class 'env_schema'
corr_plot(
  df,
  constituents = NULL,
  sample_locations = NULL,
  viridis_option = "D",
  ...
)
```

## Arguments

- df:

  A data frame or \`env_schema\` object.

- ...:

  Additional arguments passed to methods.

- location:

  Location identifier column.

- analyte:

  Analyte column.

- result:

  Analytical result column.

- unit:

  Reporting unit column.

- sample_date:

  Sample date column.

- constituents:

  Character vector of analytes to include. Defaults to all analytes in
  the data.

- sample_locations:

  Character vector of locations to include. Defaults to all locations in
  the data.

- viridis_option:

  Viridis palette option. One of \`"A"\` through \`"H"\`.

## Value

A \`GGally::ggpairs\` correlation plot.

## Details

For data frames, \`corr_plot()\` converts the supplied data to an
\`env_schema\` object using the specified column mappings, then
dispatches to \[corr_plot.env_schema()\].
