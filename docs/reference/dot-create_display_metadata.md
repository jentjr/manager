# Build display-ready analytical results and metadata

Build display-ready analytical results and metadata

## Usage

``` r
.create_display_metadata(
  df,
  env_mapping,
  display_options = default_display_options()
)
```

## Arguments

- df:

  Data frame.

- env_mapping:

  Schema mapping list.

- display_options:

  description

## Value

A tibble containing:

- .display_threshold:

  Display threshold value.

- .display_threshold_type:

  Source of the threshold.

- .display_result:

  Display-ready result string.
