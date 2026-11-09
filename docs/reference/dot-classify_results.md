# Classify reporting qualifiers

Classifies analytical results based on reporting qualifiers and assigns
result type, detection status, censoring status, estimation flags, and
usability flags.

## Usage

``` r
.classify_results(flag, lookup = .qualifier_tbl)
```

## Arguments

- flag:

  Reporting flag vector.

- lookup:

  Qualifier lookup table.

## Value

A tibble containing:

- `result_type`

- `detected`

- `censored`

- `estimated`

- `rejected`
