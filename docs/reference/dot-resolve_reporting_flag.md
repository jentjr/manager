# Resolve laboratory reporting qualifiers

Combines qualifier information from laboratory flags and less-than
indicators into a single reporting flag suitable for classification with
\`.classify_results()\`.

## Usage

``` r
.resolve_reporting_flag(flags = NULL, lt_measure = NULL)
```

## Arguments

- flags:

  Character vector of laboratory qualifiers.

- lt_measure:

  Character vector of less-than indicators.

## Value

Character vector.

## Details

Priority: 1. FLAGS 2. LT_MEASURE 3. NA
