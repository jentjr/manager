# Guidance Profiles

Returns predefined statistical decision thresholds used by
\[statistical_decision()\].

## Usage

``` r
guidance_profile(profile = c("epa_unified", "conservative", "site_specific"))
```

## Arguments

- profile:

  Character value specifying the profile. One of:

  - \`"epa_unified"\`

  - \`"conservative"\`

  - \`"site_specific"\`

## Value

A named list containing:

- low_threshold:

  Boundary between low and moderate censoring.

- moderate_threshold:

  Boundary between moderate and high censoring.

- high_threshold:

  Boundary between high and extreme censoring.

## Details

Guidance profiles control the censoring levels used to distinguish
between low, moderate, high, and extreme censoring conditions.

The built-in profiles are:

- epa_unified:

  15

- conservative:

  10

- site_specific:

  20

These profiles influence recommended distribution fitting, trend
interpretation, and censored-data handling methods.

## See also

\[guidance_options()\], \[statistical_decision()\]

## Examples

``` r
guidance_profile()
#> $low_threshold
#> [1] 15
#> 
#> $moderate_threshold
#> [1] 50
#> 
#> $high_threshold
#> [1] 90
#> 

guidance_profile("conservative")
#> $low_threshold
#> [1] 10
#> 
#> $moderate_threshold
#> [1] 25
#> 
#> $high_threshold
#> [1] 75
#> 
```
