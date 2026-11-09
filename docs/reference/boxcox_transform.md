# Boxcox transformation

Boxcox transformation

## Usage

``` r
boxcox_transform(df, combine_locations = FALSE, objective_name = "PPCC")
```

## Arguments

- df:

  data frame

- combine_locations:

  logical

- objective_name:

  character string indicating what objective to use. The possible values
  are "PPCC" (probability plot correlation coefficient; the default),
  "Shapiro-Wilk" (the Shapiro-Wilk goodness-of-fit statistic), and
  "Log-Likelihood" (the log-likelihood function).
