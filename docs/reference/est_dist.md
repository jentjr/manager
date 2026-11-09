# Estimate statistical distribution

Determines whether grouped data are best represented by a normal,
lognormal, or other candidate distribution.

## Usage

``` r
est_dist(
  df,
  return_result = FALSE,
  alpha = 0.05,
  method = "sw",
  choices = c("norm", "lnorm"),
  min_n = 4
)
```

## Arguments

- df:

  Tidy groundwater data frame. Grouping determines the level at which
  distributions are estimated.

- return_result:

  TRUE/FALSE.

- alpha:

  Significance level.

- method:

  Goodness-of-fit method passed to EnvStats::distChoose().

- choices:

  Candidate distributions.

- min_n:

  Minimum sample size required for estimation.

## Value

A tibble with one row per group containing:

- distribution:

  Selected distribution.

- n:

  Number of non-missing observations used in estimation.

- data:

  Nested data frame containing the observations used for the group.

- result:

  EnvStats result object stored as a list-column when return_result =
  TRUE.

## Details

The returned object retains the original grouped observations in a
nested \`data\` list-column. This allows downstream functions such as
prediction interval, tolerance interval, and control chart calculations
to access the observations used for distribution estimation.

## Examples

``` r
data("gw_data")

gw_data %>%
  dplyr::group_by(
    location_id,
    param_name,
    default_unit
  ) %>%
  percent_lt() %>%
  est_dist()
#> Error in gw_data %>% dplyr::group_by(location_id, param_name, default_unit) %>%     percent_lt() %>% est_dist(): could not find function "%>%"
```
