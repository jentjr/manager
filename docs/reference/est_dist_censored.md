# Function to return either normal, lognormal, gamma, or non-parametric distribution estimate

Function to return either normal, lognormal, gamma, or non-parametric
distribution estimate

## Usage

``` r
est_dist_censored(
  df,
  return_result = FALSE,
  alpha = 0.05,
  method = "sf",
  choices = c("norm", "lnorm"),
  min_n = 4
)
```

## Arguments

- df:

  groundwater data frame in tidy format

- alpha:

  alpha

- method:

  default is "sf"

- choices:

  vector of distributions to check. Default is c("norm, "lnorm")

- left_censored:

  logical vector.

- combine_locations:

  TRUE/FALSE to estimate distribution by individual location, or grouped
  together. Default is FALSE.

- keep_data_object:

  Default is FALSE

## Examples

``` r
data("gw_data")
wells <- c("MW-1", "MW-2", "MW-3")
params <- c("Fluoride, total",
            "Arsenic, dissolved",
            "Nickel, dissolved")

gw_data <- gw_data %>%
to_censored() %>%
filter(location_id %in% wells, param_name %in% params) %>%
percent_lt() %>%
filter(percent_lt >=15, percent_lt <= 50)
#> Error in gw_data %>% to_censored() %>% filter(location_id %in% wells,     param_name %in% params) %>% percent_lt() %>% filter(percent_lt >=     15, percent_lt <= 50): could not find function "%>%"

gw_data %>%
group_by(location_id, param_name, default_unit) %>%
est_dist_censored()
#> Error in gw_data %>% group_by(location_id, param_name, default_unit) %>%     est_dist_censored(): could not find function "%>%"


gw_data %>%
group_by(param_name, default_unit) %>%
est_dist_censored()
#> Error in gw_data %>% group_by(param_name, default_unit) %>% est_dist_censored(): could not find function "%>%"
```
