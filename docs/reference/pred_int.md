# Function to calculate prediction interval

Function to calculate prediction interval

## Usage

``` r
pred_int(
  df,
  method = "Bonferroni",
  n_mean = 1,
  k = 1,
  pi_type = "upper",
  conf_level = 0.95
)
```

## Arguments

- df:

  df data frame of groundwater data in tidy format

- method:

  default is "Bonferroni"

- n_mean:

  n.mean positive integer specifying the sample size associated with the
  future averages. The default value is n.mean=1 (i.e., individual
  observations). Note that all future averages must be based on the same
  sample size.

- k:

  k positive integer specifying the number of future observations or
  averages the prediction interval should contain with confidence level
  conf.level. The default value is k=1.

- pi_type:

  character string indicating what kind of prediction interval to
  compute. The possible values are pi_type="two-sided" (the default),
  pi_type="upper", and pi_type="lower".

- conf_level:

  a scalar between 0 and 1 indicating the confidence level of the
  prediction interval. The default value is conf.level=0.95

- analysis_result:

  the analysis result column

- distribution:

  the distribution column

## Examples

``` r
data("gw_data")

wells <- c("MW-1", "MW-2", "MW-3", "MW-4")

params <- c("Sulfate, total",
            "Arsenic, dissolved",
            "Boron, dissolved")

background <- lubridate::ymd(c("2007-12-20", "2012-01-01"), tz = "UTC")

# first group data by location, param, and background
# estimate percent less than and distribution
background_data <- gw_data %>%
 filter(location_id %in% wells, param_name %in% params,
         sample_date >= background[1] & sample_date <= background[2]) %>%
  group_by(location_id, param_name, default_unit) %>%
  percent_lt() %>%
  est_dist(., keep_data_object = TRUE) %>%
  arrange(location_id, param_name)
#> Error in gw_data %>% filter(location_id %in% wells, param_name %in% params,     sample_date >= background[1] & sample_date <= background[2]) %>%     group_by(location_id, param_name, default_unit) %>% percent_lt() %>%     est_dist(., keep_data_object = TRUE) %>% arrange(location_id,     param_name): could not find function "%>%"

background_data %>%
pred_int(., pi_type = "upper", conf_level = 0.99)
#> Error in background_data %>% pred_int(., pi_type = "upper", conf_level = 0.99): could not find function "%>%"

```
