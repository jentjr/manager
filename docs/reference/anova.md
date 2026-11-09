# Analysis of Variance

Analysis of Variance

## Usage

``` r
anova(df, keep_data_object = FALSE)
```

## Arguments

- df:

  data frame

- keep_data_object:

  logical to keep objects from aov and TukeyHSD

## Examples

``` r
data(gw_data)

wells <- c("MW-1", "MW-2", "MW-3", "MW-4")

params <- c("Calcium, dissolved", "Sodium, dissolved", "Sulfate, total")

gw_data %>%
  filter(location_id %in% wells, param_name %in% params) %>%
  anova()
#> Error in gw_data %>% filter(location_id %in% wells, param_name %in% params) %>%     anova(): could not find function "%>%"

anova_object <- gw_data %>%
  filter(location_id %in% wells, param_name %in% params) %>%
  anova(., keep_data_object = TRUE)
#> Error in gw_data %>% filter(location_id %in% wells, param_name %in% params) %>%     anova(., keep_data_object = TRUE): could not find function "%>%"

plot(anova_object$anova[[1]], 1)
#> Error: object 'anova_object' not found
```
