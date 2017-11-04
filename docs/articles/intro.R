## ---- echo=FALSE, message=FALSE------------------------------------------
library(knitr)
library(manager)

## ---- eval = FALSE-------------------------------------------------------
#  data <- read_manages3("C:/path/to/manages/Site.mdb")

## ---- eval=FALSE---------------------------------------------------------
#  data("ohio_data")
#  data("indiana_data")

## ----echo=TRUE-----------------------------------------------------------
data("gw_data")

wells <- c("MW-1", "MW-2", "MW-3", "MW-4")

params <- c("Sulfate, total",
            "Arsenic, dissolved",
            "Boron, dissolved")

background <- lubridate::ymd(c("2007-12-20", "2012-01-01"))

# first group data by location, param, and background
# estimate percent less than
gw_data <- gw_data %>%
  filter(location_id %in% wells, param_name %in% params,
         sample_date >= background[1] & sample_date <= background[2]) %>%
  group_by(location_id, param_name, default_unit) %>%
  percent_lt() %>%
  est_dist(.) %>%
  arrange(location_id, param_name)

nested_gw <- gw_data %>%
  group_by(location_id, param_name, default_unit) %>%
  nest()

pred_int <- nested_gw %>%
  mutate(pred_int = map(.x = data, ~pred_int(.)))

pred_int_table <- pred_int %>%
  mutate(distribution = map(.x = pred_int, ~ .x$distribution),
         sample_size = map(.x = pred_int, ~ .x$sample.size),
         lpl = map(.x = pred_int, ~ .x$interval$limits["LPL"]),
         upl = map(.x = pred_int, ~ .x$interval$limits["UPL"]),
         conf_level = map(.x = pred_int, ~ .x$interval$conf.level)) %>%
  select(-data, -pred_int) %>%
  unnest()

kable(pred_int_table)

## ----conf_int------------------------------------------------------------
conf_int <- nested_gw %>%
  mutate(conf_int = map(.x = data, ~conf_int(.)))

conf_int_table <- conf_int %>%
  mutate(distribution = map(.x = conf_int, ~ .x$distribution),
         sample_size = map(.x = conf_int, ~ .x$sample.size),
         lpl = map(.x = conf_int, ~ .x$interval$limits["LCL"]),
         upl = map(.x = conf_int, ~ .x$interval$limits["UCL"]),
         conf_level = map(.x = conf_int, ~ .x$interval$conf.level)) %>%
  select(-data, -conf_int) %>%
  unnest()

kable(conf_int_table)

