## ---- echo=FALSE, message=FALSE------------------------------------------
library(knitr)
devtools::load_all("/usr/local/src/manager")

## ---- eval = FALSE-------------------------------------------------------
#  data <- read_manages3("C:/path/to/manages/Site.mdb")

## ---- eval=FALSE---------------------------------------------------------
#  data("gw_data")
#  data("ohio_data")
#  data("indiana_data")

## ----echo=TRUE-----------------------------------------------------------
data("gw_data")

wells <- c("MW-1", "MW-2", "MW-3", "MW-4")

params <- c("Sulfate, total",
            "Arsenic, dissolved",
            "Boron, dissolved")

background <- lubridate::ymd(c("2007-12-20", "2012-01-01"), tz = "UTC")

# first group data by location, param, and background
# estimate percent less than
background_data <- gw_data %>%
  filter(location_id %in% wells, param_name %in% params,
         sample_date >= background[1] & sample_date <= background[2]) %>%
  group_by(location_id, param_name, default_unit) %>%
  percent_lt() %>%
  est_dist(., keep_data_object = TRUE) %>%
  arrange(location_id, param_name)

pred_int <- background_data %>%
  mutate(pred_int = case_when(
    distribution == "Normal" ~ map(.x = data,
                                   ~EnvStats::predIntNorm(x = .x$analysis_result)),
    distribution == "Lognormal" ~ map(.x = data,
                                      ~EnvStats::predIntLnorm(x = .x$analysis_result)),
    distribution == "Nonparametric" ~ map(.x = data,
                                          ~EnvStats::predIntNpar(x = .x$analysis_result))
    )
  )

pred_int_table <- pred_int %>%
  mutate(distribution = distribution,
         sample_size = map(.x = pred_int, ~ .x$sample.size),
         lpl = map(.x = pred_int, ~ .x$interval$limits["LPL"]),
         upl = map(.x = pred_int, ~ .x$interval$limits["UPL"]),
         conf_level = map(.x = pred_int, ~ .x$interval$conf.level)) %>%
  select(-data, -pred_int) %>%
  unnest()

kable(pred_int_table)

## ----conf_int------------------------------------------------------------
conf_int <- background_data %>%
  mutate(conf_int = case_when(
    distribution == "Normal" ~ map(.x=data,
                                   ~EnvStats::enorm(x = .x$analysis_result,
                                          ci = TRUE, ci.type = "lower",
                                          conf.level = 0.99,
                                          ci.param = "mean")),
    distribution == "Lognormal" ~ map(.x = data,
                                      ~EnvStats::elnormAlt(x = .x$analysis_result,
                                                 ci = TRUE, ci.type = "lower",
                                                 ci.method = "land",
                                                 conf.level = 0.99)),
    distribution == "Nonparametric" ~ map(.x = data,
                                          ~EnvStats::eqnpar(x = .x$analysis_result,
                                                  ci = TRUE, ci.type = "lower",
                                                  ci.method = "interpolate",
                                                  approx.conf.level = 0.99))
    )
  )

conf_int_table <- conf_int %>%
  mutate(distribution = distribution,
         sample_size = map(.x = conf_int, ~ .x$sample.size),
         lcl = map(.x = conf_int, ~ round(.x$interval$limits["LCL"], 3)),
         ucl = map(.x = conf_int, ~ .x$interval$limits["UCL"]),
         conf_level = map(.x = conf_int, ~ .x$interval$conf.level)) %>%
  select(-data, -conf_int) %>%
  unnest()


kable(conf_int_table)

## ------------------------------------------------------------------------
manager_pred_int <- background_data %>% pred_int(.)
kable(manager_pred_int)

manager_conf_int <- background_data %>%
  conf_int(., ci_type = "lower", conf_level = 0.99)

kable(manager_conf_int)

