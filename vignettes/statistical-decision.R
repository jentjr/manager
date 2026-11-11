## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(manager)
library(dplyr)

## -----------------------------------------------------------------------------
guidance_profile()

## -----------------------------------------------------------------------------
guidance_profile("epa_unified")

## -----------------------------------------------------------------------------
guidance_profile("conservative")

guidance_profile("site_specific")

## -----------------------------------------------------------------------------
decision <- statistical_decision(
  percent_censored = 12,
  n_used = 20,
  n_detect = 18
)

decision

## -----------------------------------------------------------------------------
names(decision)

## -----------------------------------------------------------------------------
statistical_decision(
  percent_censored = c(5, 25, 75, 95),
  n_used = 20,
  n_detect = c(19, 15, 5, 1)
) |>
  select(
    percent_censored,
    censoring_class
  )

## -----------------------------------------------------------------------------
statistical_decision(
  percent_censored = c(5, 25, 75, 95),
  n_used = 20,
  n_detect = c(19, 15, 5, 1)
) |>
  select(
    percent_censored,
    distribution_method
  )

## -----------------------------------------------------------------------------
statistical_decision(
  percent_censored = 5,
  n_used = 15,
  n_detect = 14
) |>
  select(
    distribution_method,
    distribution_config
  )

## -----------------------------------------------------------------------------
statistical_decision(
  percent_censored = 35,
  n_used = 20,
  n_detect = 13
) |>
  select(
    distribution_method
  )

## -----------------------------------------------------------------------------
statistical_decision(
  percent_censored = 75,
  n_used = 20,
  n_detect = 5
) |>
  select(
    distribution_method
  )

## -----------------------------------------------------------------------------
statistical_decision(
  percent_censored = 95,
  n_used = 20,
  n_detect = 1
) |>
  select(
    distribution_method
  )

## -----------------------------------------------------------------------------
decision <- statistical_decision(
  percent_censored = 25,
  n_used = 20,
  n_detect = 15
)

decision |>
  select(
    trend_method,
    trend_guidance,
    censored_method
  )

## -----------------------------------------------------------------------------
statistical_decision(
  percent_censored = 90,
  n_used = 20,
  n_detect = 2
) |>
  select(
    trend_guidance
  )

## -----------------------------------------------------------------------------
statistical_decision(
  percent_censored = c(5, 40, 75),
  n_used = 20,
  n_detect = c(19, 12, 5)
) |>
  select(
    percent_censored,
    censored_method
  )

## ----eval = FALSE-------------------------------------------------------------
# decision <- derive_decision(gw)
# 
# decision

## ----eval = FALSE-------------------------------------------------------------
# gw |>
#   filter(
#     param_name == "Arsenic, dissolved"
#   ) |>
#   derive_decision()

## ----eval = FALSE-------------------------------------------------------------
# gw |>
#   group_by(
#     location_id,
#     param_name,
#     default_unit
#   ) |>
#   estimate_distribution()

## ----eval = FALSE-------------------------------------------------------------
# gw |>
#   group_by(
#     location_id,
#     param_name,
#     default_unit
#   ) |>
#   estimate_distribution()

## -----------------------------------------------------------------------------
decision <- statistical_decision(
  percent_censored = 10,
  n_used = 20,
  n_detect = 18
)

decision$distribution_config[[1]]

## -----------------------------------------------------------------------------
decision$interval_config[[1]]

## ----eval = FALSE-------------------------------------------------------------
# dist_est <- gw |>
#   group_by(
#     location_id,
#     param_name,
#     default_unit
#   ) |>
#   estimate_distribution()

## ----eval = FALSE-------------------------------------------------------------
# pred_int(dist_est)

## ----eval = FALSE-------------------------------------------------------------
# pred_int(
#   dist_est,
#   pi_type = "upper",
#   conf_level = 0.99
# )

## ----eval = FALSE-------------------------------------------------------------
# pred_int_sim(
#   dist_est,
#   pi_type = "upper",
#   conf_level = 0.99
# )

## ----eval = FALSE-------------------------------------------------------------
# gw |>
#   group_by(
#     location_id,
#     param_name
#   ) |>
#   stats_summary()

## -----------------------------------------------------------------------------
options(
  manager.guidance = list(
    low_threshold = 10,
    moderate_threshold = 25,
    high_threshold = 75
  )
)

## -----------------------------------------------------------------------------
guidance_options()

## ----eval = FALSE-------------------------------------------------------------
# gw |>
#   group_by(
#     location_id,
#     param_name,
#     default_unit
#   ) |>
#   estimate_distribution() |>
#   pred_int(
#     pi_type = "upper",
#     conf_level = 0.95
#   )

## ----eval = FALSE-------------------------------------------------------------
# gw |>
#   group_by(
#     location_id,
#     param_name
#   ) |>
#   stats_summary()

