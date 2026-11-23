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
decisions <- statistical_decision(
  percent_censored = c(5, 25, 75, 95),
  n_used = c(20, 20, 20, 20),
  n_detect = c(19, 15, 5, 1)
  )

decisions |>
  select(
    percent_censored,
    n_used,
    n_detect,
    censoring_class,
    distribution_method,
    trend_method,
    trend_guidance,
    censored_method,
    interval_method,
    comparison_method
    )

## -----------------------------------------------------------------------------
decisions <- statistical_decision(
  percent_censored = c(5, 25, 75, 95),
  n_used = c(20, 20, 20, 20),
  n_detect = c(19, 15, 5, 1)
  )

decisions |>
  select(
    percent_censored,
    n_used,
    n_detect,
    censoring_class,
    distribution_method,
    trend_method,
    trend_guidance,
    censored_method,
    interval_method,
    comparison_method
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
    n_used,
    n_detect,
    trend_method,
    trend_guidance, 
    comparison_method
    )

## -----------------------------------------------------------------------------
statistical_decision(
  percent_censored = c(5, 40, 75, 95),
  n_used = c(20, 20, 20, 20),
  n_detect = c(19, 12, 5, 1)
) |>
  select(
    percent_censored,
    censoring_class,
    censored_method
  )

## -----------------------------------------------------------------------------
trend_summary(
  gw,
  censored_method = "auto"
)

## -----------------------------------------------------------------------------
gw_example <- data.frame(
  analysis_result = c(
    1.2, 0.8, 0.5, 0.3, 0.2,
    0.1, 0.4, 0.7, 1.1, 0.9
  ),
  censored = c(
    FALSE, FALSE, FALSE, TRUE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  )
)

decision <- derive_decision(gw_example)

decision |>
  select(
    n_used,
    n_detect,
    percent_censored,
    censoring_class,
    distribution_method,
    trend_method,
    trend_guidance
  )

## ----eval = FALSE-------------------------------------------------------------
# decision <- derive_decision(gw)
# 
# decision

## ----eval = FALSE-------------------------------------------------------------
# custom_data <- data.frame(
#   concentration = c(1.2, 0.8, 0.5, 0.3),
#   is_censored = c(FALSE, FALSE, TRUE, FALSE)
#   )
# 
# derive_decision(
#   custom_data,
#   result_col = "concentration",
#   censored_col = "is_censored"
#   )

## -----------------------------------------------------------------------------
grouped_data <- data.frame(
  location = c("A", "A", "A", "B", "B", "B"),
  analysis_result = c(1.2, 0.8, 0.3, 0.2, 0.1, 0.4),
  censored = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
  ) |>
  group_by(location)

grouped_decisions <- derive_decision(grouped_data)

grouped_decisions |>
  select(
    location,
    n_used,
    n_detect,
    percent_censored,
    censoring_class,
    distribution_method
    )

## ----eval = FALSE-------------------------------------------------------------
# gw |>
#   filter(
#     location_id == "MW-2",
#     param_name %in% c(
#       "Chloride, total",
#       "Sulfate, total",
#       "Selenium, dissolved",
#       "Copper, dissolved"
#       )
#     ) |>
#   group_by(
#     location_id,
#     param_name,
#     default_unit
#     ) |>
#   estimate_distribution() |>
#   select(
#     location_id,
#     param_name,
#     n_detect,
#     percent_censored,
#     decision_basis,
#     distribution
#   )

## ----eval = FALSE-------------------------------------------------------------
# gw |>
#   filter(
#     location_id == "MW-1",
#     param_name %in% c(
#       "Chloride, total",
#       "Sulfate, total",
#       "Tin, dissolved"
#       )
#     ) |>
#   group_by(
#     location_id,
#     param_name,
#     default_unit
#     ) |>
#   estimate_distribution() |>
#   select(
#     location_id,
#     param_name,
#     n_detect,
#     percent_censored,
#     decision_basis,
#     distribution
#   )

## -----------------------------------------------------------------------------
decision <- statistical_decision(
  percent_censored = 10,
  n_used = 20,
  n_detect = 18
)

decision$distribution_config[[1]]

## -----------------------------------------------------------------------------
decision <- statistical_decision(
  percent_censored = 10,
  n_used = 20,
  n_detect = 18
  )

decision |>
  select(
    interval_method,
    interval_config
    )

decision$interval_config

## ----eval = FALSE-------------------------------------------------------------
# dist_est <- gw |>
#   filter(
#     location_id == "MW-2",
#     param_name %in% c(
#       "Chloride, total",
#       "Sulfate, total",
#       "Copper, dissolved"
#       )
#     ) |>
#   group_by(
#     location_id,
#     param_name,
#     default_unit
#     ) |>
#   estimate_distribution()

## ----eval = FALSE-------------------------------------------------------------
# pred_int(dist_est)

## ----eval = FALSE-------------------------------------------------------------
# pred_int(
#   dist_est,
#   pi_type = "lower",
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
statistical_decision(
  percent_censored = 25,
  n_used = 20,
  n_detect = 15,
  profile = guidance_profile("conservative")
)

## -----------------------------------------------------------------------------
guidance_options()

## -----------------------------------------------------------------------------
distribution_plan <- analysis_plan(
objective = "distribution",
response = "analysis_result",
censored = "censored"
)

distribution_plan

## ----eval = FALSE-------------------------------------------------------------
# gw |>
#   filter(
#     location_id == "MW-2",
#     param_name %in% c(
#       "Chloride, total",
#       "Sulfate, total",
#       "Copper, dissolved"
#       )
#     ) |>
#   group_by(
#     location_id,
#     param_name,
#     default_unit
#     ) |>
#   estimate_distribution() |>
#   pred_int(
#     pi_type = "upper",
#     conf_level = 0.95
#   )

## ----eval = FALSE-------------------------------------------------------------
# gw |>
#   filter(
#     location_id == "MW-2",
#     param_name %in% c(
#       "Chloride, total",
#       "Sulfate, total",
#       "Copper, dissolved"
#       )
#     ) |>
#   group_by(
#     location_id,
#     param_name,
#     default_unit
#     ) |>
#   stats_summary()

