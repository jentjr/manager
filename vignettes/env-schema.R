## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(manager)
library(dplyr)

## ----eval = FALSE-------------------------------------------------------------
# names(gw)

## ----eval = FALSE-------------------------------------------------------------
# gw <- as_env_schema(
#   gw_data,
#   location = location_id,
#   sample_date = sample_date,
#   analyte = param_name,
#   result = analysis_result,
#   unit = default_unit
#   )

## ----eval = FALSE-------------------------------------------------------------
# class(gw)

## ----eval = FALSE-------------------------------------------------------------
# location(gw)
# sample_date(gw)
# analyte(gw)
# analysis_result(gw)
# analysis_unit(gw)
# detected(gw)
# reporting_limit(gw)
# detection_limit(gw)
# method(gw)

## ----eval = FALSE-------------------------------------------------------------
# dplyr::n_distinct(location(gw))
# 
# mean(analysis_result(gw), na.rm = TRUE)

## ----eval = FALSE-------------------------------------------------------------
# dplyr::n_distinct(gw$location_id)
# 
# mean(gw$.analysis_result, na.rm = TRUE)

## ----eval = FALSE-------------------------------------------------------------
# method(gw)

## ----eval = FALSE-------------------------------------------------------------
# schema_metadata(gw)

## ----eval = FALSE-------------------------------------------------------------
# validate_env_schema(gw)

## ----eval = FALSE-------------------------------------------------------------
# validate_env_schema(gw)
# 
# #> TRUE

## ----eval = FALSE-------------------------------------------------------------
# gw_small <- gw %>%
#   select(
#     location_id,
#     sample_date,
#     param_name,
#     .analysis_result
#   )
# 
# gw_small

## ----eval = FALSE-------------------------------------------------------------
# inherits(gw_small, "env_schema")

## ----eval = FALSE-------------------------------------------------------------
# chloride <- gw %>%
#   filter(.source_analyte == "Chloride, total")

## ----eval = FALSE-------------------------------------------------------------
# inherits(chloride, "env_schema")

## ----eval = FALSE-------------------------------------------------------------
# chloride_log <- chloride %>%
#   mutate(
#     result_log = log(.analysis_result)
#   )

## ----eval = FALSE-------------------------------------------------------------
# inherits(chloride_log, "env_schema")

## ----eval = FALSE-------------------------------------------------------------
# gw %>%
#   arrange(
#     location_id,
#     sample_date
#   )

## ----eval = FALSE-------------------------------------------------------------
# gw %>%
#   rename(location = location_id)

## ----eval = FALSE-------------------------------------------------------------
# gw %>%
#   group_by(param_name) %>%
#   summarize(
#     mean_result = mean(.analysis_result, na.rm = TRUE)
#   )

## ----eval = FALSE-------------------------------------------------------------
# function(x) {
#   mean(analysis_result(x), na.rm = TRUE)
# }

## ----eval = FALSE-------------------------------------------------------------
# function(x) {
#   mean(x$.analysis_result, na.rm = TRUE)
# }

## ----eval = FALSE-------------------------------------------------------------
# gw %>%
#   filter(
#     analyte(.) == "Chloride, total"
#   ) %>%
#   mutate(
#     result_mg_l = analysis_result(.)
#   ) %>%
#   arrange(
#     sample_date(.)
#   )

