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
# gw <- as_env_schema(raw_data)

## ----eval = FALSE-------------------------------------------------------------
# class(gw)

## ----eval = FALSE-------------------------------------------------------------
# location(gw)
# sample_date(gw)
# analyte(gw)
# result(gw)
# unit(gw)
# detect(gw)
# limit(gw)
# method(gw)

## ----eval = FALSE-------------------------------------------------------------
# dplyr::n_distinct(location(gw))
# 
# mean(result(gw), na.rm = TRUE)

## ----eval = FALSE-------------------------------------------------------------
# dplyr::n_distinct(gw$.location)
# 
# mean(gw$.result, na.rm = TRUE)

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
#     .location,
#     .sample_date,
#     .analyte,
#     .result
#   )
# 
# gw_small

## ----eval = FALSE-------------------------------------------------------------
# inherits(gw_small, "env_schema")

## ----eval = FALSE-------------------------------------------------------------
# benzene <- gw %>%
#   filter(.analyte == "Benzene")

## ----eval = FALSE-------------------------------------------------------------
# inherits(benzene, "env_schema")

## ----eval = FALSE-------------------------------------------------------------
# gw2 <- gw %>%
#   mutate(
#     result_log = log(.result)
#   )

## ----eval = FALSE-------------------------------------------------------------
# inherits(gw2, "env_schema")

## ----eval = FALSE-------------------------------------------------------------
# gw %>%
#   arrange(
#     .location,
#     .sample_date
#   )

## ----eval = FALSE-------------------------------------------------------------
# gw %>%
#   rename(location = .location)

## ----eval = FALSE-------------------------------------------------------------
# gw %>%
#   group_by(.analyte) %>%
#   summarize(
#     mean_result = mean(.result, na.rm = TRUE)
#   )

## ----eval = FALSE-------------------------------------------------------------
# function(x) {
#   mean(result(x), na.rm = TRUE)
# }

## ----eval = FALSE-------------------------------------------------------------
# function(x) {
#   mean(x$.result, na.rm = TRUE)
# }

## ----eval = FALSE-------------------------------------------------------------
# gw %>%
#   filter(
#     analyte(.) == "Benzene"
#   ) %>%
#   mutate(
#     result_ug_l = result(.)
#   ) %>%
#   arrange(
#     sample_date(.)
#   )

