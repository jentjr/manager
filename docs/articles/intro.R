## ---- message=FALSE------------------------------------------------------
library(manager)

## ---- eval = FALSE-------------------------------------------------------
#  data <- read_manages3("C:/path/to/manages/Site.mdb")

## ------------------------------------------------------------------------
data("ohio_data")
data("indiana_data")

## ---- eval=FALSE---------------------------------------------------------
#  data("gw_data")
#  
#  
#  nested_gw <- gw_data %>%
#    group_by(location_id, param_name, default_unit) %>%
#    nest()
#  
#  nested_gw %>%
#    filter(location_id == "MW-2",
#           param_name %in% c("Potassium, dissolved", "Magnesium, dissolved")) %>%
#    mutate(trend = map(data, kendall_trend))
#  
#  
#  mw1_K <- nested_gw %>%
#    filter(location_id == "MW-1", param_name == "Potassium, dissolved")
#  
#  test <- mw1_K %>%
#    mutate(start = map(data, start_date),
#           end  = map(data, nth_date, start = start[[1]], n = 7),
#           background = map(data, set_background, start = start, end = end),
#           percent_left_censored = map(background, percent_lt),
#           distribution = map(percent_left_censored, est_dist),
#           pred_int = map(distribution, pred_int))
#  

