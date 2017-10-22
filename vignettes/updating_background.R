## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- message=FALSE------------------------------------------------------
library(manager)
data("gw_data")

MW1_Arsenic <- gw_data %>%
  filter(location_id == "MW-1", param_name == "Arsenic, dissolved")

ts_plot(MW1_Arsenic)

