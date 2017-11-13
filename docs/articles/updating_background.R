## ---- echo = FALSE, message=FALSE----------------------------------------
library(manager)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "figures/geochemical-"
)

## ---- message=FALSE------------------------------------------------------
data("gw_data")

MW1_Arsenic <- gw_data %>%
  filter(location_id == "MW-1", param_name == "Arsenic, dissolved")

ts_plot(MW1_Arsenic)

