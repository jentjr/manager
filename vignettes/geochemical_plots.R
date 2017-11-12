## ---- echo = FALSE, message=FALSE----------------------------------------
devtools::load_all("/usr/local/src/manager")

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "figures/geochemical-"
)

## ---- piper, fig.height=6, fig.width=9, results='hide', warning=FALSE----
# load example data
data("gw_data")

gw_data %>%
  piper_plot()

## ----schoeller, fig.height=6, fig.width=9, results='hide', warning=FALSE----
gw_data %>%
  schoeller_plot()

## ----stiff, fig.height=6, fig.width=9, warning=FALSE, results='hide'-----
gw_data %>%
  filter(location_id %in% c("MW-1", "MW-2")) %>%
  stiff_plot(., total_dissolved_solids = "Total Dissolved Solids")

