## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 6,
  warning = FALSE,
  message = FALSE
)

## ----data-gw------------------------------------------------------------------
library(dplyr)
library(manager)

data(gw)

gw

## ----gw-series----------------------------------------------------------------
plots <- series_plot(gw)

plots[[1]]

## ----data-gw-data-------------------------------------------------------------
data(gw_data)

glimpse(gw_data)

## ----subset-data--------------------------------------------------------------
example_wells <- c(
  "MW-1",
  "MW-2",
  "MW-3"
)

gw_subset <- gw_data %>%
  dplyr::filter(
    location_id %in% example_wells
  )

## ----env-schema---------------------------------------------------------------
gw_schema <- env_schema(
  gw_subset,
  location = location_id,
  sample_date = sample_date,
  analyte = param_name,
  result = analysis_result,
  unit = default_unit,
  reporting_flag = lt_measure
)

gw_schema

## ----series-basic-------------------------------------------------------------
plots <- series_plot(gw)

plots[[1]]

## ----series-lm----------------------------------------------------------------
plots <- series_plot(
  gw,
  trend = "lm"
)

plots[[1]]

## ----series-mk, eval=FALSE----------------------------------------------------
# plots <- series_plot(
#   gw,
#   trend = "mann_kendall",
#   trend_label = "compact"
# )
# 
# plots[[1]]

## ----series-combined----------------------------------------------------------
series_plot(
  gw,
  plot_type = "combined"
)

## ----series-standards---------------------------------------------------------
plots <- series_plot(
  gw,
  standards = TRUE
)

plots[[1]]

## ----boxplot-basic------------------------------------------------------------
plots <- boxplot(gw)

plots[[1]]

## ----boxplot-points-----------------------------------------------------------
plots <- boxplot(
  gw,
  show_points = TRUE
)

plots[[1]]

## ----boxplot-ci---------------------------------------------------------------
plots <- boxplot(
  gw,
  show_points = TRUE,
  plot_ci = TRUE
)

plots[[1]]

## ----boxplot-standards--------------------------------------------------------
plots <- boxplot(
  gw,
  standards = TRUE
)

plots[[1]]

## ----boxplot-explanation------------------------------------------------------
plots <- boxplot(
  gw,
  add_explanation = TRUE
)

plots[[1]]

## ----corr-plot, eval=FALSE----------------------------------------------------
# corr_plot(
#   gw_data,
#   constituents = c(
#     "Iron, dissolved",
#     "Manganese, dissolved",
#     "Sulfate, total"
#   ),
#   sample_locations = c(
#     "MW-1",
#     "MW-2"
#   )
# )

## ----piper-basic--------------------------------------------------------------
piper_plot(gw)

## ----piper-normalized---------------------------------------------------------
piper_plot(
  gw,
  analyte_matching = "normalized"
)

## ----piper-exact--------------------------------------------------------------
piper_plot(
  gw,
  analyte_matching = "exact"
)

## ----piper-tds, eval=FALSE----------------------------------------------------
# piper_plot(
#   gw,
#   scale_analyte = "Total Dissolved Solids"
# )

## ----piper-group, eval=FALSE--------------------------------------------------
# piper_plot(
#   gw,
#   group_col = "aquifer_unit"
# )

## ----piper-animation, eval=FALSE----------------------------------------------
# piper_time_plot(gw)

## ----piper-html, eval=FALSE---------------------------------------------------
# piper_time_html(gw)

## ----schoeller-basic----------------------------------------------------------
schoeller_plot(gw)

## ----schoeller-location-------------------------------------------------------
schoeller_plot(
  gw,
  facet_var = location_var(gw)
)

## ----schoeller-date-----------------------------------------------------------
schoeller_plot(
  gw,
  facet_var = sample_date_var(gw)
)

## ----schoeller-title----------------------------------------------------------
schoeller_plot(
  gw,
  title = "Groundwater Major-Ion Chemistry"
)

## ----stiff-basic--------------------------------------------------------------
plots <- stiff_plot(gw)

plots[[1]]

## ----stiff-scaled, eval=FALSE-------------------------------------------------
# plots <- stiff_plot(
#   gw,
#   scale_analyte = "Total Dissolved Solids"
# )
# 
# plots[[1]]

## ----stiff-date---------------------------------------------------------------
plots <- stiff_plot(
  gw,
  group_var = sample_date_var(gw)
)

plots[[1]]

## ----stiff-lines--------------------------------------------------------------
plots <- stiff_plot(
  gw,
  lines = TRUE
)

plots[[1]]

## ----stiff-animation, eval=FALSE----------------------------------------------
# stiff_time_plot(gw)

## ----stiff-html, eval=FALSE---------------------------------------------------
# stiff_time_html(gw)

## ----durov-basic--------------------------------------------------------------
durov_plot(gw)

## ----durov-normalized---------------------------------------------------------
durov_plot(
  gw,
  analyte_matching = "normalized"
)

## ----durov-exact--------------------------------------------------------------
durov_plot(
  gw,
  analyte_matching = "exact"
)

## ----standards-series---------------------------------------------------------
plots <- series_plot(
  gw,
  standards = TRUE
)

plots[[1]]

## ----standards-boxplot--------------------------------------------------------
plots <- boxplot(
  gw,
  standards = TRUE
)

plots[[1]]

## ----extension-dataframe, eval=FALSE------------------------------------------
# my_plot.data.frame <- function(
#   df,
#   location,
#   sample_date,
#   analyte,
#   result,
#   ...
# ) {
# 
#   df <- env_schema(
#     df,
#     location = {{ location }},
#     sample_date = {{ sample_date }},
#     analyte = {{ analyte }},
#     result = {{ result }}
#   )
# 
#   my_plot(df, ...)
# }

## ----extension-schema, eval=FALSE---------------------------------------------
# my_plot.env_schema <- function(
#   df,
#   ...
# ) {
# 
#   location_col <- location_var(df)
# 
#   sample_date_col <- sample_date_var(df)
# 
#   analyte_col <- analyte_var(df)
# 
#   result_col <- result_var(df)
# 
#   ...
# }

## ----session-info-------------------------------------------------------------
sessionInfo()

