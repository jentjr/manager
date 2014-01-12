\name{combo_plot}
\alias{combo_plot}
\title{This function plots groundwater data faceted by parameters with shaded regions
for background and compliance date ranges. Horizontal lines are plotted for
Groundwater Protection Standards.}
\usage{
combo_plot(df, by, ...)
}
\arguments{
  \item{df}{data frame in long format containing
  groundwater monitoring data assumes column names of
  location_id, param_name, analysis_result, default_unit,
  lt_measure, sample_date. Dates for sample_date column
  must be in as.POSIXct format}

  \item{by}{the column to facet the plot by}

  \item{...}{other parameters passed to
  \link(\code(gw_ts_plot))}
}
\description{
This function plots groundwater data faceted by parameters
with shaded regions for background and compliance date
ranges. Horizontal lines are plotted for Groundwater
Protection Standards.
}

