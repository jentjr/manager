% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/series_plot.R
\name{series_plot}
\alias{series_plot}
\title{This function plots multiple groundwater data time series by location,
or constituent}
\usage{
series_plot(
  df,
  x = "sample_date",
  y = "analysis_result",
  facet_var = "location_id",
  group_var = "param_name",
  lt_measure = "lt_measure",
  scale = "free",
  scale_y_trans = "identity",
  trend = NULL,
  background = NULL,
  limit1 = NULL,
  limit2 = NULL,
  short_name = FALSE,
  pnt = 3,
  ncol = NULL
)
}
\arguments{
\item{df}{df groundwater data in tidy format}

\item{x}{x column for x variable}

\item{y}{y column for y variable}

\item{facet_var}{column to facet wrap plots by, default is by location}

\item{group_var}{column to group plots by, default is by constituent}

\item{lt_measure}{column for non-detect symbol}

\item{scale}{"fixed" or "free", or in one dimension "free_x", "free_y"}

\item{scale_y_trans}{type of transformation to use for y scale. Default is
"identity".  Built-in transformations include "asn", "atanh", "boxcox",
"exp", "identity", "log", "log10", "log1p", "log2", "logit", "probability",
"probit", "reciprocal", "reverse" and "sqrt".}

\item{trend}{trend add trend line to time series plot}

\item{background}{vector of dates for background start and end dates.}

\item{limit1}{horizontal line 1}

\item{limit2}{horizontal line 2}

\item{short_name}{If TRUE, the constituent name will be abbreviated}

\item{pnt}{size of points on time series plots}

\item{ncol}{number of columns}
}
\description{
This function plots multiple groundwater data time series by location,
or constituent
}
