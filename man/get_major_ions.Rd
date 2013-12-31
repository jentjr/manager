\name{get_major_ions}
\alias{get_major_ions}
\title{function to read monitoring well data in the form location_id, sample_date,
analysis_result, default_unit, param_name and gather parameters needed for
geochemistry plots. The data is cast by location_id + sample_date ~ param_name}
\usage{
get_major_ions(df, Mg, Ca, Na, K, Cl, SO4, CO3, HCO3, TDS)
}
\arguments{
  \item{df}{data frame of groundwater monitoring data}
}
\description{
function to read monitoring well data in the form
location_id, sample_date, analysis_result, default_unit,
param_name and gather parameters needed for geochemistry
plots. The data is cast by location_id + sample_date ~
param_name
}

