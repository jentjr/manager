\name{transform_stiff_data}
\alias{transform_stiff_data}
\title{Function to transform data from \code{\link{get_major_ions}}
into x, y coordinates and in the
correct path for geom_polygon of ggplot}
\usage{
transform_stiff_data(df, Mg = "Mg", Ca = "Ca", Na = "Na", K = "K",
  Cl = "Cl", SO4 = "SO4", HCO3 = "HCO3", name = "location_id",
  date = "sample_date", TDS = NULL, units = NULL)
}
\arguments{
  \item{df}{data frame of groundwater data from
  \code{\link{get_major_ions}}}
}
\description{
Function to transform data from
\code{\link{get_major_ions}} into x, y coordinates and in
the correct path for geom_polygon of ggplot
}

