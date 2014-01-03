\name{transform_stiff_data}
\alias{transform_stiff_data}
\title{Function to transform data from \code{\link{get_major_ions}} into x, y coordinates and in the
correct path for geom_polygon of ggplot}
\usage{
transform_stiff_data(df, Mg = df$Mg, Ca = df$Ca, Na = df$Na, K = df$K,
  Cl = df$Cl, SO4 = df$SO4, HCO3 = df$HCO3, name = df$location_id,
  date = df$sample_date, TDS = NULL, units = NULL)
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

