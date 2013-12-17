\name{transform_piper_data}
\alias{transform_piper_data}
\title{function to transform data from get_plot_data() into x, y coordinates for
cation, anion and diamond of a Piper plot}
\usage{
transform_piper_data(df, Mg = df$Mg, Ca = df$Ca, Na = df$Na, K = df$K,
  Cl = df$Cl, SO4 = df$SO4, CO3 = df$CO3, HCO3 = df$HCO3,
  TDS = df$TDS, name = df$location_id, date = df$sample_date,
  units = NULL)
}
\arguments{
  \item{df}{data frame of groundwater data}

  \item{Mg}{Magnesium}

  \item{Ca}{Calcium}

  \item{Na}{Sodium}

  \item{K}{Potassum}

  \item{Cl}{Chloride}

  \item{SO4}{Sulfate}

  \item{CO3}{Carbonate}

  \item{HCO3}{Bicarbonate}

  \item{TDS}{Total Dissolved Solids}

  \item{name}{column of well names}

  \item{date}{column of dates}

  \item{units}{units, right now can handle mg/L and meq/L}
}
\description{
function to transform data from get_plot_data() into x, y
coordinates for cation, anion and diamond of a Piper plot
}
\keyword{diagram}
\keyword{piper}

