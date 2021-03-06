% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conc_to_meq.R
\name{conc_to_meq}
\alias{conc_to_meq}
\title{Function to convert chemical data from mg/L to meq/L}
\usage{
conc_to_meq(
  df,
  magnesium = "Magnesium, dissolved",
  calcium = "Calcium, dissolved",
  sodium = "Sodium, dissolved",
  potassium = "Potassium, dissolved",
  chloride = "Chloride, total",
  sulfate = "Sulfate, total",
  alkalinity = "Alkalinity, total (lab)"
)
}
\arguments{
\item{df}{data frame in wide format i.e, column names are major ions}

\item{magnesium}{magnesium}

\item{calcium}{calcium}

\item{sodium}{sodium}

\item{potassium}{potassium}

\item{chloride}{chloride}

\item{sulfate}{sulfate}

\item{alkalinity}{Total Alkalinity}
}
\description{
Function to convert chemical data from mg/L to meq/L
}
