# Create a correlation plot

Create a correlation plot

## Usage

``` r
corr_plot(
  df,
  location_id = "location_id",
  sample_date = "sample_date",
  param_name = "param_name",
  analysis_result = "analysis_result",
  constituents = NULL,
  sample_locations = NULL
)
```

## Arguments

- df:

  dataframe

- location_id:

  column of sample locations

- sample_date:

  column of sample dates

- param_name:

  column of constituents

- analysis_result:

  column of numerical results

- constituents:

  list of consituents to plot e.g. c("Iron, dissolved", "Manganese,
  dissolved")

- sample_locations:

  list of sample locations to plot e.g. c("MW-1", "MW-2")
