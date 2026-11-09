# Function to export summary table for a sampling event

Function to export summary table for a sampling event

## Usage

``` r
write_event_summary(df, start, end, gw_elev = TRUE, short_name = TRUE, file)
```

## Arguments

- df:

  groundwater data frame in tidy format

- start:

  start date

- end:

  end date

- gw_elev:

  if TRUE, list date well sampled and annotate date for GW Elev

- short_name:

  TRUE/FALSE to abbreviate constituent name
