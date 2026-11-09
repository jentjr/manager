# Function to update background data

Function to update background data

## Usage

``` r
update_background(df, well, param, start = NULL, n_end = 8, n_prop = 4)
```

## Arguments

- df:

  data frame of groundwater data in tidy format

- well:

  the location to update

- param:

  the constituent to update

- start:

  the start date of the background data. IF not provide the start of the
  data will be found using @seealso \[start_date()\]

- n_end:

  the number of samples in current background. The date will be found
  from this sample.

- n_prop:

  the number of samples to include in the proposed background
