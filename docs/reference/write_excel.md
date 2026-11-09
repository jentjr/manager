# Function to export data from manages to excel. Each sampling location is saved to a tab.

Function to export data from manages to excel. Each sampling location is
saved to a tab.

## Usage

``` r
write_excel(
  df,
  wells,
  constituents,
  file,
  short_name = TRUE,
  lab_id = FALSE,
  overwrite = FALSE
)
```

## Arguments

- df:

  groundwater data frame in tidy format

- wells:

  list of wells to be exported

- constituents:

  list of constituents to be exported

- file:

  full file path name with extension for export

- short_name:

  TRUE/FALSE to abbreviate constituent name

- lab_id:

  TRUE/FALSE to include the lab id

- overwrite:

  TRUE/FALSE overwrite existing spreadsheet if it exists
