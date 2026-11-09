# Function to read MANAGES 4.0 database

Function to read MANAGES 4.0 database

## Usage

``` r
read_manages4(
  driver = "SQL Server",
  server,
  database,
  site_names,
  analyte_lookup = analyte_tbl
)
```

## Arguments

- driver:

  default is "SQL Server"

- server:

  server name

- database:

  database name

- site_names:

  Character vector of site names
