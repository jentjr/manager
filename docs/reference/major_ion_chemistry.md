# Major Ion Chemistry

Extracts, standardizes, and converts major-ion chemistry data for
hydrogeochemical analysis.

## Usage

``` r
major_ion_chemistry(df, ...)

# S3 method for class 'data.frame'
major_ion_chemistry(df, location, analyte, result, unit, sample_date, ...)

# S3 method for class 'env_schema'
major_ion_chemistry(
  df,
  analytes = c("Calcium, dissolved", "Magnesium, dissolved", "Sodium, dissolved",
    "Potassium, dissolved", "Chloride, total", "Sulfate, total",
    "Alkalinity, total (lab)"),
  analyte_matching = c("exact", "strict", "normalized"),
  ...
)
```

## Arguments

- df:

  A data frame or \`env_schema\` object.

- ...:

  Additional arguments passed to methods.

- location:

  Location identifier column.

- analyte:

  Analyte column.

- result:

  Analytical result column.

- unit:

  Reporting unit column.

- sample_date:

  Sample date column.

- analytes:

  Major-ion analytes to retain.

- analyte_matching:

  Analyte matching strategy. One of \`"exact"\`, \`"strict"\`, or
  \`"normalized"\`.

## Value

An \`env_schema\` object containing standardized major-ion chemistry
data.

## Details

\`major_ion_chemistry()\` is an S3 generic that supports both raw data
frames and \`env_schema\` objects.

For data frames, environmental column mappings must be supplied. For
\`env_schema\` objects, mapped schema fields are used automatically.

Major-ion analytes are standardized, duplicate measurements are
consolidated, and analytical results are converted to milliequivalents
per liter (meq/L).

For data frames, \`major_ion_chemistry()\` first converts the supplied
data to an \`env_schema\` object using the specified column mappings and
then dispatches to \[major_ion_chemistry.env_schema()\].

\`major_ion_chemistry()\` serves as the primary preprocessing function
for geochemical analyses and diagrams.

The function:

- resolves analyte names;

- filters requested analytes;

- consolidates duplicate measurements;

- standardizes analytical metadata;

- converts concentrations to milliequivalents per liter (meq/L).

Duplicate measurements within a location and sample-date combination are
averaged prior to conversion.

## Analyte Matching

- exact:

  Uses analyte names exactly as reported.

- strict:

  Applies text normalization while preserving analytical distinctions
  such as dissolved versus total fractions.

- normalized:

  Resolves analyte aliases to canonical analyte names using package
  analyte metadata.

## Output Structure

One record is returned for each unique analyte, location, and
sample-date combination.

Analytical concentrations are available in both native units and
standardized milliequivalent units suitable for geochemical
calculations.

## Examples

``` r
data(gw)

chem <- major_ion_chemistry(gw)

chem
#> # A tibble: 1,038 × 15
#>    location_id sample_date         analyte      .analyte_id .original_lab_result
#>  * <chr>       <dttm>              <chr>        <chr>                      <dbl>
#>  1 MW-1        2007-12-20 00:00:00 Bicarbonate… bicarbonate               179   
#>  2 MW-1        2007-12-20 00:00:00 Calcium, di… calcium                    64.7 
#>  3 MW-1        2007-12-20 00:00:00 Chloride, t… chloride                   30   
#>  4 MW-1        2007-12-20 00:00:00 Magnesium, … magnesium                  21   
#>  5 MW-1        2007-12-20 00:00:00 Potassium, … potassium                   3.25
#>  6 MW-1        2007-12-20 00:00:00 Sodium, dis… sodium                     17.9 
#>  7 MW-1        2007-12-20 00:00:00 Sulfate, to… sulfate                    51   
#>  8 MW-1        2008-02-20 00:00:00 Bicarbonate… bicarbonate               137   
#>  9 MW-1        2008-02-20 00:00:00 Calcium, di… calcium                    71.3 
#> 10 MW-1        2008-02-20 00:00:00 Chloride, t… chloride                   25   
#> # ℹ 1,028 more rows
#> # ℹ 10 more variables: .original_lab_unit <chr>, .analysis_result <dbl>,
#> #   .analysis_unit <chr>, .detected <lgl>, .censored <lgl>, .estimated <lgl>,
#> #   .rejected <lgl>, .usable <lgl>, .analysis_result_meq <dbl>,
#> #   .analysis_unit_meq <chr>

data(gw)

major_ion_chemistry(gw)
#> # A tibble: 1,038 × 15
#>    location_id sample_date         analyte      .analyte_id .original_lab_result
#>  * <chr>       <dttm>              <chr>        <chr>                      <dbl>
#>  1 MW-1        2007-12-20 00:00:00 Bicarbonate… bicarbonate               179   
#>  2 MW-1        2007-12-20 00:00:00 Calcium, di… calcium                    64.7 
#>  3 MW-1        2007-12-20 00:00:00 Chloride, t… chloride                   30   
#>  4 MW-1        2007-12-20 00:00:00 Magnesium, … magnesium                  21   
#>  5 MW-1        2007-12-20 00:00:00 Potassium, … potassium                   3.25
#>  6 MW-1        2007-12-20 00:00:00 Sodium, dis… sodium                     17.9 
#>  7 MW-1        2007-12-20 00:00:00 Sulfate, to… sulfate                    51   
#>  8 MW-1        2008-02-20 00:00:00 Bicarbonate… bicarbonate               137   
#>  9 MW-1        2008-02-20 00:00:00 Calcium, di… calcium                    71.3 
#> 10 MW-1        2008-02-20 00:00:00 Chloride, t… chloride                   25   
#> # ℹ 1,028 more rows
#> # ℹ 10 more variables: .original_lab_unit <chr>, .analysis_result <dbl>,
#> #   .analysis_unit <chr>, .detected <lgl>, .censored <lgl>, .estimated <lgl>,
#> #   .rejected <lgl>, .usable <lgl>, .analysis_result_meq <dbl>,
#> #   .analysis_unit_meq <chr>

major_ion_chemistry(
  gw,
  analyte_matching = "normalized"
)
#> # A tibble: 1,038 × 15
#>    location_id sample_date         analyte     .analyte_id .original_lab_result
#>  * <chr>       <dttm>              <chr>       <chr>                      <dbl>
#>  1 MW-1        2007-12-20 00:00:00 bicarbonate bicarbonate               179   
#>  2 MW-1        2007-12-20 00:00:00 calcium     calcium                    64.7 
#>  3 MW-1        2007-12-20 00:00:00 chloride    chloride                   30   
#>  4 MW-1        2007-12-20 00:00:00 magnesium   magnesium                  21   
#>  5 MW-1        2007-12-20 00:00:00 potassium   potassium                   3.25
#>  6 MW-1        2007-12-20 00:00:00 sodium      sodium                     17.9 
#>  7 MW-1        2007-12-20 00:00:00 sulfate     sulfate                    51   
#>  8 MW-1        2008-02-20 00:00:00 bicarbonate bicarbonate               137   
#>  9 MW-1        2008-02-20 00:00:00 calcium     calcium                    71.3 
#> 10 MW-1        2008-02-20 00:00:00 chloride    chloride                   25   
#> # ℹ 1,028 more rows
#> # ℹ 10 more variables: .original_lab_unit <chr>, .analysis_result <dbl>,
#> #   .analysis_unit <chr>, .detected <lgl>, .censored <lgl>, .estimated <lgl>,
#> #   .rejected <lgl>, .usable <lgl>, .analysis_result_meq <dbl>,
#> #   .analysis_unit_meq <chr>
```
