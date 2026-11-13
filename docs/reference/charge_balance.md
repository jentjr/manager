# Charge Balance Calculation

Calculates the ionic charge balance of major-ion chemistry data.

## Usage

``` r
charge_balance(df, ...)

# S3 method for class 'data.frame'
charge_balance(df, location, analyte, result, unit, sample_date, ...)

# S3 method for class 'env_schema'
charge_balance(df, analyte_matching = c("exact", "strict", "normalized"), ...)
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

- analyte_matching:

  Analyte matching strategy. One of \`"exact"\`, \`"strict"\`, or
  \`"normalized"\`.

## Value

A data frame containing:

- location identifiers;

- sample dates;

- total cations (meq/L);

- total anions (meq/L);

- charge balance percentage.

## Details

\`charge_balance()\` is an S3 generic that supports both raw data frames
and \`env_schema\` objects.

For data frames, environmental column mappings must be supplied. For
\`env_schema\` objects, mapped schema fields are used automatically.

Charge balance is calculated using concentrations converted to
milliequivalents per liter (meq/L) and provides a quality-control
evaluation of analytical completeness and consistency.

For data frames, \`charge_balance()\` first converts the supplied data
to an \`env_schema\` object using the specified column mappings and then
dispatches to \[charge_balance.env_schema()\].

Charge balance is calculated from the sum of major dissolved cations and
anions after conversion to milliequivalents per liter (meq/L).

Major ions used in the calculation include:

- calcium;

- magnesium;

- sodium;

- potassium;

- chloride;

- sulfate;

- bicarbonate (alkalinity).

The charge balance percentage is calculated as:

\$\$ 100 \times \frac{\sum cations - \sum anions} {\sum cations + \sum
anions} \$\$

Values near zero generally indicate internally consistent analytical
results, whereas large positive or negative values may indicate
analytical error, missing constituents, transcription errors, or
unit-conversion problems.

For \`env_schema\` objects, analyte, result, unit, sample_date, and
location fields are obtained directly from schema mappings.

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

One record is returned for each unique location and sample-date
combination.

When multiple measurements of the same analyte occur within a location
and sample-date group, values are averaged during geochemical
standardization prior to charge-balance calculation.

Returned cation and anion totals are expressed in milliequivalents per
liter (meq/L).

## Interpretation

Typical charge balance error guidelines are:

- \< ±5%:

  Excellent agreement.

- ±5% to ±10%:

  Generally acceptable for most environmental investigations.

- \> ±10%:

  May indicate incomplete chemistry or analytical issues and should be
  reviewed.

These thresholds should be interpreted in the context of project
objectives, laboratory methods, and sample chemistry.

## Validation

Input data are validated for:

- analyte resolution;

- major-ion chemistry availability;

- supported analytical units;

- successful conversion to milliequivalents;

- duplicate analyte measurements.

## Examples

``` r
data(gw)

charge_balance(gw)
#> # A tibble: 150 × 5
#>    location_id sample_date         total_cations total_anions charge_balance_pct
#>    <chr>       <dttm>                      <dbl>        <dbl>              <dbl>
#>  1 MW-1        2007-12-20 00:00:00          5.82         4.84               9.16
#>  2 MW-1        2008-02-20 00:00:00          5.87         3.99              19.1 
#>  3 MW-1        2008-04-24 00:00:00          8.70         6.34              15.6 
#>  4 MW-1        2008-06-18 00:00:00          9.73         6.69              18.5 
#>  5 MW-1        2008-08-26 00:00:00         10.2          6.69              21.0 
#>  6 MW-1        2008-10-28 00:00:00         10.7          4.66              39.1 
#>  7 MW-1        2008-12-16 00:00:00          9.34         6.75              16.1 
#>  8 MW-1        2009-02-25 00:00:00         10.2          8.91               6.90
#>  9 MW-1        2009-04-29 00:00:00          9.72         8.52               6.54
#> 10 MW-1        2009-10-21 00:00:00          9.32         8.36               5.45
#> # ℹ 140 more rows

data(gw)

charge_balance(gw)
#> # A tibble: 150 × 5
#>    location_id sample_date         total_cations total_anions charge_balance_pct
#>    <chr>       <dttm>                      <dbl>        <dbl>              <dbl>
#>  1 MW-1        2007-12-20 00:00:00          5.82         4.84               9.16
#>  2 MW-1        2008-02-20 00:00:00          5.87         3.99              19.1 
#>  3 MW-1        2008-04-24 00:00:00          8.70         6.34              15.6 
#>  4 MW-1        2008-06-18 00:00:00          9.73         6.69              18.5 
#>  5 MW-1        2008-08-26 00:00:00         10.2          6.69              21.0 
#>  6 MW-1        2008-10-28 00:00:00         10.7          4.66              39.1 
#>  7 MW-1        2008-12-16 00:00:00          9.34         6.75              16.1 
#>  8 MW-1        2009-02-25 00:00:00         10.2          8.91               6.90
#>  9 MW-1        2009-04-29 00:00:00          9.72         8.52               6.54
#> 10 MW-1        2009-10-21 00:00:00          9.32         8.36               5.45
#> # ℹ 140 more rows

charge_balance(
  gw,
  analyte_matching = "normalized"
)
#> # A tibble: 150 × 5
#>    location_id sample_date         total_cations total_anions charge_balance_pct
#>    <chr>       <dttm>                      <dbl>        <dbl>              <dbl>
#>  1 MW-1        2007-12-20 00:00:00          5.82         4.84               9.16
#>  2 MW-1        2008-02-20 00:00:00          5.87         3.99              19.1 
#>  3 MW-1        2008-04-24 00:00:00          8.70         6.34              15.6 
#>  4 MW-1        2008-06-18 00:00:00          9.73         6.69              18.5 
#>  5 MW-1        2008-08-26 00:00:00         10.2          6.69              21.0 
#>  6 MW-1        2008-10-28 00:00:00         10.7          4.66              39.1 
#>  7 MW-1        2008-12-16 00:00:00          9.34         6.75              16.1 
#>  8 MW-1        2009-02-25 00:00:00         10.2          8.91               6.90
#>  9 MW-1        2009-04-29 00:00:00          9.72         8.52               6.54
#> 10 MW-1        2009-10-21 00:00:00          9.32         8.36               5.45
#> # ℹ 140 more rows
```
