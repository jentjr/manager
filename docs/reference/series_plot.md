# Environmental Time Series Plot

Creates time-series plots for environmental monitoring data.

## Usage

``` r
series_plot(df, ...)

# S3 method for class 'data.frame'
series_plot(
  df,
  location,
  sample_date,
  analyte,
  result,
  unit = NULL,
  reporting_flag = NULL,
  reporting_limit = NULL,
  detection_limit = NULL,
  ...
)

# S3 method for class 'env_schema'
series_plot(
  df,
  scale = "free",
  scale_y_trans = "identity",
  trend = "none",
  trend_ci = FALSE,
  trend_conf_level = 0.95,
  trend_label = c("none", "compact", "full"),
  trend_label_sig_only = TRUE,
  background = NULL,
  standards = TRUE,
  short_name = FALSE,
  pnt = 3,
  ncol = NULL,
  plot_type = c("split", "combined"),
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

- sample_date:

  Sample date column.

- analyte:

  Analyte column.

- result:

  Analytical result column.

- unit:

  Reporting unit column.

- reporting_flag:

  Reporting qualifier column.

- reporting_limit:

  Reporting limit column.

- detection_limit:

  Detection limit column.

- scale:

  Facet scaling behavior passed to \[ggplot2::facet_wrap()\]. One of
  \`"fixed"\`, \`"free"\`, \`"free_x"\`, or \`"free_y"\`.

- scale_y_trans:

  Transformation applied to the y-axis. One of \`"identity"\`,
  \`"log"\`, \`"log10"\`, \`"log2"\`, \`"log1p"\`, \`"sqrt"\`,
  \`"reverse"\`, \`"reciprocal"\`, \`"exp"\`, \`"boxcox"\`, \`"logit"\`,
  \`"probit"\`, \`"probability"\`, \`"atanh"\`, or \`"asn"\`.

- trend:

  Trend line method. One of \`"none"\`, \`"lm"\`, \`"loess"\`,
  \`"gam"\`, or \`"mann_kendall"\`.

- trend_ci:

  Logical. If \`TRUE\`, confidence intervals are displayed for supported
  trend models.

- trend_conf_level:

  Confidence level used when \`trend_ci = TRUE\`.

- trend_label:

  Trend annotation format. One of \`"none"\`, \`"compact"\`, or
  \`"full"\`.

- trend_label_sig_only:

  Logical. If \`TRUE\`, trend labels are displayed only for
  statistically significant trends.

- background:

  Optional vector containing a start date and end date used to shade a
  background monitoring period.

- standards:

  Logical. If \`TRUE\`, applicable analytical standards are displayed.

- short_name:

  Logical. If \`TRUE\`, analyte titles use short analyte labels when
  available.

- pnt:

  Point size for plotted observations.

- ncol:

  Number of facet columns.

- plot_type:

  Plot layout. One of \`"split"\` or \`"combined"\`.

## Value

If \`plot_type = "split"\`, a named list of ggplot objects, one per
analyte.

If \`plot_type = "combined"\`, a single ggplot object.

## Details

\`series_plot()\` is an S3 generic that supports both raw data frames
and \`env_schema\` objects.

For data frames, environmental column mappings must be supplied. For
\`env_schema\` objects, mapped schema fields are used automatically.

For data frames, \`series_plot()\` first converts the supplied data to
an \`env_schema\` object using the specified column mappings and then
dispatches to \[series_plot.env_schema()\].

\`series_plot()\` produces time-series visualizations of environmental
analytical results.

Data are grouped by analyte and location and displayed as chronological
sampling records.

In \`"split"\` mode, a separate plot is created for each analyte. In
\`"combined"\` mode, all analytes are displayed on a single plot using
colour to distinguish analytes.

Non-detect observations are automatically distinguished from detected
observations using symbol shape.

Optional trend lines, trend annotations, analytical standards, and
background periods may be added to aid interpretation.

For \`env_schema\` objects, analyte, result, sample_date, unit,
detection status, and location information are obtained directly from
schema mappings and standardized analytical fields.

## Plot Types

- split:

  Produces a separate plot for each analyte.

- combined:

  Produces a single plot containing all analytes with analytes
  distinguished by colour.

## Trend Analysis

Several trend-fitting options are available:

- none:

  No trend line is added.

- lm:

  Linear regression trend.

- loess:

  Locally weighted smooth trend.

- gam:

  Generalized additive model smooth.

- mann_kendall:

  Mann-Kendall trend test with associated annotation.

## Detection Status

Detected and non-detect observations are displayed using different point
symbols. Detection status is obtained from standardized censoring
information contained in the \`env_schema\` object.

## Schema-aware Plotting

For \`env_schema\` objects, \`series_plot()\` automatically uses:

- the mapped sample_date field;

- the mapped location field;

- the mapped analyte field;

- standardized analysis results;

- standardized analysis units;

- standardized censoring indicators.

This allows plots to be generated consistently regardless of original
source column names.

## Validation

Input arguments are validated for:

- supported facet scaling options;

- supported y-axis transformations;

- supported trend-fitting methods;

- presence of observations available for plotting.

## Examples

``` r
data(gw)

plots <- series_plot(gw)

names(plots)
#>  [1] "Alkalinity, total (lab)"            "Aluminum, dissolved"               
#>  [3] "Antimony, dissolved"                "Arsenic, dissolved"                
#>  [5] "Barium, dissolved"                  "Beryllium, dissolved"              
#>  [7] "Boron, dissolved"                   "Cadmium, dissolved"                
#>  [9] "Calcium, dissolved"                 "Chloride, total"                   
#> [11] "Chromium, dissolved"                "Chromium, hexavalent"              
#> [13] "Cobalt, Dis"                        "Copper, dissolved"                 
#> [15] "Dissolved Oxygen"                   "Fluoride, total"                   
#> [17] "Groundwater Elevation (ref MSL)"    "Groundwater, depth from TOC"       
#> [19] "Hardness, total (mg/L as CaCO3)"    "Iron, dissolved"                   
#> [21] "Lead, dissolved"                    "Magnesium, dissolved"              
#> [23] "Manganese, dissolved"               "Mercury, dissolved"                
#> [25] "Mercury, total"                     "Molybdenum, dissolved"             
#> [27] "Nickel, dissolved"                  "Nitrate nitrogen, total"           
#> [29] "Nitrite + Nitrate, total"           "Nitrite nitrogen, total"           
#> [31] "Oxidation Reduction Potential"      "Phosphorus, total"                 
#> [33] "Potassium, dissolved"               "Selenium, dissolved"               
#> [35] "SiO2"                               "Silver, dissolved"                 
#> [37] "Sodium, dissolved"                  "Specific Conductance @ 25C (field)"
#> [39] "Strontium, dissolved"               "Sulfate, total"                    
#> [41] "Temperature (Celsius)"              "Thallium, dissolved"               
#> [43] "Tin, dissolved"                     "Total Dissolved Solids"            
#> [45] "Turbidity, field"                   "Uranium, dissolved"                
#> [47] "Vanadium, dissolved"                "Zinc, dissolved"                   
#> [49] "pH (field)"                        

plots[[1]]


data(gw)

plots <- series_plot(gw)

names(plots)
#>  [1] "Alkalinity, total (lab)"            "Aluminum, dissolved"               
#>  [3] "Antimony, dissolved"                "Arsenic, dissolved"                
#>  [5] "Barium, dissolved"                  "Beryllium, dissolved"              
#>  [7] "Boron, dissolved"                   "Cadmium, dissolved"                
#>  [9] "Calcium, dissolved"                 "Chloride, total"                   
#> [11] "Chromium, dissolved"                "Chromium, hexavalent"              
#> [13] "Cobalt, Dis"                        "Copper, dissolved"                 
#> [15] "Dissolved Oxygen"                   "Fluoride, total"                   
#> [17] "Groundwater Elevation (ref MSL)"    "Groundwater, depth from TOC"       
#> [19] "Hardness, total (mg/L as CaCO3)"    "Iron, dissolved"                   
#> [21] "Lead, dissolved"                    "Magnesium, dissolved"              
#> [23] "Manganese, dissolved"               "Mercury, dissolved"                
#> [25] "Mercury, total"                     "Molybdenum, dissolved"             
#> [27] "Nickel, dissolved"                  "Nitrate nitrogen, total"           
#> [29] "Nitrite + Nitrate, total"           "Nitrite nitrogen, total"           
#> [31] "Oxidation Reduction Potential"      "Phosphorus, total"                 
#> [33] "Potassium, dissolved"               "Selenium, dissolved"               
#> [35] "SiO2"                               "Silver, dissolved"                 
#> [37] "Sodium, dissolved"                  "Specific Conductance @ 25C (field)"
#> [39] "Strontium, dissolved"               "Sulfate, total"                    
#> [41] "Temperature (Celsius)"              "Thallium, dissolved"               
#> [43] "Tin, dissolved"                     "Total Dissolved Solids"            
#> [45] "Turbidity, field"                   "Uranium, dissolved"                
#> [47] "Vanadium, dissolved"                "Zinc, dissolved"                   
#> [49] "pH (field)"                        

plots[[1]]


series_plot(
  gw,
  trend = "lm"
)
#> $`Alkalinity, total (lab)`

#> 
#> $`Aluminum, dissolved`

#> 
#> $`Antimony, dissolved`

#> 
#> $`Arsenic, dissolved`

#> 
#> $`Barium, dissolved`

#> 
#> $`Beryllium, dissolved`

#> 
#> $`Boron, dissolved`

#> 
#> $`Cadmium, dissolved`

#> 
#> $`Calcium, dissolved`

#> 
#> $`Chloride, total`

#> 
#> $`Chromium, dissolved`

#> 
#> $`Chromium, hexavalent`

#> 
#> $`Cobalt, Dis`

#> 
#> $`Copper, dissolved`

#> 
#> $`Dissolved Oxygen`
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?

#> 
#> $`Fluoride, total`

#> 
#> $`Groundwater Elevation (ref MSL)`

#> 
#> $`Groundwater, depth from TOC`

#> 
#> $`Hardness, total (mg/L as CaCO3)`

#> 
#> $`Iron, dissolved`

#> 
#> $`Lead, dissolved`

#> 
#> $`Magnesium, dissolved`

#> 
#> $`Manganese, dissolved`

#> 
#> $`Mercury, dissolved`

#> 
#> $`Mercury, total`

#> 
#> $`Molybdenum, dissolved`

#> 
#> $`Nickel, dissolved`

#> 
#> $`Nitrate nitrogen, total`

#> 
#> $`Nitrite + Nitrate, total`

#> 
#> $`Nitrite nitrogen, total`

#> 
#> $`Oxidation Reduction Potential`
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?

#> 
#> $`Phosphorus, total`

#> 
#> $`Potassium, dissolved`

#> 
#> $`Selenium, dissolved`

#> 
#> $SiO2

#> 
#> $`Silver, dissolved`

#> 
#> $`Sodium, dissolved`

#> 
#> $`Specific Conductance @ 25C (field)`
#> Warning: Removed 145 rows containing missing values or values outside the scale range
#> (`geom_path()`).
#> Warning: Removed 145 rows containing missing values or values outside the scale range
#> (`geom_point()`).

#> 
#> $`Strontium, dissolved`

#> 
#> $`Sulfate, total`

#> 
#> $`Temperature (Celsius)`

#> 
#> $`Thallium, dissolved`

#> 
#> $`Tin, dissolved`
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?

#> 
#> $`Total Dissolved Solids`

#> 
#> $`Turbidity, field`

#> 
#> $`Uranium, dissolved`

#> 
#> $`Vanadium, dissolved`

#> 
#> $`Zinc, dissolved`

#> 
#> $`pH (field)`

#> 

series_plot(
  gw,
  trend = "mann_kendall",
  trend_label = "compact"
)
#> $`Alkalinity, total (lab)`

#> 
#> $`Aluminum, dissolved`

#> 
#> $`Antimony, dissolved`

#> 
#> $`Arsenic, dissolved`

#> 
#> $`Barium, dissolved`

#> 
#> $`Beryllium, dissolved`

#> 
#> $`Boron, dissolved`

#> 
#> $`Cadmium, dissolved`

#> 
#> $`Calcium, dissolved`

#> 
#> $`Chloride, total`

#> 
#> $`Chromium, dissolved`

#> 
#> $`Chromium, hexavalent`

#> 
#> $`Cobalt, Dis`

#> 
#> $`Copper, dissolved`

#> 
#> $`Dissolved Oxygen`
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?

#> 
#> $`Fluoride, total`

#> 
#> $`Groundwater Elevation (ref MSL)`

#> 
#> $`Groundwater, depth from TOC`

#> 
#> $`Hardness, total (mg/L as CaCO3)`

#> 
#> $`Iron, dissolved`

#> 
#> $`Lead, dissolved`

#> 
#> $`Magnesium, dissolved`

#> 
#> $`Manganese, dissolved`

#> 
#> $`Mercury, dissolved`

#> 
#> $`Mercury, total`

#> 
#> $`Molybdenum, dissolved`

#> 
#> $`Nickel, dissolved`

#> 
#> $`Nitrate nitrogen, total`

#> 
#> $`Nitrite + Nitrate, total`

#> 
#> $`Nitrite nitrogen, total`

#> 
#> $`Oxidation Reduction Potential`
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?

#> 
#> $`Phosphorus, total`

#> 
#> $`Potassium, dissolved`

#> 
#> $`Selenium, dissolved`

#> 
#> $SiO2

#> 
#> $`Silver, dissolved`

#> 
#> $`Sodium, dissolved`

#> 
#> $`Specific Conductance @ 25C (field)`
#> Warning: Removed 145 rows containing missing values or values outside the scale range
#> (`geom_path()`).
#> Warning: Removed 145 rows containing missing values or values outside the scale range
#> (`geom_point()`).

#> 
#> $`Strontium, dissolved`

#> 
#> $`Sulfate, total`

#> 
#> $`Temperature (Celsius)`

#> 
#> $`Thallium, dissolved`

#> 
#> $`Tin, dissolved`
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?

#> 
#> $`Total Dissolved Solids`

#> 
#> $`Turbidity, field`

#> 
#> $`Uranium, dissolved`

#> 
#> $`Vanadium, dissolved`

#> 
#> $`Zinc, dissolved`

#> 
#> $`pH (field)`

#> 

series_plot(
  gw,
  plot_type = "combined"
)
#> Warning: Removed 145 rows containing missing values or values outside the scale range
#> (`geom_path()`).
#> Warning: Removed 145 rows containing missing values or values outside the scale range
#> (`geom_point()`).


series_plot(
  gw,
  scale_y_trans = "log10"
)
#> $`Alkalinity, total (lab)`

#> 
#> $`Aluminum, dissolved`

#> 
#> $`Antimony, dissolved`

#> 
#> $`Arsenic, dissolved`

#> 
#> $`Barium, dissolved`

#> 
#> $`Beryllium, dissolved`

#> 
#> $`Boron, dissolved`

#> 
#> $`Cadmium, dissolved`

#> 
#> $`Calcium, dissolved`

#> 
#> $`Chloride, total`

#> 
#> $`Chromium, dissolved`

#> 
#> $`Chromium, hexavalent`

#> 
#> $`Cobalt, Dis`

#> 
#> $`Copper, dissolved`

#> 
#> $`Dissolved Oxygen`
#> Warning: NaNs produced
#> Warning: log-10 transformation introduced infinite values.
#> Warning: NaNs produced
#> Warning: log-10 transformation introduced infinite values.
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_path()`).
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_point()`).

#> 
#> $`Fluoride, total`

#> 
#> $`Groundwater Elevation (ref MSL)`

#> 
#> $`Groundwater, depth from TOC`

#> 
#> $`Hardness, total (mg/L as CaCO3)`

#> 
#> $`Iron, dissolved`

#> 
#> $`Lead, dissolved`

#> 
#> $`Magnesium, dissolved`

#> 
#> $`Manganese, dissolved`

#> 
#> $`Mercury, dissolved`

#> 
#> $`Mercury, total`
#> Warning: log-10 transformation introduced infinite values.
#> Warning: log-10 transformation introduced infinite values.

#> 
#> $`Molybdenum, dissolved`

#> 
#> $`Nickel, dissolved`

#> 
#> $`Nitrate nitrogen, total`

#> 
#> $`Nitrite + Nitrate, total`

#> 
#> $`Nitrite nitrogen, total`

#> 
#> $`Oxidation Reduction Potential`
#> Warning: NaNs produced
#> Warning: log-10 transformation introduced infinite values.
#> Warning: NaNs produced
#> Warning: log-10 transformation introduced infinite values.
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: Removed 7 rows containing missing values or values outside the scale range
#> (`geom_path()`).
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> Warning: Removed 7 rows containing missing values or values outside the scale range
#> (`geom_point()`).

#> 
#> $`Phosphorus, total`

#> 
#> $`Potassium, dissolved`

#> 
#> $`Selenium, dissolved`
#> Warning: log-10 transformation introduced infinite values.
#> Warning: log-10 transformation introduced infinite values.

#> 
#> $SiO2

#> 
#> $`Silver, dissolved`

#> 
#> $`Sodium, dissolved`

#> 
#> $`Specific Conductance @ 25C (field)`
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: Removed 145 rows containing missing values or values outside the scale range
#> (`geom_path()`).
#> Warning: Removed 145 rows containing missing values or values outside the scale range
#> (`geom_point()`).

#> 
#> $`Strontium, dissolved`

#> 
#> $`Sulfate, total`

#> 
#> $`Temperature (Celsius)`
#> Warning: NaNs produced
#> Warning: log-10 transformation introduced infinite values.
#> Warning: NaNs produced
#> Warning: log-10 transformation introduced infinite values.
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).

#> 
#> $`Thallium, dissolved`

#> 
#> $`Tin, dissolved`
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?

#> 
#> $`Total Dissolved Solids`

#> 
#> $`Turbidity, field`
#> Warning: NaNs produced
#> Warning: log-10 transformation introduced infinite values.
#> Warning: NaNs produced
#> Warning: log-10 transformation introduced infinite values.
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_path()`).
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_point()`).

#> 
#> $`Uranium, dissolved`
#> Warning: log-10 transformation introduced infinite values.
#> Warning: log-10 transformation introduced infinite values.

#> 
#> $`Vanadium, dissolved`

#> 
#> $`Zinc, dissolved`

#> 
#> $`pH (field)`

#> 

series_plot(
  gw,
  background = as.POSIXct(
    c(
      "2010-01-01",
      "2012-12-31"
    )
  )
)
#> $`Alkalinity, total (lab)`

#> 
#> $`Aluminum, dissolved`

#> 
#> $`Antimony, dissolved`

#> 
#> $`Arsenic, dissolved`

#> 
#> $`Barium, dissolved`

#> 
#> $`Beryllium, dissolved`

#> 
#> $`Boron, dissolved`

#> 
#> $`Cadmium, dissolved`

#> 
#> $`Calcium, dissolved`

#> 
#> $`Chloride, total`

#> 
#> $`Chromium, dissolved`

#> 
#> $`Chromium, hexavalent`

#> 
#> $`Cobalt, Dis`

#> 
#> $`Copper, dissolved`

#> 
#> $`Dissolved Oxygen`
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?

#> 
#> $`Fluoride, total`

#> 
#> $`Groundwater Elevation (ref MSL)`

#> 
#> $`Groundwater, depth from TOC`

#> 
#> $`Hardness, total (mg/L as CaCO3)`

#> 
#> $`Iron, dissolved`

#> 
#> $`Lead, dissolved`

#> 
#> $`Magnesium, dissolved`

#> 
#> $`Manganese, dissolved`

#> 
#> $`Mercury, dissolved`

#> 
#> $`Mercury, total`

#> 
#> $`Molybdenum, dissolved`

#> 
#> $`Nickel, dissolved`

#> 
#> $`Nitrate nitrogen, total`

#> 
#> $`Nitrite + Nitrate, total`

#> 
#> $`Nitrite nitrogen, total`

#> 
#> $`Oxidation Reduction Potential`
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?

#> 
#> $`Phosphorus, total`

#> 
#> $`Potassium, dissolved`

#> 
#> $`Selenium, dissolved`

#> 
#> $SiO2

#> 
#> $`Silver, dissolved`

#> 
#> $`Sodium, dissolved`

#> 
#> $`Specific Conductance @ 25C (field)`
#> Warning: Removed 145 rows containing missing values or values outside the scale range
#> (`geom_path()`).
#> Warning: Removed 145 rows containing missing values or values outside the scale range
#> (`geom_point()`).

#> 
#> $`Strontium, dissolved`

#> 
#> $`Sulfate, total`

#> 
#> $`Temperature (Celsius)`

#> 
#> $`Thallium, dissolved`

#> 
#> $`Tin, dissolved`
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?

#> 
#> $`Total Dissolved Solids`

#> 
#> $`Turbidity, field`

#> 
#> $`Uranium, dissolved`

#> 
#> $`Vanadium, dissolved`

#> 
#> $`Zinc, dissolved`

#> 
#> $`pH (field)`

#> 

series_plot(
  gw,
  standards = TRUE
)
#> $`Alkalinity, total (lab)`

#> 
#> $`Aluminum, dissolved`

#> 
#> $`Antimony, dissolved`

#> 
#> $`Arsenic, dissolved`

#> 
#> $`Barium, dissolved`

#> 
#> $`Beryllium, dissolved`

#> 
#> $`Boron, dissolved`

#> 
#> $`Cadmium, dissolved`

#> 
#> $`Calcium, dissolved`

#> 
#> $`Chloride, total`

#> 
#> $`Chromium, dissolved`

#> 
#> $`Chromium, hexavalent`

#> 
#> $`Cobalt, Dis`

#> 
#> $`Copper, dissolved`

#> 
#> $`Dissolved Oxygen`
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?

#> 
#> $`Fluoride, total`

#> 
#> $`Groundwater Elevation (ref MSL)`

#> 
#> $`Groundwater, depth from TOC`

#> 
#> $`Hardness, total (mg/L as CaCO3)`

#> 
#> $`Iron, dissolved`

#> 
#> $`Lead, dissolved`

#> 
#> $`Magnesium, dissolved`

#> 
#> $`Manganese, dissolved`

#> 
#> $`Mercury, dissolved`

#> 
#> $`Mercury, total`

#> 
#> $`Molybdenum, dissolved`

#> 
#> $`Nickel, dissolved`

#> 
#> $`Nitrate nitrogen, total`

#> 
#> $`Nitrite + Nitrate, total`

#> 
#> $`Nitrite nitrogen, total`

#> 
#> $`Oxidation Reduction Potential`
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?

#> 
#> $`Phosphorus, total`

#> 
#> $`Potassium, dissolved`

#> 
#> $`Selenium, dissolved`

#> 
#> $SiO2

#> 
#> $`Silver, dissolved`

#> 
#> $`Sodium, dissolved`

#> 
#> $`Specific Conductance @ 25C (field)`
#> Warning: Removed 145 rows containing missing values or values outside the scale range
#> (`geom_path()`).
#> Warning: Removed 145 rows containing missing values or values outside the scale range
#> (`geom_point()`).

#> 
#> $`Strontium, dissolved`

#> 
#> $`Sulfate, total`

#> 
#> $`Temperature (Celsius)`

#> 
#> $`Thallium, dissolved`

#> 
#> $`Tin, dissolved`
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?

#> 
#> $`Total Dissolved Solids`

#> 
#> $`Turbidity, field`

#> 
#> $`Uranium, dissolved`

#> 
#> $`Vanadium, dissolved`

#> 
#> $`Zinc, dissolved`

#> 
#> $`pH (field)`

#> 
```
