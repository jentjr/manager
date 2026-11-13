# Environmental Plotting System

## Introduction

The plotting module provides a unified suite of visualization functions
for environmental monitoring datasets.

All plotting functions support both raw data frames and `env_schema`
objects through S3 method dispatch. The system is designed to operate
independently of source column names by relying on schema mappings and
accessor functions.

Supported visualizations include:

- Time-series plots
- Boxplots
- Correlation plots
- Piper diagrams
- Schoeller diagrams
- Stiff diagrams
- Durov diagrams
- Spatial plots
- Geochemical animations

## Design Philosophy

### Schema-Aware Architecture

All environmental plotting functions ultimately operate on an
`env_schema` object.

When a raw data frame is supplied, it is first converted using
user-provided column mappings.

For example:

``` r

series_plot(
  groundwater_data,
  location = well_id,
  sample_date = sample_date,
  analyte = constituent,
  result = concentration
)
```

internally becomes:

``` r

env_schema(
  groundwater_data,
  location = well_id,
  sample_date = sample_date,
  analyte = constituent,
  result = concentration
)
```

Internally, plotting methods interact with schema accessors rather than
hard-coded column names:

``` r

location_var()
sample_date_var()
analyte_var()
analysis_result_var()
analysis_unit_var()
reporting_flag_var()
```

This architecture ensures plots remain portable across different
environmental datasets.

## Example Datasets

The package includes groundwater monitoring datasets that can be used to
demonstrate plotting functionality.

### Standardized Example Dataset

The `gw` dataset is an `env_schema` object and can be used directly with
plotting functions.

``` r

library(dplyr)
library(manager)

data(gw)

gw
#> # A tibble: 5,878 × 45
#>    location_id param_name       sample_date         analysis_result default_unit
#>  * <chr>       <chr>            <dttm>                        <dbl> <chr>       
#>  1 MW-2        Magnesium, diss… 2008-10-28 00:00:00           36.3  mg/L        
#>  2 MW-2        Sodium, dissolv… 2008-10-28 00:00:00           18.9  mg/L        
#>  3 MW-2        Potassium, diss… 2008-10-28 00:00:00            0.5  mg/L        
#>  4 MW-2        Chloride, total  2008-10-28 00:00:00           24.9  mg/L        
#>  5 MW-2        Sulfate, total   2008-10-28 00:00:00          170    mg/L        
#>  6 MW-2        Fluoride, total  2008-10-28 00:00:00            0.29 mg/L        
#>  7 MW-2        Arsenic, dissol… 2008-10-28 00:00:00            1.9  ug/L        
#>  8 MW-2        Barium, dissolv… 2008-10-28 00:00:00          107    ug/L        
#>  9 MW-2        Beryllium, diss… 2008-10-28 00:00:00            0.5  ug/L        
#> 10 MW-2        Boron, dissolved 2008-10-28 00:00:00            2.34 mg/L        
#> # ℹ 5,868 more rows
#> # ℹ 40 more variables: lt_measure <chr>, .analysis_result <dbl>,
#> #   .analysis_unit <chr>, .censored <lgl>, .source_analyte <chr>,
#> #   .normalized_input <chr>, .analyte <chr>, .analyte_id <chr>,
#> #   .normalized_analyte <chr>, .fraction <chr>, .is_known_analyte <lgl>,
#> #   .result_type <chr>, .detected <lgl>, .estimated <lgl>, .rejected <lgl>,
#> #   .usable <lgl>, .original_lab_result <dbl>, .analysis_method <chr>, …
```

Because `gw` is already an `env_schema`, no additional mapping is
required.

``` r

plots <- series_plot(gw)

plots[[1]]
```

![](plotting_files/figure-html/gw-series-1.png)

### Raw Groundwater Dataset

The `gw_data` dataset contains environmental monitoring data in tabular
format.

``` r

data(gw_data)

glimpse(gw_data)
#> Rows: 5,878
#> Columns: 6
#> $ location_id     <chr> "MW-2", "MW-2", "MW-2", "MW-2", "MW-2", "MW-2", "MW-2"…
#> $ sample_date     <dttm> 2008-10-28, 2008-10-28, 2008-10-28, 2008-10-28, 2008-…
#> $ analysis_result <dbl> 36.300, 18.900, 0.500, 24.900, 170.000, 0.290, 1.900, …
#> $ lt_measure      <chr> NA, NA, NA, NA, NA, NA, NA, NA, "<", NA, "<", "<", NA,…
#> $ default_unit    <chr> "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "ug/L"…
#> $ param_name      <chr> "Magnesium, dissolved", "Sodium, dissolved", "Potassiu…
```

Create a smaller subset for examples.

``` r

example_wells <- c(
  "MW-1",
  "MW-2",
  "MW-3"
)

gw_subset <- gw_data %>%
  dplyr::filter(
    location_id %in% example_wells
  )
```

### Creating an env_schema Object

Raw datasets can be converted into an `env_schema` object.

``` r

gw_schema <- env_schema(
  gw_subset,
  location = location_id,
  sample_date = sample_date,
  analyte = param_name,
  result = analysis_result,
  unit = default_unit,
  reporting_flag = lt_measure
)

gw_schema
#> # A tibble: 2,372 × 45
#>    location_id param_name       sample_date         analysis_result default_unit
#>  * <chr>       <chr>            <dttm>                        <dbl> <chr>       
#>  1 MW-2        Magnesium, diss… 2008-10-28 00:00:00           36.3  mg/L        
#>  2 MW-2        Sodium, dissolv… 2008-10-28 00:00:00           18.9  mg/L        
#>  3 MW-2        Potassium, diss… 2008-10-28 00:00:00            0.5  mg/L        
#>  4 MW-2        Chloride, total  2008-10-28 00:00:00           24.9  mg/L        
#>  5 MW-2        Sulfate, total   2008-10-28 00:00:00          170    mg/L        
#>  6 MW-2        Fluoride, total  2008-10-28 00:00:00            0.29 mg/L        
#>  7 MW-2        Arsenic, dissol… 2008-10-28 00:00:00            1.9  ug/L        
#>  8 MW-2        Barium, dissolv… 2008-10-28 00:00:00          107    ug/L        
#>  9 MW-2        Beryllium, diss… 2008-10-28 00:00:00            0.5  ug/L        
#> 10 MW-2        Boron, dissolved 2008-10-28 00:00:00            2.34 mg/L        
#> # ℹ 2,362 more rows
#> # ℹ 40 more variables: lt_measure <chr>, .analysis_result <dbl>,
#> #   .analysis_unit <chr>, .censored <lgl>, .source_analyte <chr>,
#> #   .normalized_input <chr>, .analyte <chr>, .analyte_id <chr>,
#> #   .normalized_analyte <chr>, .fraction <chr>, .is_known_analyte <lgl>,
#> #   .result_type <chr>, .detected <lgl>, .estimated <lgl>, .rejected <lgl>,
#> #   .usable <lgl>, .original_lab_result <dbl>, .analysis_method <chr>, …
```

## Time Series Plots

### Basic Example

``` r

plots <- series_plot(gw)

plots[[1]]
```

![](plotting_files/figure-html/series-basic-1.png)

### Linear Trend

``` r

plots <- series_plot(
  gw,
  trend = "lm"
)

plots[[1]]
```

![](plotting_files/figure-html/series-lm-1.png)

### Mann-Kendall Trend

``` r

plots <- series_plot(
  gw,
  trend = "mann_kendall",
  trend_label = "compact"
)

plots[[1]]
```

### Combined Plot

``` r

gw |>
  filter(param_name %in% c("Chloride, total", "Sulfate, total", "Total Dissolved Solids")) |>
  series_plot(
    plot_type = "combined"
    )
```

![](plotting_files/figure-html/series-combined-1.png)

### Standards Overlay

``` r

gw |>
  filter(param_name == "Arsenic, dissolved") |>
  series_plot(
    standards = TRUE
    )
#> $`Arsenic, dissolved`
```

![](plotting_files/figure-html/series-standards-1.png)

## Boxplots

### Basic Boxplot

``` r

plots <- boxplot(gw)

plots[[1]]
```

![](plotting_files/figure-html/boxplot-basic-1.png)

### Display Individual Observations

``` r

plots <- boxplot(
  gw,
  show_points = TRUE
)

plots[[1]]
```

![](plotting_files/figure-html/boxplot-points-1.png)

### Bootstrap Confidence Intervals

``` r

plots <- boxplot(
  gw,
  show_points = FALSE,
  plot_ci = TRUE
)

plots[[1]]
```

![](plotting_files/figure-html/boxplot-ci-1.png)

### Standards Overlay

``` r

gw |>
  filter(param_name == "Arsenic, dissolved") |>
  boxplot(
    standards = TRUE
    )
#> $`Arsenic, dissolved`
```

![](plotting_files/figure-html/boxplot-standards-1.png)

### Add Explanation Panel

``` r

gw |>
  filter(param_name == "Arsenic, dissolved") |>
  boxplot(
    standards = TRUE,
    add_explanation = TRUE
    )
#> $`Arsenic, dissolved`
```

![](plotting_files/figure-html/boxplot-explanation-1.png)

## Correlation Plots

Correlation plots require the optional `GGally` package.

``` r

gw |>
  corr_plot(
  constituents = c(
    "Iron, dissolved",
    "Manganese, dissolved",
    "Sulfate, total"
  ),
  sample_locations = c(
    "MW-1",
    "MW-2"
  )
)
```

## Piper Diagrams

### Basic Piper Diagram

``` r

piper_plot(gw)
```

![](plotting_files/figure-html/piper-basic-1.png)

### Scale Point Size by TDS

``` r

piper_plot(
  gw,
  scale_analyte = "Total Dissolved Solids"
)
```

### Group by Category

``` r

gw |>
  mutate(
    aquifer_unit = case_when(
      location_id %in% c("MW-1", "MW-2", "MW-3") ~ "Sand",
      location_id %in% c("MW-4", "MW-5") ~ "Shale",
      .default = "Limestone"
    )
  ) |>
  piper_plot(
    group_col = "aquifer_unit"
    )
```

## Piper Animations

Create an interactive animation:

``` r

piper_time_plot(gw)
```

Export animation:

``` r

piper_time_html(gw)
```

## Schoeller Diagrams

### Basic Schoeller Plot

``` r

schoeller_plot(gw)
```

![](plotting_files/figure-html/schoeller-basic-1.png)

### Facet by Location

``` r

schoeller_plot(
  gw,
  facet_var = location_var(gw)
)
```

![](plotting_files/figure-html/schoeller-location-1.png)

### Facet by Date

``` r

schoeller_plot(
  gw,
  facet_var = sample_date_var(gw)
)
```

![](plotting_files/figure-html/schoeller-date-1.png)

### Custom Title

``` r

schoeller_plot(
  gw,
  title = "Groundwater Major-Ion Chemistry"
)
```

![](plotting_files/figure-html/schoeller-title-1.png)

## Stiff Diagrams

### Basic Stiff Diagram

``` r

plots <- stiff_plot(gw)

plots[[1]]
```

![](plotting_files/figure-html/stiff-basic-1.png)

### Scale Fill by TDS

``` r

plots <- stiff_plot(
  gw,
  scale_analyte = "Total Dissolved Solids"
)

plots[[1]]
```

### Add Reference Lines

``` r

plots <- stiff_plot(
  gw,
  lines = TRUE
)

plots[[1]]
```

![](plotting_files/figure-html/stiff-lines-1.png)

## Stiff Diagram Animations

Interactive display:

``` r

stiff_time_plot(gw)
```

HTML export:

``` r

stiff_time_html(gw)
```

## Durov Diagrams

### Basic Durov Diagram

``` r

durov_plot(gw)
```

![](plotting_files/figure-html/durov-basic-1.png)

### Normalized Matching

``` r

durov_plot(
  gw,
  analyte_matching = "normalized"
)
```

![](plotting_files/figure-html/durov-normalized-1.png)

### Exact Matching

``` r

durov_plot(
  gw,
  analyte_matching = "exact"
)
```

![](plotting_files/figure-html/durov-exact-1.png)

## Regulatory Standards

Several plotting functions support standards overlays.

Time-series example:

``` r

plots <- series_plot(
  gw,
  standards = TRUE
)

plots[[7]]
```

![](plotting_files/figure-html/standards-series-1.png)

Boxplot example:

``` r

plots <- boxplot(
  gw,
  standards = TRUE
)

plots[[7]]
```

![](plotting_files/figure-html/standards-boxplot-1.png)

## Animation Framework

The package includes shared animation helpers for geochemical plotting.

Animated workflows:

- [`piper_time_plot()`](https://jentjr.github.io/manager/reference/piper_time_plot.md)
- [`piper_time_html()`](https://jentjr.github.io/manager/reference/piper_time_html.md)
- [`stiff_time_plot()`](https://jentjr.github.io/manager/reference/stiff_time_plot.md)
- [`stiff_time_html()`](https://jentjr.github.io/manager/reference/stiff_time_html.md)

All animation utilities automatically determine the mapped date field
from the schema metadata.

## Extending the Plotting Framework

New visualization functions should follow the same S3 design pattern
used throughout the package.

### data.frame Method

``` r

my_plot.data.frame <- function(
  df,
  location,
  sample_date,
  analyte,
  result,
  ...
) {

  df <- env_schema(
    df,
    location = {{ location }},
    sample_date = {{ sample_date }},
    analyte = {{ analyte }},
    result = {{ result }}
  )

  my_plot(df, ...)
}
```

### env_schema Method

``` r

my_plot.env_schema <- function(
  df,
  ...
) {

  location_col <- location_var(df)

  sample_date_col <- sample_date_var(df)

  analyte_col <- analyte_var(df)

  result_col <- result_var(df)

  ...
}
```

Following this pattern ensures all future visualizations remain
independent of source column names while remaining fully compatible with
the package schema system.

## Summary

The environmental plotting framework provides:

- Schema-aware plotting
- Support for raw data frames and `env_schema` objects
- Consistent handling of analytical metadata
- Hydrochemical interpretation tools
- Animation support
- Extensible S3-based architecture

By leveraging schema accessors rather than hard-coded field names, plots
remain portable, reusable, and robust across diverse environmental
datasets.

## Session Information

``` r

sessionInfo()
#> R version 4.6.1 (2026-06-24 ucrt)
#> Platform: x86_64-w64-mingw32/x64
#> Running under: Windows 11 x64 (build 26100)
#> 
#> Matrix products: default
#>   LAPACK version 3.12.1
#> 
#> locale:
#> [1] LC_COLLATE=English_United States.utf8 
#> [2] LC_CTYPE=English_United States.utf8   
#> [3] LC_MONETARY=English_United States.utf8
#> [4] LC_NUMERIC=C                          
#> [5] LC_TIME=English_United States.utf8    
#> 
#> time zone: America/New_York
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] manager_0.5.0.9000 dplyr_1.2.1       
#> 
#> loaded via a namespace (and not attached):
#>  [1] gtable_0.3.6       beeswarm_0.4.0     xfun_0.62          bslib_0.12.0      
#>  [5] ggplot2_4.0.3      htmlwidgets_1.6.4  vctrs_0.7.3        tools_4.6.1       
#>  [9] generics_0.1.4     tibble_3.3.1       cluster_2.1.8.3    pkgconfig_2.0.3   
#> [13] data.table_1.18.99 checkmate_2.3.4    RColorBrewer_1.1-3 S7_0.2.2          
#> [17] desc_1.4.3         lifecycle_1.0.5    compiler_4.6.1     farver_2.1.2      
#> [21] stringr_1.6.0      textshaping_1.0.5  vipor_0.4.7        htmltools_0.5.9   
#> [25] sass_0.4.10        yaml_2.3.12        htmlTable_2.5.0    Formula_1.2-6     
#> [29] pillar_1.11.1      pkgdown_2.2.1      jquerylib_0.1.4    cachem_1.1.0      
#> [33] Hmisc_5.3-0        viridis_0.6.5      rpart_4.1.27       tidyselect_1.2.1  
#> [37] digest_0.6.39      stringi_1.8.9      purrr_1.2.2        labeling_0.4.3    
#> [41] cowplot_1.2.0      fastmap_1.2.0      grid_4.6.1         colorspace_2.1-4  
#> [45] cli_3.6.6          magrittr_2.0.5     base64enc_0.1-6    utf8_1.2.6        
#> [49] foreign_0.8-92     withr_3.0.3        scales_1.4.0       backports_1.5.1   
#> [53] ggbeeswarm_0.7.3   rmarkdown_2.32     otel_0.3.0         nnet_7.3-21       
#> [57] gridExtra_2.3.1    ragg_1.5.2         evaluate_1.0.5     knitr_1.52        
#> [61] EnvStats_3.1.0     viridisLite_0.4.3  rlang_1.3.0        Rcpp_1.1.2        
#> [65] glue_1.8.1         rstudioapi_0.19.0  jsonlite_2.0.0     R6_2.6.1          
#> [69] systemfonts_1.3.2  fs_2.1.0           units_1.0-1.6
```
