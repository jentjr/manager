# Box-Cox Transformation

Applies a Box-Cox transformation to analytical results within groups.

## Usage

``` r
boxcox_transform(df, ...)

# S3 method for class 'data.frame'
boxcox_transform(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  objective_name = "PPCC",
  result_col = analysis_result_var(df),
  keep_data_object = FALSE,
  ...
)

# S3 method for class 'env_schema'
boxcox_transform(
  df,
  result_col = analysis_result_var(df),
  objective_name = "PPCC",
  keep_data_object = FALSE,
  ...
)
```

## Arguments

- df:

  A data frame or \`env_schema\` object.

- ...:

  Additional arguments passed to methods.

- objective_name:

  Character string specifying the optimization criterion passed to
  \[EnvStats::boxcox()\].

- keep_data_object:

  Logical indicating whether the complete Box-Cox result object should
  be retained.

## Value

The original data with additional columns:

- .boxcox_transform:

  Box-Cox transformed analytical result.

- .boxcox_lambda:

  Estimated Box-Cox transformation parameter.

- boxcox:

  Full Box-Cox optimization result object when \`keep_data_object =
  TRUE\`.

## Details

Transformation parameters are estimated independently for each group
using \[EnvStats::boxcox()\]. Transformed values are calculated using
\[EnvStats::boxcoxTransform()\].

Existing analytical results are preserved. Transformed values are
returned in a new \`.boxcox_transform\` column together with the
estimated transformation parameter \`.boxcox_lambda\`.

If the input is grouped using \[dplyr::group_by()\], transformations are
performed within the existing groups.

For ungrouped \`env_schema\` objects, transformations are performed by:

- location

- analyte

- analysis unit

For ungrouped \`data.frame\` inputs, data are first converted to an
env_schema object and transformations are performed separately by
location, analyte, and analysis unit.

## env_schema Method

For \`env_schema\` objects, Box-Cox transformations are performed using
standardized analytical results stored in \`.analysis_result\`.

Existing \`dplyr::group_by()\` groups are preserved.

When the input is not grouped, transformations are performed by:

- location

- analyte

- analysis unit

Schema metadata are preserved.

## Examples

``` r
gw |>
  dplyr::group_by(
    location_id,
    param_name
  ) |>
  boxcox_transform()
#> Warning: There were 102 warnings in `dplyr::mutate()`.
#> The first warning was:
#> ℹ In argument: `boxcox = purrr::map(...)`.
#> ℹ In group 12: `location_id = "MW-1"`, `param_name = "Chromium, hexavalent"`.
#> Caused by warning in `cor()`:
#> ! the standard deviation is zero
#> ℹ Run dplyr::last_dplyr_warnings() to see the 101 remaining warnings.
#> # A tibble: 5,878 × 46
#> # Groups:   location_id, param_name [387]
#>    location_id param_name .source_analyte .normalized_input .analyte .analyte_id
#>  * <chr>       <chr>      <chr>           <chr>             <chr>    <chr>      
#>  1 MW-2        Magnesium… Magnesium, dis… magnesium dissol… Magnesi… magnesium  
#>  2 MW-2        Magnesium… Magnesium, dis… magnesium dissol… Magnesi… magnesium  
#>  3 MW-2        Magnesium… Magnesium, dis… magnesium dissol… Magnesi… magnesium  
#>  4 MW-2        Magnesium… Magnesium, dis… magnesium dissol… Magnesi… magnesium  
#>  5 MW-2        Magnesium… Magnesium, dis… magnesium dissol… Magnesi… magnesium  
#>  6 MW-2        Magnesium… Magnesium, dis… magnesium dissol… Magnesi… magnesium  
#>  7 MW-2        Magnesium… Magnesium, dis… magnesium dissol… Magnesi… magnesium  
#>  8 MW-2        Magnesium… Magnesium, dis… magnesium dissol… Magnesi… magnesium  
#>  9 MW-2        Magnesium… Magnesium, dis… magnesium dissol… Magnesi… magnesium  
#> 10 MW-2        Magnesium… Magnesium, dis… magnesium dissol… Magnesi… magnesium  
#> # ℹ 5,868 more rows
#> # ℹ 40 more variables: .normalized_analyte <chr>, .fraction <chr>,
#> #   .is_known_analyte <lgl>, sample_date <dttm>, analysis_result <dbl>,
#> #   lt_measure <chr>, default_unit <chr>, .result_type <chr>, .detected <lgl>,
#> #   .censored <lgl>, .estimated <lgl>, .rejected <lgl>, .usable <lgl>,
#> #   .original_lab_result <dbl>, .analysis_result <dbl>, .analysis_method <chr>,
#> #   .original_lab_unit <chr>, .analysis_unit <chr>, .short_name <chr>, …

gw |>
  boxcox_transform()
#> Warning: There were 102 warnings in `dplyr::mutate()`.
#> The first warning was:
#> ℹ In argument: `boxcox = purrr::map(...)`.
#> ℹ In group 12: `location_id = "MW-1"`, `param_name = "Chromium, hexavalent"`,
#>   `.analysis_unit = "ug/l"`.
#> Caused by warning in `cor()`:
#> ! the standard deviation is zero
#> ℹ Run dplyr::last_dplyr_warnings() to see the 101 remaining warnings.
#> # A tibble: 5,878 × 46
#>    location_id param_name       .analysis_unit .source_analyte .normalized_input
#>  * <chr>       <chr>            <chr>          <chr>           <chr>            
#>  1 MW-2        Magnesium, diss… mg/l           Magnesium, dis… magnesium dissol…
#>  2 MW-2        Magnesium, diss… mg/l           Magnesium, dis… magnesium dissol…
#>  3 MW-2        Magnesium, diss… mg/l           Magnesium, dis… magnesium dissol…
#>  4 MW-2        Magnesium, diss… mg/l           Magnesium, dis… magnesium dissol…
#>  5 MW-2        Magnesium, diss… mg/l           Magnesium, dis… magnesium dissol…
#>  6 MW-2        Magnesium, diss… mg/l           Magnesium, dis… magnesium dissol…
#>  7 MW-2        Magnesium, diss… mg/l           Magnesium, dis… magnesium dissol…
#>  8 MW-2        Magnesium, diss… mg/l           Magnesium, dis… magnesium dissol…
#>  9 MW-2        Magnesium, diss… mg/l           Magnesium, dis… magnesium dissol…
#> 10 MW-2        Magnesium, diss… mg/l           Magnesium, dis… magnesium dissol…
#> # ℹ 5,868 more rows
#> # ℹ 41 more variables: .analyte <chr>, .analyte_id <chr>,
#> #   .normalized_analyte <chr>, .fraction <chr>, .is_known_analyte <lgl>,
#> #   sample_date <dttm>, analysis_result <dbl>, lt_measure <chr>,
#> #   default_unit <chr>, .result_type <chr>, .detected <lgl>, .censored <lgl>,
#> #   .estimated <lgl>, .rejected <lgl>, .usable <lgl>,
#> #   .original_lab_result <dbl>, .analysis_result <dbl>, …
```
