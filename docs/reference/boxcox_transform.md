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
#> Warning: There were 118 warnings in `dplyr::mutate()`.
#> The first warning was:
#> ℹ In argument: `boxcox = purrr::map(...)`.
#> ℹ In group 12: `location_id = "MW-1"`, `param_name = "Chromium, hexavalent"`.
#> Caused by warning in `cor()`:
#> ! the standard deviation is zero
#> ℹ Run dplyr::last_dplyr_warnings() to see the 117 remaining warnings.
#> # A tibble: 5,878 × 47
#> # Groups:   location_id, param_name [387]
#>    location_id param_name       sample_date         analysis_result default_unit
#>  * <chr>       <chr>            <dttm>                        <dbl> <chr>       
#>  1 MW-2        Magnesium, diss… 2008-10-28 00:00:00            36.3 mg/L        
#>  2 MW-2        Magnesium, diss… 2007-12-20 00:00:00            34   mg/L        
#>  3 MW-2        Magnesium, diss… 2008-12-16 00:00:00            29.7 mg/L        
#>  4 MW-2        Magnesium, diss… 2008-08-26 00:00:00            34.7 mg/L        
#>  5 MW-2        Magnesium, diss… 2008-06-18 00:00:00            34.8 mg/L        
#>  6 MW-2        Magnesium, diss… 2009-04-29 00:00:00            31.8 mg/L        
#>  7 MW-2        Magnesium, diss… 2008-04-24 00:00:00            30.5 mg/L        
#>  8 MW-2        Magnesium, diss… 2012-08-22 00:00:00            31.2 mg/L        
#>  9 MW-2        Magnesium, diss… 2009-02-25 00:00:00            33.8 mg/L        
#> 10 MW-2        Magnesium, diss… 2008-02-20 00:00:00            35.7 mg/L        
#> # ℹ 5,868 more rows
#> # ℹ 42 more variables: lt_measure <chr>, .analysis_result <dbl>,
#> #   .analysis_unit <chr>, .censored <lgl>, .source_analyte <chr>,
#> #   .normalized_input <chr>, .analyte <chr>, .analyte_id <chr>,
#> #   .normalized_analyte <chr>, .fraction <chr>, .is_known_analyte <lgl>,
#> #   .result_type <chr>, .detected <lgl>, .estimated <lgl>, .rejected <lgl>,
#> #   .usable <lgl>, .original_lab_result <dbl>, .analysis_method <chr>, …

gw |>
  boxcox_transform()
#> Warning: There were 118 warnings in `dplyr::mutate()`.
#> The first warning was:
#> ℹ In argument: `boxcox = purrr::map(...)`.
#> ℹ In group 12: `location_id = "MW-1"`, `param_name = "Chromium, hexavalent"`,
#>   `.analysis_unit = "ug/l"`.
#> Caused by warning in `cor()`:
#> ! the standard deviation is zero
#> ℹ Run dplyr::last_dplyr_warnings() to see the 117 remaining warnings.
#> # A tibble: 5,878 × 47
#>    location_id param_name     .analysis_unit sample_date         analysis_result
#>  * <chr>       <chr>          <chr>          <dttm>                        <dbl>
#>  1 MW-2        Magnesium, di… mg/l           2008-10-28 00:00:00            36.3
#>  2 MW-2        Magnesium, di… mg/l           2007-12-20 00:00:00            34  
#>  3 MW-2        Magnesium, di… mg/l           2008-12-16 00:00:00            29.7
#>  4 MW-2        Magnesium, di… mg/l           2008-08-26 00:00:00            34.7
#>  5 MW-2        Magnesium, di… mg/l           2008-06-18 00:00:00            34.8
#>  6 MW-2        Magnesium, di… mg/l           2009-04-29 00:00:00            31.8
#>  7 MW-2        Magnesium, di… mg/l           2008-04-24 00:00:00            30.5
#>  8 MW-2        Magnesium, di… mg/l           2012-08-22 00:00:00            31.2
#>  9 MW-2        Magnesium, di… mg/l           2009-02-25 00:00:00            33.8
#> 10 MW-2        Magnesium, di… mg/l           2008-02-20 00:00:00            35.7
#> # ℹ 5,868 more rows
#> # ℹ 42 more variables: default_unit <chr>, lt_measure <chr>,
#> #   .analysis_result <dbl>, .censored <lgl>, .source_analyte <chr>,
#> #   .normalized_input <chr>, .analyte <chr>, .analyte_id <chr>,
#> #   .normalized_analyte <chr>, .fraction <chr>, .is_known_analyte <lgl>,
#> #   .result_type <chr>, .detected <lgl>, .estimated <lgl>, .rejected <lgl>,
#> #   .usable <lgl>, .original_lab_result <dbl>, .analysis_method <chr>, …
```
