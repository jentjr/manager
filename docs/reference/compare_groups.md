# Compare Analytical Results Among Groups

Performs multiple-group statistical comparisons on environmental
analytical results.

## Usage

``` r
compare_groups(df, ...)

# S3 method for class 'data.frame'
compare_groups(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  compare = location_id,
  keep_data_object = FALSE,
  ...
)

# S3 method for class 'grouped_df'
compare_groups(df, ...)

# S3 method for class 'env_schema'
compare_groups(
  df,
  compare = NULL,
  result_col = NULL,
  censored_col = NULL,
  keep_data_object = FALSE,
  ...
)

anova_test(df, ...)

# S3 method for class 'data.frame'
anova_test(
  df,
  location = "location_id",
  analyte = "param_name",
  sample_date = "sample_date",
  result = "analysis_result",
  unit = "default_unit",
  compare = location,
  keep_data_object = FALSE,
  ...
)

# S3 method for class 'env_schema'
anova_test(
  df,
  compare = location_var(df),
  result_col = analysis_result_var(df),
  censored_col = censored_var(df),
  keep_data_object = FALSE,
  ...
)

welch_test(df, ...)

# S3 method for class 'data.frame'
welch_test(
  df,
  location = "location_id",
  analyte = "param_name",
  sample_date = "sample_date",
  result = "analysis_result",
  unit = "default_unit",
  compare = location,
  keep_data_object = FALSE,
  ...
)

# S3 method for class 'env_schema'
welch_test(
  df,
  compare = location_var(df),
  result_col = analysis_result_var(df),
  censored_col = censored_var(df),
  keep_data_object = FALSE,
  ...
)

kruskal_test(df, ...)

# S3 method for class 'data.frame'
kruskal_test(
  df,
  location = "location_id",
  analyte = "param_name",
  sample_date = "sample_date",
  result = "analysis_result",
  unit = "default_unit",
  compare = location,
  keep_data_object = FALSE,
  ...
)

# S3 method for class 'env_schema'
kruskal_test(
  df,
  compare = location_var(df),
  result_col = analysis_result_var(df),
  censored_col = censored_var(df),
  keep_data_object = FALSE,
  ...
)
```

## Arguments

- df:

  A data frame or \`env_schema\` object.

- ...:

  Additional arguments passed to methods.

- compare:

  Character column name identifying the grouping factor to compare.

- keep_data_object:

  Logical indicating whether the complete statistical test object should
  be retained in the returned output.

- result_col:

  Analytical result column.

- variance_alpha:

  Significance level used by Levene's test to determine whether group
  variances are considered unequal. Defaults to \`0.05\`.

- levene_center:

  Centering method for Levene's test. \`"median"\` performs the
  Brown-Forsythe version and is the default. \`"mean"\` performs the
  classical mean-centered version.

## Value

A tibble containing one row per comparison group.

The returned table includes:

- comparison_method:

  Statistical procedure used.

- statistic:

  Test statistic returned by the selected procedure.

- p_value:

  P-value associated with the test statistic.

- test:

  Full statistical test object when \`keep_data_object = TRUE\`.

## Details

\`compare_groups()\` automatically selects a recommended comparison
procedure using \[derive_decision()\] and \[statistical_decision()\].

Recommended methods are based primarily on the degree of censoring:

- \`anova\` for low-censoring datasets

- \`kruskal\` for moderate or high censoring

- \`insufficient_data\` when too few observations are available

Explicit alternatives are available through:

- \[anova_test()\]

- \[welch_test()\]

- \[kruskal_test()\]

Existing grouping created with \[dplyr::group_by()\] is respected.
Comparisons are performed independently within each grouping level.

For ungrouped \`env_schema\` objects, comparisons are performed
separately for each analyte and analysis unit.

Automatic method selection follows the active \[statistical_decision()\]
framework.

Under the default EPA-style profile:

|                |               |                          |                |
|----------------|---------------|--------------------------|----------------|
| Condition      | Variance test | Recommendation           | Method         |
| \<15% censored | p \>= alpha   | Equal variance           | ANOVA          |
| \<15% censored | p \< alpha    | Unequal variance         | Welch          |
| 15-90%         | Not used      | Nonparametric comparison | Kruskal-Wallis |
| \>=90%         | Not used      | Insufficient information | No comparison  |

Comparisons require at least two distinct comparison groups.

If the comparison variable contains only one level, test statistics and
p-values are returned as \`NA\`.

The comparison variable cannot also be an active grouping variable.

Existing grouping controls the strata within which comparisons are
performed.

For example:

    gw |>
      dplyr::group_by(param_name) |>
      compare_groups(compare = "location_id")

compares locations separately within each analyte.

    gw |>
      dplyr::group_by(well_class) |>
      compare_groups(compare = "location_id")

compares locations separately within each well classification.

Under the default EPA-style guidance profile, \`compare_groups()\`
recommends classical ANOVA for low-censored datasets and Kruskal-Wallis
tests for datasets with moderate or high censoring.

Explicit method-selection functions are available for users who wish to
override the automatic recommendation framework:

- \[anova_test()\]

- \[welch_test()\]

- \[kruskal_test()\]

## ANOVA Method

\`anova_test()\` performs a classical one-way analysis of variance using
\[stats::aov()\].

This method assumes:

- approximately normal residuals

- homogeneous variances among groups

- independent observations

This function bypasses automatic method selection and always performs
ANOVA.

## Welch Method

\`welch_test()\` performs Welch's unequal-variance analysis of variance
using \[stats::oneway.test()\].

Welch's ANOVA relaxes the equal-variance assumption required by
classical ANOVA and is provided as an explicit analyst override.

This method is not selected automatically by \[compare_groups()\].

## Kruskal-Wallis Method

\`kruskal_test()\` performs a Kruskal-Wallis rank-sum test using
\[stats::kruskal.test()\].

This nonparametric method is appropriate when normality or variance
assumptions are not satisfied and is commonly recommended for datasets
with moderate or extensive censoring.

This function bypasses automatic method selection and always performs a
Kruskal-Wallis comparison.

## See also

\[statistical_decision()\], \[derive_decision()\], \[anova_test()\],
\[welch_test()\], \[kruskal_test()\]

## Examples

``` r
compare_groups(
  gw,
  compare = "location_id"
)

#> Error in dplyr::mutate(nested, decision = purrr::map(data, derive_decision,     result_col = result_col, censored_col = censored_col), comparison_method = if (identical(method,     "auto")) {    purrr::map_chr(decision, ~.x$comparison_method[[1L]])} else {    rep(method, dplyr::n())}, test = purrr::pmap(list(data, comparison_method), function(data,     selected_method) {    if (selected_method %in% c("cenanova", "cenperm2", "censored_nonparametric")) {        .run_censored_comparison_test(df = data, method = selected_method,             compare = compare, result_col = result_col, censored_col = censored_col)    }    else {        .run_comparison_test(df = data, method = selected_method,             compare = compare, result_col = result_col)    }})): ℹ In argument: `test = purrr::pmap(...)`.
#> ℹ In group 3: `param_name = "Antimony, dissolved"`, `.analysis_unit = "ug/l"`.
#> Caused by error in `purrr::pmap()`:
#> ℹ In index: 1.
#> Caused by error in `.run_comparison_test()` at manager/R/stats.R:3550:13:
#> ! Unsupported comparison method: `insufficient_data`.

anova_test(
  gw,
  compare = "location_id"
)
#> # A tibble: 49 × 5
#>    param_name           .analysis_unit comparison_method statistic   p_value
#>    <chr>                <chr>          <chr>                 <dbl>     <dbl>
#>  1 Magnesium, dissolved mg/l           anova              19.8     2.71 e-18
#>  2 Sodium, dissolved    mg/l           anova              45.5     4.70 e-33
#>  3 Potassium, dissolved mg/l           anova              58.2     2.21 e-38
#>  4 Chloride, total      mg/l           anova              45.2     5.25 e-33
#>  5 Sulfate, total       mg/l           anova             145.      5.47 e-61
#>  6 Fluoride, total      mg/l           anova               1.08    3.77 e- 1
#>  7 Arsenic, dissolved   ug/l           anova              24.0     2.83 e-21
#>  8 Barium, dissolved    ug/l           anova              82.8     1.28 e-46
#>  9 Beryllium, dissolved ug/l           anova               0.00605 1.000e+ 0
#> 10 Boron, dissolved     ug/l           anova             119.      9.03 e-56
#> # ℹ 39 more rows

welch_test(
  gw,
  compare = "location_id"
)
#> Error in dplyr::mutate(nested, decision = purrr::map(data, derive_decision,     result_col = result_col, censored_col = censored_col), comparison_method = if (identical(method,     "auto")) {    purrr::map_chr(decision, ~.x$comparison_method[[1L]])} else {    rep(method, dplyr::n())}, test = purrr::pmap(list(data, comparison_method), function(data,     selected_method) {    if (selected_method %in% c("cenanova", "cenperm2", "censored_nonparametric")) {        .run_censored_comparison_test(df = data, method = selected_method,             compare = compare, result_col = result_col, censored_col = censored_col)    }    else {        .run_comparison_test(df = data, method = selected_method,             compare = compare, result_col = result_col)    }})): ℹ In argument: `test = purrr::pmap(...)`.
#> ℹ In group 15: `param_name = "Dissolved Oxygen"`, `.analysis_unit = "mg/l"`.
#> Caused by error in `purrr::pmap()`:
#> ℹ In index: 1.
#> Caused by error in `stats::oneway.test()`:
#> ! not enough observations

kruskal_test(
  gw,
  compare = "location_id"
)
#> # A tibble: 49 × 5
#>    param_name           .analysis_unit comparison_method statistic   p_value
#>    <chr>                <chr>          <chr>                 <dbl>     <dbl>
#>  1 Magnesium, dissolved mg/l           kruskal             75.1    1.38 e-13
#>  2 Sodium, dissolved    mg/l           kruskal            113.     2.77 e-21
#>  3 Potassium, dissolved mg/l           kruskal            110.     8.74 e-21
#>  4 Chloride, total      mg/l           kruskal            100.     8.53 e-19
#>  5 Sulfate, total       mg/l           kruskal            121.     4.11 e-23
#>  6 Fluoride, total      mg/l           kruskal             40.3    1.08 e- 6
#>  7 Arsenic, dissolved   ug/l           kruskal             90.8    8.60 e-17
#>  8 Barium, dissolved    ug/l           kruskal            106.     6.21 e-20
#>  9 Beryllium, dissolved ug/l           kruskal              0.0445 1.000e+ 0
#> 10 Boron, dissolved     ug/l           kruskal            128.     1.79 e-24
#> # ℹ 39 more rows

gw |>
  dplyr::group_by(param_name) |>
  compare_groups(
    compare = "location_id"
  )
#> Error in dplyr::mutate(nested, decision = purrr::map(data, derive_decision,     result_col = result_col, censored_col = censored_col), comparison_method = if (identical(method,     "auto")) {    purrr::map_chr(decision, ~.x$comparison_method[[1L]])} else {    rep(method, dplyr::n())}, test = purrr::pmap(list(data, comparison_method), function(data,     selected_method) {    if (selected_method %in% c("cenanova", "cenperm2", "censored_nonparametric")) {        .run_censored_comparison_test(df = data, method = selected_method,             compare = compare, result_col = result_col, censored_col = censored_col)    }    else {        .run_comparison_test(df = data, method = selected_method,             compare = compare, result_col = result_col)    }})): ℹ In argument: `test = purrr::pmap(...)`.
#> ℹ In group 3: `param_name = "Antimony, dissolved"`.
#> Caused by error in `purrr::pmap()`:
#> ℹ In index: 1.
#> Caused by error in `.run_comparison_test()` at manager/R/stats.R:3550:13:
#> ! Unsupported comparison method: `insufficient_data`.

gw |>
  dplyr::group_by(well_class) |>
  compare_groups(
    compare = "location_id"
  )
#> Error in dplyr::group_by(tibble::as_tibble(.data), ..., .add = .add, .drop = .drop): Must group by variables found in `.data`.
#> ✖ Column `well_class` is not found.
```
