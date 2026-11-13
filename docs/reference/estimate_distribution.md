# Estimate Distributions Using Statistical Decision Guidance

Estimates an appropriate distributional model for each group using
censoring-aware statistical decision rules.

## Usage

``` r
estimate_distribution(
  df,
  distribution_config = "auto",
  result_col = analysis_result_var(df)
)
```

## Arguments

- df:

  Grouped data frame or \`env_schema\` object.

- distribution_config:

  Configuration list supplied to distribution fitting functions.

  If \`"auto"\` (the default), configuration settings are obtained from
  the group's \[statistical_decision()\] object.

## Value

A tibble containing one row per group.

The returned table includes statistical decision metadata together with
the distribution evaluation result.

- n_detect:

  Number of non-censored observations.

- percent_censored:

  Percentage of observations classified as censored.

- censoring_class:

  Censoring classification used for method selection.

- distribution_method:

  Recommended distribution estimation approach.

- trend_method:

  Recommended trend evaluation method.

- trend_guidance:

  Recommended interpretation of trend analyses.

- censored_method:

  Recommended censored-data handling method.

- distribution_config:

  Distribution-fitting configuration used during evaluation. This is a
  named list of arguments passed to \[EnvStats::distChoose()\] or
  \[EnvStats::distChooseCensored()\].

- interval_config:

  Recommended interval estimation settings.

- decision_basis:

  Basis used to select the statistical approach.

- distribution:

  Selected distribution or recommended distributional approach.

- n_used:

  Number of observations used during evaluation.

- result:

  Distribution fitting result object.

## Details

\`estimate_distribution()\` automatically evaluates the percentage of
censored observations and available sample size using
\[derive_decision()\]. A \[statistical_decision()\] object is then used
to select the recommended distribution estimation pathway for each
group.

Depending on the degree of censoring, groups are evaluated using:

- \`"parametric"\` distribution fitting

- \`"ros_censored"\` censored distribution fitting

- \`"nonparametric"\` distribution classification

- \`"descriptive_only"\` evaluation

- \`"insufficient_data"\` when too few observations are available

Distribution fitting is performed independently within each group and
requires the input data to already be grouped using
\[dplyr::group_by()\].

Method selection follows the active guidance profile used by
\[statistical_decision()\].

Under the default EPA-style guidance:

|                 |                   |                               |
|-----------------|-------------------|-------------------------------|
| Condition       | Recommendation    | Action                        |
| n_used \< 4     | insufficient_data | no distribution fitting       |
| \<15% censored  | parametric        | fit candidate distributions   |
| 15-50% censored | ros_censored      | censored distribution fitting |
| 50-90% censored | nonparametric     | classify as nonparametric     |
| \>=90% censored | descriptive_only  | descriptive evaluation only   |

Parametric evaluations use candidate distributions specified by
\`distribution_config\`, typically through \[EnvStats::distChoose()\].
By default, distribution fitting evaluates normal and lognormal
candidate distributions using the \`"sf"\` selection method.

Censored evaluations use \[EnvStats::distChooseCensored()\] with
left-censoring assumptions.

Nonparametric and descriptive-only groups do not perform formal
distribution fitting and instead return recommendation metadata for
downstream analyses.

## See also

\[statistical_decision()\], \[derive_decision()\],
\[guidance_profile()\], \[guidance_options()\]

## Examples

``` r
gw |>
  dplyr::group_by(
    location_id,
    param_name,
    default_unit
  ) |>
  estimate_distribution()
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: NA/NaN function evaluation
#> Warning: NA/NaN function evaluation
#> Warning: NA/NaN function evaluation
#> Warning: NA/NaN function evaluation
#> Warning: NA/NaN function evaluation
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> # A tibble: 387 × 18
#>    location_id param_name default_unit n_detect percent_censored censoring_class
#>    <chr>       <chr>      <chr>           <int>            <dbl> <chr>          
#>  1 MW-1        Alkalinit… mg/L               20              0   low_censoring  
#>  2 MW-1        Aluminum,… ug/L                6             68.4 high_censoring 
#>  3 MW-1        Antimony,… ug/L                3             85   high_censoring 
#>  4 MW-1        Arsenic, … ug/L               17             19.0 moderate_censo…
#>  5 MW-1        Barium, d… ug/L               20              0   low_censoring  
#>  6 MW-1        Beryllium… ug/L                0            100   extreme_censor…
#>  7 MW-1        Boron, di… mg/L               20              0   low_censoring  
#>  8 MW-1        Cadmium, … ug/L                1             95   extreme_censor…
#>  9 MW-1        Calcium, … mg/L               20              0   low_censoring  
#> 10 MW-1        Chloride,… mg/L               20              0   low_censoring  
#> # ℹ 377 more rows
#> # ℹ 12 more variables: distribution_method <chr>, trend_method <chr>,
#> #   trend_guidance <chr>, censored_method <chr>, distribution_config <list>,
#> #   interval_config <list>, decision_basis <chr>, comparison_method <chr>,
#> #   distribution <chr>, n_used <int>, result <list>, data <list>

gw |>
  dplyr::group_by(param_name) |>
  estimate_distribution()
#> # A tibble: 49 × 16
#>    param_name      n_detect percent_censored censoring_class distribution_method
#>    <chr>              <int>            <dbl> <chr>           <chr>              
#>  1 Alkalinity, to…      149            0     low_censoring   parametric         
#>  2 Aluminum, diss…       34           75.9   high_censoring  nonparametric      
#>  3 Antimony, diss…        5           96.6   extreme_censor… descriptive_only   
#>  4 Arsenic, disso…      126           15.4   moderate_censo… ros_censored       
#>  5 Barium, dissol…      147            0.676 low_censoring   parametric         
#>  6 Beryllium, dis…        0          100     extreme_censor… descriptive_only   
#>  7 Boron, dissolv…      147            0.676 low_censoring   parametric         
#>  8 Cadmium, disso…        7           95.3   extreme_censor… descriptive_only   
#>  9 Calcium, disso…      148            0     low_censoring   parametric         
#> 10 Chloride, total      149            0     low_censoring   parametric         
#> # ℹ 39 more rows
#> # ℹ 11 more variables: trend_method <chr>, trend_guidance <chr>,
#> #   censored_method <chr>, distribution_config <list>, interval_config <list>,
#> #   decision_basis <chr>, comparison_method <chr>, distribution <chr>,
#> #   n_used <int>, result <list>, data <list>

estimate_distribution(
  gw |>
    dplyr::group_by(
      location_id,
      param_name
    )
)
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: NA/NaN function evaluation
#> Warning: NA/NaN function evaluation
#> Warning: NA/NaN function evaluation
#> Warning: NA/NaN function evaluation
#> Warning: NA/NaN function evaluation
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> Warning: Too few or too many observations.  This approximation only works if the number of observations is between 20 and 5000.
#> # A tibble: 387 × 17
#>    location_id param_name              n_detect percent_censored censoring_class
#>    <chr>       <chr>                      <int>            <dbl> <chr>          
#>  1 MW-1        Alkalinity, total (lab)       20              0   low_censoring  
#>  2 MW-1        Aluminum, dissolved            6             68.4 high_censoring 
#>  3 MW-1        Antimony, dissolved            3             85   high_censoring 
#>  4 MW-1        Arsenic, dissolved            17             19.0 moderate_censo…
#>  5 MW-1        Barium, dissolved             20              0   low_censoring  
#>  6 MW-1        Beryllium, dissolved           0            100   extreme_censor…
#>  7 MW-1        Boron, dissolved              20              0   low_censoring  
#>  8 MW-1        Cadmium, dissolved             1             95   extreme_censor…
#>  9 MW-1        Calcium, dissolved            20              0   low_censoring  
#> 10 MW-1        Chloride, total               20              0   low_censoring  
#> # ℹ 377 more rows
#> # ℹ 12 more variables: distribution_method <chr>, trend_method <chr>,
#> #   trend_guidance <chr>, censored_method <chr>, distribution_config <list>,
#> #   interval_config <list>, decision_basis <chr>, comparison_method <chr>,
#> #   distribution <chr>, n_used <int>, result <list>, data <list>
```
