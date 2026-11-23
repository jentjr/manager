# Statistical Decision Guidance

Selects recommended statistical methods based on sample size and the
degree of censoring.

## Usage

``` r
statistical_decision(
  percent_censored,
  n_used,
  n_detect,
  profile = guidance_options()
)
```

## Arguments

- percent_censored:

  Numeric percentage of censored observations.

- n_used:

  Number of non-missing observations available for statistical
  evaluation.

- n_detect:

  Number of detected (non-censored) observations.

- profile:

  A guidance profile created by \[guidance_profile()\].

## Value

A tibble with class \`"statistical_decision"\`.

The returned object contains:

- n_used:

  Number of analytical results used in evaluation.

- n_detect:

  Number of uncensored observations.

- percent_censored:

  Percentage of observations classified as censored.

- censoring_class:

  Censoring category: \`"low_censoring"\`, \`"moderate_censoring"\`,
  \`"high_censoring"\`, or \`"extreme_censoring"\`.

- distribution_method:

  Recommended distributional analysis approach.

- trend_method:

  Recommended trend evaluation method.

- trend_guidance:

  Recommended interpretation of trend results.

- censored_method:

  Recommended censored-data treatment method for trend analyses.

- distribution_config:

  Configuration list suitable for \[estimate_distribution()\].

- interval_config:

  Configuration list suitable for interval estimation functions.

- interval_method:

  Recommended interval method.

- decision_basis:

  Primary basis for the recommendation.

- comparison_method:

  Recommended multiple-group comparison procedure.

## Details

This function implements a rule-based statistical decision framework for
environmental monitoring data. Recommendations are based on the
percentage of censored observations and the number of observations
available for evaluation.

Returned guidance can be used to select:

- distribution estimation methods

- trend analysis approaches

- censored-data handling methods

- interval estimation settings

The default profile follows EPA-style guidance in which parametric
methods are preferred for low censoring, regression-on-order statistics
(ROS) is preferred for moderate censoring, and nonparametric methods are
preferred when censoring becomes extensive.

Distribution recommendations are assigned as follows:

|                    |                   |                               |
|--------------------|-------------------|-------------------------------|
| Condition          | Recommendation    | Method                        |
| n_used \< 4        | insufficient_data | no distribution fit           |
| low censoring      | parametric        | normal/lognormal evaluation   |
| moderate censoring | ros_censored      | censored distribution fitting |
| high censoring     | nonparametric     | nonparametric methods         |
| extreme censoring  | descriptive_only  | descriptive summaries only    |

Trend recommendations always use a Mann-Kendall framework, although
interpretation guidance may recommend screening-level evaluation when
censoring is extensive.

## See also

\[derive_decision()\], \[estimate_distribution()\],
\[guidance_profile()\], \[guidance_options()\]

## Examples

``` r
statistical_decision(
  percent_censored = 12,
  n_used = 20,
  n_detect = 18
)
#> <statistical_decision>
#> # A tibble: 1 × 13
#>   n_used n_detect percent_censored censoring_class    distribution_method
#>    <dbl>    <dbl>            <dbl> <chr>              <chr>              
#> 1     20       18               12 moderate_censoring ros_censored       
#> # ℹ 8 more variables: trend_method <chr>, trend_guidance <chr>,
#> #   censored_method <chr>, distribution_config <list>, interval_config <list>,
#> #   interval_method <chr>, comparison_method <chr>, decision_basis <chr>

statistical_decision(
  percent_censored = 75,
  n_used = 16,
  n_detect = 4
)
#> <statistical_decision>
#> # A tibble: 1 × 13
#>   n_used n_detect percent_censored censoring_class   distribution_method
#>    <dbl>    <dbl>            <dbl> <chr>             <chr>              
#> 1     16        4               75 extreme_censoring descriptive_only   
#> # ℹ 8 more variables: trend_method <chr>, trend_guidance <chr>,
#> #   censored_method <chr>, distribution_config <list>, interval_config <list>,
#> #   interval_method <chr>, comparison_method <chr>, decision_basis <chr>

statistical_decision(
  percent_censored = c(5, 25, 80),
  n_used = c(10, 10, 10),
  n_detect = c(9, 7, 2)
)
#> <statistical_decision>
#> # A tibble: 3 × 13
#>   n_used n_detect percent_censored censoring_class   distribution_method
#>    <dbl>    <dbl>            <dbl> <chr>             <chr>              
#> 1     10        9                5 low_censoring     parametric         
#> 2     10        7               25 high_censoring    nonparametric      
#> 3     10        2               80 extreme_censoring descriptive_only   
#> # ℹ 8 more variables: trend_method <chr>, trend_guidance <chr>,
#> #   censored_method <chr>, distribution_config <list>, interval_config <list>,
#> #   interval_method <chr>, comparison_method <chr>, decision_basis <chr>
```
