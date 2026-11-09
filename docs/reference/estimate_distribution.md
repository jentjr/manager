# Estimate Distributions Using Statistical Decision Guidance

Estimates an appropriate distributional model for each group using
censoring-aware statistical decision rules.

## Usage

``` r
estimate_distribution(df, distribution_config = "auto")
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
#> Error in df %>% dplyr::group_modify(~.estimate_distribution_group(.x,     distribution_config = distribution_config)) %>% dplyr::ungroup(): could not find function "%>%"

gw |>
  dplyr::group_by(param_name) |>
  estimate_distribution()
#> Error in df %>% dplyr::group_modify(~.estimate_distribution_group(.x,     distribution_config = distribution_config)) %>% dplyr::ungroup(): could not find function "%>%"

estimate_distribution(
  gw |>
    dplyr::group_by(
      location_id,
      param_name
    )
)
#> Error in df %>% dplyr::group_modify(~.estimate_distribution_group(.x,     distribution_config = distribution_config)) %>% dplyr::ungroup(): could not find function "%>%"
```
