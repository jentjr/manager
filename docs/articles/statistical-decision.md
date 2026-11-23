# Statistical Decision Guidance and Automated Method Selection

## Introduction

Environmental monitoring datasets frequently contain censored
observations, limited sample sizes, and varying analytical detection
frequencies. These characteristics can substantially influence the
validity of distribution fitting, trend analysis, and interval
estimation procedures ([Helsel 2012](#ref-helsel2012); [Helsel et al.
2020](#ref-helsel2020)). Statistical methods that perform well under low
censoring may become unreliable when censoring is extensive.

The framework isinspired by recommendations commonly used in groundwater
monitoring programs and EPA statistical guidance documents ([United
States Environmental Protection Agency 2009](#ref-epa2009),
[1989](#ref-epa1989); [Interstate Technology and Regulatory Council
2013](#ref-itrc2013)).

The `manager` package includes a statistical decision framework that
automatically evaluates data quality and recommends appropriate
statistical methods for:

- Distribution estimation
- Trend analysis
- Censored-data handling
- Prediction intervals
- Confidence intervals
- Tolerance intervals

Decision recommendations are based on:

- Number of usable observations (`n_used`)
- Number of detected observations (`n_detect`)
- Percent censored (`percent_censored`)

The goal is to provide a repeatable and defensible framework for
selecting analytical methods across environmental monitoring programs.

## Guidance Profiles

Decision rules are controlled through predefined guidance profiles.

``` r

guidance_profile()
#> $low_threshold
#> [1] 15
#> 
#> $moderate_threshold
#> [1] 50
#> 
#> $high_threshold
#> [1] 90
```

The default profile is `"epa_unified"`. The default profile reflects
thresholds commonly encountered in EPA groundwater monitoring guidance
and environmental censored-data applications ([United States
Environmental Protection Agency 2009](#ref-epa2009); [Helsel
2012](#ref-helsel2012)).

``` r

guidance_profile("epa_unified")
#> $low_threshold
#> [1] 15
#> 
#> $moderate_threshold
#> [1] 50
#> 
#> $high_threshold
#> [1] 90
```

Available profiles include:

``` r

guidance_profile("conservative")
#> $low_threshold
#> [1] 10
#> 
#> $moderate_threshold
#> [1] 25
#> 
#> $high_threshold
#> [1] 75

guidance_profile("site_specific")
#> $low_threshold
#> [1] 20
#> 
#> $moderate_threshold
#> [1] 60
#> 
#> $high_threshold
#> [1] 90
```

### Profile Thresholds

| Profile       | Low | Moderate | High |
|---------------|-----|----------|------|
| epa_unified   | 15% | 50%      | 90%  |
| conservative  | 10% | 25%      | 75%  |
| site_specific | 20% | 60%      | 90%  |

These thresholds determine how censoring severity is classified and
which statistical methods are recommended.

## Statistical Decisions

A statistical decision object records method recommendations for a
dataset.

``` r

decision <- statistical_decision(
  percent_censored = 12,
  n_used = 20,
  n_detect = 18
)

decision
#> <statistical_decision>
#> # A tibble: 1 × 13
#>   n_used n_detect percent_censored censoring_class distribution_method
#>    <dbl>    <dbl>            <dbl> <chr>           <chr>              
#> 1     20       18               12 low_censoring   parametric         
#> # ℹ 8 more variables: trend_method <chr>, trend_guidance <chr>,
#> #   censored_method <chr>, distribution_config <list>, interval_config <list>,
#> #   interval_method <chr>, comparison_method <chr>, decision_basis <chr>
```

The resulting object contains:

``` r

names(decision)
#>  [1] "n_used"              "n_detect"            "percent_censored"   
#>  [4] "censoring_class"     "distribution_method" "trend_method"       
#>  [7] "trend_guidance"      "censored_method"     "distribution_config"
#> [10] "interval_config"     "interval_method"     "comparison_method"  
#> [13] "decision_basis"
```

Key recommendation fields include:

- `censoring_class`
- `distribution_method`
- `trend_method`
- `trend_guidance`
- `censored_method`
- `distribution_config`
- `interval_config`

## Censoring Classification

The degree of censoring is often the primary factor governing method
selection for environmental datasets. As censoring increases, parametric
distribution assumptions become more difficult to assess and trend
analyses may require censored-data techniques such as ROS or censored
Kendall methods ([Helsel 2012](#ref-helsel2012)).

The first stage of the decision framework is classification of censoring
severity.

``` r

decisions <- statistical_decision(
  percent_censored = c(5, 25, 75, 95),
  n_used = c(20, 20, 20, 20),
  n_detect = c(19, 15, 5, 1)
  )

decisions |>
  select(
    percent_censored,
    n_used,
    n_detect,
    censoring_class,
    distribution_method,
    trend_method,
    trend_guidance,
    censored_method,
    interval_method,
    comparison_method
    )
#> <statistical_decision>
#> # A tibble: 4 × 10
#>   percent_censored n_used n_detect censoring_class    distribution_method
#>              <dbl>  <dbl>    <dbl> <chr>              <chr>              
#> 1                5     20       19 low_censoring      parametric         
#> 2               25     20       15 moderate_censoring ros_censored       
#> 3               75     20        5 high_censoring     nonparametric      
#> 4               95     20        1 extreme_censoring  descriptive_only   
#> # ℹ 5 more variables: trend_method <chr>, trend_guidance <chr>,
#> #   censored_method <chr>, interval_method <chr>, comparison_method <chr>
```

Under the default EPA profile:

| Censored (%) | Classification     |
|--------------|--------------------|
| \<15         | low_censoring      |
| 15-50        | moderate_censoring |
| 50-90        | high_censoring     |
| \>=90        | extreme_censoring  |

## Distribution Recommendations

Distribution estimation recommendations depend on both censoring and
sample size.

### Decision Matrix

| Condition          | Recommendation    |
|--------------------|-------------------|
| n_used \< 4        | insufficient_data |
| Low censoring      | parametric        |
| Moderate censoring | ros_censored      |
| High censoring     | nonparametric     |
| Extreme censoring  | descriptive_only  |

Example:

``` r

decisions <- statistical_decision(
  percent_censored = c(5, 25, 75, 95),
  n_used = c(20, 20, 20, 20),
  n_detect = c(19, 15, 5, 1)
  )

decisions |>
  select(
    percent_censored,
    n_used,
    n_detect,
    censoring_class,
    distribution_method,
    trend_method,
    trend_guidance,
    censored_method,
    interval_method,
    comparison_method
    )
#> <statistical_decision>
#> # A tibble: 4 × 10
#>   percent_censored n_used n_detect censoring_class    distribution_method
#>              <dbl>  <dbl>    <dbl> <chr>              <chr>              
#> 1                5     20       19 low_censoring      parametric         
#> 2               25     20       15 moderate_censoring ros_censored       
#> 3               75     20        5 high_censoring     nonparametric      
#> 4               95     20        1 extreme_censoring  descriptive_only   
#> # ℹ 5 more variables: trend_method <chr>, trend_guidance <chr>,
#> #   censored_method <chr>, interval_method <chr>, comparison_method <chr>
```

The progression from parametric methods to censored-data methods and
eventually nonparametric approaches is consistent with recommendations
described by Helsel ([Helsel 2012](#ref-helsel2012)) and EPA groundwater
guidance ([United States Environmental Protection Agency
2009](#ref-epa2009)).

### Parametric Recommendation

Low-censoring datasets are evaluated using conventional distribution
fitting.Parametric model selection is performed using
[`EnvStats::distChoose()`](https://alexkowa.github.io/EnvStats/reference/distChoose.html),
which evaluates candidate probability distributions using
goodness-of-fit procedures described in Millard ([Millard and Kowarik
2024](#ref-envstats)).

``` r

statistical_decision(
  percent_censored = 5,
  n_used = 15,
  n_detect = 14
) |>
  select(
    distribution_method,
    distribution_config
  )
#> <statistical_decision>
#> # A tibble: 1 × 2
#>   distribution_method distribution_config
#>   <chr>               <list>             
#> 1 parametric          <named list [2]>
```

The default configuration evaluates:

- Normal
- Lognormal

distributions using
[`EnvStats::distChoose()`](https://alexkowa.github.io/EnvStats/reference/distChoose.html).

### ROS-Censored Recommendation

Moderately censored datasets are evaluated using censored distribution
fitting. Regression on Order Statistics (ROS) is among the most widely
used approaches for estimating distributions from moderately censored
environmental datasets ([Helsel 2012](#ref-helsel2012)).

``` r

statistical_decision(
  percent_censored = 35,
  n_used = 20,
  n_detect = 13
) |>
  select(
    distribution_method
  )
#> <statistical_decision>
#> # A tibble: 1 × 1
#>   distribution_method
#>   <chr>              
#> 1 ros_censored
```

This recommendation is implemented using:

``` r

EnvStats::distChooseCensored()
```

with left-censoring assumptions.

### Nonparametric Recommendation

Highly censored datasets are classified as nonparametric. When censoring
becomes extensive, distributional assumptions may no longer be
adequately supported and nonparametric methods are generally preferred
([Helsel 2012](#ref-helsel2012); [United States Environmental Protection
Agency 2009](#ref-epa2009)).

``` r

statistical_decision(
  percent_censored = 75,
  n_used = 20,
  n_detect = 5
) |>
  select(
    distribution_method
  )
#> <statistical_decision>
#> # A tibble: 1 × 1
#>   distribution_method
#>   <chr>              
#> 1 nonparametric
```

Formal distribution fitting is not attempted.

### Descriptive-Only Recommendation

Extremely censored datasets are limited to descriptive evaluation.

``` r

statistical_decision(
  percent_censored = 95,
  n_used = 20,
  n_detect = 1
) |>
  select(
    distribution_method
  )
#> <statistical_decision>
#> # A tibble: 1 × 1
#>   distribution_method
#>   <chr>              
#> 1 descriptive_only
```

## Trend Recommendations

Trend evaluations are based on a Mann-Kendall framework, a widely used
nonparametric approach for environmental time-series analysis ([Helsel
et al. 2020](#ref-helsel2020); [Gilbert 1987](#ref-gilbert1987)).
Mann-Kendall methods are particularly attractive for groundwater
monitoring because they do not require the assumption of normally
distributed concentrations and are robust to skewed environmental data
([Gilbert 1987](#ref-gilbert1987)).

``` r

decision <- statistical_decision(
  percent_censored = 25,
  n_used = 20,
  n_detect = 15
)

decision |>
  select(
    trend_method,
    trend_guidance,
    censored_method
  )
#> <statistical_decision>
#> # A tibble: 1 × 3
#>   trend_method trend_guidance    censored_method
#>   <chr>        <chr>             <chr>          
#> 1 mann_kendall formal_trend_test ros
```

### Trend Guidance Categories

Trend recommendations use a Mann-Kendall framework whenever enough
observations are available.

#### Formal Trend Testing

For datasets with at least five usable observations and low or moderate
censoring, the framework returns:

``` text
trend_method = "mann_kendall"
trend_guidance = "formal_trend_test"
```

is recommended.

#### Screening-Only Evaluation

For high or extreme censoring, the Mann-Kendall framework is still
selected, but interpretation is limited to screening-level evaluation::

``` text
trend_method = "mann_kendall"
trend_guidance = "screening_only"
```

is returned.

#### Insufficient Detects

When fewer than five usable observations are available:

``` text
trend_method = "insufficient_data"
trend_guidance = "insufficient_data"
```

is returned.

``` r

statistical_decision(
  percent_censored = 90,
  n_used = 20,
  n_detect = 2
  ) |>
  select(
    n_used,
    n_detect,
    trend_method,
    trend_guidance, 
    comparison_method
    )
#> <statistical_decision>
#> # A tibble: 1 × 5
#>   n_used n_detect trend_method trend_guidance comparison_method
#>    <dbl>    <dbl> <chr>        <chr>          <chr>            
#> 1     20        2 mann_kendall screening_only insufficient_data
```

Expected

    trend_method       = "mann_kendall"
    trend_guidance     = "screening_only"
    comparison_method  = "insufficient_data"

The number of detected observations is used separately when selecting
the recommended comparison method. It does not independently create an
insufficient_detects trend category.

## Censored-Data Handling Recommendations

The decision object includes a `censored_method` field describing the
recommended treatment of censored observations.

``` r

statistical_decision(
  percent_censored = c(5, 40, 75, 95),
  n_used = c(20, 20, 20, 20),
  n_detect = c(19, 12, 5, 1)
) |>
  select(
    percent_censored,
    censoring_class,
    censored_method
  )
#> <statistical_decision>
#> # A tibble: 4 × 3
#>   percent_censored censoring_class    censored_method 
#>              <dbl> <chr>              <chr>           
#> 1                5 low_censoring      standard        
#> 2               40 moderate_censoring ros             
#> 3               75 high_censoring     nonparametric   
#> 4               95 extreme_censoring  descriptive_only
```

For censored environmental datasets, ordinary Mann-Kendall analyses may
not adequately account for reporting limits. In these situations,
censored Kendall methods such as
[`NADA2::cenken()`](https://rdrr.io/pkg/NADA2/man/cenken.html) are often
recommended ([Helsel 2012](#ref-helsel2012); [Julian et al.
2024](#ref-nada2)).

The current recommendations are:

| Censoring class    |
|--------------------|
| Low censoring      |
| Moderate censoring |
| High censoring     |
| Extreme censoring  |

These recommendations are automatically used by functions such as:

``` r

trend_summary(
  gw,
  censored_method = "auto"
)
#> # A tibble: 387 × 22
#>    location_id param_name              n first_date          last_date          
#>    <chr>       <chr>               <int> <dttm>              <dttm>             
#>  1 MW-1        Alkalinity, total …    20 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  2 MW-1        Aluminum, dissolved    19 2008-02-20 00:00:00 2013-10-02 00:00:00
#>  3 MW-1        Antimony, dissolved    20 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  4 MW-1        Arsenic, dissolved     21 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  5 MW-1        Barium, dissolved      20 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  6 MW-1        Beryllium, dissolv…    20 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  7 MW-1        Boron, dissolved       20 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  8 MW-1        Cadmium, dissolved     20 2007-12-20 00:00:00 2013-10-02 00:00:00
#>  9 MW-1        Calcium, dissolved     20 2007-12-20 00:00:00 2013-10-02 00:00:00
#> 10 MW-1        Chloride, total        20 2007-12-20 00:00:00 2013-10-02 00:00:00
#> # ℹ 377 more rows
#> # ℹ 17 more variables: date_range_days <dbl>, date_range_years <dbl>,
#> #   sampling_frequency <dbl>, minimum_concentration <dbl>,
#> #   median_concentration <dbl>, mean_concentration <dbl>,
#> #   maximum_concentration <dbl>, standard_deviation <dbl>,
#> #   coefficient_of_variation <dbl>, trend_method <chr>, censored_method <chr>,
#> #   slope <dbl>, p_value <dbl>, conf_low <dbl>, conf_high <dbl>, …
```

## Deriving Decisions from Data

Use
[`derive_decision()`](https://jentjr.github.io/manager/reference/derive_decision.md)
to calculate censoring metrics directly from a data frame.

For ordinary data frames, the default columns are:

- `analysis_result`: numeric analytical result;
- `censored`: logical censoring indicator.

A value of `TRUE` indicates a censored observation and `FALSE` indicates
a detected observation.

``` r

gw_example <- data.frame(
  analysis_result = c(
    1.2, 0.8, 0.5, 0.3, 0.2,
    0.1, 0.4, 0.7, 1.1, 0.9
  ),
  censored = c(
    FALSE, FALSE, FALSE, TRUE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  )
)

decision <- derive_decision(gw_example)

decision |>
  select(
    n_used,
    n_detect,
    percent_censored,
    censoring_class,
    distribution_method,
    trend_method,
    trend_guidance
  )
#> <statistical_decision>
#> # A tibble: 1 × 7
#>   n_used n_detect percent_censored censoring_class distribution_method
#>    <int>    <int>            <dbl> <chr>           <chr>              
#> 1     10        9               10 low_censoring   parametric         
#> # ℹ 2 more variables: trend_method <chr>, trend_guidance <chr>
```

This approach allows data characteristics rather than analyst preference
to determine the recommended statistical workflow, improving
reproducibility and consistency across monitoring programs ([United
States Environmental Protection Agency 2009](#ref-epa2009)).

``` r

decision <- derive_decision(gw)

decision
```

[`derive_decision()`](https://jentjr.github.io/manager/reference/derive_decision.md)
calculates:

- `n_used`
- `n_detect`
- `percent_censored`

and passes those values into
[`statistical_decision()`](https://jentjr.github.io/manager/reference/statistical_decision.md).
Rows with missing analytical results or missing censoring indicators are
excluded from these calculations.

### Custom Column Names

Use `result_col` and `censored_col` when the data does not use the
default column names.

``` r

custom_data <- data.frame(
  concentration = c(1.2, 0.8, 0.5, 0.3), 
  is_censored = c(FALSE, FALSE, TRUE, FALSE)
  )

derive_decision( 
  custom_data,
  result_col = "concentration",
  censored_col = "is_censored"
  )
```

The result column must be numeric and the censoring column must be
logical. Missing required columns, invalid column types, and missing
column names produce informative errors.

### Grouped Data

[`derive_decision()`](https://jentjr.github.io/manager/reference/derive_decision.md)
supports grouped data frames and returns one decision for each group.

``` r

grouped_data <- data.frame(
  location = c("A", "A", "A", "B", "B", "B"),
  analysis_result = c(1.2, 0.8, 0.3, 0.2, 0.1, 0.4),
  censored = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
  ) |>
  group_by(location)

grouped_decisions <- derive_decision(grouped_data)

grouped_decisions |>
  select(
    location,
    n_used,
    n_detect,
    percent_censored,
    censoring_class,
    distribution_method
    )
#> # A tibble: 2 × 6
#> # Groups:   location [2]
#>   location n_used n_detect percent_censored censoring_class  distribution_method
#>   <chr>     <int>    <int>            <dbl> <chr>            <chr>              
#> 1 A             3        2             33.3 moderate_censor… insufficient_data  
#> 2 B             3        1             66.7 high_censoring   insufficient_data
```

For env_schema objects, standardized analysis and censoring variables
are resolved through analysis_result_var() and censored_var().

## Distribution Estimation Workflow

[`estimate_distribution()`](https://jentjr.github.io/manager/reference/estimate_distribution.md)
uses the statistical decision framework to select a
distribution-estimation pathway for each group.

The input must be grouped with
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

Distribution estimation relies on methods implemented in the `EnvStats`
package ([Millard and Kowarik 2024](#ref-envstats)), which contains many
of the statistical procedures described in EPA groundwater monitoring
guidance([United States Environmental Protection Agency
2009](#ref-epa2009)).

``` r

gw |>
  filter(
    location_id == "MW-2", 
    param_name %in% c(
      "Chloride, total", 
      "Sulfate, total",
      "Selenium, dissolved",
      "Copper, dissolved"
      )
    ) |>
  group_by(
    location_id,
    param_name,
    default_unit
    ) |>
  estimate_distribution() |>
  select(
    location_id, 
    param_name,
    n_detect,
    percent_censored,
    decision_basis,
    distribution
  )
```

With the default distribution_config = “auto”:

1.  analytical columns are resolved;
2.  censoring metrics are calculated with derive_decision();
3.  a statistical decision is created;
4.  the recommended distribution pathway is selected; and
5.  results and decision metadata are returned.

Calling
[`estimate_distribution()`](https://jentjr.github.io/manager/reference/estimate_distribution.md)
on an ungrouped data frame produces an error. Group the data before
calling the function.

## Automatic Method Selection

Under the default EPA profile:

| Percent Censored     | Classification       | Recommendation     |
|----------------------|----------------------|--------------------|
| `< 15%`              | `low_censoring`      | `parametric`       |
| `>= 15%` and `< 50%` | `moderate_censoring` | `ros_censored`     |
| `>= 50%` and `< 90%` | `high_censoring`     | `nonparametric`    |
| `>=90%`              | `extreme_censoring`  | `descriptive_only` |

A separate minimum-sample-size rule takes precedence: fewer than four
usable observations results in `insufficient_data`.

Example:

``` r

gw |>
  filter(
    location_id == "MW-1", 
    param_name %in% c(
      "Chloride, total", 
      "Sulfate, total",
      "Tin, dissolved"
      )
    ) |>
  group_by(
    location_id,
    param_name,
    default_unit
    ) |>
  estimate_distribution() |>
  select(
    location_id, 
    param_name,
    n_detect,
    percent_censored,
    decision_basis,
    distribution
  )
```

The method-selection process occurs automatically.

## Distribution Configuration

Each decision includes a recommended configuration object.

``` r

decision <- statistical_decision(
  percent_censored = 10,
  n_used = 20,
  n_detect = 18
)

decision$distribution_config[[1]]
#> $method
#> [1] "sf"
#> 
#> $choices
#> [1] "norm"  "lnorm"
```

Default settings:

``` r

list(
  method = "sf",
  choices = c(
    "norm",
    "lnorm"
  )
)
```

These settings are automatically supplied to:

``` r

EnvStats::distChoose()
```

or

``` r

EnvStats::distChooseCensored()
```

depending on the decision outcome.

## Interval Configuration

The decision object includes interval-estimation guidance for methods
supported by the selected distributional approach.

``` r

decision <- statistical_decision(
  percent_censored = 10,
  n_used = 20,
  n_detect = 18
  )

decision |>
  select(
    interval_method,
    interval_config
    )
#> <statistical_decision>
#> # A tibble: 1 × 2
#>   interval_method interval_config 
#>   <chr>           <list>          
#> 1 parametric      <named list [4]>

decision$interval_config
#> [[1]]
#> [[1]]$conf_level
#> [1] 0.95
#> 
#> [[1]]$method
#> [1] "Bonferroni"
#> 
#> [[1]]$k
#> [1] 1
#> 
#> [[1]]$n_mean
#> [1] 1
```

For parametric, ROS-censored, and nonparametric recommendations, the
default configuration is:

``` r

list(
  conf_level = 0.95,
  method = "Bonferroni",
  k = 1L,
  n_mean = 1L
)
```

For insufficient-data and descriptive-only recommendations, the interval
configuration is an empty list.

These values are configuration metadata intended for interval-estimation
functions. The decision function does not execute interval estimation by
itself.

## Prediction Intervals

Prediction intervals are commonly used in groundwater monitoring to
establish concentration thresholds expected to contain future
observations with a specified degree of confidence ([United States
Environmental Protection Agency 2009](#ref-epa2009); [Gilbert
1987](#ref-gilbert1987)).

Prediction intervals require a distribution classification.

A common workflow is:

``` r

dist_est <- gw |>
  filter(
    location_id == "MW-2", 
    param_name %in% c(
      "Chloride, total", 
      "Sulfate, total",
      "Copper, dissolved"
      )
    ) |>
  group_by(
    location_id,
    param_name,
    default_unit
    ) |>
  estimate_distribution()
```

Standard prediction intervals:

``` r

pred_int(dist_est)
```

Lower prediction intervals:

``` r

pred_int(
  dist_est,
  pi_type = "lower",
  conf_level = 0.99
)
```

Simultaneous prediction intervals:

``` r

pred_int_sim(
  dist_est,
  pi_type = "upper",
  conf_level = 0.99
)
```

## Summary Statistics with Decision Guidance

[`stats_summary()`](https://jentjr.github.io/manager/reference/stats_summary.md)
automatically integrates statistical decision outputs. Incorporating
statistical decision metadata directly into summary tables helps
document methodological decisions and provides a transparent audit trail
for regulatory and technical review ([United States Environmental
Protection Agency 2009](#ref-epa2009); [Interstate Technology and
Regulatory Council 2013](#ref-itrc2013)).

``` r

gw |>
  group_by(
    location_id,
    param_name
  ) |>
  stats_summary()
```

Additional fields include:

``` text
censoring_class
distribution_method
trend_method
trend_guidance
censored_method
decision_basis
```

This allows routine reports to include recommended statistical
approaches directly within summary tables.

## Custom Guidance Thresholds

A profile can be supplied directly to a decision:

``` r

statistical_decision(
  percent_censored = 25,
  n_used = 20,
  n_detect = 15,
  profile = guidance_profile("conservative")
)
#> <statistical_decision>
#> # A tibble: 1 × 13
#>   n_used n_detect percent_censored censoring_class distribution_method
#>    <dbl>    <dbl>            <dbl> <chr>           <chr>              
#> 1     20       15               25 high_censoring  nonparametric      
#> # ℹ 8 more variables: trend_method <chr>, trend_guidance <chr>,
#> #   censored_method <chr>, distribution_config <list>, interval_config <list>,
#> #   interval_method <chr>, comparison_method <chr>, decision_basis <chr>
```

The active package option can also be changed:

``` r

old_guidance <- getOption("manager.guidance")

options(
manager.guidance = guidance_profile("conservative")
)

guidance_options()
```

Restore the previous setting after the example:

``` r

if (is.null(old_guidance)) {
options(manager.guidance = NULL)
} else {
options(manager.guidance = old_guidance)
}
```

For reproducible analyses, passing profile explicitly is preferable to
depending on a mutable global option.

Retrieve the active settings:

``` r

guidance_options()
#> $low_threshold
#> [1] 15
#> 
#> $moderate_threshold
#> [1] 50
#> 
#> $high_threshold
#> [1] 90
```

Decisions derived afterward will use the updated thresholds.

## Analysis Plans

Use
[`analysis_plan()`](https://jentjr.github.io/manager/reference/analysis_plan.md)
to define the objective and required analysis fields before running a
statistical procedure.

``` r

distribution_plan <- analysis_plan(
objective = "distribution",
response = "analysis_result",
censored = "censored"
)

distribution_plan
#> $objective
#> [1] "distribution"
#> 
#> $response
#> [1] "analysis_result"
#> 
#> $censored
#> [1] "censored"
#> 
#> $date
#> NULL
#> 
#> $group
#> NULL
#> 
#> $comparison
#> NULL
#> 
#> $method
#> NULL
#> 
#> $alpha
#> [1] 0.05
#> 
#> $confidence_level
#> [1] 0.95
#> 
#> $censoring_method
#> NULL
#> 
#> $missing_policy
#> [1] "exclude_required_fields"
#> 
#> $guidance
#> $guidance$low_threshold
#> [1] 15
#> 
#> $guidance$moderate_threshold
#> [1] 50
#> 
#> $guidance$high_threshold
#> [1] 90
#> 
#> 
#> $parameters
#> list()
#> 
#> attr(,"class")
#> [1] "statistical_analysis_plan"
```

Trend analyses require a date column:

``` r

trend_plan <- analysis_plan(
objective = "trend",
response = "analysis_result",
censored = "censored",
date = "sample_date"
)

trend_plan
```

Comparison analyses require a comparison column:

``` r

comparison_plan <- analysis_plan(
objective = "comparison",
response = "analysis_result",
censored = "censored",
comparison = "location"
)

comparison_plan
```

Plans are validated when created. Use
[`validate_analysis_plan()`](https://jentjr.github.io/manager/reference/validate_analysis_plan.md)
to validate an existing compatible list:

``` r
validate_analysis_plan(distribution_plan)`
```

The validator checks the objective, response column, error and
confidence levels, guidance thresholds, and objective-specific required
fields.

## Example Workflow

A typical environmental monitoring workflow may look like:

``` r

gw |>
  filter(
    location_id == "MW-2", 
    param_name %in% c(
      "Chloride, total", 
      "Sulfate, total",
      "Copper, dissolved"
      )
    ) |>
  group_by(
    location_id,
    param_name,
    default_unit
    ) |>
  estimate_distribution() |>
  pred_int(
    pi_type = "upper",
    conf_level = 0.95
  )
```

Or for sitewide reporting:

``` r

gw |>
  filter(
    location_id == "MW-2", 
    param_name %in% c(
      "Chloride, total", 
      "Sulfate, total",
      "Copper, dissolved"
      )
    ) |>
  group_by(
    location_id,
    param_name,
    default_unit
    ) |>
  stats_summary()
```

The decision framework automatically:

- Evaluates data sufficiency
- Quantifies censoring
- Selects recommended methods
- Configures distribution fitting
- Recommends trend approaches
- Provides interval defaults
- Documents the decision basis

This approach promotes reproducible, transparent, and defensible
statistical analyses for environmental monitoring programs.

## References

The statistical decision framework implemented in `manager` draws upon
commonly accepted approaches for environmental monitoring data,
including censored-data analysis, distribution fitting, trend
evaluation, and interval estimation.

### Regulatory and Guidance Documents

United States Environmental Protection Agency (EPA). 2009. *Statistical
Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified
Guidance*. EPA 530/R-09-007. Washington, DC.

United States Environmental Protection Agency (EPA). 1989. *Statistical
Analysis of Ground-Water Monitoring Data at RCRA Facilities: Interim
Final Guidance*. EPA/530-SW-89-026.

Interstate Technology & Regulatory Council (ITRC). 2013. *Groundwater
Statistics and Monitoring Compliance (GSMC-1)*. Washington, DC.

### Statistical Methods

Helsel, D.R. (2012). *Statistics for Censored Environmental Data Using
Minitab and R*, Second Edition. John Wiley & Sons, Hoboken, NJ.

Helsel, D.R., Hirsch, R.M., Ryberg, K.R., Archfield, S.A., and Gilroy,
E.J. (2020). *Statistical Methods in Water Resources*. U.S. Geological
Survey Techniques and Methods, Book 4, Chapter A3.

Gilbert, R.O. (1987). *Statistical Methods for Environmental Pollution
Monitoring*. Van Nostrand Reinhold, New York.

### R Packages

Millard, S.P. and Kowarik, A. (Current Version). *EnvStats: Package for
Environmental Statistics*. R package.

Julian, J. and Helsel, D.R. (Current Version). *NADA2: Data Analysis for
Censored Environmental Data*. R package.

### Methods Referenced in This Vignette

- Parametric distribution fitting via
  [`EnvStats::distChoose()`](https://alexkowa.github.io/EnvStats/reference/distChoose.html)

- Censored distribution fitting via
  [`EnvStats::distChooseCensored()`](https://alexkowa.github.io/EnvStats/reference/distChooseCensored.html)

- Mann-Kendall trend testing via
  [`EnvStats::kendallTrendTest()`](https://alexkowa.github.io/EnvStats/reference/kendallTrendTest.html)

- Censored Kendall trend estimation via
  [`NADA2::cenken()`](https://rdrr.io/pkg/NADA2/man/cenken.html)

- Prediction intervals using:

  - [`EnvStats::predIntNorm()`](https://alexkowa.github.io/EnvStats/reference/predIntNorm.html)
  - [`EnvStats::predIntLnorm()`](https://alexkowa.github.io/EnvStats/reference/predIntLnorm.html)
  - [`EnvStats::predIntNpar()`](https://alexkowa.github.io/EnvStats/reference/predIntNpar.html)
  - corresponding simultaneous interval procedures

- Confidence intervals using:

  - [`EnvStats::enorm()`](https://alexkowa.github.io/EnvStats/reference/enorm.html)
  - [`EnvStats::elnormAlt()`](https://alexkowa.github.io/EnvStats/reference/elnormAlt.html)
  - [`EnvStats::eqnpar()`](https://alexkowa.github.io/EnvStats/reference/eqnpar.html)

- Tolerance intervals using:

  - [`EnvStats::tolIntNorm()`](https://alexkowa.github.io/EnvStats/reference/tolIntNorm.html)
  - [`EnvStats::tolIntLnorm()`](https://alexkowa.github.io/EnvStats/reference/tolIntLnorm.html)
  - [`EnvStats::tolIntNpar()`](https://alexkowa.github.io/EnvStats/reference/tolIntNpar.html)

## Summary

The statistical decision framework provides a centralized mechanism for
selecting analytical methods based on sample size, detection frequency,
and censoring severity. The approach follows principles described in EPA
groundwater monitoring guidance and modern environmental statistics
references ([United States Environmental Protection Agency
2009](#ref-epa2009); [Helsel 2012](#ref-helsel2012); [Helsel et al.
2020](#ref-helsel2020)).

Key functions include:

- [`guidance_profile()`](https://jentjr.github.io/manager/reference/guidance_profile.md)
- [`guidance_options()`](https://jentjr.github.io/manager/reference/guidance_options.md)
- [`statistical_decision()`](https://jentjr.github.io/manager/reference/statistical_decision.md)
- [`derive_decision()`](https://jentjr.github.io/manager/reference/derive_decision.md)
- [`analysis_plan()`](https://jentjr.github.io/manager/reference/analysis_plan.md)
- [`validate_analysis_plan()`](https://jentjr.github.io/manager/reference/validate_analysis_plan.md)
- [`estimate_distribution()`](https://jentjr.github.io/manager/reference/estimate_distribution.md)

Together, these functions support consistent and reproducible method
selection across projects, analytes, locations, and monitoring programs.

Gilbert, Richard O. 1987. *Statistical Methods for Environmental
Pollution Monitoring*. Van Nostrand Reinhold.

Helsel, Dennis R. 2012. *Statistics for Censored Environmental Data
Using Minitab and r*. 2nd ed. John Wiley & Sons.

Helsel, Dennis R., Robert M. Hirsch, Karen R. Ryberg, Stacey A.
Archfield, and Edward J. Gilroy. 2020. *Statistical Methods in Water
Resources*. Book 4, Chapter A3. Techniques and Methods. U.S. Geological
Survey.

Interstate Technology and Regulatory Council. 2013. *Groundwater
Statistics and Monitoring Compliance*. ITRC.

Julian, Joseph et al. 2024. *NADA2: Data Analysis for Censored
Environmental Data*.

Millard, Steven P., and Alexander Kowarik. 2024. *EnvStats: An r Package
for Environmental Statistics*.

United States Environmental Protection Agency. 1989. *Statistical
Analysis of Ground-Water Monitoring Data at RCRA Facilities: Interim
Final Guidance*. EPA/530-SW-89-026. U.S. Environmental Protection
Agency.

United States Environmental Protection Agency. 2009. *Statistical
Analysis of Groundwater Monitoring Data at RCRA Facilities: Unified
Guidance*. EPA 530/R-09-007. U.S. Environmental Protection Agency.
