# Statistical Decision Guidance and Automated Method Selection

## Introduction

Environmental monitoring datasets frequently contain censored
observations, limited sample sizes, and varying analytical detection
frequencies. These characteristics can substantially influence the
validity of distribution fitting, trend analysis, and interval
estimation procedures ([Helsel 2012](#ref-helsel2012); [Helsel et al.
2020](#ref-helsel2020)). Statistical methods that perform well under low
censoring may become unreliable when censoring is extensive.

The `manager` package includes a statistical decision framework that
automatically evaluates data quality and recommends appropriate
statistical methods for environmental monitoring data. The framework is
inspired by recommendations commonly used in groundwater monitoring
programs and EPA statistical guidance documents ([United States
Environmental Protection Agency 2009](#ref-epa2009),
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
#> # A tibble: 1 × 12
#>   n_used n_detect percent_censored censoring_class distribution_method
#>    <dbl>    <dbl>            <dbl> <chr>           <chr>              
#> 1     20       18               12 low_censoring   parametric         
#> # ℹ 7 more variables: trend_method <chr>, trend_guidance <chr>,
#> #   censored_method <chr>, distribution_config <list>, interval_config <list>,
#> #   decision_basis <chr>, comparison_method <chr>
```

The resulting object contains:

``` r

names(decision)
#>  [1] "n_used"              "n_detect"            "percent_censored"   
#>  [4] "censoring_class"     "distribution_method" "trend_method"       
#>  [7] "trend_guidance"      "censored_method"     "distribution_config"
#> [10] "interval_config"     "decision_basis"      "comparison_method"
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

statistical_decision(
  percent_censored = c(5, 25, 75, 95),
  n_used = 20,
  n_detect = c(19, 15, 5, 1)
) |>
  select(
    percent_censored,
    censoring_class
  )
#> <statistical_decision>
#> # A tibble: 4 × 2
#>   percent_censored censoring_class   
#>              <dbl> <chr>             
#> 1                5 low_censoring     
#> 2               25 moderate_censoring
#> 3               75 high_censoring    
#> 4               95 extreme_censoring
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

statistical_decision(
  percent_censored = c(5, 25, 75, 95),
  n_used = 20,
  n_detect = c(19, 15, 5, 1)
) |>
  select(
    percent_censored,
    distribution_method
  )
#> <statistical_decision>
#> # A tibble: 4 × 2
#>   percent_censored distribution_method
#>              <dbl> <chr>              
#> 1                5 parametric         
#> 2               25 ros_censored       
#> 3               75 nonparametric      
#> 4               95 descriptive_only
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
#> 1 mann_kendall formal_trend_test cenken
```

### Trend Guidance Categories

#### Formal Trend Testing

When censoring remains below the high-censoring threshold and adequate
detects are available:

``` text
formal_trend_test
```

is recommended.

#### Screening-Only Evaluation

When censoring becomes extensive:

``` text
screening_only
```

is returned.

#### Insufficient Detects

When fewer than four detects are available:

``` text
insufficient_detects
```

is returned.

``` r

statistical_decision(
  percent_censored = 90,
  n_used = 20,
  n_detect = 2
) |>
  select(
    trend_guidance
  )
#> <statistical_decision>
#> # A tibble: 1 × 1
#>   trend_guidance      
#>   <chr>               
#> 1 insufficient_detects
```

## Censored Trend Methods

The decision framework also recommends how censored observations should
be handled during trend evaluation.

``` r

statistical_decision(
  percent_censored = c(5, 40, 75),
  n_used = 20,
  n_detect = c(19, 12, 5)
) |>
  select(
    percent_censored,
    censored_method
  )
#> <statistical_decision>
#> # A tibble: 3 × 2
#>   percent_censored censored_method
#>              <dbl> <chr>          
#> 1                5 as_reported    
#> 2               40 cenken         
#> 3               75 cenken
```

For censored environmental datasets, ordinary Mann-Kendall analyses may
not adequately account for reporting limits. In these situations,
censored Kendall methods such as
[`NADA2::cenken()`](https://rdrr.io/pkg/NADA2/man/cenken.html) are often
recommended ([Helsel 2012](#ref-helsel2012); [Julian et al.
2024](#ref-nada2)).

Recommended methods:

| Condition                     | Method      |
|-------------------------------|-------------|
| Low censoring                 | as_reported |
| Moderate or greater censoring | cenken      |

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

Most workflows will not create decisions manually.

Instead, recommendations are derived automatically from environmental
data.

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

### Example

``` r

gw |>
  filter(
    param_name == "Arsenic, dissolved"
  ) |>
  derive_decision()
```

## Distribution Estimation Workflow

The primary consumer of statistical decision guidance is
[`estimate_distribution()`](https://jentjr.github.io/manager/reference/estimate_distribution.md).
Distribution estimation relies on methods implemented in the `EnvStats`
package ([Millard and Kowarik 2024](#ref-envstats)), which contains many
of the statistical procedures described in EPA groundwater monitoring
guidance ([United States Environmental Protection Agency
2009](#ref-epa2009)).

``` r

gw |>
  group_by(
    location_id,
    param_name,
    default_unit
  ) |>
  estimate_distribution()
```

For each group:

1.  Censoring is evaluated
2.  A statistical decision is derived
3.  The recommended workflow is selected
4.  Distribution results are returned along with decision metadata

## Automatic Method Selection

Under the default EPA profile:

| Percent Censored | Recommendation   | Action                      |
|------------------|------------------|-----------------------------|
| \<15             | parametric       | Fit candidate distributions |
| 15-50            | ros_censored     | Fit censored distributions  |
| 50-90            | nonparametric    | Classify as nonparametric   |
| \>=90            | descriptive_only | Descriptive evaluation only |

Example:

``` r

gw |>
  group_by(
    location_id,
    param_name,
    default_unit
  ) |>
  estimate_distribution()
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

The decision object also includes interval-estimation guidance.

``` r

decision$interval_config[[1]]
#> $conf_level
#> [1] 0.95
#> 
#> $method
#> [1] "Bonferroni"
#> 
#> $k
#> [1] 1
#> 
#> $n_mean
#> [1] 1
```

Default settings:

``` r

list(
  conf_level = 0.95,
  method = "Bonferroni",
  k = 1,
  n_mean = 1
)
```

These values are intended as defaults for:

- [`pred_int()`](https://jentjr.github.io/manager/reference/pred_int.md)
- [`pred_int_sim()`](https://jentjr.github.io/manager/reference/pred_int_sim.md)
- [`conf_int()`](https://jentjr.github.io/manager/reference/conf_int.md)
- [`tol_int()`](https://jentjr.github.io/manager/reference/tol_int.md)

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

Upper prediction intervals:

``` r

pred_int(
  dist_est,
  pi_type = "upper",
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

Organizations may prefer different censoring thresholds.

Global options can be used to override defaults.

``` r

options(
  manager.guidance = list(
    low_threshold = 10,
    moderate_threshold = 25,
    high_threshold = 75
  )
)
```

Retrieve the active settings:

``` r

guidance_options()
#> $low_threshold
#> [1] 10
#> 
#> $moderate_threshold
#> [1] 25
#> 
#> $high_threshold
#> [1] 75
```

Decisions derived afterward will use the updated thresholds.

## Example Workflow

A typical environmental monitoring workflow may look like:

``` r

gw |>
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
  group_by(
    location_id,
    param_name
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
- [`estimate_distribution()`](https://jentjr.github.io/manager/reference/estimate_distribution.md)
- [`stats_summary()`](https://jentjr.github.io/manager/reference/stats_summary.md)

Together these functions allow environmental datasets to be evaluated
using method-selection rules that are consistent across projects,
analytes, and monitoring programs.

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
