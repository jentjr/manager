# Print Statistical Decision Object

Prints a compact summary of a \`"statistical_decision"\` object.

## Usage

``` r
# S3 method for class 'statistical_decision'
print(x, ...)
```

## Arguments

- x:

  A \`"statistical_decision"\` object.

- ...:

  Unused.

## Value

The input object, invisibly.

## Examples

``` r
decision <- statistical_decision(
  percent_censored = 25,
  n_used = 12,
  n_detect = 9
)

print(decision)
#> <statistical_decision>
#>   n_used n_detect percent_censored    censoring_class distribution_method
#> 1     12        9               25 moderate_censoring        ros_censored
#>   trend_method    trend_guidance censored_method distribution_config
#> 1 mann_kendall formal_trend_test          cenken     sf, norm, lnorm
#>          interval_config     decision_basis
#> 1 0.95, Bonferroni, 1, 1 moderate_censoring
```
