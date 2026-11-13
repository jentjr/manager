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
#> # A tibble: 1 × 12
#>   n_used n_detect percent_censored censoring_class    distribution_method
#>    <dbl>    <dbl>            <dbl> <chr>              <chr>              
#> 1     12        9               25 moderate_censoring ros_censored       
#> # ℹ 7 more variables: trend_method <chr>, trend_guidance <chr>,
#> #   censored_method <chr>, distribution_config <list>, interval_config <list>,
#> #   decision_basis <chr>, comparison_method <chr>
```
