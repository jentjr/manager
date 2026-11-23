# Select a statistical method

Select a statistical method

## Usage

``` r
select_statistical_method(data, plan, allow_override = TRUE)
```

## Arguments

- data:

  Validated analysis data.

- plan:

  A statistical_analysis_plan object.

- allow_override:

  Whether an explicitly supplied method may override the recommendation.

## Value

A list with selected method, decision basis, diagnostics, and
configuration.
