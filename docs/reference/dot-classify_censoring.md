# Internal helper: classify censoring percentages.

Boundary behavior: \[0, low_threshold) low_censoring \[low_threshold,
moderate_threshold) moderate_censoring \[moderate_threshold,
high_threshold) high_censoring \[high_threshold, 100\] extreme_censoring

## Usage

``` r
.classify_censoring(percent_censored, profile = guidance_options())
```

## Details

Missing percentages remain NA.
