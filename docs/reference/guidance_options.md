# Guidance Options

Retrieves the active statistical guidance options used to classify
censoring severity and select recommended analytical methods.

## Usage

``` r
guidance_options()
```

## Value

A named list containing:

- low_threshold:

  Threshold separating low and moderate censoring.

- moderate_threshold:

  Threshold separating moderate and high censoring.

- high_threshold:

  Threshold separating high and extreme censoring.

## Details

Guidance options are read from the global package option
\`"manager.guidance"\`. If no option has been set, EPA-style default
thresholds are returned.

These thresholds are used by \[statistical_decision()\] to classify
censoring as low, moderate, high, or extreme and to determine
recommended distribution fitting, trend analysis, and censored-data
handling approaches.

The default values are:

- \`low_threshold = 15\`

- \`moderate_threshold = 50\`

- \`high_threshold = 90\`

These defaults correspond to the thresholds used by the
\`"epa_unified"\` \[guidance_profile()\].

Users may override the defaults using \[options()\]:

    options(
      manager.guidance = list(
        low_threshold = 10,
        moderate_threshold = 25,
        high_threshold = 75
      )
    )

## See also

\[guidance_profile()\], \[statistical_decision()\]

## Examples

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
#> 

options(
  manager.guidance = list(
    low_threshold = 10,
    moderate_threshold = 25,
    high_threshold = 75
  )
)

guidance_options()
#> $low_threshold
#> [1] 10
#> 
#> $moderate_threshold
#> [1] 25
#> 
#> $high_threshold
#> [1] 75
#> 
```
