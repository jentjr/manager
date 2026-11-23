# Create a statistical analysis plan

Create a statistical analysis plan

## Usage

``` r
analysis_plan(
  objective = c("distribution", "trend", "comparison", "interval", "control_chart"),
  response = NULL,
  censored = NULL,
  date = NULL,
  group = NULL,
  comparison = NULL,
  method = NULL,
  alpha = 0.05,
  confidence_level = 0.95,
  censoring_method = NULL,
  missing_policy = c("exclude_required_fields", "error"),
  guidance = guidance_options(),
  parameters = list()
)
```

## Arguments

- objective:

  Analysis objective: "distribution", "trend", "comparison", "interval",
  or "control_chart".

- response:

  Name of the numeric response column.

- censored:

  Name of the logical censoring-indicator column.

- date:

  Optional date/time column used for trend analyses.

- group:

  Optional grouping column.

- comparison:

  Optional comparison column.

- method:

  Optional pre-specified method. If NULL, the method is selected using
  the guidance rules.

- alpha:

  Type I error rate.

- confidence_level:

  Confidence level for intervals.

- censoring_method:

  Explicit censored-data method, if required.

- missing_policy:

  Missing-data policy.

- guidance:

  Guidance profile returned by guidance_profile().

- parameters:

  Additional method-specific parameters.

## Value

An object of class "statistical_analysis_plan".
