# Run an analysis from an analysis plan

Method execution is intentionally delegated to handler functions. This
prevents run_analysis() from silently substituting a different
statistical method when the requested method is unavailable or
assumptions fail.

## Usage

``` r
run_analysis(data, plan, handlers = list(), allow_override = TRUE)
```

## Arguments

- data:

  Analysis data.

- plan:

  A statistical_analysis_plan object.

- handlers:

  Named list of analysis handler functions. Each handler must accept
  \`data\`, \`plan\`, and \`selection\`.

- allow_override:

  Whether an explicit plan method may override guidance.

## Value

An object of class "environmental_analysis_result".
