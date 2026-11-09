# Validate est_dist Output

Checks that a data frame contains the required structure produced by
\`est_dist()\`, including:

## Usage

``` r
.validate_est_dist_output(df)
```

## Arguments

- df:

  A data frame returned by \`est_dist()\`.

## Value

Invisibly returns \`df\`.

## Details

\* a \`distribution\` column \* a \`data\` list-column \* nested data
frames containing \`analysis_result\` \* supported distribution values

This validator ensures compatibility with downstream functions such as:

\* \`pred_int()\` \* \`tol_int()\` \* control chart calculations
