# Match standards to environmental data

Matches standards to environmental chemistry observations using analyte
identity and, when available, sampling location.

## Usage

``` r
.match_standards(df, standards)
```

## Arguments

- df:

  An `env_schema` object.

- standards:

  A `standards_schema` object.

## Value

An `env_schema` with at most one matched standard attached to each
observation.

The output contains at most one standard assignment per observation.

## Details

Matching is performed using the base analyte name. For analytes that
include fraction or descriptor text, only the portion preceding the
first comma is used during matching.

Candidate standards are generated through a many-to-many join between
observation records and the standards table. Location-specific standards
are then filtered so that:

- standards with missing locations apply globally, and

- standards with a location value apply only to matching locations.

When multiple standards remain for a single observation, the standard
with the lowest `.priority` value is selected.

Internal helper used by \[assign_standards()\].

Selection precedence is determined entirely by the `.priority` field in
the standards schema. Lower numeric values indicate higher-priority
standards.

## See also

\[\`assign_standards\`()\], \[\`as_standards_schema\`()\]
