# Assign regulatory standards

Assigns regulatory, screening, permit, background, or project-specific
standards to environmental chemistry results.

## Usage

``` r
assign_standards(df, standards = standards_tbl)
```

## Arguments

- df:

  Environmental chemistry data as an `env_schema` object.

- standards:

  Standards table or `standards_schema` object. Defaults to
  `standards_tbl`.

## Value

An enriched `env_schema` containing matched standard metadata, including
standard values, units, categories, and related fields from the
standards schema.

Schema attributes from `df` are preserved.

## Details

Standards are matched using the analyte name stored in `.analyte`.
Matching is performed through \[as_standards_schema()\] and
\[.match_standards()\], which attach the highest-priority applicable
standard to each observation.

If multiple standards match a result, the standard with the lowest
`.priority` value is retained.

Following assignment, standard values are converted to the analytical
units present in `.analysis_unit` so that exceedance comparisons can be
performed directly.

When multiple standards apply to a result, the standard with the lowest
`.priority` value is assigned.

Recommended priority ranges are:

- 1-9 = Site- or project-specific objectives

- 10-19 = Permit limits and compliance criteria

- 20-49 = Regulatory standards

- 50-99 = Screening levels and guidance values

- 100+ = Background or informational values

## See also

\[\`as_standards_schema\`()\], \[standards_tbl\]
