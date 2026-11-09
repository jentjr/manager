# Create a standards schema

Standardizes a standards lookup table for use with environmental
chemistry analyses.

## Usage

``` r
standards_schema(
  data,
  analyte,
  standard_value,
  standard_unit,
  standard_type,
  location = NULL,
  medium = NULL,
  source = NULL,
  provenance = NULL,
  reference = NULL,
  effective_date = NULL,
  notes = NULL,
  priority = NULL
)
```

## Arguments

- data:

  Data frame containing standards information.

- analyte:

  Column containing analyte names.

- standard_value:

  Column containing standard values.

- standard_unit:

  Column containing standard units.

- standard_type:

  Column containing standard categories or criterion names.

- location:

  Optional column containing location-specific standards.

- medium:

  Optional column containing medium information.

- source:

  Optional column containing source organization or authority.

- provenance:

  Optional column containing regulation, program, permit, guidance
  document, or project identifier.

- reference:

  Optional citation, URL, permit number, regulation citation, or other
  reference.

- effective_date:

  Optional effective date.

- notes:

  Optional comments.

- priority:

  Optional numeric priority used to resolve conflicts when multiple
  standards match the same observation. Lower values indicate higher
  precedence.

## Value

A tibble with class `"standards_schema"` containing the following
standardized columns:

- .analyte:

  Analyte name.

- .standard_type:

  Standard category or criterion name.

- .standard_value:

  Numeric standard value.

- .standard_unit:

  Unit associated with the standard.

- .location:

  Optional location identifier.

- .medium:

  Environmental medium.

- .source:

  Source authority or organization.

- .provenance:

  Regulation, permit, program, or guidance source.

- .reference:

  Reference citation or URL.

- .effective_date:

  Effective date of the standard.

- .notes:

  Additional comments.

- .priority:

  Conflict-resolution priority. Lower values take precedence over higher
  values.

## Details

This constructor converts a user-supplied standards table into a
validated `standards_schema` object that can be used by
\[assign_standards()\].

Standards may represent regulatory criteria, permit limits, screening
levels, background concentrations, or project-specific objectives.
Optional metadata may be included to document the source, applicability,
and provenance of each standard.

The `.priority` field determines which standard is selected when
multiple standards apply to a single observation.

Lower values represent higher priority.

Recommended priority ranges are:

- 1-9 = Site- or project-specific objectives

- 10-19 = Permit limits and compliance criteria

- 20-49 = Regulatory standards

- 50-99 = Screening levels and guidance values

- 100+ = Background or informational values

If `priority` is not supplied, default priorities are assigned:

- Location-specific standards receive priority `1`

- All other standards receive priority `100`

## See also

\[assign_standards()\], \[validate_standards_schema()\]
