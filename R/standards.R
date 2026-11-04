#' Environmental Standards Lookup Table
#'
#' Default environmental standards used by functions such as
#' [`assign_standards`()].
#'
#' The dataset is a standardized [`standards_schema`()] object containing
#' regulatory, screening, permit, background, and project-specific
#' environmental standards.
#'
#' By default, records included with the package represent groundwater
#' screening criteria and drinking water standards. Users may extend or
#' replace this table with custom standards and supply them to
#' [assign_standards()] through the `standards` argument.
#'
#' Standards are selected according to `.priority`, where lower values
#' indicate higher priority. When multiple standards match a single
#' observation, the standard with the lowest `.priority` value is used.
#'
#' @format A tibble with the following columns:
#' \describe{
#'   \item{.analyte}{Analyte or constituent name.}
#'   \item{.standard_type}{Standard category or criterion name.}
#'   \item{.standard_value}{Numeric standard value.}
#'   \item{.standard_unit}{Unit associated with the standard value.}
#'   \item{.location}{Optional location identifier for location-specific
#'   standards.}
#'   \item{.medium}{Environmental medium associated with the standard
#'   (for example groundwater, soil, sediment, or surface water).}
#'   \item{.source}{Source authority, regulation, permit, guidance
#'   document, or project-specific basis.}
#'   \item{.priority}{Numeric conflict-resolution priority. Lower values
#'   take precedence over higher values.}
#' }
#'
#' @details
#' Standards are stored in a schema-based framework that supports
#' location-specific, medium-specific, permit-specific, and project-
#' specific criteria.
#'
#' The `.priority` field is used solely to resolve situations where
#' multiple standards apply to the same observation. Lower values
#' represent higher precedence.
#'
#' Recommended priority ranges are:
#'
#' \itemize{
#'   \item 1-9 = Site- or project-specific objectives
#'   \item 10-19 = Permit limits and compliance criteria
#'   \item 20-49 = Regulatory standards
#'   \item 50-99 = Screening levels and guidance values
#'   \item 100+ = Background, reference, or informational values
#' }
#'
#' These ranges are recommendations only. Users may define any priority
#' system appropriate for their project provided that lower values
#' represent higher precedence.
#' @examples
#' data("standards_tbl", package = "manager")
#'
#' standards_tbl
#'
#' # Add a custom site-specific standard
#' custom_standards <- dplyr::bind_rows(
#'   standards_tbl,
#'   tibble::tibble(
#'     .analyte = "Arsenic",
#'     .standard_type = "Site Cleanup Standard",
#'     .standard_value = 5,
#'     .standard_unit = "ug/L",
#'     .location = "MW-01",
#'     .medium = "groundwater",
#'     .source = "Project",
#'     .priority = 1L
#'   )
#' )
#'
#' @name standards_tbl
#' @docType data
#' @keywords datasets
NULL

#' Create a standards schema
#'
#' Standardizes a standards lookup table for use with environmental
#' chemistry analyses.
#'
#' This constructor converts a user-supplied standards table into a
#' validated \code{standards_schema} object that can be used by
#' [assign_standards()].
#'
#' Standards may represent regulatory criteria, permit limits,
#' screening levels, background concentrations, or project-specific
#' objectives. Optional metadata may be included to document the source,
#' applicability, and provenance of each standard.
#'
#' @param data Data frame containing standards information.
#' @param analyte Column containing analyte names.
#' @param standard_value Column containing standard values.
#' @param standard_unit Column containing standard units.
#' @param standard_type Column containing standard categories or
#'   criterion names.
#' @param location Optional column containing location-specific
#'   standards.
#' @param medium Optional column containing medium information.
#' @param source Optional column containing source organization or
#'   authority.
#' @param provenance Optional column containing regulation, program,
#'   permit, guidance document, or project identifier.
#' @param reference Optional citation, URL, permit number, regulation
#'   citation, or other reference.
#' @param effective_date Optional effective date.
#' @param notes Optional comments.
#' @param priority Optional numeric priority used to resolve conflicts
#'   when multiple standards match the same observation. Lower values
#'   indicate higher precedence.
#'
#' @details
#' The \code{.priority} field determines which standard is selected when
#' multiple standards apply to a single observation.
#'
#' Lower values represent higher priority.
#'
#' Recommended priority ranges are:
#'
#' \itemize{
#'   \item 1-9 = Site- or project-specific objectives
#'   \item 10-19 = Permit limits and compliance criteria
#'   \item 20-49 = Regulatory standards
#'   \item 50-99 = Screening levels and guidance values
#'   \item 100+ = Background or informational values
#' }
#'
#' If \code{priority} is not supplied, default priorities are assigned:
#'
#' \itemize{
#'   \item Location-specific standards receive priority \code{1}
#'   \item All other standards receive priority \code{100}
#' }
#'
#' @returns
#' A tibble with class \code{"standards_schema"} containing the
#' following standardized columns:
#'
#' \describe{
#'   \item{.analyte}{Analyte name.}
#'   \item{.standard_type}{Standard category or criterion name.}
#'   \item{.standard_value}{Numeric standard value.}
#'   \item{.standard_unit}{Unit associated with the standard.}
#'   \item{.location}{Optional location identifier.}
#'   \item{.medium}{Environmental medium.}
#'   \item{.source}{Source authority or organization.}
#'   \item{.provenance}{Regulation, permit, program, or guidance source.}
#'   \item{.reference}{Reference citation or URL.}
#'   \item{.effective_date}{Effective date of the standard.}
#'   \item{.notes}{Additional comments.}
#'   \item{.priority}{Conflict-resolution priority. Lower values take
#'   precedence over higher values.}
#' }
#'
#' @seealso
#' [assign_standards()],
#' [validate_standards_schema()]
#'
#' @export
standards_schema <- function(
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
) {
  data <- tibble::as_tibble(data)

  location_quo <- rlang::enquo(location)
  medium_quo <- rlang::enquo(medium)
  source_quo <- rlang::enquo(source)
  provenance_quo <- rlang::enquo(provenance)
  reference_quo <- rlang::enquo(reference)
  effective_date_quo <- rlang::enquo(effective_date)
  notes_quo <- rlang::enquo(notes)
  priority_quo <- rlang::enquo(priority)

  out <- data |>
    dplyr::mutate(
      .analyte = {{ analyte }},
      .standard_value = {{ standard_value }},
      .standard_unit = {{ standard_unit }},
      .standard_type = {{ standard_type }},

      .location = if (rlang::quo_is_null(location_quo)) {
        NA_character_
      } else {
        as.character({{ location }})
      },

      .medium = if (rlang::quo_is_null(medium_quo)) {
        NA_character_
      } else {
        as.character({{ medium }})
      },

      .source = if (rlang::quo_is_null(source_quo)) {
        NA_character_
      } else {
        as.character({{ source }})
      },

      .provenance = if (rlang::quo_is_null(provenance_quo)) {
        NA_character_
      } else {
        as.character({{ provenance }})
      },

      .reference = if (rlang::quo_is_null(reference_quo)) {
        NA_character_
      } else {
        as.character({{ reference }})
      },

      .effective_date = if (rlang::quo_is_null(effective_date_quo)) {
        as.Date(NA)
      } else {
        as.Date({{ effective_date }})
      },

      .notes = if (rlang::quo_is_null(notes_quo)) {
        NA_character_
      } else {
        as.character({{ notes }})
      }
    )

  if (rlang::quo_is_null(priority_quo)) {
    out <- out |>
      dplyr::mutate(
        .priority = dplyr::if_else(
          !is.na(.data$.location),
          1L,
          100L
        )
      )
  } else {
    out <- out |>
      dplyr::mutate(
        .priority = as.integer({{ priority }})
      )
  }

  out <- out |>
    dplyr::select(
      .analyte,
      .standard_type,
      .standard_value,
      .standard_unit,
      .location,
      .medium,
      .source,
      .provenance,
      .reference,
      .effective_date,
      .notes,
      .priority
    )

  attr(out, "standards_schema") <- list(
    analyte = ".analyte",
    standard_type = ".standard_type",
    standard_value = ".standard_value",
    standard_unit = ".standard_unit",
    location = ".location",
    medium = ".medium",
    source = ".source",
    provenance = ".provenance",
    reference = ".reference",
    effective_date = ".effective_date",
    notes = ".notes",
    priority = ".priority"
  )

  class(out) <- c(
    "standards_schema",
    class(out)
  )

  validate_standards_schema(out)

  out
}

#' Coerce to standards schema
#'
#' @param x Object to coerce.
#'
#' @returns
#' A standards_schema object.
#'
#' @export
as_standards_schema <- function(x) {
  if (is_standards_schema(x)) {
    return(x)
  }

  required_cols <- c(
    ".analyte",
    ".standard_type",
    ".standard_value",
    ".standard_unit"
  )

  missing_cols <- setdiff(
    required_cols,
    names(x)
  )

  if (length(missing_cols) > 0) {
    rlang::abort(
      paste(
        "Cannot coerce to standards_schema. Missing columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  if (!".location" %in% names(x)) {
    x$.location <- NA_character_
  }

  if (!".medium" %in% names(x)) {
    x$.medium <- NA_character_
  }

  if (!".source" %in% names(x)) {
    x$.source <- NA_character_
  }

  if (!".priority" %in% names(x)) {
    x$.priority <- ifelse(
      is.na(x$.location),
      100L,
      1L
    )
  }

  class(x) <- c(
    "standards_schema",
    class(x)
  )

  validate_standards_schema(x)

  x
}

#' Test if object is a standards schema
#'
#' @param x Object to test.
#'
#' @returns Logical scalar.
#'
#' @keywords internal
is_standards_schema <- function(x) {
  inherits(x, "standards_schema")
}

#' Prepare standards data for plotting
#'
#' @param df Plotting dataframe.
#'
#' @return Tibble of standards applicable to the plotting data.
#'
#' @noRd

.prepare_standards_data <- function(df) {
  assign_standards(df) |>
    dplyr::select(
      .standard_type,
      .standard_value
    ) |>
    dplyr::filter(
      !is.na(.standard_type),
      !is.na(.standard_value),
      .standard_value > 0
    ) |>
    dplyr::distinct()
}

#' Assign regulatory standards
#'
#' Assigns regulatory, screening, permit, background, or project-specific
#' standards to environmental chemistry results.
#'
#' Standards are matched using the analyte name stored in
#' \code{.analyte}. Matching is performed through
#' [as_standards_schema()] and [.match_standards()], which attach the
#' highest-priority applicable standard to each observation.
#'
#' If multiple standards match a result, the standard with the lowest
#' \code{.priority} value is retained.
#'
#' Following assignment, standard values are converted to the analytical
#' units present in \code{.analysis_unit} so that exceedance comparisons
#' can be performed directly.
#'
#' @param df Environmental chemistry data as an \code{env_schema}
#'   object.
#' @param standards Standards table or \code{standards_schema} object.
#'   Defaults to \code{standards_tbl}.
#'
#' @returns
#' An enriched \code{env_schema} containing matched standard metadata,
#' including standard values, units, categories, and related fields from
#' the standards schema.
#'
#' Schema attributes from \code{df} are preserved.
#'
#' @details
#' When multiple standards apply to a result, the standard with the
#' lowest \code{.priority} value is assigned.
#'
#' Recommended priority ranges are:
#'
#' \itemize{
#'   \item 1-9 = Site- or project-specific objectives
#'   \item 10-19 = Permit limits and compliance criteria
#'   \item 20-49 = Regulatory standards
#'   \item 50-99 = Screening levels and guidance values
#'   \item 100+ = Background or informational values
#' }
#'
#' @seealso
#' [`as_standards_schema`()],
#' [standards_tbl]
#'
#' @export
assign_standards <- function(
  df,
  standards = standards_tbl
) {
  validate_env_schema(df)

  standards <- as_standards_schema(
    standards
  )

  out <- .match_standards(
    df,
    standards
  ) |>
    .convert_standard_units(
      data_unit_col = ".analysis_unit"
    )

  out <- .copy_env_schema_attributes(
    from = df,
    to = out
  )

  out
}

#' Match standards to environmental data
#'
#' Matches standards to environmental chemistry observations using
#' analyte identity and, when available, sampling location.
#'
#' Matching is performed using the base analyte name. For analytes that
#' include fraction or descriptor text, only the portion preceding the
#' first comma is used during matching.
#'
#' Candidate standards are generated through a many-to-many join between
#' observation records and the standards table. Location-specific
#' standards are then filtered so that:
#'
#' \itemize{
#'   \item standards with missing locations apply globally, and
#'   \item standards with a location value apply only to matching
#'   locations.
#' }
#'
#' When multiple standards remain for a single observation, the
#' standard with the lowest \code{.priority} value is selected.
#'
#' @param df An \code{env_schema} object.
#' @param standards A \code{standards_schema} object.
#'
#' @returns
#' An \code{env_schema} with at most one matched standard attached to
#' each observation.
#'
#' The output contains at most one standard assignment per observation.
#'
#' @details
#' Internal helper used by [assign_standards()].
#'
#' Selection precedence is determined entirely by the
#' \code{.priority} field in the standards schema. Lower numeric values
#' indicate higher-priority standards.
#'
#' @seealso
#' [`assign_standards`()],
#' [`as_standards_schema`()]
#'
#' @keywords internal
.match_standards <- function(
  df,
  standards
) {
  df <- df |>
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      .param_base = sub(
        ",.*$",
        "",
        .data$.analyte
      )
    )

  candidates <- df |>
    dplyr::left_join(
      standards,
      by = c(
        ".param_base" = ".analyte"
      ),
      relationship = "many-to-many"
    )

  if (
    ".location.x" %in% names(candidates) && ".location.y" %in% names(candidates)
  ) {
    candidates <- candidates |>
      dplyr::filter(
        is.na(.data$.location.y) |
          .data$.location.x == .data$.location.y
      )
  }

  out <- candidates |>
    dplyr::group_by(.data$.row_id) |>
    dplyr::slice_min(
      .data$.priority,
      n = 1,
      with_ties = FALSE
    ) |>
    dplyr::ungroup()

  if (".location.x" %in% names(out) && ".location.y" %in% names(out)) {
    out <- out |>
      dplyr::rename(
        .location = ".location.x"
      ) |>
      dplyr::select(
        -".location.y"
      )
  }

  out |>
    dplyr::select(
      -.row_id,
      -.param_base
    )
}
