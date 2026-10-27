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

  out <- data %>%
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
    out <- out %>%
      dplyr::mutate(
        .priority = dplyr::if_else(
          !is.na(.data$.location),
          1L,
          100L
        )
      )
  } else {
    out <- out %>%
      dplyr::mutate(
        .priority = as.integer({{ priority }})
      )
  }

  out <- out %>%
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
