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
  ) %>%
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
  df <- df %>%
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      .param_base = sub(
        ",.*$",
        "",
        .data$.analyte
      )
    )

  candidates <- df %>%
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
    candidates <- candidates %>%
      dplyr::filter(
        is.na(.data$.location.y) |
          .data$.location.x == .data$.location.y
      )
  }

  out <- candidates %>%
    dplyr::group_by(.data$.row_id) %>%
    dplyr::slice_min(
      .data$.priority,
      n = 1,
      with_ties = FALSE
    ) %>%
    dplyr::ungroup()

  if (".location.x" %in% names(out) && ".location.y" %in% names(out)) {
    out <- out %>%
      dplyr::rename(
        .location = ".location.x"
      ) %>%
      dplyr::select(
        -".location.y"
      )
  }

  out %>%
    dplyr::select(
      -.row_id,
      -.param_base
    )
}
