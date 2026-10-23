#' Classify reporting qualifiers
#'
#' Classifies analytical results based on reporting qualifiers and
#' assigns result type, detection status, censoring status,
#' estimation flags, and usability flags.
#'
#' @param flag Reporting flag vector.
#' @param lookup Qualifier lookup table.
#'
#' @returns A tibble containing:
#' \itemize{
#'   \item \code{result_type}
#'   \item \code{detected}
#'   \item \code{censored}
#'   \item \code{estimated}
#'   \item \code{rejected}
#' }
#'
#' @keywords internal
.classify_results <- function(
  flag,
  lookup = .qualifier_tbl
) {
  flag <- stringr::str_squish(
    as.character(flag)
  )

  x <- tibble::tibble(
    reporting_flag = toupper(flag)
  )

  out <- x %>%
    dplyr::left_join(
      lookup,
      by = "reporting_flag"
    )

  # ------------------------------------------------------------
  # Unqualified results are assumed to be detects
  # ------------------------------------------------------------

  out$result_type[
    is.na(out$reporting_flag)
  ] <- "detect"

  out$detected[
    is.na(out$reporting_flag)
  ] <- TRUE

  out$censored[
    is.na(out$reporting_flag)
  ] <- FALSE

  out$estimated[
    is.na(out$reporting_flag)
  ] <- FALSE

  out$rejected[
    is.na(out$reporting_flag)
  ] <- FALSE

  # ------------------------------------------------------------
  # Unknown qualifiers
  # ------------------------------------------------------------

  unknown <- !is.na(out$reporting_flag) &
    is.na(out$result_type)

  out$result_type[unknown] <- "unknown"

  out$detected[unknown] <- FALSE

  out$censored[unknown] <- FALSE

  out$estimated[unknown] <- FALSE

  out$rejected[unknown] <- FALSE

  out
}
