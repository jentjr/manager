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

#' Resolve laboratory reporting qualifiers
#'
#' Combines qualifier information from laboratory flags and
#' less-than indicators into a single reporting flag suitable
#' for classification with `.classify_results()`.
#'
#' Priority:
#' 1. FLAGS
#' 2. LT_MEASURE
#' 3. NA
#'
#' @param flags Character vector of laboratory qualifiers.
#' @param lt_measure Character vector of less-than indicators.
#'
#' @returns Character vector.
#'
#' @keywords internal
.resolve_reporting_flag <- function(
  flags = NULL,
  lt_measure = NULL
) {
  flags <- trimws(
    as.character(flags)
  )

  lt_measure <- trimws(
    as.character(lt_measure)
  )

  flags[
    flags %in%
      c(
        "",
        "NA"
      )
  ] <- NA_character_

  out <- flags

  missing_flag <- is.na(out)

  out[
    missing_flag &
      lt_measure %in% "<"
  ] <- "U"

  out
}

#' Default qualifier lookup table
#'
#' @keywords internal
.qualifier_tbl <- tibble::tribble(
  ~reporting_flag , ~result_type , ~detected , ~censored , ~estimated , ~rejected ,

  "<"             , "nondetect"  , FALSE     , TRUE      , FALSE      , FALSE     ,
  "ND"            , "nondetect"  , FALSE     , TRUE      , FALSE      , FALSE     ,
  "U"             , "nondetect"  , FALSE     , TRUE      , FALSE      , FALSE     ,

  "J"             , "estimated"  , TRUE      , FALSE     , TRUE       , FALSE     ,
  "UJ"            , "estimated"  , FALSE     , TRUE      , TRUE       , FALSE     ,
  "JB"            , "estimated"  , TRUE      , FALSE     , TRUE       , FALSE     ,
  "EST"           , "estimated"  , TRUE      , FALSE     , TRUE       , FALSE     ,

  "R"             , "rejected"   , FALSE     , FALSE     , FALSE      , TRUE      ,
  "REJ"           , "rejected"   , FALSE     , FALSE     , FALSE      , TRUE
)

#' Return qualifier lookup table
#'
#' @returns Tibble of supported qualifier mappings.
#'
#' @export
qualifier_table <- function() {
  .qualifier_tbl
}
