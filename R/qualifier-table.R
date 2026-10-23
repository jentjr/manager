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
