#' Safe row sum
#'
#' Returns NA when all inputs are missing.
#'
#' @noRd
.safe_rowsum <- function(data) {
  vals <- as.matrix(data)

  out <- rowSums(
    vals,
    na.rm = TRUE
  )

  out[
    rowSums(!is.na(vals)) == 0
  ] <- NA_real_

  out
}

.major_cations <- function() {
  c(
    "calcium",
    "magnesium",
    "sodium",
    "potassium"
  )
}

.major_anions <- function() {
  c(
    "chloride",
    "sulfate",
    "bicarbonate"
  )
}
