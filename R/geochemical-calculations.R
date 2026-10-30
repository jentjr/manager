#' Calculate charge balance
#'
#' @noRd
.calculate_charge_balance <- function(
  chemistry_df
) {
  totals <- .create_geochemical_components(
    chemistry_df,

    list(
      total_cations = c(
        "calcium",
        "magnesium",
        "sodium",
        "potassium"
      ),

      total_anions = c(
        "chloride",
        "sulfate",
        "bicarbonate"
      )
    ),

    value_col = analysis_result_meq_var(
      chemistry_df
    )
  )

  totals %>%
    dplyr::mutate(
      charge_balance_pct = 100 *
        (total_cations - total_anions) /
        (total_cations + total_anions)
    )
}

#' Calculate major ion groups
#'
#' @noRd
.calculate_major_ions <- function(
  chemistry_df
) {
  .create_geochemical_components(
    chemistry_df,
    .major_ion_components(),
    value_col = analysis_result_meq_var(
      chemistry_df
    )
  )
}

#' Calculate ternary coordinates
#'
#' @noRd
.calculate_ternary_coordinates <- function(
  left,
  top,
  right,
  x_offset = 0,
  prefix = NULL
) {
  total <- rowSums(
    cbind(
      left,
      top,
      right
    ),
    na.rm = TRUE
  )

  total[total <= 0] <- NA_real_

  left_pct <- 100 * left / total

  top_pct <- 100 * top / total

  right_pct <- 100 * right / total

  out <- tibble::tibble(
    left_pct = left_pct,

    top_pct = top_pct,

    right_pct = right_pct,

    x = x_offset +
      right_pct +
      top_pct / 2,

    y = sqrt(3) / 2 * top_pct
  )

  if (!is.null(prefix)) {
    names(out) <- paste0(
      prefix,
      "_",
      names(out)
    )
  }

  out
}

#' Calculate cation ternary coordinates
#'
#' @noRd
.calculate_cation_triangle <- function(
  left,
  top,
  right
) {
  .calculate_ternary_coordinates(
    left = left,
    top = top,
    right = right,
    x_offset = 0,
    prefix = "cation"
  )
}

#' Calculate anion ternary coordinates
#'
#' @noRd
.calculate_anion_triangle <- function(
  left,
  top,
  right,
  x_offset = 120
) {
  .calculate_ternary_coordinates(
    left = left,
    top = top,
    right = right,
    x_offset = x_offset,
    prefix = "anion"
  )
}
