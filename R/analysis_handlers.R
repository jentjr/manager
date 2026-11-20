handle_distribution <- function(
  data,
  plan,
  selection
) {
  if (!exists("estimate_distribution", mode = "function")) {
    rlang::abort(
      "`estimate_distribution()` is required for distribution analyses."
    )
  }

  grouped <- data

  if (!dplyr::is_grouped_df(grouped)) {
    grouping_vars <- compact(
      c(plan$group, plan$comparison)
    )

    if (length(grouping_vars) == 0L) {
      rlang::abort(
        "Distribution analysis requires grouped data or a grouping variable."
      )
    }

    grouped <- dplyr::group_by(
      grouped,
      dplyr::across(dplyr::all_of(grouping_vars))
    )
  }

  estimate_distribution(
    grouped,
    distribution_config = selection$parameters$distribution_config %||%
      "auto",
    result_col = plan$response,
    censored_col = plan$censored
  )
}


handle_descriptive_only <- function(
  data,
  plan,
  selection
) {
  values <- data[[plan$response]]
  usable <- !is.na(values)

  list(
    method = "descriptive_only",
    n = sum(usable),
    n_censored = sum(
      data[[plan$censored]][usable],
      na.rm = TRUE
    ),
    mean = if (sum(usable) > 0L) mean(values[usable]) else NA_real_,
    median = if (sum(usable) > 0L) median(values[usable]) else NA_real_,
    minimum = if (sum(usable) > 0L) min(values[usable]) else NA_real_,
    maximum = if (sum(usable) > 0L) max(values[usable]) else NA_real_
  )
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
