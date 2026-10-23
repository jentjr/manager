#' Prepare standards data for plotting
#'
#' @param df Plotting dataframe.
#'
#' @return Tibble of standards applicable to the plotting data.
#'
#' @noRd

.prepare_standards_data <- function(df) {
  assign_standards(df) %>%
    dplyr::select(
      .standard_type,
      .standard_value
    ) %>%
    dplyr::filter(
      !is.na(.standard_type),
      !is.na(.standard_value),
      .standard_value > 0
    ) %>%
    dplyr::distinct()
}

#' Add standards to plot
#'
#' @param plot ggplot object.
#' @param df Plotting dataframe.
#' @param standards Logical.
#'
#' @noRd

.add_standards <- function(
  plot,
  df,
  standards = TRUE
) {
  if (!isTRUE(standards)) {
    return(plot)
  }

  std_df <- .prepare_standards_data(df)

  if (nrow(std_df) == 0) {
    return(plot)
  }

  plot +
    ggplot2::geom_hline(
      data = std_df,
      ggplot2::aes(
        yintercept = .standard_value,
        linetype = .standard_type
      ),
      color = "red",
      linewidth = 0.9,
      inherit.aes = FALSE,
      show.legend = TRUE
    ) +
    ggplot2::scale_linetype_discrete(
      name = "Standard"
    )
}
