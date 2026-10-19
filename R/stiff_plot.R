#' @title Stiff Diagram
#'
#' @description plots a stiff diagram
#'
#' @details tidy dataframe of water quality data in mg/L is converted to
#' meq/L and then transformed to coordinates for Stiff diagram.
#'
#' @param data data frame of water quality data
#' @param location_id column for sample locations
#' @param sample_date column for sample date
#' @param param_name column for constituent names
#' @param analysis_result column for analysis result
#' @param default_unit column for units. Assumes all units are in mg/L and
#' converts using \code{\link{convert_ions_to_meq}}
#' @param magnesium name for magnesium
#' @param calcium name for calcium
#' @param sodium name for sodium
#' @param potassium name for potassium
#' @param chloride name for chloride
#' @param sulfate name for sulfate
#' @param alkalinity name for alkalinity
#' @param scale_analyte name of analyte to scale color of plot.
#' Default is NULL
#' @param group_var Character column name used to split the data into
#' separate Stiff diagrams. One ggplot object is returned for each
#' unique value. Default is the value supplied to `location_id`.
#' @param facet_var Character column name used for faceting.
#' Default is the value supplied to `sample_date`.
#' @param lines TRUE/FALSE plots lines
#' @param title Optional plot title. If NULL, the value of the current
#' grouping variable is used.
#' @keywords geochemical plots Stiff Diagram
#'
#' @return
#' A named list of ggplot objects, one element per value of
#' `group_var`.
#'
#' @examples
#' data(gw_data)
#' gw_data %>%
#' filter(location_id == "MW-1", sample_date < lubridate::ymd("2008-01-01")) %>%
#' stiff_plot()
#'
#' gw_data %>%
#' filter(location_id %in% c("MW-1", "MW-2")) %>%
#' stiff_plot(scale_analyte = "Total Dissolved Solids")
#'
#' @export

stiff_plot <- function(
  data,
  location_id = "location_id",
  sample_date = "sample_date",
  param_name = "param_name",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  calcium = "Calcium, dissolved",
  magnesium = "Magnesium, dissolved",
  sodium = "Sodium, dissolved",
  potassium = "Potassium, dissolved",
  chloride = "Chloride, total",
  sulfate = "Sulfate, total",
  alkalinity = "Alkalinity, total (lab)",
  scale_analyte = NULL,
  group_var = location_id,
  facet_var = sample_date,
  lines = FALSE,
  title = NULL
) {
  if (!facet_var %in% c(location_id, sample_date)) {
    cli::cli_abort(
      "{.arg facet_var} must be either the location_id or sample_date column."
    )
  }
  if (!group_var %in% c(location_id, sample_date)) {
    cli::cli_abort(
      "{.arg group_var} must be either the location_id or sample_date column."
    )
  }

  split(data, data[[group_var]]) %>%
    purrr::imap(
      ~ .stiff_plot(
        data = .x,
        group_name = .y,
        location_id = location_id,
        sample_date = sample_date,
        param_name = param_name,
        analysis_result = analysis_result,
        default_unit = default_unit,
        calcium = calcium,
        magnesium = magnesium,
        sodium = sodium,
        potassium = potassium,
        chloride = chloride,
        sulfate = sulfate,
        alkalinity = alkalinity,
        scale_analyte = scale_analyte,
        facet_var = facet_var,
        group_var = group_var,
        lines = lines,
        title = title
      )
    )
}

#' Helper function for Stiff Diagrams
#'
#' @noRd
.stiff_plot <- function(
  data,
  group_name,
  location_id,
  sample_date,
  param_name,
  analysis_result,
  default_unit,
  calcium,
  magnesium,
  sodium,
  potassium,
  chloride,
  sulfate,
  alkalinity,
  scale_analyte,
  facet_var,
  group_var,
  lines,
  title
) {
  chemistry_df <- .prepare_major_ion_chemistry(
    df = data,
    analytes = c(
      calcium,
      magnesium,
      sodium,
      potassium,
      chloride,
      sulfate,
      alkalinity
    ),
    location_id = location_id,
    sample_date = sample_date,
    param_name = param_name,
    analysis_result = analysis_result,
    default_unit = default_unit
  )

  scale_df <- NULL

  if (!is.null(scale_analyte)) {
    scale_df <- .prepare_geochemical_data(
      df = data,
      analytes = scale_analyte,
      location_id = location_id,
      sample_date = sample_date,
      param_name = param_name,
      analysis_result = analysis_result,
      default_unit = default_unit
    )
  }

  stiff_data <- .transform_stiff_data(
    chemistry_df = chemistry_df,
    scale_df = scale_df,
    scale_analyte = scale_analyte
  )

  .build_stiff_plot(
    data = stiff_data,
    location_id = location_id,
    sample_date = sample_date,
    facet_var = facet_var,
    group_var = group_var,
    group_name = group_name,
    lines = lines,
    title = title,
    scale_analyte = scale_analyte
  )
}

#' Helper function for Stiff Diagrams
#'
#' @noRd

.build_stiff_plot <- function(
  data,
  location_id,
  sample_date,
  facet_var,
  group_var,
  group_name,
  lines,
  title,
  scale_analyte
) {
  plot_title <- if (is.null(title)) {
    paste(group_var, group_name, sep = ": ")
  } else {
    title
  }
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = stiff_x,
      y = stiff_y,
      group = interaction(
        .data[[location_id]],
        .data[[sample_date]]
      )
    )
  ) +
    ggplot2::geom_vline(
      xintercept = 0,
      colour = "grey50",
      linewidth = 0.4
    )

  if (!is.null(scale_analyte)) {
    p <- p +
      ggplot2::geom_polygon(
        ggplot2::aes(fill = scale_value),
        colour = "black"
      ) +
      ggplot2::scale_fill_viridis_c(
        name = paste(scale_analyte)
      )
  } else {
    p <- p +
      ggplot2::geom_polygon(
        fill = "white",
        colour = "black"
      )
  }

  label_data <- data %>%
    dplyr::distinct(
      .data[[facet_var]],
      ion,
      stiff_x_label,
      stiff_y
    )
  p <- p +
    ggplot2::geom_text(
      data = label_data,
      ggplot2::aes(
        x = stiff_x_label,
        y = stiff_y,
        label = ion
      ),
      inherit.aes = FALSE,
      parse = TRUE,
      size = 3
    )
  if (isTRUE(lines)) {
    p <- p +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = -abs(stiff_x_label) + 0.3,
          xend = abs(stiff_x_label) - 0.3,
          y = stiff_y,
          yend = stiff_y
        ),
        linetype = "dotted"
      )
  }

  global_extent <- max(
    abs(data$stiff_x),
    na.rm = TRUE
  )

  if (
    !is.finite(global_extent) ||
      global_extent <= 0
  ) {
    global_extent <- 1
  }

  return(
    p +
      ggplot2::facet_wrap(
        stats::reformulate(facet_var),
        scales = "fixed"
      ) +
      ggplot2::scale_x_continuous(
        limits = c(
          -global_extent * 1.15,
          global_extent * 1.15
        ),
        labels = abs
      ) +
      ggplot2::labs(
        title = plot_title,
        x = "meq/L",
        y = NULL
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5
        ),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank()
      )
  )
}

#' Function to create an animated Stiff Diagram
#'
#' @param df data frame of water quality data
#' @inheritDotParams stiff_plot
#'
#' @export

stiff_time_plot <- function(
  df,
  group_var = "location_id",
  sample_date = "sample_date",
  ...
) {
  dates <- sort(unique(df[[sample_date]]))

  for (date in dates) {
    plots <- stiff_plot(
      dplyr::filter(
        df,
        .data[[sample_date]] == date
      ),
      group_var = group_var,
      sample_date = sample_date,
      ...
    )

    purrr::walk(
      plots,
      ~ {
        print(.x)
        animation::ani.pause()
      }
    )
  }
}

#' Function to create an animated Stiff Diagram and save to html
#'
#' @param df data frame of water quality data
#' @inheritDotParams stiff_plot
#'
#' @export

stiff_time_html <- function(
  df,
  sample_date = "sample_date",
  group_var = "location_id",
  ...
) {
  n_frames <- df %>%
    dplyr::distinct(
      .data[[sample_date]],
      .data[[group_var]]
    ) %>%
    nrow()

  animation::saveHTML(
    {
      animation::ani.options(
        nmax = n_frames
      )

      stiff_time_plot(
        df,
        sample_date = sample_date,
        group_var = group_var,
        ...
      )
    },
    img.name = "StiffPlot",
    htmlfile = "Stiff_plot.html",
    autobrowse = TRUE,
    title = "Stiff Diagram"
  )
}
