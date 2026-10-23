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
  stiff_limit = NULL,
  stiff_breaks = 1,
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

  required_ions <- c(
    calcium,
    magnesium,
    sodium,
    potassium,
    chloride,
    sulfate,
    alkalinity
  )

  missing <- setdiff(
    required_ions,
    unique(data[[param_name]])
  )

  if (length(missing) == length(required_ions)) {
    cli::cli_abort(
      "No required major ions were found in the data."
    )
  }

  if (length(missing) > 0) {
    cli::cli_warn(
      "Missing major ions: {paste(missing, collapse = ', ')}"
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
        stiff_limit = stiff_limit,
        stiff_breaks = stiff_breaks,
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
  stiff_limit,
  stiff_breaks,
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
    scale_analyte = scale_analyte,
    stiff_limit = stiff_limit,
    stiff_breaks = stiff_breaks
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
  scale_analyte,
  stiff_limit,
  stiff_breaks
) {
  plot_title <- if (is.null(title)) {
    as.character(group_name)
  } else {
    title
  }

  if (is.null(stiff_limit)) {
    stiff_limit <- ceiling(
      max(abs(data$stiff_x), na.rm = TRUE)
    )

    stiff_limit <- max(
      stiff_limit,
      1
    )
  }

  grid_breaks <- setdiff(seq(-stiff_limit, stiff_limit, by = stiff_breaks), 0)

  if (max(abs(data$stiff_x), na.rm = TRUE) > stiff_limit) {
    cli::cli_warn(
      "Some samples exceed {.arg stiff_limit} and will be truncated."
    )
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
      xintercept = grid_breaks,
      colour = "grey85",
      linewidth = 0.2
    )

  if (!is.null(scale_analyte)) {
    p <- p +
      ggplot2::geom_polygon(
        ggplot2::aes(
          fill = scale_value,
          group = interaction(
            .data[[location_id]],
            .data[[sample_date]]
          )
        ),
        colour = "black"
      ) +
      ggplot2::scale_fill_viridis_c(
        name = paste(scale_analyte)
      )
  } else {
    p <- p +
      ggplot2::geom_polygon(
        ggplot2::aes(
          group = interaction(
            .data[[location_id]],
            .data[[sample_date]]
          )
        ),
        fill = "white",
        colour = "black"
      )
  }

  # draw vertical reference line at 0 meq/L on top.
  p <- p +
    ggplot2::geom_vline(
      xintercept = 0,
      colour = "black",
      linewidth = 0.8
    )

  label_data <- data %>%
    dplyr::distinct(
      .data[[facet_var]],
      ion,
      label_side,
      stiff_y
    )

  label_data <- label_data %>%
    dplyr::mutate(
      label_x = dplyr::if_else(
        label_side == "left",
        -stiff_limit,
        stiff_limit
      ),
      hjust = dplyr::if_else(
        label_side == "left",
        1,
        0
      )
    )

  p <- p +
    ggplot2::geom_text(
      data = label_data,
      ggplot2::aes(
        x = label_x,
        y = stiff_y,
        label = ion,
        hjust = hjust
      ),
      inherit.aes = FALSE,
      parse = TRUE,
      size = 3
    )

  if (isTRUE(lines)) {
    p <- p +
      ggplot2::geom_segment(
        data = label_data,
        ggplot2::aes(
          x = -stiff_limit,
          xend = stiff_limit,
          y = stiff_y,
          yend = stiff_y
        ),
        inherit.aes = FALSE,
        linetype = "dotted",
        colour = "grey70"
      )
  }

  return(
    p +
      ggplot2::facet_wrap(
        stats::reformulate(facet_var),
        scales = "fixed"
      ) +
      ggplot2::coord_equal(
        clip = "off"
      ) +
      ggplot2::scale_x_continuous(
        limits = c(
          -stiff_limit * 1.10,
          stiff_limit * 1.10
        ),
        breaks = seq(
          -stiff_limit,
          stiff_limit,
          by = stiff_breaks
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
        plot.margin = ggplot2::margin(
          5.5,
          25,
          5.5,
          25
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

#' Animate Stiff Diagrams
#'
#' @param data Data frame of water quality data.
#' @param sample_date Sample date column.
#' @param pause Delay between frames in seconds.
#' @param ...
#' Additional arguments passed to `stiff_plot()`.
#'
#' @export

stiff_time_plot <- function(
  data,
  sample_date = "sample_date",
  pause = 1,
  ...
) {
  .animate_geochemical_plots(
    data = data,
    plot_fn = stiff_plot,
    sample_date = sample_date,
    pause = pause,
    ...
  )
}

#' Save animated Stiff Diagram to HTML
#'
#' @param data Data frame of water quality data.
#' @param sample_date Sample date column.
#' @param htmlfile Output HTML file.
#' @param pause Delay between frames in seconds.
#' @param ...
#' Additional arguments passed to `stiff_plot()`.
#'
#' @return Invisibly returns htmlfile.
#'
#' @export

stiff_time_html <- function(
  data,
  sample_date = "sample_date",
  htmlfile = "Stiff_plot.html",
  pause = 1,
  ...
) {
  .save_geochemical_animation(
    data = data,
    plot_fn = stiff_plot,
    sample_date = sample_date,
    htmlfile = htmlfile,
    title = "Stiff Diagram",
    img.name = "StiffPlot",
    pause = pause,
    ...
  )
}
