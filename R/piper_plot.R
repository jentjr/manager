#' @export
piper_plot <- function(x, ...) {
  UseMethod("piper_plot")
}

#' @export
piper_plot.data.frame <- function(
  x,
  location,
  analyte,
  result,
  unit,
  date,
  ...
) {
  x <- env_schema(
    x,
    location = {{ location }},
    analyte = {{ analyte }},
    result = {{ result }},
    unit = {{ unit }},
    date = {{ date }}
  )

  piper_plot(x, ...)
}

#' Create a Piper diagram for hydrochemical facies interpretation
#'
#' Creates a Piper diagram from major ion chemistry reported in a tidy
#' groundwater or surface water dataset. Major ions are converted to
#' milliequivalents per liter (meq/L), normalized to relative ionic
#' percentages, and plotted in the cation triangle, anion triangle, and
#' central diamond.
#'
#' Labels accept plotmath expressions.
#'
#' @param df A data frame containing water quality results in tidy format.
#'   Must contain one row per parameter result and include the columns
#'   specified by `location_id`, `sample_date`, `param_name`,
#'   `analysis_result`, and `default_unit`.
#'
#' @param location_id Character string identifying the column containing
#'   sample location identifiers.
#'
#' @param sample_date Character string identifying the column containing
#'   sample dates.
#'
#' @param param_name Character string identifying the column containing
#'   analyte or parameter names.
#'
#' @param analysis_result Character string identifying the column containing
#'   numeric analytical results.
#'
#' @param default_unit Character string identifying the column containing
#'   analytical reporting units.
#'
#' @param analyte_matching Character string specifying how analyte names are
#'   matched. Must be one of `"normalized"`, `"strict"`, or `"exact"`.
#'   The default is `"normalized"`.
#'
#'   \describe{
#'     \item{exact}{
#'       Uses analyte names exactly as reported. For example,
#'       `"Calcium, dissolved"` and `"Calcium, total"` are treated as
#'       separate analytes.
#'     }
#'
#'     \item{strict}{
#'       Applies text normalization (case, punctuation, and whitespace)
#'       while preserving distinctions such as dissolved versus total
#'       fractions. For example, `"Calcium, dissolved"` resolves to
#'       `"calcium dissolved"` and remains distinct from
#'       `"calcium total"`.
#'     }
#'
#'     \item{normalized}{
#'       Applies text normalization and resolves aliases using the package
#'       analyte lookup table. For example,
#'       `"Calcium, dissolved"`, `"Calcium, total"`, and `"Ca"` may all
#'       resolve to `"calcium"`.
#'     }
#'   }
#'
#' @param x_cation Character vector of analytes representing the left corner
#'   cation component of the Piper diagram. Default is
#'   `"Calcium, dissolved"`.
#'
#' @param x_cation_label Label displayed for the left corner cation axis.
#'   Labels are parsed as plotmath expressions.
#'
#' @param y_cation Character vector of analytes representing the upper
#'   cation component of the Piper diagram. Default is
#'   `"Magnesium, dissolved"`.
#'
#' @param y_cation_label Label displayed for the upper cation axis.
#'   Labels are parsed as plotmath expressions.
#'
#' @param z_cation Character vector of analytes representing the right corner
#'   cation component of the Piper diagram. Defaults to sodium plus
#'   potassium:
#'   `c("Sodium, dissolved", "Potassium, dissolved")`.
#'
#' @param z_cation_label Label displayed for the right cation axis.
#'   Labels are parsed as plotmath expressions.
#'
#' @param x_anion Character vector of analytes representing the left corner
#'   anion component of the Piper diagram. Defaults to chloride plus
#'   fluoride:
#'   `c("Chloride, total", "Fluoride, total")`.
#'
#' @param x_anion_label Label displayed for the left anion axis.
#'   Labels are parsed as plotmath expressions.
#'
#' @param y_anion Character vector of analytes representing the lower-right
#'   anion component of the Piper diagram. The default assumes alkalinity is
#'   reported as `"Alkalinity, total (lab)"` in units of mg/L as CaCO3.
#'
#' @param y_anion_label Label displayed for the bicarbonate/carbonate axis.
#'   Labels are parsed as plotmath expressions.
#'
#' @param z_anion Character vector of analytes representing the upper anion
#'   component of the Piper diagram. Default is `"Sulfate, total"`.
#'
#' @param z_anion_label Label displayed for the sulfate axis. Labels are
#'   parsed as plotmath expressions.
#'
#' @param x_z_anion_label Label displayed on the upper-left side of the
#'   central diamond. Labels are parsed as plotmath expressions.
#'
#' @param x_y_cation_label Label displayed on the upper-right side of the
#'   central diamond. Labels are parsed as plotmath expressions.
#'
#' @param scale_analyte Optional analyte name used to scale point
#' size within the central diamond.
#'
#' The analyte must be present in the prepared Piper dataset.
#' If `NULL`, all points are plotted with a constant size.
#'
#' @param group_col Optional column name used to assign point shape.
#'   Each location must correspond to a single grouping value.
#'
#' @param transparency Numeric transparency value passed to
#'   [ggplot2::geom_point()]. Must be between 0 and 1. Default is `0.7`.
#'
#' @param pnt_size Numeric point size used when
#'   `scale_analyte = NULL`. Default is `3`.
#'
#' @param label_size Numeric text size used for Piper axis labels. Default
#'   is `3`.
#'
#' @param title Optional plot title. Defaults to `NULL`.
#'
#' @return
#' An object of class `"ggplot"` containing a Piper diagram.
#'
#' @details
#' Major ion analytes are processed through the package geochemical
#' preparation pipeline before Piper coordinates are calculated.
#'
#' The pipeline performs:
#'
#' \itemize{
#'   \item analyte resolution;
#'   \item analyte validation;
#'   \item unit validation;
#'   \item duplicate detection and averaging;
#'   \item conversion to milliequivalents per liter (meq/L);
#'   \item construction of a wide-format geochemical dataset.
#' }
#'
#' The resulting geochemical dataset is transformed into cation,
#' anion, and central-diamond Piper coordinates for plotting.
#'
#' @section Analyte matching:
#'
#' The same water chemistry dataset may contain multiple reporting
#' conventions for a given constituent (for example `"SO4"` versus
#' `"Sulfate"` or `"Calcium, dissolved"` versus `"Calcium, total"`).
#'
#' The `analyte_matching` argument controls whether these values are
#' treated as distinct analytes or resolved to a common analyte name prior
#' to ion conversion and Piper coordinate calculation.
#'
#' For most hydrochemical applications,
#' `analyte_matching = "normalized"` is recommended.
#'
#' When `analyte_matching = "normalized"`, analyte aliases are resolved
#' using the package analyte lookup table. This allows multiple laboratory
#' reporting conventions to be treated as the same chemical constituent.
#'
#' Examples include:
#'
#' \itemize{
#'   \item `"Ca"` and `"Calcium"`
#'   \item `"SO4"` and `"Sulfate"`
#'   \item `"Alkalinity, total"` and `"Bicarbonate"`
#'   \item `"Calcium, dissolved"` and `"Calcium, total"`
#' }
#'
#' The `"exact"` option preserves laboratory-reported analyte names and may
#' be useful for QA/QC, data auditing, or evaluating dissolved versus total
#' fractions independently. The `"strict"` option provides normalized text
#' matching while preserving those analytical distinctions.
#'
#' @section Validation and error handling:
#'
#' The function performs several validation checks before constructing the
#' Piper diagram.
#'
#' \strong{Required columns}
#'
#' The input data frame must contain the columns identified by
#' `location_id`, `sample_date`, `param_name`, `analysis_result`, and
#' `default_unit`. An error is thrown if any required columns are missing.
#'
#' \strong{Analyte resolution}
#'
#' Specified Piper ions are resolved according to the selected
#' `analyte_matching` strategy. Analytes that cannot be resolved are
#' retained using their normalized names and are not automatically dropped.
#'
#' \strong{Unit validation}
#'
#' Major ion analytes are validated and converted through the package
#' geochemical preparation pipeline. Unsupported units generate an error.
#'
#' \strong{Duplicate analytes}
#'
#' Duplicate analyte measurements within a sample are detected after analyte
#' resolution. A warning is issued when duplicate analyte/sample
#' combinations are found. Duplicate results are averaged before
#' conversion to milliequivalents and Piper coordinate calculation.
#'
#' Under `analyte_matching = "normalized"`, analytes such as
#' `"Calcium, dissolved"` and `"Calcium, total"` may resolve to the same
#' analyte and therefore be treated as duplicates.
#'
#' \strong{Missing major ions}
#'
#' A warning is issued when one or more requested Piper analytes are not
#' present in the input data.
#'
#' Samples with missing major ion concentrations are retained during data
#' preparation but omitted from the corresponding Piper triangle or diamond
#' when coordinates cannot be calculated.
#'
#' Informational messages report the number and percentage of complete
#' Piper analyses available for plotting.
#'
#' \strong{Grouping variables}
#'
#' When `group_col` is supplied, each `location_id` must map to exactly one
#' grouping value. If multiple grouping values are associated with the same
#' location, an error is thrown.
#'
#' @section Warnings:
#'
#' The function may generate warnings when:
#' \itemize{
#'   \item requested Piper ions are not present in the dataset;
#'   \item duplicate analyte/sample combinations are detected;
#'   \item samples are omitted due to incomplete major-ion chemistry.
#' }
#'
#' These warnings are intended to help identify data-quality issues and
#' unexpected analyte resolution behavior.
#'
#' @examples
#' data(gw_data)
#' wells <- c("MW-1", "MW-2", "MW-3", "MW-4", "MW-5")
#'
#' # preserve dissolved and total analytes as separate measurements
#' gw_data %>%
#'   dplyr::filter(location_id %in% wells) %>%
#'   piper_plot(
#'     analyte_matching = "exact"
#'   )
#'
#' # normalize analyte aliases to canonical ion names
#' gw_data %>%
#'   dplyr::filter(location_id %in% wells) %>%
#'   piper_plot(
#'     analyte_matching = "normalized"
#'   )
#'
#' # scaled by analyte
#' gw_data %>%
#' dplyr::filter(location_id %in% wells) %>%
#' piper_plot(
#' scale_analyte = "Total Dissolved Solids",
#' title = "Example Piper Diagram"
#' )
#'
#' # use plotmath expressions for labels
#' gw_data %>%
#' dplyr::filter(location_id %in% wells) %>%
#' piper_plot(x_cation_label = "Ca~phantom()^+2")
#'
#' @export

piper_plot.env_schema <- function(
  df,
  location_id = "location_id",
  sample_date = "sample_date",
  param_name = "param_name",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  analyte_matching = c("normalized", "strict", "exact"),
  x_cation = "Calcium, dissolved",
  x_cation_label = "Ca",
  y_cation = "Magnesium, dissolved",
  y_cation_label = "Mg",
  z_cation = c(
    "Sodium, dissolved",
    "Potassium, dissolved"
  ),
  z_cation_label = "Na + K",
  x_anion = c(
    "Chloride, total",
    "Fluoride, total"
  ),
  x_anion_label = "Cl + F",
  y_anion = "Alkalinity, total (lab)",
  y_anion_label = "HCO[3] + CO[3]",
  z_anion = "Sulfate, total",
  z_anion_label = "SO4",
  x_z_anion_label = "SO4 + Cl + F",
  x_y_cation_label = "Ca + Mg",
  scale_analyte = NULL,
  group_col = NULL,
  transparency = 0.7,
  pnt_size = 3,
  label_size = 3,
  title = NULL
) {
  analyte_matching <- rlang::arg_match(analyte_matching)

  scale_col <- NULL

  if (!is.null(scale_analyte)) {
    scale_col <- .resolve_geochemical_analytes(
      scale_analyte,
      analyte_matching
    )

    if (length(scale_col) == 0) {
      cli::cli_warn(
        c(
          "Unable to resolve {.val {scale_analyte}}.",
          "i" = "Point size scaling will be disabled."
        )
      )

      scale_col <- NULL
    } else {
      scale_col <- scale_col[1]
    }
  }

  required_cols <- c(
    location_id,
    sample_date,
    param_name,
    analysis_result,
    default_unit
  )

  missing_cols <- setdiff(
    required_cols,
    names(df)
  )

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "Missing required columns.",
        "x" = paste(missing_cols, collapse = ", ")
      )
    )
  }

  piper_df <- df %>%
    .transform_piper_data(
      location_id = location_id,
      sample_date = sample_date,
      param_name = param_name,
      analysis_result = analysis_result,
      default_unit = default_unit,
      analyte_matching = analyte_matching,
      x_cation = x_cation,
      y_cation = y_cation,
      z_cation = z_cation,
      x_anion = x_anion,
      y_anion = y_anion,
      z_anion = z_anion,
      scale_analyte = scale_analyte
    )

  if (!is.null(group_col)) {
    group_df <- df %>%
      dplyr::distinct(
        !!rlang::sym(location_id),
        !!rlang::sym(group_col)
      )
    dup_groups <- group_df %>%
      dplyr::count(!!rlang::sym(location_id)) %>%
      dplyr::filter(n > 1)

    if (nrow(dup_groups) > 0) {
      cli::cli_abort(
        c(
          "{.field {group_col}} must contain only one value per location.",
          "x" = "{nrow(dup_groups)} locations contain multiple grouping values."
        )
      )
    }

    join_by <- stats::setNames(
      location_id,
      location_id
    )

    piper_df <- piper_df %>%
      dplyr::left_join(
        group_df,
        by = join_by
      )
  }

  cation_df <- piper_df %>%
    dplyr::filter(
      !is.na(cation_x),
      !is.na(cation_y)
    )

  anion_df <- piper_df %>%
    dplyr::filter(
      !is.na(anion_x),
      !is.na(anion_y)
    )

  diamond_df <- piper_df %>%
    dplyr::filter(
      !is.na(diamond_x),
      !is.na(diamond_y)
    )

  piper <- .ggplot_piper()

  # cation triangle
  piper <- .add_piper_points(
    plot = piper,
    data = cation_df,
    x = "cation_x",
    y = "cation_y",
    location_id = location_id,
    group_col = group_col,
    alpha = transparency,
    point_size = pnt_size
  )

  # anion triangle
  piper <- .add_piper_points(
    plot = piper,
    data = anion_df,
    x = "anion_x",
    y = "anion_y",
    location_id = location_id,
    group_col = group_col,
    alpha = transparency,
    point_size = pnt_size
  )

  # diamond
  piper <- .add_piper_points(
    plot = piper,
    data = diamond_df,
    x = "diamond_x",
    y = "diamond_y",
    location_id = location_id,
    group_col = group_col,
    scale_col = scale_col,
    alpha = transparency,
    point_size = pnt_size
  )

  piper <- .add_piper_labels(
    plot = piper,
    label_size = label_size,
    x_cation_label = x_cation_label,
    y_cation_label = y_cation_label,
    z_cation_label = z_cation_label,
    x_anion_label = x_anion_label,
    y_anion_label = y_anion_label,
    z_anion_label = z_anion_label,
    x_z_anion_label = x_z_anion_label,
    x_y_cation_label = x_y_cation_label
  )

  if (!is.null(scale_col)) {
    piper <- piper +
      ggplot2::scale_size_area(
        name = tools::toTitleCase(scale_analyte),
        max_size = 8
      )
  }

  if (requireNamespace("viridis", quietly = TRUE)) {
    piper <- piper +
      viridis::scale_colour_viridis(discrete = TRUE)
  } else {
    piper <- piper +
      ggplot2::scale_colour_discrete()
  }

  guide_args <- list(
    colour = ggplot2::guide_legend(location_id),
    alpha = "none"
  )

  if (!is.null(group_col)) {
    guide_args$shape <- ggplot2::guide_legend(group_col)
  }

  piper <- piper +
    do.call(ggplot2::guides, guide_args)

  piper +
    ggplot2::ggtitle(title) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}

#' Add a Piper point layer
#'
#' @noRd
.add_piper_points <- function(
  plot,
  data,
  x,
  y,
  location_id = "location_id",
  group_col = NULL,
  scale_col = NULL,
  alpha = 0.7,
  point_size = 3
) {
  aes_args <- list(
    x = rlang::sym(x),
    y = rlang::sym(y),
    colour = rlang::sym(location_id)
  )

  if (!is.null(group_col)) {
    aes_args$shape <- rlang::expr(
      factor(!!rlang::sym(group_col), exclude = NULL)
    )
  }

  if (!is.null(scale_col)) {
    if (!scale_col %in% names(data)) {
      cli::cli_warn(
        c(
          "Size scaling analyte not available.",
          "i" = "Using constant point size."
        )
      )

      scale_col <- NULL
    } else {
      aes_args$size <- rlang::sym(scale_col)
    }
  }

  mapping <- do.call(ggplot2::aes, aes_args)

  layer_args <- list(
    data = data,
    mapping = mapping,
    alpha = alpha
  )

  if (is.null(scale_col)) {
    layer_args$size <- point_size
  }

  plot +
    do.call(
      ggplot2::geom_point,
      layer_args
    )
}

#' Add Piper labels
#'
#' @noRd
.add_piper_labels <- function(
  plot,
  label_size,
  x_cation_label,
  y_cation_label,
  z_cation_label,
  x_anion_label,
  y_anion_label,
  z_anion_label,
  x_z_anion_label,
  x_y_cation_label
) {
  label_df <- tibble::tribble(
    ~x    , ~y  , ~label           , ~angle ,
     17   ,  50 , y_cation_label   ,     60 ,
     82   ,  50 , z_cation_label   ,    -60 ,
     50   , -10 , x_cation_label   ,      0 ,
    170   , -10 , x_anion_label    ,      0 ,
    205   ,  50 , z_anion_label    ,    -60 ,
    138.5 ,  50 , y_anion_label    ,     60 ,
     72.5 , 150 , x_z_anion_label  ,     60 ,
    147.5 , 150 , x_y_cation_label ,    -60
  )

  plot +
    ggplot2::geom_text(
      data = label_df,
      ggplot2::aes(
        x = x,
        y = y,
        label = label,
        angle = angle
      ),
      parse = TRUE,
      inherit.aes = FALSE,
      size = label_size
    )
}


#' Function to create an animated Piper plot through time
#' using the animation package
#'
#' @param df data frame of groundwater data transformed using
#' @inheritDotParams piper_plot
#' @export

piper_time_plot <- function(df, ...) {
  if (!requireNamespace("animation", quietly = TRUE)) {
    stop(
      "animation package needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  iter <- unique(df$sample_date)

  for (i in seq_along(iter)) {
    piper_plot(df[df$sample_date == iter[i], ], title = paste(iter[i]), ...)
    animation::ani.pause()
  }
}

#' Function to create an aminated Piper plot and save to html
#'
#' @param df data frame of groundwater data transformed using
#' @inheritDotParams piper_plot
#' @export

piper_time_html <- function(df, ...) {
  if (!requireNamespace("animation", quietly = TRUE)) {
    stop(
      "animation package needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  animation::saveHTML(
    {
      animation::ani.options(
        nmax = length(unique(df$sample_date)),
        outdir = getwd()
      )

      piper_time_plot(df, ...)
    },

    img.name = "PiperPlot",
    htmlfile = "Piper_plot.html",
    autobrowse = TRUE,
    title = "Piper Plot"
  )
}

#' Function to create base Piper plot
#'
#' @noRd

.ggplot_piper <- function() {
  p <- ggplot2::ggplot() +

    ## left hand ternary plot
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 100, yend = 0)) +
    ggplot2::geom_segment(ggplot2::aes(
      x = 0,
      y = 0,
      xend = 50,
      yend = 86.603
    )) +
    ggplot2::geom_segment(ggplot2::aes(
      x = 50,
      y = 86.603,
      xend = 100,
      yend = 0
    )) +

    ## right hand ternary plot
    ## shifted right by 20 points
    ggplot2::geom_segment(ggplot2::aes(x = 120, y = 0, xend = 220, yend = 0)) +
    ggplot2::geom_segment(ggplot2::aes(
      x = 120,
      y = 0,
      xend = 170,
      yend = 86.603
    )) +
    ggplot2::geom_segment(ggplot2::aes(
      x = 170,
      y = 86.603,
      xend = 220,
      yend = 0
    )) +

    ## Upper diamond
    ggplot2::geom_segment(ggplot2::aes(
      x = 110,
      y = 190.5266,
      xend = 60,
      yend = 103.9236
    )) +
    ggplot2::geom_segment(ggplot2::aes(
      x = 110,
      y = 190.5266,
      xend = 160,
      yend = 103.9236
    )) +
    ggplot2::geom_segment(ggplot2::aes(
      x = 110,
      y = 17.3206,
      xend = 160,
      yend = 103.9236
    )) +
    ggplot2::geom_segment(ggplot2::aes(
      x = 110,
      y = 17.3206,
      xend = 60,
      yend = 103.9236
    )) +

    ## Add grid lines to the plots
    ggplot2::geom_segment(
      ggplot2::aes(x = x1, y = y1, yend = y2, xend = x2),
      data = data.frame(
        x1 = c(20, 40, 60, 80),
        x2 = c(10, 20, 30, 40),
        y1 = c(0, 0, 0, 0),
        y2 = c(17.3206, 34.6412, 51.9618, 69.2824)
      ),
      linetype = "dashed",
      linewidth = 0.25,
      colour = "grey50"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x1, y = y1, yend = y2, xend = x2),
      data = data.frame(
        x1 = c(20, 40, 60, 80),
        x2 = c(60, 70, 80, 90),
        y1 = c(0, 0, 0, 0),
        y2 = c(69.2824, 51.9618, 34.6412, 17.3206)
      ),
      linetype = "dashed",
      linewidth = 0.25,
      colour = "grey50"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x1, y = y1, yend = y2, xend = x2),
      data = data.frame(
        x1 = c(10, 20, 30, 40),
        x2 = c(90, 80, 70, 60),
        y1 = c(17.3206, 34.6412, 51.9618, 69.2824),
        y2 = c(17.3206, 34.6412, 51.9618, 69.2824)
      ),
      linetype = "dashed",
      linewidth = 0.25,
      colour = "grey50"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x1, y = y1, yend = y2, xend = x2),
      data = data.frame(
        x1 = c(140, 160, 180, 200),
        x2 = c(130, 140, 150, 160),
        y1 = c(0, 0, 0, 0),
        y2 = c(17.3206, 34.6412, 51.9618, 69.2824)
      ),
      linetype = "dashed",
      linewidth = 0.25,
      colour = "grey50"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x1, y = y1, yend = y2, xend = x2),
      data = data.frame(
        x1 = c(140, 160, 180, 200),
        x2 = c(180, 190, 200, 210),
        y1 = c(0, 0, 0, 0),
        y2 = c(69.2824, 51.9618, 34.6412, 17.3206)
      ),
      linetype = "dashed",
      linewidth = 0.25,
      colour = "grey50"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x1, y = y1, yend = y2, xend = x2),
      data = data.frame(
        x1 = c(130, 140, 150, 160),
        x2 = c(210, 200, 190, 180),
        y1 = c(17.3206, 34.6412, 51.9618, 69.2824),
        y2 = c(17.3206, 34.6412, 51.9618, 69.2824)
      ),
      linetype = "dashed",
      linewidth = 0.25,
      colour = "grey50"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x1, y = y1, yend = y2, xend = x2),
      data = data.frame(
        x1 = c(100, 90, 80, 70),
        y1 = c(34.6412, 51.9618, 69.2824, 86.603),
        x2 = c(150, 140, 130, 120),
        y2 = c(121.2442, 138.5648, 155.8854, 173.2060)
      ),
      linetype = "dashed",
      linewidth = 0.25,
      colour = "grey50"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x1, y = y1, yend = y2, xend = x2),
      data = data.frame(
        x1 = c(70, 80, 90, 100),
        y1 = c(121.2442, 138.5648, 155.8854, 173.2060),
        x2 = c(120, 130, 140, 150),
        y2 = c(34.6412, 51.9618, 69.2824, 86.603)
      ),
      linetype = "dashed",
      linewidth = 0.25,
      colour = "grey50"
    ) +

    # add the grid labels
    # X Cation
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(20, 40, 60, 80),
        y = c(-5, -5, -5, -5),
        label = c(80, 60, 40, 20)
      ),
      size = 2.5
    ) +
    # Y Cation
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(35, 25, 15, 5),
        y = c(69.2824, 51.9618, 34.6412, 17.3206),
        label = c(80, 60, 40, 20)
      ),
      size = 2.5
    ) +
    # Z Cation
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(95, 85, 75, 65),
        y = c(17.3206, 34.6412, 51.9618, 69.2824),
        label = c(80, 60, 40, 20)
      ),
      size = 2.5
    ) +
    ggplot2::coord_equal(ratio = 1) +

    # Y Anion
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(155, 145, 135, 125),
        y = c(69.2824, 51.9618, 34.6412, 17.3206),
        label = c(20, 40, 60, 80)
      ),
      size = 2.5
    ) +
    # Z Anion
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(215, 205, 195, 185),
        y = c(17.3206, 34.6412, 51.9618, 69.2824),
        label = c(20, 40, 60, 80)
      ),
      size = 2.5
    ) +
    # X Anion
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(140, 160, 180, 200),
        y = c(-5, -5, -5, -5),
        label = c(20, 40, 60, 80)
      ),
      size = 2.5
    ) +
    # left lower diamond
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(95, 85, 75, 65),
        y = c(34.6412, 51.9618, 69.2824, 86.603),
        label = c(80, 60, 40, 20)
      ),
      size = 2.5
    ) +
    # right upper diamond
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(155, 145, 135, 125),
        y = c(121.2442, 138.5648, 155.8854, 173.2060),
        label = c(20, 40, 60, 80)
      ),
      size = 2.5
    ) +
    # left upper diamond
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(65, 75, 85, 95),
        y = c(121.2442, 138.5648, 155.8854, 173.2060),
        label = c(20, 40, 60, 80)
      ),
      size = 2.5
    ) +
    # right lower diamond
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(125, 135, 145, 155),
        y = c(34.6412, 51.9618, 69.2824, 86.603),
        label = c(80, 60, 40, 20)
      ),
      size = 2.5
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    )

  return(p)
}
