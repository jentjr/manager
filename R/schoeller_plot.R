#' @export
schoeller_plot <- function(x, ...) {
  UseMethod("schoeller_plot")
}

#' @export
schoeller_plot.data.frame <- function(
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

  schoeller_plot(x, ...)
}


#' Schoeller Diagram
#'
#' Creates a Schoeller diagram for comparing major ion chemistry among
#' groundwater or surface water samples.
#'
#' The function automatically:
#'
#' * Resolves analyte names using the package analyte metadata tables.
#' * Converts analyte concentrations to milliequivalents per liter (meq/L).
#' * Aggregates replicate results within each
#'   location/sample/analyte combination.
#' * Combines sodium and potassium into a single
#'   Na + K Schoeller axis.
#' * Produces a logarithmic concentration profile suitable for
#'   hydrochemical interpretation.
#'
#' Analyte names may be supplied using any supported alias when
#' `analyte_matching = "normalized"` or `"strict"`.
#'
#' @param df Data frame containing environmental analytical results in
#'   tidy format.
#'
#' @param location_id Character. Column containing location identifiers.
#'
#' @param sample_date Character. Column containing sample dates.
#'
#' @param param_name Character. Column containing analyte names.
#'
#' @param analysis_result Character. Column containing analytical results.
#'
#' @param default_unit Character. Column containing analytical units.
#'
#' @param calcium Character. Calcium analyte name or alias.
#'
#' @param magnesium Character. Magnesium analyte name or alias.
#'
#' @param sodium Character. Sodium analyte name or alias.
#'
#' @param potassium Character. Potassium analyte name or alias.
#'
#' @param chloride Character. Chloride analyte name or alias.
#'
#' @param sulfate Character. Sulfate analyte name or alias.
#'
#' @param alkalinity Character. Alkalinity analyte name or alias.
#'
#' @param analyte_matching Character. Method used to match analyte names.
#' One of:
#'
#' * `"normalized"` (default)
#' * `"strict"`
#' * `"exact"`
#'
#' See [resolve_analyte_name()] for details.
#'
#' @param facet_var Character. Variable used for faceting.
#' One of:
#'
#' * `"location_id"`
#' * `"sample_date"`
#'
#' When `"location_id"` is selected, each location receives its own panel
#' and individual sample events are plotted as separate lines.
#'
#' When `"sample_date"` is selected, each sampling event receives its
#' own panel and individual locations are plotted as separate lines.
#'
#' @param title Optional plot title.
#'
#' @param lwt Numeric line width.
#'
#' @return
#' A ggplot object.
#'
#' @details
#' Schoeller diagrams display major ion concentrations on a logarithmic
#' y-axis and are commonly used to:
#'
#' * Compare groundwater chemistry between monitoring wells.
#' * Evaluate temporal geochemical trends.
#' * Identify mixing relationships.
#' * Identify hydrochemical facies.
#' * Screen for anomalous ion signatures.
#'
#' Concentrations are internally converted to milliequivalents per liter
#' (meq/L) using analyte metadata stored in `analyte_tbl`.
#'
#' Missing analytes are permitted and are omitted from the plotted profile.
#'
#' @examples
#'
#' # Default Schoeller diagram
#' schoeller_plot(gw_data)
#'
#' # Facet by sampling event
#' schoeller_plot(
#'   gw_data,
#'   facet_var = "sample_date"
#' )
#'
#' # Custom analyte aliases using normalized matching
#' schoeller_plot(
#'   gw_data,
#'   calcium = "Ca",
#'   magnesium = "Mg",
#'   sodium = "Na",
#'   potassium = "K",
#'   chloride = "Cl",
#'   sulfate = "SO4",
#'   alkalinity = "HCO3",
#'   analyte_matching = "normalized"
#' )
#'
#' # Restrict to selected monitoring wells
#' gw_data %>%
#'   dplyr::filter(
#'     location_id %in% c(
#'       "MW-1",
#'       "MW-2",
#'       "MW-3"
#'     )
#'   ) %>%
#'   schoeller_plot()
#'
#' # Temporal comparison for a single well
#' gw_data %>%
#'   dplyr::filter(
#'     location_id == "MW-1"
#'   ) %>%
#'   schoeller_plot(
#'     facet_var = "location_id"
#'   )
#'
#' # Customized appearance
#' schoeller_plot(
#'   gw_data,
#'   title = "Major Ion Chemistry",
#'   lwt = 1.5
#' )
#'
#' @export
schoeller_plot.env_schmema <- function(
  df,
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
  analyte_matching = c(
    "normalized",
    "strict",
    "exact"
  ),
  facet_var = c(
    "location_id",
    "sample_date"
  ),
  title = NULL,
  lwt = 1
) {
  analyte_matching <- rlang::arg_match(
    analyte_matching
  )

  facet_var <- rlang::arg_match(
    facet_var
  )

  chemistry_df <- .prepare_major_ion_chemistry(
    df = df,
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
    default_unit = default_unit,
    analyte_matching = analyte_matching
  )

  df_plot <- .transform_schoeller_data(
    chemistry_df = chemistry_df
  )

  p <- ggplot2::ggplot(
    df_plot,
    ggplot2::aes(
      x = schoeller_axis,
      y = analysis_result
    )
  ) +
    ggplot2::scale_y_log10(
      labels = scales::label_number()
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = "Concentration (meq/L)",
      colour = NULL
    ) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        hjust = 0.5
      )
    )

  p <- .add_schoeller_lines(
    p = p,
    facet_var = facet_var,
    lwt = lwt
  )

  if (
    requireNamespace(
      "viridis",
      quietly = TRUE
    )
  ) {
    p <- p +
      viridis::scale_colour_viridis(
        discrete = TRUE
      )
  }

  p
}

#' Add Schoeller lines
#'
#' @noRd
.add_schoeller_lines <- function(
  p,
  facet_var,
  lwt
) {
  if (facet_var == "sample_date") {
    p +
      ggplot2::geom_line(
        ggplot2::aes(
          group = location_id,
          colour = location_id
        ),
        linewidth = lwt
      ) +
      ggplot2::facet_wrap(
        ggplot2::vars(sample_date)
      ) +
      ggplot2::guides(
        colour = ggplot2::guide_legend(
          title = "Location ID"
        )
      )
  } else {
    p +
      ggplot2::geom_line(
        ggplot2::aes(
          group = sample_date,
          colour = factor(sample_date)
        ),
        linewidth = lwt
      ) +
      ggplot2::facet_wrap(
        ggplot2::vars(location_id)
      ) +
      ggplot2::guides(
        colour = ggplot2::guide_legend(
          title = "Sample Date"
        )
      )
  }
}
