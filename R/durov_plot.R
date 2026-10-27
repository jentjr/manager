#' Durov Diagram
#'
#' Creates a Durov diagram for interpretation of hydrochemical facies,
#' mixing processes, ion exchange, and geochemical evolution.
#'
#' @param df Data frame containing analytical results.
#' @param location_id Location identifier column.
#' @param sample_date Sample date column.
#' @param param_name Analyte column.
#' @param analysis_result Result column.
#' @param default_unit Unit column.
#' @param analyte_matching Analyte matching method.
#' @param facet_var Variable used for faceting.
#' @param title Plot title.
#'
#' @return
#' A ggplot object.
#'
#' @export
durov_plot <- function(
  df,
  location_id = "location_id",
  sample_date = "sample_date",
  param_name = "param_name",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  analyte_matching = c(
    "normalized",
    "strict",
    "exact"
  ),
  title = NULL
) {
  analyte_matching <- rlang::arg_match(
    analyte_matching
  )

  chemistry_df <- .prepare_major_ion_chemistry(
    df = df,
    location_id = location_id,
    sample_date = sample_date,
    param_name = param_name,
    analysis_result = analysis_result,
    default_unit = default_unit,
    analyte_matching = analyte_matching
  )

  plot_df <- .transform_durov_data(
    chemistry_df
  )

  if (
    !all(
      c(
        "location_id",
        "sample_date",
        "durov_x_plot",
        "durov_y_plot"
      ) %in%
        names(plot_df)
    )
  ) {
    rlang::abort(
      "Invalid Durov transform output."
    )
  }

  plot_df <- plot_df %>%
    dplyr::filter(
      !is.na(durov_x_plot),
      !is.na(durov_y_plot)
    )

  framework <- .durov_framework()

  legend_text <- paste(
    c(
      "Durov Field Interpretation",
      "",
      paste0(
        .durov_field_legend()$field,
        ": ",
        .durov_field_legend()$interpretation
      )
    ),
    collapse = "\n"
  )

  ggplot2::ggplot() +

    # -------------------------------------------------
    # CATION TRIANGLE
    # -------------------------------------------------

    ggplot2::geom_path(
      data = framework$cation_triangle,
      ggplot2::aes(
        x,
        y
      )
    ) +

    ggplot2::geom_point(
      data = plot_df,
      ggplot2::aes(
        cation_x,
        cation_y,
        colour = location_id
      ),
      show.legend = FALSE
    ) +
    ggplot2::annotate(
      "text",
      x = 0,
      y = -5,
      label = "Ca"
    ) +

    ggplot2::annotate(
      "text",
      x = 100,
      y = -5,
      label = "Na + K"
    ) +

    ggplot2::annotate(
      "text",
      x = 50,
      y = 90,
      label = "Mg"
    ) +

    # -------------------------------------------------
    # ANION TRIANGLE
    # -------------------------------------------------

    ggplot2::geom_path(
      data = framework$anion_triangle,
      ggplot2::aes(
        x,
        y
      )
    ) +

    ggplot2::geom_point(
      data = plot_df,
      ggplot2::aes(
        anion_x,
        anion_y,
        colour = location_id
      ),
      show.legend = FALSE
    ) +
    ggplot2::annotate(
      "text",
      x = 120,
      y = -5,
      label = expression(
        HCO[3]
      )
    ) +

    ggplot2::annotate(
      "text",
      x = 220,
      y = -5,
      label = "Cl"
    ) +

    ggplot2::annotate(
      "text",
      x = 170,
      y = 90,
      label = expression(
        SO[4]
      )
    ) +

    # -------------------------------------------------
    # DUROV SQUARE
    # -------------------------------------------------

    ggplot2::geom_path(
      data = framework$square,
      ggplot2::aes(
        x,
        y
      )
    ) +

    ggplot2::geom_segment(
      data = framework$square_grid_v,
      ggplot2::aes(
        x = x,
        xend = xend,
        y = y,
        yend = yend
      ),
      linetype = 2,
      linewidth = 0.25
    ) +

    ggplot2::geom_segment(
      data = framework$square_grid_h,
      ggplot2::aes(
        x = x,
        xend = xend,
        y = y,
        yend = yend
      ),
      linetype = 2,
      linewidth = 0.25
    ) +

    ggplot2::geom_point(
      data = plot_df,
      ggplot2::aes(
        durov_x_plot,
        durov_y_plot,
        colour = location_id
      ),
      size = 2
    ) +

    ggplot2::coord_equal(
      xlim = c(-10, 230),
      ylim = c(-10, 210),
      expand = FALSE,
      clip = "off"
    ) +

    ggplot2::theme_bw() +

    ggplot2::annotate(
      "text",
      x = 110,
      y = 95,
      label = "Na + K (% of cations)"
    ) +

    ggplot2::annotate(
      "text",
      x = 50,
      y = 150,
      label = "Cl (% of anions)",
      angle = 90
    ) +

    ggplot2::labs(
      title = title,
      colour = NULL,
      caption = legend_text
    ) +

    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        hjust = 0.5
      ),
      plot.margin = ggplot2::margin(20, 20, 20, 20),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        size = 8,
        margin = ggplot2::margin(
          t = 10
        )
      )
    ) +
    ggplot2::geom_text(
      data = framework$field_labels,
      ggplot2::aes(
        x,
        y,
        label = label
      ),
      size = 3
    )
}

#' Durov framework
#'
#' @noRd
.durov_framework <- function() {
  h <- sqrt(3) / 2 * 100

  list(
    cation_triangle = tibble::tribble(
      ~x  , ~y ,
        0 ,  0 ,
      100 ,  0 ,
       50 , h  ,
        0 ,  0
    ),

    anion_triangle = tibble::tribble(
      ~x  , ~y ,
      120 ,  0 ,
      220 ,  0 ,
      170 , h  ,
      120 ,  0
    ),

    square = tibble::tribble(
      ~x  , ~y      ,
       60 , h       ,
      160 , h       ,
      160 , h + 100 ,
       60 , h + 100 ,
       60 , h
    ),

    square_grid_v = tibble::tribble(
      ~x  , ~xend , ~y , ~yend   ,
       85 ,    85 , h  , h + 100 ,
      110 ,   110 , h  , h + 100 ,
      135 ,   135 , h  , h + 100
    ),

    square_grid_h = tibble::tribble(
      ~x , ~xend , ~y     , ~yend  ,
      60 ,   160 , h + 25 , h + 25 ,
      60 ,   160 , h + 50 , h + 50 ,
      60 ,   160 , h + 75 , h + 75
    ),

    field_labels = tibble::tribble(
      ~label , ~x    , ~y       ,

      "1"    ,  72.5 , h + 87.5 ,
      "2"    ,  97.5 , h + 87.5 ,
      "3"    , 122.5 , h + 87.5 ,

      "4"    ,  72.5 , h + 62.5 ,
      "5"    ,  97.5 , h + 62.5 ,
      "6"    , 122.5 , h + 62.5 ,

      "7"    ,  72.5 , h + 37.5 ,
      "8"    ,  97.5 , h + 37.5 ,
      "9"    , 122.5 , h + 37.5
    )
  )
}


#' Durov field legend
#'
#' @noRd
.durov_field_legend <- function() {
  tibble::tribble(
    ~field , ~interpretation                          ,

         1 , "Ca-HCO3 facies (recharge waters)"       ,
         2 , "Mixed Ca-Mg-HCO3 facies"                ,
         3 , "Na-HCO3 facies"                         ,

         4 , "Ca-Mg-Cl facies"                        ,
         5 , "Mixed water / no dominant facies"       ,
         6 , "Na-Cl facies"                           ,

         7 , "Ca-Cl facies"                           ,
         8 , "Ion exchange influenced waters"         ,
         9 , "Advanced ion exchange or saline waters"
  )
}
