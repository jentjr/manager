#' @title Stiff Diagram
#'
#' @description plots a stiff diagram
#'
#' @details tidy dataframe of water quality data in mg/L is converted to
#' meq/L and then transformed to coordinates for Stiff diagmram.
#'
#' @param df data frame of water quality data
#' @param location_id column for sample locations
#' @param sample_date column for sample date
#' @param param_name column for constituent names
#' @param analysis_result column for analysis result
#' @param default_unit column for units. Assumes all units are in mg/L and
#' converts usign \code{\link{conc_to_meq}}
#' @param magnesium name for magnesium
#' @param calcium name for calcium
#' @param sodium name for sodium
#' @param potassium name for potassium
#' @param chloride name for chloride
#' @param sulfate name for sulfate
#' @param alkalinity name for alkalinity
#' @param total_dissolved_solids name for TDS to scale color of plot.
#' Default is NULL
#' @param group_var parameter to separate plots by. Default is sample location
#' @param facet_var parameter to facet the plot by. Default is sample date
#' @param lines TRUE/FALSE plots lines
#' @param cex multiplier to scale plot height
#' @keywords geochemical plots Stiff Diagram
#'
#' @examples
#' data(gw_data)
#' gw_data %>%
#' filter(location_id == "MW-1", sample_date < lubridate::ymd("2008-01-01")) %>%
#' stiff_plot()
#'
#' gw_data %>%
#' filter(location_id %in% c("MW-1", "MW-2")) %>%
#' stiff_plot(., total_dissolved_solids = "Total Dissolved Solids")
#'
#' @export

stiff_plot <- function(df,
                       location_id = "location_id",
                       sample_date = "sample_date",
                       param_name = "param_name",
                       analysis_result = "analysis_result",
                       default_unit = "default_unit",
                       magnesium = "Magnesium, dissolved",
                       calcium = "Calcium, dissolved",
                       sodium = "Sodium, dissolved",
                       potassium = "Potassium, dissolved",
                       chloride = "Chloride, total",
                       sulfate = "Sulfate, total",
                       alkalinity = "Alkalinity, total (lab)",
                       total_dissolved_solids = NULL,
                       group_var = "location_id",
                       facet_var = "sample_date",
                       lines = FALSE,
                       cex = 1
                       ){

  df %>%
    group_by_(group_var) %>%
    do(plot = .stiff_plot(.,
                       location_id = location_id,
                       sample_date = sample_date,
                       param_name = param_name,
                       analysis_result = analysis_result,
                       default_unit = default_unit,
                       magnesium = magnesium,
                       calcium = calcium,
                       sodium = sodium,
                       potassium = potassium,
                       chloride = chloride,
                       sulfate = sulfate,
                       alkalinity = alkalinity,
                       total_dissolved_solids = total_dissolved_solids,
                       facet_var = facet_var,
                       lines = lines,
                       cex = cex
                       )
    )
}

#' Helper function for Stiff Diagrams
#'
#' @noRd

.stiff_plot <- function(df,
                        location_id = "location_id",
                        sample_date = "sample_date",
                        param_name = "param_name",
                        analysis_result = "analysis_result",
                        default_unit = "default_unit",
                        magnesium = "Magnesium, dissolved",
                        calcium = "Calcium, dissolved",
                        sodium = "Sodium, dissolved",
                        potassium = "Potassium, dissolved",
                        chloride = "Chloride, total",
                        sulfate = "Sulfate, total",
                        alkalinity = "Alkalinity, total (lab)",
                        total_dissolved_solids = NULL,
                        facet_var = "sample_date",
                        lines = FALSE,
                        cex = 1
                        ) {

  df <- df %>%
    .transform_stiff_data(location_id = location_id,
                          sample_date = sample_date,
                          param_name = param_name,
                          analysis_result = analysis_result,
                          default_unit = default_unit,
                          magnesium = magnesium,
                          calcium = calcium,
                          sodium = sodium,
                          potassium = potassium,
                          chloride = chloride,
                          sulfate = sulfate,
                          alkalinity = alkalinity,
                          total_dissolved_solids = total_dissolved_solids)

  if (!is.null(total_dissolved_solids)) {
    # try to fix error message when only 1 location plotted
    p <- ggplot(df) + 
      geom_polygon(aes(x = stiff_x, y = stiff_y, fill = TDS), colour = "black") 

    if (requireNamespace("viridis", quietly = TRUE)) {
      p <- p + viridis::scale_fill_viridis()
    } else{
      p <- p + scale_fill_continuous()
    }

  } else{
    p <- ggplot(df) + geom_polygon(aes(x = stiff_x, y = stiff_y),
                                   fill = "white", colour = "black")

  }

  p <- p + facet_wrap(paste(facet_var), scale = "free_x",
                      strip.position = "top") +
    xlab("meq/L") + theme_bw() + 
    theme(strip.background = element_rect(colour = "white",
                                          fill = "grey50",
                                          size = 1),
          strip.placement = "outside",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(colour = "black",
                                     linetype = "solid",
                                     size = 1),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          plot.margin = grid::unit(c(1, 1, 1, 1), "lines")) +
    geom_text(aes(x = stiff_x_label, y = stiff_y, label = param_name),
              size = 2) +
    scale_x_continuous(labels = abs)

  if (facet_var == "sample_date") {
      p <- p +
        ggtitle(paste0("Stiff Diagram for ", df$location_id[1], "\n")) +
        theme(plot.title = element_text(hjust = 0.5))
  }

  if (facet_var == "location_id") {
    p <- p +
      ggtitle(paste0("Stiff Diagram for ", df$sample_date[1], "\n")) +
      theme(plot.title = element_text(hjust = 0.5))
  }

  if (isTRUE(lines)) {
    p <- p + geom_segment(aes(x = -stiff_x_label + 0.3,
                              xend = stiff_x_label - 0.3,
                              y = stiff_y,
                              yend = stiff_y),  
                          linetype = "dotted")
    }

  print(p)

}


#' Function to transform data to Stiff diagram coordinates
#'
#' @noRd

.transform_stiff_data <- function(df,
                                  location_id = "location_id",
                                  sample_date = "sample_date",
                                  param_name = "param_name",
                                  analysis_result = "analysis_result",
                                  default_unit = "default_unit",
                                  magnesium = "Magnesium, dissolved",
                                  calcium = "Calcium, dissolved",
                                  sodium = "Sodium, dissolved",
                                  potassium = "Potassium, dissolved",
                                  chloride = "Chloride, total",
                                  sulfate = "Sulfate, total",
                                  alkalinity = "Alkalinity, total (lab)",
                                  total_dissolved_solids = NULL
                                  ) {

  df <- df %>%
    .get_stiff_ions(location_id = location_id,
                    sample_date = sample_date,
                    param_name = param_name,
                    analysis_result = analysis_result,
                    default_unit = default_unit,
                    calcium = calcium,
                    magnesium = magnesium,
                    sodium = sodium,
                    potassium = potassium,
                    chloride = chloride,
                    alkalinity = alkalinity,
                    sulfate = sulfate,
                    total_dissolved_solids = total_dissolved_solids) %>%
    conc_to_meq(., magnesium = magnesium, calcium = calcium, sodium = sodium,
                potassium = potassium, chloride = chloride, sulfate = sulfate,
                alkalinity = alkalinity)

  if (!is.null(total_dissolved_solids)) {
    df <- df %>%
      select(sodium, potassium) %>%
      mutate(`Na + K` = rowSums(.)) %>%
      select(`Na + K`) %>%
      bind_cols(df) %>%
      select(location_id = location_id, sample_date = sample_date,
             default_unit = default_unit, `Na + K` = `Na + K`,
             Ca = calcium, Mg = magnesium, SO4 = sulfate, Alk = alkalinity,
             Cl = chloride,TDS = total_dissolved_solids)
    # The order here affects geom_polygon and geom_path 
  } else {
    df <- df %>%
      select(sodium, potassium) %>%
      mutate(`Na + K` = rowSums(.)) %>%
      select(`Na + K`) %>%
      bind_cols(df) %>%
      select(location_id = location_id, sample_date = sample_date,
             default_unit = default_unit, `Na + K` = `Na + K`,
             Ca = calcium, Mg = magnesium, SO4 = sulfate, Alk = alkalinity,
             Cl = chloride)
    # The order here affects geom_polygon and geom_path
  }

  if (!is.null(total_dissolved_solids)) {
    df <- df %>%
      gather(key = param_name,
             value = analysis_result,
             -c(location_id, default_unit, sample_date, TDS))
  } else {
    df <- df %>%
      gather(key = param_name,
             value = analysis_result,
             -c(location_id, default_unit, sample_date))
  }


  cations <- c("Mg", "Ca", "Na + K")
  anions <- c("SO4", "Alk", "Cl")

  df_cations <- df %>%
    filter(param_name %in% cations) %>%
    mutate(stiff_x = -1 * analysis_result)

  df_anions <- df %>%
    filter(param_name %in% anions | param_name == "TDS") %>%
    mutate(stiff_x = analysis_result)

  df <- bind_rows(df_cations, df_anions) %>%
    mutate(stiff_x_label = case_when(
      param_name == "Na + K" ~ -max(stiff_x, na.rm = TRUE) - 0.3,
      param_name == "Ca" ~ -max(stiff_x, na.rm = TRUE) - 0.3,
      param_name == "Mg" ~ -max(stiff_x, na.rm = TRUE) - 0.3,
      param_name == "SO4" ~ max(stiff_x, na.rm = TRUE) + 0.3,
      param_name == "Alk" ~ max(stiff_x, na.rm = TRUE) + 0.3,
      param_name == "Cl" ~ max(stiff_x, na.rm = TRUE) + 0.3
    ))

  poly_order <-  tibble(param_name = c("Mg", "SO4", "Alk",
                                       "Cl", "Na + K", "Ca"),
                            stiff_y = c(1, 1, 2, 3, 3, 2))

  stiff <- df %>%
    left_join(poly_order)

  stiff

}

#' Function to gather major ions for stiff diagram.
#' 
#' @noRd

.get_stiff_ions <- function(df,
                            location_id = "location_id",
                            sample_date = "sample_date",
                            param_name = "param_name",
                            analysis_result = "analysis_result",
                            default_unit = "default_unit",
                            calcium = "Calcium, dissolved",
                            magnesium = "Magnesium, dissolved",
                            sodium = "Sodium, dissolved",
                            potassium = "Potassium, dissolved",
                            chloride =  "Chloride, total",
                            alkalinity = "Alkalinity, total (lab)",
                            sulfate = "Sulfate, total",
                            total_dissolved_solids = NULL
                            ) {
  
  ions <- c(calcium, magnesium, sodium,
            potassium, chloride, alkalinity,
            sulfate, total_dissolved_solids)
  
  df <- df %>%
    filter_(~param_name %in% ions) %>%
    spread_(param_name, analysis_result) %>%
    group_by_(~location_id, ~sample_date, ~default_unit) %>%
    summarise_at(vars(ions), mean, na.rm = TRUE) %>%
    ungroup()
  
  df
  
}

#' Function to create an animated Stiff Diagram 
#' 
#' @param df data frame of water quality data
#' @inheritDotParams stiff_plot
#' 
#' @export

stiff_time_plot <- function(df, ...) {

  for (i in seq_along(unique(df$sample_date))) {
      .stiff_plot(df[df$sample_date == df$sample_date[i], ])
      animation::ani.pause()
  }

}

#' Function to create an animated Stiff Diagram and save to html
#' 
#' @param df data frame of water quality data
#' @inheritDotParams stiff_plot
#' 
#' @export

stiff_time_html <- function(df, ...) {
  
  animation::saveHTML({
    animation::ani.options(nmax = length(unique(df$sample_date)))
      stiff_time_plot(df, ...)
  },
  img.name = "StiffPlot", htmlfile = "Stiff_plot.html",
  autobrowse = TRUE, title = "Stiff Diagram"
  )
}