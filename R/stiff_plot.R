#' @title Stiff Diagram. 
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
#' @param lines TRUE/FALSE plots lines
#' @param cex multiplier to scale plot height
#' @keywords geochemical plots Stiff Diagram
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
  
  grid::grid.newpage()
  
  line_width <- max(abs(df$stiff_x))
  
  xmin <- -line_width
  xmin_lab <- xmin - 0.2
  xmin_line <- xmin + 0.3
  xmax <- line_width
  xmax_lab = xmax + 0.2
  xmax_line <- xmax - 0.3

  df2 <- data.frame(y = c(3, 2, 1), cations = c("Mg", "Ca", "Na + K"), 
                    anions = c("SO4", "Alk", "Cl"))
  df2$y <- df2$y*cex
  df$stiff_y <- df$stiff_y*cex

  if (!is.null(total_dissolved_solids)) {
    # try to fix error message when only 1 location plotted
    p <- ggplot(df) + geom_polygon(aes(x = stiff_x, y = stiff_y, fill = TDS),
                                   colour = "black") 

    for (i in 1:length(df2$anions)) {
      p <- p + annotation_custom(
        grob = grid::textGrob(label = df2$anions[i], 
                              hjust = 0.4, 
                              gp = grid::gpar(cex = 0.5)),
        ymin = df2$y[i],
        ymax = df2$y[i],
        xmin = xmax_lab,
        xmax = xmax_lab
      )
    }

    for (i in 1:length(df2$cations)) {
      p <- p + annotation_custom(
        grob = grid::textGrob(label = df2$cations[i], 
                              hjust = 0.3, 
                              gp = grid::gpar(cex = 0.5)),
        ymin = df2$y[i],
        ymax = df2$y[i],
        xmin = xmin_lab,
        xmax = xmin_lab
      )
    }

  } else{
    p <- ggplot(df) + geom_polygon(aes(x = stiff_x, y = stiff_y), 
                                   fill = "GREY50", colour = "black")
    for (i in 1:length(df2$anions)) {
      p <- p + annotation_custom(
        grob = grid::textGrob(label = df2$anions[i], 
                              hjust = 0.4, 
                              gp = grid::gpar(cex = 0.5)),
        ymin = df2$y[i],
        ymax = df2$y[i],
        xmin = xmax_lab,
        xmax = xmax_lab
      )
    }

    for (i in 1:length(df2$cations)) {
      p <- p + annotation_custom(
        grob = grid::textGrob(label = df2$cations[i], 
                              hjust = 0.3,
                              gp = grid::gpar(cex = 0.5)),
        ymin = df2$y[i],
        ymax = df2$y[i],
        xmin = xmin_lab,
        xmax = xmin_lab
      )
    }
  }

  p <- p + facet_wrap(~sample_date, scale = "free_x") +
    xlab("\nmeq/L") + theme_bw() + 
    scale_x_continuous(limits = c(xmin, xmax)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.margin = grid::unit(c(1, 1, 1, 1), "lines")) +
    ggtitle(paste0("Stiff Diagram for ", df$location_id[1], "\n")) +
    theme(plot.title = element_text(hjust = 0.5))

  if (isTRUE(lines)) {
    p <- p + geom_segment(x = xmin, xend = xmax, y = 1, yend = 1,  
                          linetype = "dotted") +
      geom_segment(x = xmin, xend = xmax, y = 2, yend = 2, 
                   linetype = "dotted") +
      geom_segment(x = xmin, xend = xmax, y = 3, yend = 3, 
                   linetype = "dotted")
  }
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[grep("panel", gt$layout$name)] <- "off"
  grid::grid.draw(gt)
}


#' Function to transform data to Stiff diagram coordinates

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
             default_unit = default_unit, Mg = magnesium, Ca = calcium,
             `Na + K` = `Na + K`, Cl = chloride, SO4 = sulfate,
             Alk = alkalinity, TDS = total_dissolved_solids) 
  } else {
    df <- df %>%
      select(sodium, potassium) %>%
      mutate(`Na + K` = rowSums(.)) %>%
      select(`Na + K`) %>%
      bind_cols(df) %>%
      select(location_id = location_id, sample_date = sample_date,
             default_unit = default_unit, Mg = magnesium, Ca = calcium,
             `Na + K` = `Na + K`, Cl = chloride, SO4 = sulfate,
             Alk = alkalinity)
  }

  df <- df %>%
    gather(key = param_name,
           value = analysis_result,
           -c(location_id, default_unit, sample_date))

  cations <- c("Mg", "Ca", "Na + K")
  anions <- c("SO4", "Alk", "Cl")

  df_cations <- df %>%
    filter(param_name %in% cations) %>%
    mutate(stiff_x = -1 * analysis_result)

  df_anions <- df %>%
    filter(param_name %in% anions | param_name == "TDS") %>%
    mutate(stiff_x = analysis_result)

  df <- bind_rows(df_cations, df_anions)

  poly_order <-  tibble(param_name = c("Mg", "SO4", "Alk",
                                       "Cl", "Na + K", "Ca"),
                            stiff_y = c(3, 3, 2, 1, 1, 2))

  stiff <- df %>%
    left_join(poly_order)

  return(stiff)
}

#' Function to gather major ions for stiff diagram.

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
  
  return(df)
  
}

#' Function to plot multiple stiff plots by location
#' 
#' @param df groundwater data frame
#' @param ... arguments passed to \code{\link{stiff_plot}}
#' @export

stiff_by_loc <- function(df, ...){
  
  plyr::d_ply(df, .(location_id), .progress = "text", stiff_plot, ...,
              .print = TRUE)
}

#' Function to create an animated Stiff Diagram 
#' 
#' @param df data frame of groundwater data transformed using 
#' \code{\link{transform_stiff_data}}
#' @export

stiff_time_plot <- function(df, TDS = FALSE){
  for (i in 1:length(unique(df$sample_date))) {
    if (isTRUE(TDS)) {
      print(stiff_plot(subset(df, sample_date == df$sample_date[i]), 
                       TDS = TRUE))
      animation::ani.pause()
    }else{
      print(stiff_plot(subset(df, sample_date == df$sample_date[i]), 
                       TDS = FALSE))
      animation::ani.pause()
    }
  }
}

#' Function to create an animated Stiff Diagram and save to html
#' 
#' @param df data frame of groundwater data transformed using 
#' @param TDS TRUE/FALSE fills polygon with color gradient of TDS
#' \code{\link{transform_stiff_data}}
#' @export

stiff_time_html <- function(df, TDS = FALSE){
  animation::saveHTML({
    animation::ani.options(nmax = length(unique(df$sample_date)))
    if (isTRUE(TDS)) {
      stiff_time_plot(df, TDS = TRUE)
    }else{
      stiff_time_plot(df, TDS = FALSE)
    }
  }, 
  img.name = "StiffPlot", htmlfile = "Stiff_plot.html",
  autobrowse = TRUE, title = "Stiff Diagram"
  )
}