#' Function to transform data to Stiff diagram coordinates
#' 
#' @param df data frame of groundwater data from \code{\link{get_major_ions}}
#' @param Mg Magnesium 
#' @param Ca Calcium 
#' @param Na Sodium, 
#' @param K Potassium 
#' @param Cl Chloride 
#' @param SO4 Sulfate 
#' @param HCO3 Alkalinity 
#' @param name LOCATION_ID 
#' @param date SAMPLE_DATE 
#' @param TDS Total Dissolved Solids 
#' @param units Units of data
#' @export

transform_stiff_data <- function(df, 
                                 Mg = "Magnesium, dissolved", 
                                 Ca = "Calcium, dissolved", 
                                 Na = "Sodium, dissolved", 
                                 K = "Potassium, dissolved", 
                                 Cl = "Chloride, total", 
                                 SO4 = "Sulfate, total", 
                                 Alk = "Alkalinity, total (lab)", 
                                 name = "LOCATION_ID", 
                                 date = "SAMPLE_DATE", 
                                 TDS = NULL, 
                                 units = NULL) {
  
  temp <- data.frame(df[, name], df[, date], df[, Mg], df[, Ca], df[, Na] + 
                       df[, K], df[, SO4], df[, Alk], df[, Cl])
  
  colnames(temp) <- c("LOCATION_ID", "SAMPLE_DATE", "Mg", "Ca", "Na + K", 
                      "SO4", "Alk", "Cl")
  
  df_melt <- reshape2::melt(temp, id.vars = c("LOCATION_ID", "SAMPLE_DATE"))
  
  cations <- c("Mg", "Ca", "Na + K")
  anions <- c("SO4", "Alk", "Cl")
  
  df_melt$value <- ifelse(df_melt$variable %in% cations, -1 * df_melt$value, 
                          df_melt$value)
  
  stiff <- df_melt[df_melt$variable %in% c("Mg", "SO4", "Alk", "Cl", 
                                           "Na + K", "Ca"), ]
  
  poly_order <-  data.frame(c("Mg", "SO4", "Alk", "Cl", "Na + K", "Ca"),
                            c(3, 3, 2, 1, 1, 2))
  
  colnames(poly_order) <- c("variable", "stiff_y")
  
  stiff <- plyr::join(poly_order, stiff, by = "variable", type = "left")
  
  stiff <- plyr::rename(stiff, replace = c("value" = "stiff_x"))
  
  if (!is.null(TDS)) {
    
    stiff <- plyr::join(stiff, df, by = c("LOCATION_ID", "SAMPLE_DATE"))
    
    stiff <- stiff[, names(stiff) %in% c("LOCATION_ID", "SAMPLE_DATE", 
                                         "variable", "stiff_x", "stiff_y", 
                                         paste(TDS))]
    colnames(stiff[paste(TDS)]) <- "TDS"
  }  
  return(stiff)
}

#' Function to plot a Stiff Diagram. Data must have been transformed using
#'  \code{\link{transform_stiff_data}}.
#' 
#' @param df data frame of groundwater data transformed using 
#' @param lines TRUE/FALSE plots lines
#' @param TDS TRUE/FALSE fills in the color of the stiff diagram by TDS
#' @param cex multiplier to scale plot height
#' @keywords geochemical plots Stiff Diagram
#' @export

stiff_plot <- function(df, lines = FALSE, TDS = FALSE, cex = 1) {
  
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
  
  if (isTRUE(TDS)) {
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
  
  p <- p + facet_wrap(~SAMPLE_DATE, scale = "free_x") +
    xlab("\nmeq/L") + theme_bw() + 
    scale_x_continuous(limits = c(xmin, xmax)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.margin = grid::unit(c(1, 1, 1, 1), "lines")) +
    ggtitle(paste0("Stiff Diagram for ", df$LOCATION_ID[1], "\n")) +
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

#' Function to plot multiple stiff plots by location
#' 
#' @param df groundwater data frame
#' @param ... arguments passed to \code{\link{stiff_plot}}
#' @export

stiff_by_loc <- function(df, ...){
  
  plyr::d_ply(df, .(LOCATION_ID), .progress = "text", stiff_plot, ...,
              .print = TRUE)
}

#' Function to create an animated Stiff Diagram 
#' 
#' @param df data frame of groundwater data transformed using 
#' \code{\link{transform_stiff_data}}
#' @export

stiff_time_plot <- function(df, TDS = FALSE){
  for (i in 1:length(unique(df$SAMPLE_DATE))) {
    if (isTRUE(TDS)) {
      print(stiff_plot(subset(df, SAMPLE_DATE == df$SAMPLE_DATE[i]), 
                       TDS = TRUE))
      animation::ani.pause()
    }else{
      print(stiff_plot(subset(df, SAMPLE_DATE == df$SAMPLE_DATE[i]), 
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
    animation::ani.options(nmax = length(unique(df$SAMPLE_DATE)))
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