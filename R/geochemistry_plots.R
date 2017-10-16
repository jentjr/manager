#' Function to convert geochemical plot data from mg/L to meq/L
#' 
#' @param df data frame
#' @param Mg Magnesium
#' @param Ca Calcium
#' @param Na Sodium
#' @param K Potassium
#' @param Cl Chloride
#' @param SO4 Sulfate
#' @param total_alk Total Alkalinity
#' @export

conc_to_meq <- function(df, 
                        Mg = "Magnesium, dissolved", 
                        Ca = "Calcium, dissolved", 
                        Na = "Sodium, dissolved", 
                        K = "Potassium, dissolved", 
                        Cl = "Chloride, total", 
                        SO4 = "Sulfate, total", 
                        total_alk = "Alkalinity, total (lab)") {
  
  # TODO: add ... feature and a data base of elements perhaps from phreeqc.
  
  # formuala weights
  Ca_fwt <- 40.078
  Mg_fwt <- 24.305
  Na_fwt <- 22.990
  K_fwt <- 39.098
  S_fwt <- 32.06
  O_fwt <- 15.999
  H_fwt <- 1.008
  C_fwt <- 12.011
  Cl_fwt <- 35.45
  
  # absolute value of charge
  Ca_chrg <- 2
  Mg_chrg <- 2
  Na_chrg <- 1
  K_chrg <- 1
  SO4_chrg <- 2
  CO3_chrg <- 2
  HCO3_chrg <- 1
  Cl_chrg <- 1
  
  #molar mass
  Mg_mol <- Mg_fwt*Mg_chrg
  Ca_mol <- Ca_fwt*Ca_chrg
  Na_mol <- Na_fwt*Na_chrg
  K_mol <- K_fwt*K_chrg
  Cl_mol <- Cl_fwt*Cl_chrg
  SO4_mol <- (S_fwt + 4*O_fwt)*SO4_chrg
  CO3_mol <- (C_fwt + 3*O_fwt)*CO3_chrg
  HCO3_mol <- (H_fwt + C_fwt + 3*O_fwt)*HCO3_chrg
  total_alk_mol <- CO3_mol + HCO3_mol
  
  # conversion 
  df[,Mg] <- df[,Mg]/Mg_mol
  df[,Ca] <- df[,Ca]/Ca_mol
  df[,Na] <- df[,Na]/Na_mol
  df[,K] <- df[,K]/K_mol
  df[,Cl] <- df[,Cl]/Cl_mol
  df[,SO4] <- df[,SO4]/SO4_mol
  df[,total_alk] <- df[, total_alk]/total_alk_mol
  
  return(df)
}


#------------------------------------------------------------------------------
# Stiff Diagrams 
#------------------------------------------------------------------------------


#' Function to transform data from \code{\link{get_major_ions}} to Stiff diagram
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

#-------------------------------------------------------------------------------
# Schoeller Diagrams
#-------------------------------------------------------------------------------

#' Function to transoform data from \code{\link{get_major_ions}} to schoeller 
#' coordinates
#' 
#' @param df dataframe of major ions gathered using \code{\link{get_major_ions}}
#' @param Mg magnesium
#' @param Ca calcium
#' @param K potassium
#' @param Cl chloride
#' @param SO4 sulfate
#' @param HCO3 alkalininity or bicarbonate
#' @param name column of sample location id
#' @param date column of sample date
#' @export

transform_schoeller <- function(df, 
                                Mg = "Magnesium, dissolved", 
                                Ca = "Calcium, dissolved", 
                                Na = "Sodium, dissolved", 
                                K = "Potassium, dissolved", 
                                Cl = "Chloride, total", 
                                SO4 = "Sulfate, total", 
                                HCO3 = "Alkalinity, total (lab)", 
                                name = "LOCATION_ID", 
                                date = "SAMPLE_DATE") {
  
  temp <- data.frame(df[,name], df[,date], df[,Mg], df[,Ca], df[,Na] + df[,K], 
                     df[,SO4], df[,HCO3], df[,Cl])
  
  colnames(temp) <- c("LOCATION_ID", "SAMPLE_DATE", "Mg", "Ca", "Na + K", 
                      "SO4", "HCO3", "Cl")
  
  df_melt <- reshape2::melt(temp, id.vars = c("LOCATION_ID", "SAMPLE_DATE"))
  
  colnames(df_melt) <- c("LOCATION_ID", "SAMPLE_DATE", 
                         "param_name", "ANALYSIS_RESULT")
  
  return(df_melt)
  
}

#' Function to plot data transformed to schoeller coordinates using
#' \code{\link{transform_schoeller}}
#' 
#' @param df dataframe of transformed groundwater data
#' @param facet_by parameter to facet plots by
#' @param title title of plot
#' @param lwt lineweight
#' @export

schoeller_plot <- function(df, facet_by = NULL, title = NULL, lwt = 1) {
  
  p <- ggplot(df, aes(x = param_name, y = ANALYSIS_RESULT, group = 1), 
              size = lwt) + 
    scale_y_log10() +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank()) +
    ggtitle(paste(title)) +
    theme(plot.title = element_text(hjust = 0.5))
 
   if (is.null(facet_by)) {
    
     p <- p + geom_line(size = lwt) + theme_bw()
     
   }
  
  if (!is.null(facet_by)) {
    
    if (facet_by == "SAMPLE_DATE") {
      
      p <- p + facet_wrap(~SAMPLE_DATE, scale = "free_y") + 
        geom_line(aes(colour = LOCATION_ID, group = LOCATION_ID), size = lwt) +
        guides(colour = guide_legend("Location ID"))
      
    }
    
    if (facet_by == "LOCATION_ID") {
      
      p <- p + facet_wrap(~LOCATION_ID, scale = "free_y") +
        geom_line(aes(colour = factor(SAMPLE_DATE), group = SAMPLE_DATE), 
                  size = lwt) +
        guides(colour = guide_legend("Sample Date"))
      
    }
    
  }
  
  return(p)
  
}

#-------------------------------------------------------------------------------
# Series Plot
#-------------------------------------------------------------------------------

#' Function to plot a series plot of groundwater data
#' 
#' @param df datadframe of groundwater data
#' @param facet_by parameter to facet the plot by
#' @export

series_plot <- function(df, facet_by = NULL) {
  
  p <- ggplot(df, aes(x = LOCATION_ID, y = ANALYSIS_RESULT, group = 1)) + 
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank())
  
  if (is.null(facet_by)) {
    
    p <- p + geom_line(aes(colour = param_name, group = param_name))
    
  }
  
  if (!is.null(facet_by)) {
    
    if (facet_by == "SAMPLE_DATE") {
      
      p <- p + facet_wrap(~SAMPLE_DATE, scale = "free_y") + 
        geom_line(aes(colour = param_name, group = param_name))
      
    }
    
  }
  
  return(p)
  
}