#' Function to gather parameters needed for geochemistry plots. 
#'
#' @param df data frame of groundwater monitoring data in tidy format
#' @param param_name column of constituents
#' @param Mg Magnesium
#' @param Ca Calcium
#' @param Na Sodium
#' @param K Potassium
#' @param Cl Chloride
#' @param SO4 Sulfate
#' @param Alk Alkalinity
#' @param TDS Total Dissolved Solids
#' @export

get_major_ions <- function(df, 
                           location_id = "location_id",
                           sample_date = "sample_date",
                           param_name = "param_name",
                           analysis_result = "analysis_result",
                           Mg = "Magnesium, dissolved", 
                           Ca = "Calcium, dissolved", 
                           Na = "Sodium, dissolved", 
                           K = "Potassium, dissolved", 
                           Cl = "Chloride, total", 
                           SO4 = "Sulfate, total", 
                           Alk = "Alkalinity, total (lab)", 
                           TDS = NULL,
                           pH = NULL) {

#   TODO: add other major ions like Fe
#   input_list <- list(...)
  
  params <- c(Mg, Ca, Na, K, Cl, SO4, Alk, TDS)
  
  data <- df %>% 
    filter_(~param_name %in% params) %>%
    spread_(param_name, analysis_result) %>%
    group_by_(~location_id, ~sample_date) %>%
    summarise_at(vars(params), mean, na.rm = TRUE)

  return(data)
  
}

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

#' Function to transform data for piper plot 
#'
#' @param df data frame of groundwater data 
#' @param Mg Magnesium
#' @param Ca Calcium
#' @param Na Sodium
#' @param K Potassum
#' @param Cl Chloride
#' @param SO4 Sulfate
#' @param HCO3 Bicarbonate, or Alkalinity
#' @param TDS Total Dissolved Solids
#' @param name column of well names
#' @param date column of dates
#' @param units right now can handle mg/L and meq/L
#' @keywords piper diagram
#' @export

transform_piper_data <- function(df, 
                                 Mg = "Magnesium, dissolved", 
                                 Ca = "Calcium, dissolved", 
                                 Na = "Sodium, dissolved", 
                                 K = "Potassium, dissolved", 
                                 Cl = "Chloride, total", 
                                 SO4 = "Sulfate, total", 
                                 CO3 = NULL, 
                                 Alk = "Alkalinity, total (lab)", 
                                 TDS = NULL, 
                                 name = "location_id", 
                                 date = "sample_date", 
                                 units = NULL) {

  # cation data
  # Convert data to %
  cation_total <- df[,Mg] + df[,Ca] + df[,Na] + df[,K]
  cat_top <- (df[,Mg] / cation_total) * 100
  cat_left <- (df[,Ca] / cation_total) * 100
  cat_right <- 100 - (cat_top + cat_left)
  
  # Convert data into xy coordinates
  cation_x <- cat_right + cat_top / 2
  cation_y <- sqrt(3) / 2 * cat_top
  
  # anion data
  # Convert data to %
  anion_total <- df[,SO4] + df[,Cl] + df[,Alk]
  an_top <- (df[,SO4] / anion_total) * 100
  an_left <- (df[,Alk] / anion_total) * 100
  an_right <- 100 - (an_top + an_left)
  
  # Convert data into xy coordinates
  anion_x <- 120 + (an_right + an_top / 2)
  anion_y <- sqrt(3) / 2 * an_top

  # diamond points
  calc_diam_point <- function(cation_x, anion_x, cation_y, anion_y, 
                              grad = 2 * sin(pi / 3)){
    b1 <- cation_y - (grad * cation_x)
    b2 <- anion_y - (-grad * anion_x)
    M <- matrix(c(grad, -grad, -1, -1), ncol = 2)
    intercepts <- as.matrix(c(b1, b2))
    t_mat <- -solve(M) %*% intercepts
    data.frame(diam_x = t_mat[1, 1], diam_y = t_mat[2, 1])
  }
  
diam_list <- lapply(1:length(cation_x), function(i) calc_diam_point(cation_x[i],
                                                      anion_x[i], cation_y[i], 
                                                      anion_y[i]))
  
npoints <- do.call("rbind", diam_list)

piper_data <- data.frame(location_id = df[,name], sample_date = df[,date], 
                         cation_x, anion_x, cation_y, anion_y, 
                         diam_x = npoints$diam_x, diam_y = npoints$diam_y, 
                         TDS = df[,TDS])

return(piper_data)
  
}

#' Function to create base Piper plot

.ggplot_piper <- function() {
  
  p <- ggplot() +

    ## left hand ternary plot
    geom_segment(aes(x = 0, y = 0, xend = 100, yend = 0)) +
    geom_segment(aes(x = 0, y = 0, xend = 50, yend = 86.603)) +
    geom_segment(aes(x = 50, y = 86.603, xend = 100, yend = 0)) +
    
    ## right hand ternary plot
    ## shifted right by 20 points 
    geom_segment(aes(x = 120, y = 0, xend = 220, yend = 0)) +
    geom_segment(aes(x = 120, y = 0, xend = 170, yend = 86.603)) +
    geom_segment(aes(x = 170, y = 86.603, xend = 220, yend = 0)) +
    
    ## Upper diamond
    geom_segment(aes(x = 110, y = 190.5266, xend = 60, yend = 103.9236)) +
    geom_segment(aes(x = 110, y = 190.5266, xend = 160, yend = 103.9236)) +
    geom_segment(aes(x = 110, y = 17.3206, xend = 160, yend = 103.9236)) +
    geom_segment(aes(x = 110, y = 17.3206, xend = 60, yend = 103.9236)) +
    
    ## Add grid lines to the plots
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2), 
                 data = data.frame(x1 = c(20, 40, 60, 80), 
                                 x2 = c(10, 20, 30, 40), 
                                 y1 = c(0, 0, 0, 0), 
                                 y2 = c(17.3206, 34.6412, 51.9618, 69.2824)), 
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2), 
                 data = data.frame(x1 = c(20, 40, 60, 80), 
                                 x2 = c(60, 70, 80, 90), 
                                 y1 = c(0, 0, 0, 0), 
                                 y2 = c(69.2824, 51.9618, 34.6412, 17.3206)), 
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2), 
                 data = data.frame(x1 = c(10, 20, 30, 40), 
                                 x2 = c(90, 80, 70, 60), 
                                 y1 = c(17.3206, 34.6412, 51.9618, 69.2824), 
                                 y2 = c(17.3206, 34.6412, 51.9618, 69.2824)), 
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2), 
                 data = data.frame(x1 = c(140, 160, 180, 200), 
                                 x2 = c(130, 140, 150, 160), 
                                 y1 = c(0, 0, 0, 0), 
                                 y2 = c(17.3206, 34.6412, 51.9618, 69.2824)), 
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2), 
                 data = data.frame(x1 = c(140, 160, 180, 200), 
                                 x2 = c(180, 190, 200, 210), 
                                 y1 = c(0, 0, 0, 0), 
                                 y2 = c(69.2824, 51.9618, 34.6412, 17.3206)), 
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2), 
                 data = data.frame(x1 = c(130, 140, 150, 160), 
                                 x2 = c(210, 200, 190, 180), 
                                 y1 = c(17.3206, 34.6412, 51.9618, 69.2824), 
                                 y2 = c(17.3206, 34.6412, 51.9618, 69.2824)), 
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2), 
                 data = data.frame(x1 = c(100,90, 80, 70), 
                                 y1 = c(34.6412, 51.9618, 69.2824, 86.603), 
                                 x2 = c(150, 140, 130, 120), 
                                 y2 = c(121.2442, 138.5648, 155.8854, 173.2060)), 
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2), 
                 data = data.frame(x1 = c(70, 80, 90, 100), 
                                 y1 = c(121.2442, 138.5648, 155.8854, 173.2060), 
                                 x2 = c(120, 130, 140, 150), 
                                 y2 = c(34.6412, 51.9618, 69.2824, 86.603)), 
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_text(aes(x = c(20, 40, 60, 80), y = c(-5,-5,-5,-5), 
                  label = c(80, 60, 40, 20)), size = 3) +
    geom_text(aes(x = c(35, 25, 15, 5), 
                  y = c(69.2824, 51.9618, 34.6412, 17.3206), 
                  label = c(80, 60, 40, 20)), size = 3) +
    geom_text(aes(x = c(95, 85, 75, 65), 
                  y = c(17.3206, 34.6412, 51.9618, 69.2824),
                  label = c(80, 60, 40, 20)), size = 3) +
    coord_equal(ratio = 1) +  
    
    # Labels for cations
    geom_text(aes(x = 17, y = 50, label = "-phantom()~Mg^+2 %->%phantom()"), 
          angle = 60, size = 4, parse = TRUE) +  
    geom_text(aes(x = 82, y = 50, 
          label = "-phantom()~Na^+phantom()~+~K^+phantom() %->%phantom()"),
          angle = -60, size = 4, parse = TRUE) +
    geom_text(aes(x = 50,y = -10, 
          label = "phantom()%<-%phantom()~Ca^+2~-phantom()"), 
          size = 4, parse = TRUE) +
    
    # labels for anion plot
    geom_text(aes(x = 170, y = -10, 
          label = "- Cl^-phantom() %->%phantom()"), size = 4, parse = TRUE) +
    geom_text(aes(x = 205, y = 50, 
          label = "phantom()%<-%phantom()~SO[4]^+2~-phantom()"), 
          angle = -60, size = 4, parse = TRUE) +
    geom_text(aes(x = 138.5, y = 50, 
          label = "phantom()%<-%phantom()~Alkalinity~-phantom()"), 
          angle = 60, size = 4, parse = TRUE) +
    
    # Diamond Labels
    geom_text(aes(x = 72.5, y = 150, 
          label = "-phantom()~SO[4]^+2~+~Cl^-phantom()~phantom()%->%phantom()"),
          angle = 60, size = 4, parse = TRUE) +
    geom_text(aes(x = 147.5, y = 150, 
          label = "phantom()%<-%phantom()~Ca^+2~+~Mg^+2~-phantom()"), 
          angle = -60, size = 4, parse = TRUE) + 
    geom_text(aes(x = c(155, 145, 135, 125), 
                  y = c(69.2824, 51.9618, 34.6412, 17.3206),
                  label = c(20, 40, 60, 80)), size = 3) +
    geom_text(aes(x = c(215, 205, 195, 185), 
                  y = c(17.3206, 34.6412, 51.9618, 69.2824),
                  label = c(20, 40, 60, 80)), size = 3) +
    geom_text(aes(x = c(140, 160, 180, 200), 
                  y = c(-5, -5, -5, -5), 
                  label = c(20, 40, 60, 80)), size = 3) +
    geom_text(aes(x = c(95, 85, 75, 65), 
                  y = c(34.6412, 51.9618, 69.2824, 86.603), 
                  label = c(80, 60, 40, 20)), size = 3) +
    geom_text(aes(x = c(155, 145, 135, 125), 
                  y = c(121.2442, 138.5648, 155.8854, 173.2060),
                  label = c(20, 40, 60, 80)), size = 3) +
    geom_text(aes(x = c(65, 75, 85, 95), 
                  y = c(121.2442, 138.5648, 155.8854, 173.2060),
                  label = c(20, 40, 60, 80)), size = 3) +
    geom_text(aes(x = c(125, 135, 145, 155), 
                  y = c(34.6412, 51.9618, 69.2824, 86.603),
                  label = c(80, 60, 40, 20)), size = 3) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank())
 
  return(p)
  
}

#' Function to plot data from \code{\link{transform_piper_data}} onto base 
#' ggplot_piper background. Points are sized by Total Dissolved Solids
#' 
#' @param df data frame of groundwater data transformed into piper coordinates 
#'  using \code{\link{transform_piper_data}}
#' @param TDS Scale plot by Total Dissolved Solids, default = FALSE
#' @param title Title for plot, default = NULL
#' @export
  
piper_plot <- function(df, TDS=FALSE, title=NULL) {
  
  sym <- seq(1, length(unique(df$location_id)), by = 1)
  
  if (isTRUE(TDS)) {
    .ggplot_piper() + geom_point(data = df, aes(x = cation_x, y = cation_y, 
                                               colour = location_id,
                                               shape = location_id,
                                               size = TDS, alpha = 0.2)) +
      geom_point(data = df, aes(x = anion_x, y = anion_y, shape = location_id,
                                size = TDS, colour = location_id, 
                                alpha = 0.2)) +
      geom_point(data = df, aes(x = diam_x, y = diam_y, shape = location_id,
                                size = TDS, colour = location_id, 
                                alpha = 0.2)) +
      scale_size("TDS", range = c(5, 25)) +
      scale_colour_brewer(palette = "Dark2") +
      ggtitle(paste(title)) + guides(size = guide_legend("TDS"),
                                     colour = guide_legend("Location ID"),
                                     shape = guide_legend("Location ID"),
                                     alpha = guide_legend("none")) +
      theme(plot.title = element_text(hjust = 0.5))
  }else{
  .ggplot_piper() + geom_point(data = df, aes(x = cation_x, y = cation_y, 
                                             color = location_id, 
                                             shape = location_id,
                                             alpha = 0.2), size = 5) +
    geom_point(data = df, aes(x = anion_x, y = anion_y, 
                              color = location_id,
                              shape = location_id,
                              alpha = 0.2), size = 5) +
    geom_point(data = df, aes(x = diam_x, y = diam_y, 
                              color = location_id,
                              shape = location_id,
                              alpha = 0.2), size = 5) +
    ggtitle(paste(title)) + guides(color = guide_legend("Location ID"),
                                   shape = guide_legend("Location ID"),
                                   alpha = guide_legend("none")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_brewer(palette = "Dark2") +
    scale_shape_manual(values = sym)
  }
}

#' Function to create an animated Piper plot through time
#' using the animation package
#' 
#' @param df data frame of groundwater data transformed using 
#' \code{\link{transform_piper_data}}
#' @param TDS Scale by Total Dissolved Solids
#' @export

piper_time_plot <- function(df, TDS = FALSE, title = NULL) {
  iter <- unique(df$sample_date)
  .ggplot_piper()
  dev.hold()
  for (i in 1:length(iter)) {
    if (isTRUE(TDS)) {
      print(piper_plot(df[df$sample_date == iter[i], ], 
                       TDS = TRUE, title = paste(title, iter[i], 
                                               sep = "\n")))
      animation::ani.pause()
    }else{
      print(piper_plot(df[df$sample_date == iter[i], ], 
                       title = paste(title, iter[i], sep = "\n")))
      animation::ani.pause()
    }
  }
}

#' Function to create an aminated Piper plot and save to html
#'
#' @param df data frame of groundwater data transformed using 
#' \code{\link{transform_piper_data}}
#' @param TDS Scale by Total Dissolved Solids
#' @export

piper_time_html <- function(df, TDS = FALSE){
  if (isTRUE(TDS)) {
    animation::saveHTML({
      animation::ani.options(nmax = length(unique(df$sample_date)), 
                             outdir = getwd())
      piper_time_plot(df, TDS = TRUE)
    }, 
                        img.name = "PiperPlot", htmlfile = "Piper_plot.html",
                        autobrowse = TRUE, title = "Piper Plot"
    )
  }else{
    animation::saveHTML({
     animation::ani.options(nmax = length(unique(df$sample_date)), 
                            outdir = getwd())
      piper_time_plot(df)
    }, 
                        img.name = "PiperPlot", htmlfile = "Piper_plot.html",
                        autobrowse = TRUE, title = "Piper Plot"
    )
  }
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
#' @param name location_id 
#' @param date sample_date 
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
                                 name = "location_id", 
                                 date = "sample_date", 
                                 TDS = NULL, 
                                 units = NULL) {
  
  temp <- data.frame(df[, name], df[, date], df[, Mg], df[, Ca], df[, Na] + 
                       df[, K], df[, SO4], df[, Alk], df[, Cl])
  
  colnames(temp) <- c("location_id", "sample_date", "Mg", "Ca", "Na + K", 
                      "SO4", "Alk", "Cl")
  
  df_melt <- reshape2::melt(temp, id.vars = c("location_id", "sample_date"))
  
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
    
    stiff <- plyr::join(stiff, df, by = c("location_id", "sample_date"))
    
    stiff <- stiff[, names(stiff) %in% c("location_id", "sample_date", 
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
                                name = "location_id", 
                                date = "sample_date") {
  
  temp <- data.frame(df[,name], df[,date], df[,Mg], df[,Ca], df[,Na] + df[,K], 
                     df[,SO4], df[,HCO3], df[,Cl])
  
  colnames(temp) <- c("location_id", "sample_date", "Mg", "Ca", "Na + K", 
                      "SO4", "HCO3", "Cl")
  
  df_melt <- reshape2::melt(temp, id.vars = c("location_id", "sample_date"))
  
  colnames(df_melt) <- c("location_id", "sample_date", 
                         "param_name", "analysis_result")
  
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
  
  p <- ggplot(df, aes(x = param_name, y = analysis_result, group = 1), 
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
    
    if (facet_by == "sample_date") {
      
      p <- p + facet_wrap(~sample_date, scale = "free_y") + 
        geom_line(aes(colour = location_id, group = location_id), size = lwt) +
        guides(colour = guide_legend("Location ID"))
      
    }
    
    if (facet_by == "location_id") {
      
      p <- p + facet_wrap(~location_id, scale = "free_y") +
        geom_line(aes(colour = factor(sample_date), group = sample_date), 
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
  
  p <- ggplot(df, aes(x = location_id, y = analysis_result, group = 1)) + 
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank())
  
  if (is.null(facet_by)) {
    
    p <- p + geom_line(aes(colour = param_name, group = param_name))
    
  }
  
  if (!is.null(facet_by)) {
    
    if (facet_by == "sample_date") {
      
      p <- p + facet_wrap(~sample_date, scale = "free_y") + 
        geom_line(aes(colour = param_name, group = param_name))
      
    }
    
  }
  
  return(p)
  
}