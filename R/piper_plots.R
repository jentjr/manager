#' Create a piper plot 
#' 
#' @param df data frame of water quality data in tidy format
#' @param TDS Scale plot by Total Dissolved Solids, default = FALSE
#' @param title Title for plot, default = NULL
#' @export

piper_plot <- function(df, x_cation, y_cation, z_cation, 
                       x_anion, y_anion, z_anion, 
                       TDS=FALSE, title=NULL) {
  
  .get_piper_ions(x_cation, y_cation, z_cation, x_anion, y_anion, z_anion)
  
  
  sym <- seq(1, length(unique(df$LOCATION_ID)), by = 1)
  
  if (isTRUE(TDS)) {
    .ggplot_piper() + geom_point(data = df, aes(x = cation_x, y = cation_y, 
                                                colour = LOCATION_ID,
                                                shape = LOCATION_ID,
                                                size = TDS, alpha = 0.2)) +
      geom_point(data = df, aes(x = anion_x, y = anion_y, shape = LOCATION_ID,
                                size = TDS, colour = LOCATION_ID, 
                                alpha = 0.2)) +
      geom_point(data = df, aes(x = diam_x, y = diam_y, shape = LOCATION_ID,
                                size = TDS, colour = LOCATION_ID, 
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
                                                color = LOCATION_ID, 
                                                shape = LOCATION_ID,
                                                alpha = 0.2), size = 5) +
      geom_point(data = df, aes(x = anion_x, y = anion_y, 
                                color = LOCATION_ID,
                                shape = LOCATION_ID,
                                alpha = 0.2), size = 5) +
      geom_point(data = df, aes(x = diam_x, y = diam_y, 
                                color = LOCATION_ID,
                                shape = LOCATION_ID,
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
  iter <- unique(df$SAMPLE_DATE)
  .ggplot_piper()
  dev.hold()
  for (i in 1:length(iter)) {
    if (isTRUE(TDS)) {
      print(piper_plot(df[df$SAMPLE_DATE == iter[i], ], 
                       TDS = TRUE, title = paste(title, iter[i], 
                                                 sep = "\n")))
      animation::ani.pause()
    }else{
      print(piper_plot(df[df$SAMPLE_DATE == iter[i], ], 
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
      animation::ani.options(nmax = length(unique(df$SAMPLE_DATE)), 
                             outdir = getwd())
      piper_time_plot(df, TDS = TRUE)
    }, 
    img.name = "PiperPlot", htmlfile = "Piper_plot.html",
    autobrowse = TRUE, title = "Piper Plot"
    )
  }else{
    animation::saveHTML({
      animation::ani.options(nmax = length(unique(df$SAMPLE_DATE)), 
                             outdir = getwd())
      piper_time_plot(df)
    }, 
    img.name = "PiperPlot", htmlfile = "Piper_plot.html",
    autobrowse = TRUE, title = "Piper Plot"
    )
  }
}

#' Function to gather major ions for piper plot. 

.get_piper_ions <- function(df, 
                            LOCATION_ID = "LOCATION_ID",
                            SAMPLE_DATE = "SAMPLE_DATE",
                            PARAM_NAME = "PARAM_NAME",
                            ANALYSIS_RESULT = "ANALYSIS_RESULT",
                            x_cation = "Calcium, dissolved", 
                            y_cation = "Magnesium, dissolved",
                            z_cation = c("Sodium, dissolved", "Potassium, dissolved"),
                            x_anion = c("Chloride, total", "Fluoride, total"), 
                            y_anion = "Alkalinity, total (lab)", 
                            z_anion = "Sulfate, total",
                            TDS = NULL,
                            pH = NULL) {
  
  ions <- c(x_cation, y_cation, z_cation, x_anion, y_anion, z_anion, TDS, pH)
  
  data <- df %>% 
    filter_(~PARAM_NAME %in% ions) %>%
    spread_(PARAM_NAME, ANALYSIS_RESULT) %>%
    group_by_(~LOCATION_ID, ~SAMPLE_DATE) %>%
    summarise_at(vars(ions), mean, na.rm = TRUE) %>%
    ungroup()
  
  return(data)
  
}

#' Function to transform data for piper plot 

.transform_piper_data <- function(df, 
                                  LOCATION_ID = "LOCATION_ID",
                                  SAMPLE_DATE = "SAMPLE_DATE",
                                  x_cation = "Calcium, dissolved", 
                                  y_cation = "Magnesium, dissolved",
                                  z_cation = c("Sodium, dissolved", "Potassium, dissolved"),
                                  x_anion = c("Chloride, total", "Fluoride, total"), 
                                  y_anion = "Alkalinity, total (lab)", 
                                  z_anion = "Sulfate, total",
                                  TDS = NULL,
                                  pH = NULL) {
  
  # cation data
  # Convert data to %
  cations <- df %>% 
    select(x_cation, y_cation, z_cation) %>%
    mutate(cation_total = rowSums(.))
  
  cations <- cations %>% 
    mutate_at(vars(y_cation), funs(cation_top = ./cation_total*100))
  
  cations <- cations %>%
    mutate_at(vars(x_cation), funs(cation_left = ./cation_total*100))

  cations <- cations %>%
    select(cation_top, cation_left) %>%
    mutate(cation_right = 100 - (cation_top + cation_left))
  
  # Convert data into xy coordinates
  cations <- cations %>%
    mutate(cation_x = cation_right + cation_top/2, 
           cation_y = sqrt(3)/2*cation_top)

  # anion data
  # Convert data to %
  anions <- df %>% 
    select(x_anion, y_anion, z_anion) %>%
    mutate(anion_total = rowSums(.))
  
  anions <- anions %>% 
    mutate_at(vars(z_anion), funs(anion_top = ./anion_total*100))
  
  anions <- anions %>%
    mutate_at(vars(y_anion), funs(anion_left = ./anion_total*100))
  
  anions <- anions %>%
    select(anion_top, anion_left) %>%
    mutate(anion_right = 100 - (anion_top + anion_left))
  
  # Convert data into xy coordinates
  anions <- anions %>%
    mutate(anion_x = anion_right + anion_top/2, 
           anion_y = sqrt(3)/2*anion_top)
  
  # diamond points
  calc_diam_point <- function(cation_x, anion_x, cation_y, anion_y, 
                              grad = 2 * sin(pi / 3)){
    b1 <- cation_y - (grad * cation_x)
    b2 <- anion_y - (-grad * anion_x)
    M <- matrix(c(grad, -grad, -1, -1), ncol = 2)
    t_mat <- -solve(M) %*% cbind(b1, b2)
    data.frame(diam_x = t_mat[1, 1], diam_y = t_mat[2, 1])
  }
  
  diam_list <- lapply(1:length(cation_x), function(i) calc_diam_point(cation_x[i],
                                                                      anion_x[i], cation_y[i], 
                                                                      anion_y[i]))
  
  npoints <- do.call("rbind", diam_list)
  
  piper_data <- data.frame(LOCATION_ID = df[,name], SAMPLE_DATE = df[,date], 
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