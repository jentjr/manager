#' function to read monitoring well data in the form location_id, sample_date, 
#' analysis_result, default_unit, param_name and gather parameters needed for
#' geochemistry plots. 
#' The data is cast by location_id + sample_date ~ param_name
#'
#' @param df data frame of groundwater monitoring data
#' @export

get_major_ions <- function(df, Mg="Magnesium, dissolved", 
                           Ca="Calcium, dissolved", 
                           Na="Sodium, dissolved", K="Potassium, dissolved", 
                           Cl="Chloride, total", SO4="Sulfate, total", 
                           Alk="Alkalinity, total (lab)", 
                           TDS="Total Dissolved Solids",...){
#   TODO: add other major ions like Fe
#   input_list <- list(...)
  
  plot_params <- c(Mg, Ca, Na, K, Cl, SO4, Alk, TDS)
  
  plot_data <- subset(df, param_name %in% plot_params)
 
  plot_data <- reshape2::dcast(plot_data, value.var = "analysis_result", 
                        location_id + sample_date + default_unit ~ param_name)

  return(plot_data)
}

#' function to convert geochemical plot data into meq/L
#' 
#' @param df data frame
#' @param Mg Magnesium
#' @param Ca Calcium
#' @param Na Sodium
#' @param K Potassium
#' @param Cl Chloride
#' @param SO4 Sulfate
#' @param HCO3 Bicaronate
#' 
#' @export

convert_mgL_to_meqL <- function(df, Mg="Magnesium, dissolved", 
                                Ca="Calcium, dissolved", Na="Sodium, dissolved", 
                                K="Potassium, dissolved", Cl="Chloride, total", 
                                SO4="Sulfate, total", 
                                HCO3="Alkalinity, total (lab)"){
  
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
  #   CO3_chrg <- 2
  HCO3_chrg <- 1
  Cl_chrg <- 1
  
  # conversion 
  df[,Mg] <- df[,Mg] / Mg_fwt * Mg_chrg
  df[,Ca] <- df[,Ca] / Ca_fwt * Ca_chrg
  df[,Na] <- df[,Na] / Na_fwt * Na_chrg
  df[,K] <- df[,K] / K_fwt * K_chrg
  df[,Cl] <- df[,Cl] / Cl_fwt * Cl_chrg
  df[,SO4] <- df[,SO4] / (S_fwt + 4 * O_fwt) * SO4_chrg
  #   df$CO3 <- CO3 / (C_fwt + 3 * O_fwt) * CO3_chrg
  df[,HCO3] <- df[,HCO3] / (H_fwt + C_fwt + 3 * O_fwt) * HCO3_chrg
  
  return(df)
}

#' function to transform data from get_plot_data() into x, y coordinates for
#' cation, anion and diamond of a Piper plot
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
#' @param units units, right now can handle mg/L and meq/L
#' @keywords piper diagram
#' @export

transform_piper_data <- function(df, Mg="Mg", Ca="Ca", Na="Na", K="K", Cl="Cl", 
                                 SO4="SO4", CO3=NULL, Alk="HCO3", 
                                 TDS="Total Dissolved Solids", 
                                 name = "location_id", date = "sample_date", 
                                 units = NULL){

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
#' @export

ggplot_piper <- function() {
  
  # gridlines
  grid1p1 <<- data.frame(x1 = c(20, 40, 60, 80), 
                         x2= c(10, 20, 30, 40), 
                         y1 = c(0, 0, 0, 0), 
                         y2 = c(17.3206, 34.6412, 51.9618, 69.2824))
  
  grid1p2 <<- data.frame(x1 = c(20, 40, 60, 80), 
                         x2= c(60, 70, 80, 90), 
                         y1 = c(0, 0, 0, 0), 
                         y2 = c(69.2824, 51.9618, 34.6412, 17.3206))
  
  grid1p3 <<- data.frame(x1 = c(10, 20, 30, 40), 
                         x2= c(90, 80, 70, 60), 
                         y1 = c(17.3206, 34.6412, 51.9618, 69.2824), 
                         y2 = c(17.3206, 34.6412, 51.9618, 69.2824))
  
  # left slant gridlines for anion triangle
  grid2p1 <<- grid1p1 
  grid2p1$x1 <<- grid2p1$x1+120
  grid2p1$x2 <<- grid2p1$x2+120
  
  # right slant lines for anion triangle
  grid2p2 <<- grid1p2
  grid2p2$x1 <<- grid2p2$x1+120
  grid2p2$x2 <<- grid2p2$x2+120
  
  # horizontal lines for anion triangle
  grid2p3 <<- grid1p3
  grid2p3$x1 <<- grid2p3$x1+120
  grid2p3$x2 <<- grid2p3$x2+120
  
  #
  grid3p1 <<- data.frame(x1=c(100,90, 80, 70), 
                         y1=c(34.6412, 51.9618, 69.2824, 86.603), 
                         x2=c(150, 140, 130, 120), 
                         y2=c(121.2442, 138.5648, 155.8854, 173.2060))
  
  grid3p2 <<- data.frame(x1=c(70, 80, 90, 100), 
                         y1=c(121.2442, 138.5648, 155.8854, 173.2060), 
                         x2=c(120, 130, 140, 150), 
                         y2=c(34.6412, 51.9618, 69.2824, 86.603))
  
  p <- ggplot() +

    ## left hand ternary plot
    geom_segment(aes(x=0,y=0, xend=100, yend=0)) +
    geom_segment(aes(x=0,y=0, xend=50, yend=86.603)) +
    geom_segment(aes(x=50,y=86.603, xend=100, yend=0)) +
    
    ## right hand ternary plot
    ## shifted right by 20 points 
    geom_segment(aes(x=120,y=0, xend=220, yend=0)) +
    geom_segment(aes(x=120,y=0, xend=170, yend=86.603)) +
    geom_segment(aes(x=170,y=86.603, xend=220, yend=0)) +
    
    ## Upper diamond
    geom_segment(aes(x=110, y=190.5266, xend=60, yend=103.9236)) +
    geom_segment(aes(x=110, y=190.5266, xend=160, yend=103.9236)) +
    geom_segment(aes(x=110, y=17.3206, xend=160, yend=103.9236)) +
    geom_segment(aes(x=110, y=17.3206, xend=60, yend=103.9236)) +
    
    ## Add grid lines to the plots
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p1, 
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p2, 
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p3, 
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p1, 
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p2, 
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p3, 
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p1, 
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p2, 
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    
    geom_text(aes(c(20,40,60,80),c(-5,-5,-5,-5), label=c(80, 60, 40, 20)), 
              size=3) +
    geom_text(aes(c(35,25,15,5), grid1p2$y2, label=c(80, 60, 40, 20)), 
              size=3) +
    geom_text(aes(c(95,85,75,65), grid1p3$y2, label=c(80, 60, 40, 20)), 
              size=3) +
    
    # geom_text(aes(17,50, label="Mg^2"), parse=T, angle=60, size=4) +
    coord_equal(ratio=1) +  
    geom_text(aes(17,50, label="Mg^+2"), angle=60, size=4, parse=TRUE) +  
    geom_text(aes(82.5,50, label="Na + K"), angle=-60, size=4) +
    geom_text(aes(50,-10, label="Ca^+2"), size=4, parse=TRUE) +
    
    # labels for anion plot
    geom_text(aes(170,-10, label="Cl^-phantom()"), size=4, parse=TRUE) +
    geom_text(aes(205,50, label="SO[4]^+2"), angle=-60, size=4, parse=TRUE) +
    geom_text(aes(137.5,50, label="Alkalinity"), angle=60, size=4, parse=TRUE) +
    
    # 
    geom_text(aes(72.5,150, label="SO[4]^+2~+~Cl^-phantom()"), angle=60, 
              size=4, parse=TRUE) +
    geom_text(aes(147.5,150, label="Ca^+2~+~Mg^+2"), angle=-60, size=4, 
              parse=TRUE) + 

    geom_text(aes(c(155,145,135,125), grid2p2$y2, label=c(20, 40, 60, 80)), 
              size=3) +
    geom_text(aes(c(215,205,195,185), grid2p3$y2, label=c(20, 40, 60, 80)), 
              size=3) +
    geom_text(aes(c(140,160,180,200), c(-5,-5,-5,-5), label=c(20, 40, 60, 80)), 
              size=3) +
    geom_text(aes(grid3p1$x1-5, grid3p1$y1, label=c(80, 60, 40, 20)), size=3) +
    geom_text(aes(grid3p1$x2+5, grid3p1$y2, label=c(20, 40, 60, 80)), size=3) +
    geom_text(aes(grid3p2$x1-5, grid3p2$y1, label=c(20, 40, 60, 80)), size=3) +
    geom_text(aes(grid3p2$x2+5, grid3p2$y2, label=c(80, 60, 40, 20)), size=3) +
    
    theme_bw() +
    
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank())
  
  return(p)

}

#' Function to plot points from transform_piper_data() onto base ggplot_piper
#' background. Points are sized by Total Dissolved Solids
#' 
#' @param df data frame of groundwater data transformed into piper coordinates 
#'  using \code{\link{transform_piper_data}}
#'  
#' @export
  
plot_piper <- function(df, TDS=FALSE){
  if(isTRUE(TDS)){
    ggplot_piper() + geom_point(data = df, aes(x = cation_x, y = cation_y, 
                                               colour = location_id, 
                                               size = TDS)) +
      geom_point(data = df, aes(x = anion_x, y = anion_y, colour = location_id, 
                                size = TDS)) +
      geom_point(data = df, aes(x = diam_x, y = diam_y, colour = location_id, 
                                size = TDS)) +
      scale_size("TDS", range = c(1, 20)) +
      ggtitle("Piper Diagram") + guides(colour = guide_legend("Location ID"),
                                        fill = guide_legend("TDS"))
  }else{
  ggplot_piper() + geom_point(data = df, aes(x = cation_x, y = cation_y, 
                                             colour = location_id), size = 3) +
    geom_point(data = df, aes(x = anion_x, y = anion_y, colour = location_id), 
               size = 3) +
    geom_point(data = df, aes(x = diam_x, y = diam_y, colour = location_id), 
               size = 3) +
    ggtitle("Piper Diagram") + guides(colour = guide_legend("Location ID"))
  }
}

#' Function to created an animated Piper plot through time
#' using the animation package
#' 
#' @param df data frame of groundwater data transformed using 
#' \code{\link{transform_piper_data}}
#' @export

piper_time_plot <- function(df, TDS = FALSE){
  ggplot_piper()
  dev.hold()
  for(i in 1:length(unique(df$sample_date))){
    if(isTRUE(TDS)){
      print(plot_piper(subset(df, sample_date == df$sample_date[i]), 
                       TDS = TRUE))
      animation::ani.pause()
    }else{
      print(plot_piper(subset(df, sample_date == df$sample_date[i])))
      animation::ani.pause()
    }
  }
}

#'a function to create an aminated Piper plot and save to html
#'
#' @param df data frame of groundwater data transformed using 
#' \code{\link{transform_piper_data}}
#' @export

piper_time_html <- function(df, TDS = FALSE){
  if(isTRUE(TDS)){
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


#' Function to transform data from \code{\link{get_major_ions}} 
#' into x, y coordinates and in the 
#' correct path for geom_polygon of ggplot
#' 
#' @param df data frame of groundwater data from \code{\link{get_major_ions}}
#' @export

transform_stiff_data <- function(df, Mg="Mg", Ca="Ca", Na="Na", K="K", Cl="Cl", 
                                 SO4="SO4", HCO3="HCO3", name="location_id", 
                                 date="sample_date", TDS = NULL, units = NULL){
  
  temp <- data.frame(df[,name], df[,date], df[,Mg], df[,Ca], df[,Na] + df[,K], 
                     df[,SO4], df[,HCO3], df[,Cl])
  colnames(temp) <- c("location_id", "sample_date", "Mg", "Ca", "Na + K", 
                      "SO4", "HCO3", "Cl")
  
  df_melt <- reshape2::melt(temp, id.vars = c("location_id", "sample_date"))
  
  cations <- c("Mg", "Ca", "Na + K")
  anions <- c("SO4", "HCO3", "Cl")
  
  df_melt$value <- ifelse(df_melt$variable %in% cations, -1 * df_melt$value, 
                          df_melt$value)
  
  stiff <- subset(df_melt, variable %in% c("Mg", "SO4", "HCO3", "Cl", "Na + K",
                                           "Ca", "TDS"))
  
  poly_order <-  data.frame(c("Mg", "SO4", "HCO3", "Cl", "Na + K", "Ca"),
                            c(3, 3, 2, 1, 1, 2))
  
  colnames(poly_order) <- c("variable", "stiff_y")
  
  stiff <- plyr::join(poly_order, stiff, by="variable", type="left")
  
  stiff <- plyr::rename(stiff, replace=c("value" = "stiff_x"))
  
  if(!missing(TDS)){
    stiff <- plyr::join(stiff, df, by=c("location_id", "sample_date"))
    stiff <- stiff[, names(stiff) %in% c("location_id", "sample_date", 
                                         "variable", "stiff_x", "stiff_y", 
                                         "Total Dissolved Solids")]
    stiff <- plyr::rename(stiff, replace=c("Total Dissolved Solids" = "TDS"))
  }  
  return(stiff)
}

#' Function to plot a Stiff Diagram 
#' 
#' @param df data frame of groundwater data transformed using 
#' @param multiple TRUE/FALSE plots multiple locations with facet_wrap
#' @param TDS TRUE/FALSE fills in the color of the stiff diagram by TDS
#' \code{\link{transform_stiff_data}}
#' @export

stiff_plot <- function(df, TDS = FALSE){
    if(isTRUE(TDS)){
      # try to fix error message when only 1 location plotted
      p <- ggplot(df) + geom_polygon(aes(x = stiff_x, y = stiff_y, fill = TDS))
      } else{
      p <- ggplot(df) + geom_polygon(aes(x = stiff_x, y = stiff_y))
      }
    
    p <- p + facet_wrap(~location_id, scale = "free") +
      geom_hline(yintercept = 1, linetype = "dashed") +
      geom_hline(yintercept = 2, linetype = "dashed") +
      geom_hline(yintercept = 3, linetype = "dashed") +
      xlab("\nmeq/L") + xlim(-15,15) +
      annotate("text", x = -14, y = 1.1, label = "Na^+1 + K^+1", 
               size = 3, parse = TRUE) +
      annotate("text", x = -14, y = 2.1, label = "Ca^+2", size = 3, 
               parse = TRUE) +
      annotate("text", x = -14, y = 3.1, label = "Mg^+2", size = 3, 
               parse = TRUE) +
      annotate("text", x = 14, y = 1.1, label = "Cl^-phantom()", size = 3, 
               parse = TRUE) +
      annotate("text", x = 14, y = 2.1, label = "HCO[3]^-1", size = 3, 
               parse = TRUE) +
      annotate("text", x = 14, y = 3.1, label = "SO[4]^-2", size = 3, 
               parse = TRUE) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
          panel.border = element_blank(), axis.ticks = element_blank(),
          axis.text.y = element_blank(), axis.title.y = element_blank()) +
      ggtitle("Stiff Diagram \n")
    
  return(p)
}

#' function to create an animated Stiff Diagram 
#' @param df data frame of groundwater data transformed using 
#' \code{\link{transform_stiff_data}}
#' @export

stiff_time_plot <- function(df, TDS = FALSE){
  for(i in 1:length(unique(df$sample_date))){
    if(isTRUE(TDS)){
      print(stiff_plot(subset(df, sample_date == df$sample_date[i]), 
                       TDS = TRUE))
      animation::ani.pause()
    }else{
      print(stiff_plot(subset(df, sample_date == df$sample_date[i]), 
                       TDS = FALSE))
     animation:: ani.pause()
    }
  }
}

#' function to create an animated Stiff Diagram and save to html
#' @param df data frame of groundwater data transformed using 
#' @param TDS TRUE/FALSE fills polygon with color gradient of TDS
#' \code{\link{transform_stiff_data}}
#' @export

stiff_time_html <- function(df, TDS = FALSE){
  animation::saveHTML({
    animation::ani.options(nmax = length(unique(df$sample_date)))
    if(isTRUE(TDS)){
      stiff_time_plot(df, TDS = TRUE)
    }else{
      stiff_time_plot(df, TDS = FALSE)
    }
  }, 
           img.name = "StiffPlot", htmlfile = "Stiff_plot.html",
           autobrowse = TRUE, title = "Stiff Diagram"
  )
}
