#' Function to plot data transformed to schoeller coordinates using
#' \code{\link{.transform_schoeller_data}}
#' 
#' @param df dataframe of groundwater data transformed to Schoeller coordinates
#' @param facet_by parameter to facet plots by
#' @param title title of plot
#' @param lwt lineweight
#' @export

schoeller_plot <- function(df,                                  
                           LOCATION_ID = "LOCATION_ID",
                           SAMPLE_DATE = "SAMPLE_DATE",
                           PARAM_NAME = "PARAM_NAME",
                           ANALYSIS_RESULT = "ANALYSIS_RESULT",
                           DEFAULT_UNIT = "DEFAULT_UNIT",
                           Ca = "Calcium, dissolved", 
                           Mg = "Magnesium, dissolved",
                           Na = "Sodium, dissolved", 
                           K = "Potassium, dissolved",
                           Cl =  "Chloride, total", 
                           Alk = "Alkalinity, total (lab)", 
                           SO4 = "Sulfate, total",
                           facet_by = NULL, title = NULL, lwt = 1) {
  
  df <- df %>% 
    .transform_schoeller_data(LOCATION_ID = LOCATION_ID, 
                              SAMPLE_DATE = SAMPLE_DATE,
                              PARAM_NAME = PARAM_NAME,
                              ANALYSIS_RESULT = ANALYSIS_RESULT,
                              DEFAULT_UNIT = DEFAULT_UNIT,
                              Ca = Ca, 
                              Mg = Mg, 
                              Na = Na, 
                              K = K, 
                              Cl = Cl, 
                              Alk = Alk, 
                              SO4 = SO4)
  
  p <- ggplot(df, aes(x = PARAM_NAME, y = ANALYSIS_RESULT, group = 1), 
              size = lwt) + 
    scale_y_continuous(trans = scales::log10_trans(), 
                       breaks = scales::pretty_breaks(),
                       labels = prettyNum) +
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
        scale_color_viridis(discrete = TRUE) +
        guides(colour = guide_legend("Location ID"))
      
    }
    
    if (facet_by == "LOCATION_ID") {
      
      p <- p + facet_wrap(~LOCATION_ID, scale = "free_y") +
        geom_line(aes(colour = factor(SAMPLE_DATE), group = SAMPLE_DATE), 
                  size = lwt) +
        scale_colour_viridis(discrete = TRUE) +
        guides(colour = guide_legend("Sample Date"))
      
    }
    
  }
  
  return(p)
  
}

#' Function to transform data to schoeller coordinates

.transform_schoeller_data <- function(df, 
                                 LOCATION_ID = "LOCATION_ID",
                                 SAMPLE_DATE = "SAMPLE_DATE",
                                 PARAM_NAME = "PARAM_NAME",
                                 ANALYSIS_RESULT = "ANALYSIS_RESULT",
                                 DEFAULT_UNIT = "DEFAULT_UNIT",
                                 Ca = "Calcium, dissolved", 
                                 Mg = "Magnesium, dissolved",
                                 Na = "Sodium, dissolved", 
                                 K = "Potassium, dissolved",
                                 Cl =  "Chloride, total", 
                                 Alk = "Alkalinity, total (lab)", 
                                 SO4 = "Sulfate, total"
                                 ) {
  
  df <- df %>%
    .get_schoeller_ions(LOCATION_ID = LOCATION_ID, 
                        SAMPLE_DATE = SAMPLE_DATE,
                        PARAM_NAME = PARAM_NAME,
                        ANALYSIS_RESULT = ANALYSIS_RESULT,
                        DEFAULT_UNIT = DEFAULT_UNIT,
                        Ca = Ca, 
                        Mg = Mg, 
                        Na = Na, 
                        K = K, 
                        Cl = Cl,
                        Alk = Alk,
                        SO4 = SO4) %>%
    conc_to_meq(., Mg = Mg, Ca = Ca, Na = Na, 
                K = K, Cl = Cl, SO4 = SO4, Alk = Alk)
  
  df <- df %>% 
    select(Na, K) %>%
    mutate(Na_K = rowSums(.)) %>%
    select(Na_K) %>%
    bind_cols(df) %>%
    select(LOCATION_ID, SAMPLE_DATE, DEFAULT_UNIT, 
           Ca, Mg, Na_K, Cl, Alk, SO4) %>%
    rename(Mg := Mg,
           Ca := Ca,
           `Na+K` = "Na_K",
           Cl := Cl,
           HCO3 := Alk,
           SO4 := SO4)
  
  df <- df %>%
    gather(key = PARAM_NAME, 
           value = ANALYSIS_RESULT, 
           -c(LOCATION_ID, DEFAULT_UNIT, SAMPLE_DATE))
  
  return(df)
  
}

#' Function to gather major ions for scholler diagram. 

.get_schoeller_ions <- function(df, 
                                LOCATION_ID = "LOCATION_ID",
                                SAMPLE_DATE = "SAMPLE_DATE",
                                PARAM_NAME = "PARAM_NAME",
                                ANALYSIS_RESULT = "ANALYSIS_RESULT",
                                DEFAULT_UNIT = "DEFAULT_UNIT",
                                Ca = "Calcium, dissolved", 
                                Mg = "Magnesium, dissolved",
                                Na = "Sodium, dissolved", 
                                K = "Potassium, dissolved",
                                Cl =  "Chloride, total", 
                                Alk = "Alkalinity, total (lab)", 
                                SO4 = "Sulfate, total"
                                ) {
  
  ions <- c(Ca, Mg, Na, K, Cl, Alk, SO4)
  
  df <- df %>% 
    select_all(., toupper) %>%
    filter_(~PARAM_NAME %in% ions) %>%
    spread_(PARAM_NAME, ANALYSIS_RESULT) %>%
    group_by_(~LOCATION_ID, ~SAMPLE_DATE, ~DEFAULT_UNIT) %>%
    summarise_at(vars(ions), mean, na.rm = TRUE) %>%
    ungroup()
  
  return(df)
  
}