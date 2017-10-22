#' Function to plot data transformed to schoeller coordinates using
#' \code{\link{.transform_schoeller_data}}
#'
#' @param df dataframe of environmental data in tidy format. The function
#' will convert data from mg/L to meq/L
#' @param location_id the column for sample locations
#' @param sample_date column for sample date
#' @param analysis_result column containing the numerical analysis result
#' @param default_unit column containing the units. Assumes mg/L.
#' @param calcium name of calcium. Default is "Calcium, dissolved"
#' @param magnesium name of magnesium. Default is "Magnesium, dissolved"
#' @param sodium name for sodium. Default is "Sodium, dissolved"
#' @param potassium name for potassium. Default is "Potassium, dissolved"
#' @param chloride name for chloride. Default is "Chloride, total"
#' @param alkalinity name for alkalinity. Default is "Alkalinity, total (lab)"
#' @param sulfate name of sulfate. Default is "Sulfate, total"
#' @param facet_by parameter to facet plots by. Default is location
#' @param title title of plot
#' @param lwt lineweight
#'
#' @examples 
#' data(gw_data)
#' gw_data %>%
#' filter(location_id %in% c("MW-1", "MW-2")) %>%
#' schoeller_plot(., facet_by = "location_id", title = "Example Scholler Plot")
#'  
#' gw_data %>%
#' filter(location_id %in% c("MW-1", "MW-2")) %>%
#' schoeller_plot(., facet_by = "sample_date", lwt = 2)
#'
#' @export

schoeller_plot <- function(df,
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
                           facet_by = "location_id",
                           title = NULL,
                           lwt = 1) {

  df <- df %>%
    .transform_schoeller_data(location_id = location_id,
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
                              sulfate = sulfate)

  p <- ggplot(df, aes(x = param_name, y = analysis_result, group = 1),
              size = lwt) +
    scale_y_continuous(trans = scales::log10_trans(),
                       breaks = scales::pretty_breaks(),
                       labels = prettyNum) +
    scale_x_discrete() + 
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank()) +
    ggtitle(paste(title)) +
    theme(plot.title = element_text(hjust = 0.5))

  if (facet_by == "sample_date") {

    p <- p + facet_wrap(~sample_date, scale = "free_y") +
      geom_line(aes(colour = location_id, group = location_id), size = lwt) +
      scale_color_viridis(discrete = TRUE) +
      guides(colour = guide_legend("Location ID"))

    }

  if (facet_by == "location_id") {

    p <- p + facet_wrap(~location_id, scale = "free_y") +
      geom_line(aes(colour = factor(sample_date), group = sample_date),
                size = lwt) +
      scale_colour_viridis(discrete = TRUE) +
      guides(colour = guide_legend("Sample Date"))

  }

  return(p)

}

#' Help function to transform data to schoeller coordinates

.transform_schoeller_data <- function(df,
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
                                 sulfate = "Sulfate, total"
                                 ) {

  df <- df %>%
    .get_schoeller_ions(location_id = location_id,
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
                        sulfate = sulfate) %>%
    conc_to_meq(., magnesium = magnesium, calcium = calcium, sodium = sodium,
                potassium = potassium, chloride = chloride, sulfate = sulfate,
                alkalinity = alkalinity)

  df <- df %>%
    select(sodium, potassium) %>%
    mutate(`Na + K` = rowSums(.)) %>%
    select(`Na + K`) %>%
    bind_cols(df) %>%
    select(location_id = location_id, sample_date = sample_date,
           default_unit = default_unit, Mg = magnesium, Ca = calcium,
           `Na + K` = `Na + K`, Cl = chloride, SO4 = sulfate, Alk = alkalinity)

  df <- df %>%
    gather(key = param_name,
           value = analysis_result,
           -c(location_id, default_unit, sample_date))

  return(df)

}

#' Function to gather major ions for scholler diagram.

.get_schoeller_ions <- function(df,
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
                                sulfate = "Sulfate, total"
                                ) {

  ions <- c(calcium, magnesium, sodium,
            potassium, chloride, alkalinity, sulfate)

  df <- df %>%
    filter_(~param_name %in% ions) %>%
    spread_(param_name, analysis_result) %>%
    group_by_(~location_id, ~sample_date, ~default_unit) %>%
    summarise_at(vars(ions), mean, na.rm = TRUE) %>%
    ungroup()

  return(df)

}