#' Function to plot data transformed to schoeller coordinates using
#' \code{\link{.transform_schoeller_data}}
#'
#' @param df dataframe of groundwater data transformed to Schoeller coordinates
#' @param facet_by parameter to facet plots by
#' @param title title of plot
#' @param lwt lineweight
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
                           facet_by = NULL, title = NULL, lwt = 1) {

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

  }

  return(p)

}

#' Function to transform data to schoeller coordinates

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