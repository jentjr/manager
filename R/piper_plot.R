#' Create a piper diagram
#'
#' labels accept plotmath expressions
#'
#' @param df data frame of water quality data in tidy format
#' @param location_id column for sample location
#' @param sample_date column for sample date
#' @param x_cation default is Calcium, dissolved
#' @param x_cation_label label for x cation
#' @param y_cation default is Magnesium, dissolved
#' @param y_cation_label label for y cation
#' @param z_cation default is Sodium, dissolved + Potassium, dissolved
#' @param z_cation_label label for z cation
#' @param x_anion default is Chloride, total + Fluoride, total
#' @param x_anion_label label for x anion
#' @param y_anion default is Alkalinity, total (lab)
#' @param y_anion_label label for y anion
#' @param z_anion default is Sulfate, total
#' @param z_anion_label label for z anion
#' @param x_z_anion_label label for the upper left diamond
#' @param x_y_cation_label label for the upper right diamond
#' @param total_dissolved_solids Scale plot by Total Dissolved Solids,
#' default = FALSE
#' @param group_col grouping column for symbols
#' @param transparency the setting for transparency value for points. Default
#' is 0.2
#' @param pnt_size the size of the points. Default is 3
#' @param label_size size of font for labels
#' @param title Title for plot, default = NULL
#'
#' @examples
#' data(gw_data)
#' wells <- c("MW-1", "MW-2", "MW-3", "MW-4", "MW-5")
#' gw_data %>%
#' filter(location_id %in% wells) %>%
#' piper_plot(., title = "Example Piper Diagram")
#'
#' # scaled by Total Dissolved Solids
#' gw_data %>%
#' filter(location_id %in% wells) %>%
#' piper_plot(., total_dissolved_solids = "Total Dissolved Solids",
#' title = "Example Piper Diagram")
#'
#' # use plotmath expressions for labels
#' gw_data %>%
#' filter(location_id %in% wells) %>%
#' piper_plot(., x_cation_label = "Ca~phantom()^+2")
#'
#' @export

piper_plot <- function(df,
                       location_id = "location_id",
                       sample_date = "sample_date",
                       x_cation = "Calcium, dissolved",
                       x_cation_label = "Ca",
                       y_cation = "Magnesium, dissolved",
                       y_cation_label = "Mg",
                       z_cation = c("Sodium, dissolved",
                                    "Potassium, dissolved"),
                       z_cation_label = "Na + K",
                       x_anion = c("Chloride, total",
                                   "Fluoride, total"),
                       x_anion_label = "Cl + F",
                       y_anion = "Alkalinity, total (lab)",
                       y_anion_label = "HCO3 + CO2",
                       z_anion = "Sulfate, total",
                       z_anion_label = "SO4",
                       x_z_anion_label = "SO4 + Cl + F",
                       x_y_cation_label = "Ca + Mg",
                       total_dissolved_solids = NULL,
                       group_col = NULL,
                       transparency = 0.7,
                       pnt_size = 3,
                       label_size = 3,
                       title = NULL) {

  piper_df <- df %>%
    .get_piper_ions(x_cation = x_cation,
                    y_cation = y_cation,
                    z_cation = z_cation,
                    x_anion = x_anion,
                    y_anion = y_anion,
                    z_anion = z_anion,
                    total_dissolved_solids = total_dissolved_solids) %>%
    .transform_piper_data(x_cation = x_cation,
                          y_cation = y_cation,
                          z_cation = z_cation,
                          x_anion = x_anion,
                          y_anion = y_anion,
                          z_anion = z_anion,
                          total_dissolved_solids = total_dissolved_solids)

  if (!is.null(group_col)) {

    group_table <- df %>%
      select(location_id, gradient = group_col) %>%
      distinct()

    piper_df <- group_table %>%
      right_join(piper_df, by = c("location_id"))

    piper <- .ggplot_piper() +
      geom_point(data = piper_df, aes(x = cation_x,
                                      y = cation_y,
                                      shape = factor(gradient, exclude = NULL),
                                      colour = location_id),
                 alpha = transparency, size = pnt_size) +
      geom_point(data = piper_df, aes(x = anion_x,
                                      y = anion_y,
                                      shape = factor(gradient, exclude = NULL),
                                      colour = location_id),
                 alpha = transparency, size = pnt_size) +
      scale_shape_manual(values = 1:nlevels(
        factor(piper_df$gradient, exclude = NULL))
        )

  } else {

    piper <- .ggplot_piper() +
      geom_point(data = piper_df, aes(x = cation_x,
                                      y = cation_y,
                                      colour = location_id),
                 alpha = transparency, size = pnt_size) +
      geom_point(data = piper_df, aes(x = anion_x,
                                      y = anion_y,
                                      colour = location_id),
                 alpha = transparency, size = pnt_size)
  }

  piper <- piper +

    # Labels for cations
    geom_text(aes_(x = 17, y = 50, 
                   label = y_cation_label),
              angle = 60,
              size = label_size, parse = TRUE) +

    geom_text(aes_(x = 82, y = 50,
                   label = z_cation_label),
              angle = -60,
              size = label_size, parse = TRUE) +
    geom_text(aes_(x = 50, y = -10,
                   label = x_cation_label),
              size = label_size, parse = TRUE) +

    # labels for anion plot
    geom_text(aes_(x = 170, y = -10,
                   label = x_anion_label),
              size = label_size, parse = TRUE) +
    geom_text(aes_(x = 205, y = 50,
                   label = z_anion_label),
              angle = -60, size = label_size, parse = TRUE) +
    geom_text(aes_(x = 138.5, y = 50,
                   label = y_anion_label),
              angle = 60, size = label_size, parse = TRUE) +

    # Diamond Labels
    geom_text(aes_(x = 72.5, y = 150,
                   label = x_z_anion_label),
              angle = 60, size = label_size, parse = TRUE) +
    geom_text(aes_(x = 147.5, y = 150,
                   label = x_y_cation_label),
              angle = -60, size = label_size, parse = TRUE) +

    ggtitle(paste(title)) +
    guides(color = guide_legend("Location ID"),
           alpha = guide_legend("none")) +
    theme(plot.title = element_text(hjust = 0.5))

  if (requireNamespace("viridis", quietly = TRUE)) {
    piper <- piper + viridis::scale_colour_viridis(discrete = TRUE)
  } else {
    piper <- piper + scale_color_discrete()
  }

  if (!is.null(group_col)) {
    if (!is.null(total_dissolved_solids)) {
      piper <-  piper +
        geom_point(data = piper_df, aes(x = diamond_x,
                                        y = diamond_y,
                                        shape = factor(gradient, exclude = NULL),
                                        colour = location_id,
                                        size = total_dissolved_solids),
                   alpha = transparency) +
        scale_size("total_dissolved_solids", range = c(0, 10)) +
        guides(size = guide_legend("Total Dissolved Solids"),
               colour = guide_legend("Location ID"),
               shape = guide_legend(paste0(group_col)),
               alpha = guide_legend("none"))

    } else {

      piper <- piper +
        geom_point(data = piper_df, aes(x = diamond_x,
                                        y = diamond_y,
                                        shape = factor(gradient, exclude = NULL),
                                        colour = location_id),
                   alpha = transparency,
                   size = pnt_size) + 
        guides(shape = guide_legend(paste0(group_col)))
      
    }
  } else {
    if (!is.null(total_dissolved_solids)) {
      piper <-  piper +
        geom_point(data = piper_df, aes(x = diamond_x,
                                        y = diamond_y,
                                        colour = location_id,
                                        size = total_dissolved_solids),
                   alpha = transparency) +
        scale_size("total_dissolved_solids", range = c(0, 10)) +
        guides(size = guide_legend("Total Dissolved Solids"),
               colour = guide_legend("Location ID"),
               alpha = guide_legend("none"))

    } else {
      piper <- piper +
        geom_point(data = piper_df, aes(x = diamond_x,
                                        y = diamond_y,
                                        colour = location_id),
                   alpha = transparency,
                   size = pnt_size)

    }
  }

    return(piper)

}

#' Function to create an animated Piper plot through time
#' using the animation package
#'
#' @param df data frame of groundwater data transformed using
#' @inheritDotParams piper_plot
#' @export

piper_time_plot <- function(df, ...) {

  if (!requireNamespace("animation", quietly = TRUE)) {
    stop("animation package needed for this function to work. Please install it.",
         call. = FALSE)
  }

  iter <- unique(df$sample_date)

  for (i in seq_along(iter)) {
    piper_plot(df[df$sample_date == iter[i], ],
                       title = paste(iter[i]),
                       ...)
      animation::ani.pause()
  }
}

#' Function to create an aminated Piper plot and save to html
#'
#' @param df data frame of groundwater data transformed using
#' @inheritDotParams piper_plot
#' @export

piper_time_html <- function(df, ...) {

  if (!requireNamespace("animation", quietly = TRUE)) {
    stop("animation package needed for this function to work. Please install it.",
         call. = FALSE)
  }

    animation::saveHTML({

      animation::ani.options(nmax = length(unique(df$sample_date)),
                             outdir = getwd())

      piper_time_plot(df, total_dissolved_solids = total_dissolved_solids)

    },

    img.name = "PiperPlot", htmlfile = "Piper_plot.html",
    autobrowse = TRUE, title = "Piper Plot"

    )

}

#' Function to gather major ions for piper plot
#'
#' @noRd

.get_piper_ions <- function(df,
                            location_id = "location_id",
                            sample_date = "sample_date",
                            param_name = "param_name",
                            analysis_result = "analysis_result",
                            default_unit = "default_unit",
                            x_cation = "Calcium, dissolved",
                            y_cation = "Magnesium, dissolved",
                            z_cation = c("Sodium, dissolved",
                                         "Potassium, dissolved"),
                            x_anion = c("Chloride, total",
                                        "Fluoride, total"),
                            y_anion = "Alkalinity, total (lab)",
                            z_anion = "Sulfate, total",
                            total_dissolved_solids = NULL
                            ) {

  ions <- c(x_cation, y_cation, z_cation,
            x_anion, y_anion, z_anion,
            total_dissolved_solids)

  df <- df %>%
    filter_(~param_name %in% ions) %>%
    spread_(param_name, analysis_result) %>%
    group_by_(~location_id, ~sample_date, ~default_unit) %>%
    summarise_at(vars(ions), mean, na.rm = TRUE) %>%
    ungroup()

  return(df)

}

#' Function to transform data for piper plot
#'
#' @noRd

.transform_piper_data <- function(df,
                                  location_id = "location_id",
                                  sample_date = "sample_date",
                                  x_cation = "Calcium, dissolved",
                                  y_cation = "Magnesium, dissolved",
                                  z_cation = c("Sodium, dissolved",
                                               "Potassium, dissolved"),
                                  x_anion = c("Chloride, total",
                                              "Fluoride, total"),
                                  y_anion = "Alkalinity, total (lab)",
                                  z_anion = "Sulfate, total",
                                  total_dissolved_solids = NULL
                                  ) {

  # cation data
  # Convert data to %
  cations <- df %>%
    select(x_cation, y_cation, z_cation) %>%
    mutate(cation_total = rowSums(.))

  cations <- cations %>%
    mutate_at(vars(y_cation), funs(cation_top = . / cation_total * 100))

  cations <- cations %>%
    mutate_at(vars(x_cation), funs(cation_left = . / cation_total * 100))

  cations <- cations %>%
    select(cation_top, cation_left) %>%
    mutate(cation_right = 100 - (cation_top + cation_left))

  # Convert data into xy coordinates
  cations <- cations %>%
    mutate(cation_x = cation_right + cation_top / 2,
           cation_y = sqrt(3) / 2 * cation_top) %>%
    select(cation_x, cation_y)

  # anion data
  # Convert data to %
  anions <- df %>%
    select(x_anion, y_anion, z_anion) %>%
    mutate(anion_total = rowSums(.))

  anions <- anions %>%
    mutate_at(vars(z_anion), funs(anion_top = . / anion_total * 100))

  anions <- anions %>%
    mutate_at(vars(y_anion), funs(anion_left = . / anion_total * 100))

  anions <- anions %>%
    select(anion_top, anion_left) %>%
    mutate(anion_right = 100 - (anion_top + anion_left))

  # Convert data into xy coordinates
  anions <- anions %>%
    mutate(anion_x = 120 + (anion_right + anion_top / 2),
           anion_y = sqrt(3) / 2 * anion_top) %>%
    select(anion_x, anion_y)

  # diamond points
  y1 <- cations$cation_y
  x1 <- cations$cation_x
  y2 <- anions$anion_y
  x2 <- anions$anion_x

  calc_diam_point <- function(x1, x2, y1, y2, grad = 2 * sin(pi / 3)) {

    b1 <- y1 - (grad * x1)
    b2 <- y2 - (-grad * x2)

    rot_matrix <- matrix(c(grad, -grad, -1, -1), ncol = 2)
    intercepts <- as.matrix(c(b1, b2))

    t_mat <- -solve(rot_matrix) %*% intercepts

    data.frame(diamond_x = t_mat[1, 1], diamond_y = t_mat[2, 1])

  }

  diam_list <- lapply(1:length(x1),
                      function(i) calc_diam_point(x1[i], x2[i], y1[i], y2[i]))

  npoints <- do.call("rbind", diam_list)

  if (!is.null(total_dissolved_solids)) {
    piper_data <- bind_cols(df, cations, anions, npoints) %>%
      select(location_id, sample_date, cation_x, cation_y,
             anion_x, anion_y, diamond_x, diamond_y,
             total_dissolved_solids = total_dissolved_solids)
  } else {
    piper_data <- bind_cols(df, cations, anions, npoints) %>%
      select(location_id, sample_date, cation_x, cation_y,
             anion_x, anion_y, diamond_x, diamond_y)
  }

  return(piper_data)

}

#' Function to create base Piper plot
#'
#' @noRd

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
                 data = data.frame(
                   x1 = c(20, 40, 60, 80),
                   x2 = c(10, 20, 30, 40),
                   y1 = c(0, 0, 0, 0),
                   y2 = c(17.3206, 34.6412, 51.9618, 69.2824)),
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                 data = data.frame(
                   x1 = c(20, 40, 60, 80),
                   x2 = c(60, 70, 80, 90),
                   y1 = c(0, 0, 0, 0),
                   y2 = c(69.2824, 51.9618, 34.6412, 17.3206)),
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                 data = data.frame(
                   x1 = c(10, 20, 30, 40),
                   x2 = c(90, 80, 70, 60),
                   y1 = c(17.3206, 34.6412, 51.9618, 69.2824),
                   y2 = c(17.3206, 34.6412, 51.9618, 69.2824)),
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                 data = data.frame(
                   x1 = c(140, 160, 180, 200),
                   x2 = c(130, 140, 150, 160),
                   y1 = c(0, 0, 0, 0),
                   y2 = c(17.3206, 34.6412, 51.9618, 69.2824)),
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                 data = data.frame(
                   x1 = c(140, 160, 180, 200),
                   x2 = c(180, 190, 200, 210),
                   y1 = c(0, 0, 0, 0),
                   y2 = c(69.2824, 51.9618, 34.6412, 17.3206)),
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                 data = data.frame(
                   x1 = c(130, 140, 150, 160),
                   x2 = c(210, 200, 190, 180),
                   y1 = c(17.3206, 34.6412, 51.9618, 69.2824),
                   y2 = c(17.3206, 34.6412, 51.9618, 69.2824)),
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                 data = data.frame(
                   x1 = c(100, 90, 80, 70),
                   y1 = c(34.6412, 51.9618, 69.2824, 86.603),
                   x2 = c(150, 140, 130, 120),
                   y2 = c(121.2442, 138.5648, 155.8854, 173.2060)),
                 linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                 data = data.frame(
                   x1 = c(70, 80, 90, 100),
                   y1 = c(121.2442, 138.5648, 155.8854, 173.2060),
                   x2 = c(120, 130, 140, 150),
                   y2 = c(34.6412, 51.9618, 69.2824, 86.603)),
                 linetype = "dashed", size = 0.25, colour = "grey50") +

    # add the grid labels
    # X Cation
    geom_text(aes(x = c(20, 40, 60, 80),
                  y = c(-5, -5, -5, -5),
                  label = c(80, 60, 40, 20)),
              size = 2.5) +
    # Y Cation
    geom_text(aes(x = c(35, 25, 15, 5),
                  y = c(69.2824, 51.9618, 34.6412, 17.3206),
                  label = c(80, 60, 40, 20)),
              size = 2.5) +
    # Z Cation
    geom_text(aes(x = c(95, 85, 75, 65),
                  y = c(17.3206, 34.6412, 51.9618, 69.2824),
                  label = c(80, 60, 40, 20)),
              size = 2.5) +
    coord_equal(ratio = 1) +
    
    # Y Anion 
    geom_text(aes(x = c(155, 145, 135, 125),
                  y = c(69.2824, 51.9618, 34.6412, 17.3206),
                  label = c(20, 40, 60, 80)),
              size = 2.5) +
    # Z Anion
    geom_text(aes(x = c(215, 205, 195, 185),
                  y = c(17.3206, 34.6412, 51.9618, 69.2824),
                  label = c(20, 40, 60, 80)),
              size = 2.5) +
    # X Anion
    geom_text(aes(x = c(140, 160, 180, 200),
                  y = c(-5, -5, -5, -5),
                  label = c(20, 40, 60, 80)),
              size = 2.5) +
    # left lower diamond
    geom_text(aes(x = c(95, 85, 75, 65),
                  y = c(34.6412, 51.9618, 69.2824, 86.603),
                  label = c(80, 60, 40, 20)),
              size = 2.5) +
    # right upper diamond
    geom_text(aes(x = c(155, 145, 135, 125),
                  y = c(121.2442, 138.5648, 155.8854, 173.2060),
                  label = c(20, 40, 60, 80)),
              size = 2.5) +
    # left upper diamond
    geom_text(aes(x = c(65, 75, 85, 95),
                  y = c(121.2442, 138.5648, 155.8854, 173.2060),
                  label = c(20, 40, 60, 80)),
              size = 2.5) +
    # right lower diamond
    geom_text(aes(x = c(125, 135, 145, 155),
                  y = c(34.6412, 51.9618, 69.2824, 86.603),
                  label = c(80, 60, 40, 20)),
              size = 2.5) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank())

  return(p)

}