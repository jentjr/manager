#' Function to export data from manages to excel. Each sampling location 
#' is saved to a tab.
#'
#' @param df groundwater data frame in tidy format
#' @param wells list of wells to be exported
#' @param constituents list of constituents to be exported
#' @param file full file path name with extension for export
#' @param short_name TRUE/FALSE to abbreviate constituent name
#' @param overwrite TRUE/FALSE overwrite existing spreadsheet if it exists
#'
#' @export

write_excel <- function(df, wells, constituents, file, short_name = TRUE,
                        overwrite = FALSE) {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("openxlsx needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("plyr", quietly = TRUE)) {
    stop("plyr needed for this function to work. Please install it.",
         call. = FALSE)
  }

  df <- df %>%
    dplyr::filter(param_name %in% constituents, location_id %in% wells)

  df <- join_lt(df)

  df <- df %>%
    name_units(short_name = short_name)

  df <- df %>%
    dplyr::select(location_id, lab_id, sample_date, param_name, analysis_result) %>%
    tidyr::spread(param_name, analysis_result) %>%
    arrange(location_id, sample_date)

  wb <- openxlsx::createWorkbook()

  data_cast <- function(x){
    openxlsx::addWorksheet(wb, paste(x$location_id[1]))
    openxlsx::writeData(wb, x, sheet = paste(x$location_id[1]),
                        startRow = 1, startCol = 1, rowNames = FALSE)
  }

  if (!requireNamespace("plyr", quietly = TRUE)) {
    stop("plyr needed for this function to work. Please install it.",
         call. = FALSE)
  }

  plyr::d_ply(df, plyr::.(location_id), data_cast)

  openxlsx::saveWorkbook(wb, file = file, overwrite = overwrite)

}


#' Function to export summary table for a sampling event
#'
#' @param df groundwater data frame in tidy format
#' @param start start date
#' @param end end date
#' @param  gw_elev if TRUE, list date well sampled and annotate date for GW Elev
#' @param short_name TRUE/FALSE to abbreviate constituent name
#'
#' @export

write_event_summary <- function(df, start, end, gw_elev = TRUE,
                          short_name = TRUE, file) {

  df <- df %>%
    filter(sample_date >= start & sample_date <= end)

  df <- df %>%
    name_units()

  df <- join_lt(df)

  df <- df %>%
    select(location_id, param_name, analysis_result) %>%
    spread(location_id, analysis_result)

  readr::write_csv(df, path=file)

}