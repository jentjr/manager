#' Function to export data from manages to excel format required by OEPA
#' 
#' @param df groundwater data frame in tidy format
#' @param wells list of wells to be exported
#' @param constituents list of constituents to be exported
#' @param file full file path name with extension for export
#' @param short_name TRUE/FALSE to abbreviate constituent name
#' 
#' @export

export_OEPA <- function(df, wells, constituents, file, short_name = TRUE) {
  
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
    dplyr::select(location_id, sample_date, param_name, analysis_result) %>%
    tidyr::spread(param_name, analysis_result)
  
  wb <- openxlsx::createWorkbook()
  
  oepa_cast <- function(x){
    openxlsx::addWorksheet(wb, paste(x$location_id[1]))
    openxlsx::writeData(wb, x, sheet = paste(x$location_id[1]),
                        startRow = 1, startCol = 1, rowNames = FALSE)
  }
  
  if (!requireNamespace("plyr", quietly = TRUE)) {
    stop("plyr needed for this function to work. Please install it.", 
         call. = FALSE)
  }
  
  plyr::d_ply(df, plyr::.(location_id), oepa_cast)
  
  openxlsx::saveWorkbook(wb, file = file)
  
}


#' Function to export summary table for a sampling event
#' 
#' @param df groundwater data frame in tidy format
#' @param wells list of wells to include
#' @param params list of parameters to include
#' @param start start date
#' @param end end date 
#' @param  gw_elev if TRUE, list date well sampled and annotate date for GW Elev
#' @param short_name TRUE/FALSE to abbreviate constituent name
#' 
#' @export

event_summary <- function(df, wells, params, start, end, gw_elev = TRUE,
                          short_name = TRUE) {
  
  df <- df %>% 
    filter(location_id %in% wells, 
           param_name %in% params, 
           sample_date >= start & sample_date <= end)
  
  df <- df %>%
    name_units()
  
  df <- join_lt(df)
  
  df <- df %>%
    select(location_id, sample_date, param_name, analysis_result) %>%
    spread(param_name, analysis_result)
  
  return(df)
  
}