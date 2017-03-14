#' Function to export data from manages to excel format required by OEPA
#' 
#' @param df groundwater data frame in tidy format
#' @param wells list of wells to be exported
#' @param constituents list of constituents to be exported
#' @param file full file path name with extension for export
#' @export

export_OEPA <- function(df, wells, constituents, file){
  
  df <- df %>% 
    dplyr::filter(param_name %in% constituents, location_id %in% wells)
  
  df <- join_lt(df, "result")
  
  df <- name_units(df, "param_unit")
  
  df <- df %>%
    dplyr::select(lab_id, location_id, sample_date, param_unit, result) %>%
    tidyr::spread(param_unit, result)
  
  wb <- openxlsx::createWorkbook()
  
  oepa_cast <- function(x){
    openxlsx::addWorksheet(wb, paste(x$location_id[1]))
    openxlsx::writeData(wb, x, sheet = paste(x$location_id[1]),
                        startRow = 1, startCol = 1, rowNames = FALSE)
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
#' @export

event_summary <- function(df, wells, params, start, end, gw_elev = TRUE){
  
  df <- df %>% 
    filter(location_id %in% wells, 
           param_name %in% params, 
           sample_date >= start & sample_date <= end)
  
  df <- name_units(df, "param_units")
  
  df <- join_lt(df, "result")
  
  id <- dplyr::group_by(df, location_id, sample_date, param_units)
  
  out <- reshape2::dcast(id, value.var = "result", 
                         location_id + sample_date ~ param_units, 
                         margins = FALSE)
  return(out)
  
}