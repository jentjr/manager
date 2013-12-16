#' Connect to MANAGES database
#' 
#' This function connects to the MANAGES database for the path supplied.
#' R must be in 32-bit mode. 
#' 
#' @param manages_path Path to MANAGES Site.mdb file


connect_manages <- function(manages_path) {
  
  manages_conn <- odbcConnectAccess(manages_path)
  
  data <- sqlQuery(manages_conn, paste("SELECT sample_results.location_id, sample_results.sample_date, sample_results.analysis_result, sample_results.lt_measure, site_parameters.default_unit, site_parameters.param_name FROM sample_results LEFT JOIN site_parameters ON sample_results.storet_code = site_parameters.storet_code"))

  close(manages_conn)
  
  return(data)
}

connect_manages_spatial <- function(manages_path){
  
  manages_conn <- odbcConnectAccess(manages_path)
  
  sp_data <- sqlQuery(manages_conn, paste("SELECT location_id, description, long_pos, lat_pos, install_date, depth_top, depth_bottom, well_top, well_bottom FROM locations"))
  
  close(manages_conn)
  
  return(sp_data)
}