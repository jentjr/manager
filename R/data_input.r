library(RODBC)

connect_manages <- function(manages_path) {
  
  manages_conn <- odbcConnectAccess(manages_path)
  
  data <- sqlQuery(manages_conn, paste("SELECT sample_results.location_id, sample_results.sample_date, sample_results.analysis_result, sample_results.lt_measure, site_parameters.default_unit, site_parameters.param_name FROM sample_results LEFT JOIN site_parameters ON sample_results.storet_code = site_parameters.storet_code"))

  close(manages_conn)
  
  return(data)
}

