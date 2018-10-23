#' Function to create a new database for water quality data
#'
#' @param path path to sqlite database
#'
#' @export
 
create_database <- function(path) {

  db <- dbConnect(RSQLite::SQLite(), path)

  dbSendQuery(db, "CREATE TABLE IF NOT EXISTS site (
                site_id INT PRIMARY KEY,
                site_name VARCHAR,
                site_location GEOGRAPHY(POINT, 4326))"
              )

  dbSendQuery(db, "CREATE TABLE IF NOT EXISTS sample_results (
                lab_id VARCHAR,
                location_id VARCHAR,
                sample_date DATE,
                parm_cd INT PRIMARY KEY,
                analysis_result FLOAT,
                detecion_limit FLOAT,
                reporting_limit FLOAT,
                prac_quant_limit FLOAT,
                qualifier CHAR,
                measure_unit CHAR)"
              )

  dbSendQuery(db, "CREATE TABLE IF NOT EXISTS global_parameters(
                parm_cd INT PRIMARY KEY,
                description VARCHAR,
                epa_equivalence VARCHAR,
                characteristicname VARCHAR,
                measurementcode VARCHAR,
                resultsamplefraction VARCHAR,
                resulttemperaturebasis VARCHAR,
                resultstatisticalbasis VARCHAR,
                resulttimebasis VARCHAR,
                resultweightbasis VARCHAR,
                resultparticlesizebasis VARCHAR)"
              )

  dbSendQuery(db, "CREATE TABLE IF NOT EXISTS site_parameters(
                site_id INT
                parm_cd INT,
              )"
  )

  dbDisconnect(db)

}

#' Function to delete an existing water quality database
#'
#' @param path path to sqlite database
#'
#' @export

delete_datebase <- function(path) {

  db <- dbConnect(RSQLite::SQLite(), path)

  dbSendQuery(db, "DROP TABLE IF EXISTS site CASCADE")

  dbSendQuery(db, "DROP TABLE IF EXISTS sample_results CASCADE")

  dbSendQuery(db, "DROP TABLE IF EXISTS global_parameters CASCADE")

  dbSendQuery(db, "DROP TABLE IF EXISTS site_parameters CASCADE")

  dbDisconnect(db)

}

enter_data <- function(db) {

  db <- dbConnect(RSQLite::SQLite(), db)

  dbSendQuery(db, "INSERT INTO wellConstruction VALUES('MW-1', 'MW-1', '45.23',
             '80.45', 128.76, 100.56, 56.10)")

  dbDisconnect(db)

}

#' Function to return USGS parameter codes
#' 
#' @noRd

get_param_codes <- function(){
  
  pmcodes_url <- "https://qwwebservices.usgs.gov/public_srsnames.xls"
  
  httr::GET(pmcodes_url, httr::write_disk(tf <- tempfile(fileext = ".xls")))
  
  df <- readxl::read_excel(tf, skip = 5)
  
  unlink(tf)
  
  df
}