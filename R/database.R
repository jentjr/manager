#' Function to create a new database for water quality data
#'
#' @param con connection to Postgresql database
#'
#' @export
 
create_database <- function(con) {

  DBI::dbSendQuery(con, 
              "CREATE TABLE IF NOT EXISTS site (
                site_id INT PRIMARY KEY,
                site_name VARCHAR,
                site_location GEOGRAPHY(POINT, 4326)
              )"
            )

  DBI::dbSendQuery(con,
              "CREATE TABLE IF NOT EXISTS sample_results (
                lab_id VARCHAR,
                location_id VARCHAR NOT NULL,
                sample_date DATE NOT NULL,
                analysis_method VARCHAR,
                parm_cd INT PRIMARY KEY NOT NULL,
                analysis_flag CHAR,
                analysis_result FLOAT,
                reporting_unit CHAR,
                detecion_limit FLOAT,
                prac_quant_limit FLOAT,
                min_det_activity FLOAT,
                comb_stand_unc FLOAT,
                analysis_qualifier CHAR,
                disclaimer VARCHAR,
                analysis_date DATE,
                order_comment VARCHAR,
                results_comment VARCHAR
              )"
            )

  DBI::dbSendQuery(con,
              "CREATE TABLE IF NOT EXISTS global_parameters(
                parm_cd CHAR(5) PRIMARY KEY,
                description VARCHAR,
                epa_equivalence VARCHAR,
                characteristicname VARCHAR,
                measurementcode VARCHAR,
                resultsamplefraction VARCHAR,
                resulttemperaturebasis VARCHAR,
                resultstatisticalbasis VARCHAR,
                resulttimebasis VARCHAR,
                resultweightbasis VARCHAR,
                resultparticlesizebasis VARCHAR,
                CONSTRAINT parm_cd_check CHECK (parm_cd SIMILAR TO '[[:digit:]]{5}')
              )"
            )

  DBI::dbSendQuery(con,
              "CREATE TABLE IF NOT EXISTS site_parameters(
                site_id INT
                parm_cd INT,
              )"
            )

  DBI::dbDisconnect(con)

}

#' Function to delete an existing water quality database
#'
#' @param con connetcion to Postgresql database
#'
#' @export

delete_datebase <- function(con) {

  DBI::dbSendQuery(con, "DROP TABLE IF EXISTS site CASCADE")

  DBI::dbSendQuery(con, "DROP TABLE IF EXISTS sample_results CASCADE")

  DBI::dbSendQuery(con, "DROP TABLE IF EXISTS global_parameters CASCADE")

  DBI::dbSendQuery(con, "DROP TABLE IF EXISTS site_parameters CASCADE")

  DBI::dbDisconnect(con)

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