new_database <- function(path) {

  db <- dbConnect(RSQLite::SQLite(), path)

  dbSendQuery(db, "CREATE TABLE IF NOT EXISTS wellConstruction(boringID TEXT,
            wellID TEXT, longitude TEXT, latitude TEXT, bottomScreen REAL,
            topScreen REAL, topRiser REAL)")
  
  dbSendQuery(db, "CREATE TABLE IF NOT EXISTS sampleData(wellID TEXT,
              sampleDate TEXT, method TEXT, analyte TEXT, censored BOOLEAN,
              detectionLimit REAL, reportingLimit REAL, result REAL,
              units TEXT, comments TEXT)")

  dbSendQuery(db, "CREATE TABLE IF NOT EXISTS statProcedure(wellID TEXT,
               sampleDate TEXT, analyte TEXT, censored BOOLEAN,
               background BOOLEAN, outlier BOOLEAN, distribution TEXT)")

  dbDisconnect(db)

}

enter_data <- function(db) {

  db <- dbConnect(RSQLite::SQLite(), db)

  dbSendQuery(db, "INSERT INTO wellConstruction VALUES('MW-1', 'MW-1', '45.23',
             '80.45', 128.76, 100.56, 56.10)")

  dbDisconnect(db)

}
