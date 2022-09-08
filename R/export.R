#' Function to export data from manages to excel. Each sampling location 
#' is saved to a tab.
#'
#' @param df groundwater data frame in tidy format
#' @param wells list of wells to be exported
#' @param constituents list of constituents to be exported
#' @param file full file path name with extension for export
#' @param short_name TRUE/FALSE to abbreviate constituent name
#' @param lab_id TRUE/FALSE to include the lab id
#' @param overwrite TRUE/FALSE overwrite existing spreadsheet if it exists
#'
#' @export

write_excel <- function(df, wells, constituents, file, short_name = TRUE,
                        lab_id = FALSE, overwrite = FALSE) {

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

  if (lab_id == TRUE){
    df <- df %>%
      dplyr::select(location_id, lab_id, sample_date, param_name, analysis_result) %>%
      tidyr::spread(param_name, analysis_result) %>%
      arrange(location_id, sample_date)%>%
      rename(`Location ID` = location_id, `Lab ID` = lab_id,
             `Sample Date` = sample_date)
  } else {
    df <- df %>%
      dplyr::select(location_id, sample_date, param_name, analysis_result) %>%
      tidyr::spread(param_name, analysis_result) %>%
      arrange(location_id, sample_date) %>%
      rename(`Location ID` = location_id, `Sample Date` = sample_date)
  }

  hs <- openxlsx::createStyle(halign = "center", 
                              valign = "center", 
                              textDecoration = "Bold",
                              border = "TopBottomLeftRight",
                              wrapText = TRUE
                              )
  
  df$`Sample Date` <- as.Date(df$`Sample Date`)
  
  wb <- openxlsx::createWorkbook()

  data_cast <- function(x){
    openxlsx::addWorksheet(wb, paste(x$`Location ID`[1]))
    openxlsx::pageSetup(wb, sheet = paste(x$`Location ID`[1]), 
                        orientation = "landscape",
                        paperSize = 3,
                        fitToWidth = TRUE,
                        fitToHeight = TRUE)
    openxlsx::writeData(wb, x, sheet = paste(x$`Location ID`[1]),
                        startRow = 1, startCol = 1, rowNames = FALSE,
                        borders = "all",
                        headerStyle = hs)
  }

  if (!requireNamespace("plyr", quietly = TRUE)) {
    stop("plyr needed for this function to work. Please install it.",
         call. = FALSE)
  }

  plyr::d_ply(df, plyr::.(`Location ID`), data_cast)

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