#' Groundwater quality data for the state of Ohio
#'
#' A dataset containing groundwater quality data for the state of Ohio.
#'
#' @format A data frame with 213845 rows and 47 variables:
#' \describe{
#'   \item{well_id}{well_id, Site Identifier}
#'   \item{result_value}{analytical result}
#'   \item{analyte_count}{}
#'   \item{analyte_group}{}
#'   \item{analyte_id}{}
#'   \item{casing_length}{}
#'   \item{charge_balance_error}{}
#'   \item{chemistry_sheet_id}{}
#'   \item{collection_timestamp}{}
#'   \item{collector_gwqcp_person_id}{}
#'   \item{comb_result}{}
#'   \item{county}{}
#'   \item{detect_limit_unit}{}
#'   \item{detect_limit_value}{}
#'   \item{detected_count}{}
#'   \item{district}{}
#'   \item{field_comments}{}
#'   \item{field_qc_type}{}
#'   \item{geo_age}{}
#'   \item{geo_setting}{}
#'   \item{is_filtered}{}
#'   \item{lab_comments}{}
#'   \item{lab_comments.1}{}
#'   \item{latitude}{}
#'   \item{lith_open_section}{}
#'   \item{local_name}{}
#'   \item{longitude}{}
#'   \item{major_lithology}{}
#'   \item{matrix_type}{}
#'   \item{method}{}
#'   \item{name}{}
#'   \item{name.1}{}
#'   \item{original_sheet_id}{}
#'   \item{parameter}{}
#'   \item{pws_id}{}
#'   \item{remark_code}{}
#'   \item{result_unit}{}
#'   \item{sample_id}{}
#'   \item{sample_type}{}
#'   \item{sampling_status}{}
#'   \item{site_id}{}
#'   \item{status}{}
#'   \item{user_comments}{}
#'   \item{well_depth}{}
#'   \item{well_id_code}{}
#'   \item{well_log_num}{}
#'   \item{well_number}{}
#' }
#' @source \url{http://epa.ohio.gov/ddagw/gwqcp.aspx}
"ohio_data"

#' Groundwater quality data for the state of Indiana
#'
#' A dataset containing groundwater quality data for the state of Indiana.
#'
#' @format A data frame with 162923 rows and 11 variables:
#' \describe{
#'   \item{SiteID}{Site Identifier}
#'   \item{Result}{analytical result}
#'   \item{Analyte}{}
#'   \item{DetectionLimit}{}
#'   \item{Method}{}
#'   \item{SampleDate}{}
#'   \item{SampleID}{}
#'   \item{UTM_X}{}
#'   \item{UTM_Y}{}
#'   \item{Unit}{}
#'   \item{WellDepth}{}
#' }
#'
#' @source \url{http://in.gov/idem/cleanwater/2453.html}
"indiana_data"

#' Example groundwater quality data
#'
#'#' @format A data frame with 5878 rows and 6 variables:
#' \describe{
#'   \item{location_id}{Sample Location Identifier}
#'   \item{sample_data}{sample date}
#'   \item{analysis_result}{analysis result}
#'   \item{lt_measure}{non-detect flag}
#'   \item{default_unit}{unit of measure}
#'   \item{param_name}{analyte name}
#' }
#'
"gw_data"

#' Example environmental chemistry dataset
#'
#' An example environmental chemistry dataset stored as an
#' `env_schema` object.
#'
#' The dataset contains a subset of monitoring well data suitable
#' for demonstrating:
#'
#' * time series plotting
#' * trend analysis
#' * unit standardization
#' * meq conversion
#' * Piper diagrams
#' * Stiff diagrams
#' * handling of censored observations
#'
#' @format An `env_schema` object with approximately 500 observations.
#'
#' @source Internal package example dataset.
#'
#' @examples
#' data(gw)
#'
#' is_env_schema(gw)
#'
#' schema_version(gw)
#'
#' series_plot(gw)
#'
#' @name gw
NULL
