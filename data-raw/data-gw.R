#' Example Groundwater env_schema Object
#'
#' A fully processed groundwater monitoring dataset represented as an
#' [`env_schema`] object.
#'
#' The dataset was derived from [gw_data] using [env_schema()] and is
#' intended for package examples, vignettes, testing, and interactive
#' exploration of analysis workflows.
#'
#' @format An object of class `env_schema` with 5,878 rows and
#' standardized environmental monitoring fields.
#'
#' @source Derived from [gw_data].
#'
#' @seealso [gw_data], [env_schema()]
#'
#' @examples
#' data(gw)
#'
#' gw
#'
#' location(gw)
#'
#' analyte(gw)
#'
#' trend_summary(gw)
"gw"

gw <- env_schema(
  gw_data,
  location = location_id,
  date = sample_date,
  analyte = param_name,
  result = analysis_result,
  unit = default_unit,
  reporting_flag = lt_measure,
  analyte_lookup = analyte_tbl
)
