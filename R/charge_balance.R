#' @export
charge_balance <- function(df, ...) {
  UseMethod("charge_balance")
}

#' @export
charge_balance.data.frame <- function(
  df,
  location,
  analyte,
  result,
  unit,
  date,
  ...
) {
  df <- env_schema(
    df,
    location = {{ location }},
    analyte = {{ analyte }},
    result = {{ result }},
    unit = {{ unit }},
    date = {{ date }}
  )

  charge_balance(df, ...)
}

#' Charge balance calculation
#'
#' @export
charge_balance.env_schema <- function(
  df,
  analyte_matching = c(
    "exact",
    "strict",
    "normalized"
  )
) {
  analyte_matching <- rlang::arg_match(
    analyte_matching
  )

  chemistry_df <- .prepare_major_ion_chemistry(
    df = df,
    analyte_matching = analyte_matching
  )

  .calculate_charge_balance(
    chemistry_df
  )
}
