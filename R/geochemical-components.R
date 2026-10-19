#' Major ion components
#'
#' @noRd
.major_ion_components <- function() {
  list(
    calcium = "calcium",

    magnesium = "magnesium",

    sodium = "sodium",

    potassium = "potassium",

    sodium_potassium = c(
      "sodium",
      "potassium"
    ),

    chloride = "chloride",

    sulfate = "sulfate",

    alkalinity = "bicarbonate"
  )
}

#' Create geochemical components
#'
#' @noRd
.create_geochemical_components <- function(
  chemistry_df,
  components,
  value_col = "analysis_result_meq"
) {
  keys <- chemistry_df %>%
    dplyr::distinct(
      location_id,
      sample_date
    )

  component_dfs <- purrr::imap(
    components,
    function(analytes, component_name) {
      chemistry_df %>%
        dplyr::filter(
          analyte %in% analytes
        ) %>%
        dplyr::group_by(
          location_id,
          sample_date
        ) %>%
        dplyr::summarise(
          value = {
            x <- .data[[value_col]]

            if (all(is.na(x))) {
              NA_real_
            } else {
              sum(
                x,
                na.rm = TRUE
              )
            }
          },
          .groups = "drop"
        ) %>%
        dplyr::rename(
          !!component_name := value
        )
    }
  )

  result <- keys

  for (component_df in component_dfs) {
    result <- result %>%
      dplyr::left_join(
        component_df,
        by = c(
          "location_id",
          "sample_date"
        )
      )
  }

  result
}
