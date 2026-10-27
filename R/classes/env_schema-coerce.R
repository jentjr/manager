#' @export
as_env_schema <- function(x, ...) {
  UseMethod("as_env_schema")
}

#' @export
as_env_schema.env_schema <- function(x, ...) {
  x
}

#' @export
as_env_schema.data.frame <- function(x, ...) {
  if (has_env_schema_structure(x)) {
    return(
      new_env_schema(
        x,
        env_mapping = attr(x, "env_mapping", exact = TRUE),
        measurement_metadata = attr(x, "measurement_metadata", exact = TRUE),
        threshold_metadata = attr(x, "threshold_metadata", exact = TRUE),
        schema_metadata = attr(x, "schema_metadata", exact = TRUE),
        analyte_metadata = attr(x, "analyte_metadata", exact = TRUE),
        schema_version = attr(x, "env_schema_version", exact = TRUE)
      )
    )
  }

  env_schema(
    x,
    ...
  )
}

#' @export
as_env_schema.default <- function(x, ...) {
  rlang::abort(
    "`x` must be a data.frame or env_schema."
  )
}

#' @export
tbl_sum.env_schema <- function(x) {
  c(
    "env_schema",
    Rows = nrow(x),
    Locations = dplyr::n_distinct(x$.location),
    Analytes = dplyr::n_distinct(x$.analyte),
    Version = schema_version(x)
  )
}

#' Low-level env_schema constructor
#'
#' @keywords internal
new_env_schema <- function(
  x,
  env_mapping = NULL,
  measurement_metadata = character(),
  threshold_metadata = character(),
  schema_metadata = character(),
  analyte_metadata = NULL,
  schema_version = "0.4.0",
  schema_created = Sys.time(),
  analyte_lookup = NULL
) {
  x <- tibble::as_tibble(x)

  if (is.null(analyte_metadata)) {
    analyte_cols <- c(
      ".source_analyte",
      ".normalized_input",

      ".analyte",
      ".analyte_id",
      ".normalized_analyte",

      ".fraction",
      ".is_known_analyte",

      ".short_name",
      ".casrn",

      ".analyte_group",
      ".subcategory",
      ".regulatory_class",

      ".std_unit",

      ".ion_class",
      ".charge",
      ".molecular_weight",
      ".meq_factor",

      ".is_major_ion",
      ".is_trace_metal",
      ".is_radionuclide",
      ".is_general_chemistry",
      ".is_field_parameter",

      ".analyte_label",
      ".short_analyte_label"
    )

    analyte_metadata <- intersect(
      analyte_cols,
      names(x)
    )
  }

  analyte_lookup_source <-
    if (is.null(analyte_lookup)) {
      NULL
    } else if (
      isTRUE(
        all.equal(
          analyte_lookup,
          analyte_tbl,
          check.attributes = FALSE
        )
      )
    ) {
      "package"
    } else {
      "user"
    }

  structure(
    x,

    env_mapping = env_mapping,

    measurement_metadata = measurement_metadata,
    threshold_metadata = threshold_metadata,
    schema_metadata = schema_metadata,
    analyte_metadata = analyte_metadata,

    env_schema_version = schema_version,
    env_schema_created = schema_created,

    analyte_lookup_source = analyte_lookup_source,
    analyte_lookup_version = utils::packageVersion("manager"),

    analyte_lookup_n = if (!is.null(analyte_lookup)) {
      nrow(analyte_lookup)
    } else {
      NA_integer_
    },

    analyte_lookup_cols = if (!is.null(analyte_lookup)) {
      names(analyte_lookup)
    } else {
      character()
    },

    class = c(
      "env_schema",
      class(x)
    )
  )
}
