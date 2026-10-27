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
      restore_env_schema(
        x = x,
        to = x
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
    Locations = dplyr::n_distinct(location(x)),
    Analytes = dplyr::n_distinct(analyte(x)),
    Version = schema_version(x)
  )
}

#' Restore env_schema attributes
#'
#' Reattaches all env_schema metadata from a template object
#' to a modified data frame.
#'
#' @param x Data frame to restore.
#' @param to Template env_schema object.
#' @param validate Logical. Should the restored object be
#'   validated before returning?
#'
#' @returns An object of class `env_schema`.
#'
#' @keywords internal
restore_env_schema <- function(
  x,
  to,
  validate = TRUE
) {
  metadata_names <- names(x)

  mapping <- attr(
    to,
    "env_mapping",
    exact = TRUE
  )

  mapping <- mapping[
    unlist(mapping, use.names = FALSE) %in%
      names(x)
  ]

  out <- new_env_schema(
    x,

    env_mapping = mapping,

    measurement_metadata = intersect(
      attr(
        to,
        "measurement_metadata",
        exact = TRUE
      ),
      metadata_names
    ),

    threshold_metadata = intersect(
      attr(
        to,
        "threshold_metadata",
        exact = TRUE
      ),
      metadata_names
    ),

    schema_metadata = intersect(
      attr(
        to,
        "schema_metadata",
        exact = TRUE
      ),
      metadata_names
    ),

    analyte_metadata = intersect(
      attr(
        to,
        "analyte_metadata",
        exact = TRUE
      ),
      metadata_names
    ),

    schema_version = attr(
      to,
      "env_schema_version",
      exact = TRUE
    ),

    schema_created = attr(
      to,
      "env_schema_created",
      exact = TRUE
    ),

    analyte_lookup_source = attr(
      to,
      "analyte_lookup_source",
      exact = TRUE
    ),

    analyte_lookup_version = attr(
      to,
      "analyte_lookup_version",
      exact = TRUE
    ),

    analyte_lookup_n = attr(
      to,
      "analyte_lookup_n",
      exact = TRUE
    ),

    analyte_lookup_cols = attr(
      to,
      "analyte_lookup_cols",
      exact = TRUE
    )
  )

  class(out) <- unique(
    c(
      setdiff(class(x), c("tbl_df", "tbl", "data.frame")),
      class(out)
    )
  )

  dplyr_attrs <- attributes(x)

  if (!is.null(dplyr_attrs$groups)) {
    attr(out, "groups") <- dplyr_attrs$groups
  }

  if (validate) {
    validate_env_schema(out)
  }

  out
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
  schema_version = "0.5.0",
  schema_created = Sys.time(),

  analyte_lookup = NULL,

  analyte_lookup_source = NULL,
  analyte_lookup_version = NULL,
  analyte_lookup_n = NULL,
  analyte_lookup_cols = NULL
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

  if (
    is.null(analyte_lookup_source) &&
      !is.null(analyte_lookup)
  ) {
    analyte_lookup_source <-
      if (
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
  }

  if (
    is.null(analyte_lookup_version) &&
      !is.null(analyte_lookup)
  ) {
    analyte_lookup_version <-
      utils::packageVersion("manager")
  }

  if (
    is.null(analyte_lookup_n) &&
      !is.null(analyte_lookup)
  ) {
    analyte_lookup_n <- nrow(analyte_lookup)
  }

  if (
    is.null(analyte_lookup_cols) &&
      !is.null(analyte_lookup)
  ) {
    analyte_lookup_cols <- names(analyte_lookup)
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
    analyte_lookup_version = analyte_lookup_version,
    analyte_lookup_n = analyte_lookup_n,
    analyte_lookup_cols = analyte_lookup_cols,

    class = c(
      "env_schema",
      class(x)
    )
  )
}
