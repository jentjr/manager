#' Coerce object to env_schema
#'
#' Converts a data frame to an env_schema object.
#' Existing env_schema objects are returned unchanged.
#'
#' @param x A data.frame or env_schema object.
#' @param ...
#' Column specifications passed to [env_schema()].
#'
#' @returns An env_schema object.
#'
#' @export
as_env_schema <- function(x, ...) {
  if (inherits(x, "env_schema")) {
    return(x)
  }

  if (!inherits(x, "data.frame")) {
    stop(
      "`x` must be a data.frame or env_schema.",
      call. = FALSE
    )
  }

  env_schema(x, ...)
}
