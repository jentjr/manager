#' @importFrom vctrs vec_restore
NULL

#' @export
vec_restore.env_schema <- function(
  x,
  to,
  ...
) {
  restore_env_schema(
    x,
    to,
    validate = FALSE
  )
}

#' @export
dplyr_reconstruct.env_schema <- function(
  data,
  template
) {
  restore_env_schema(
    data,
    template,
    validate = FALSE
  )
}
