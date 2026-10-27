# R/env_schema-dplyr.R

#' @export
group_by.env_schema <- function(
  .data,
  ...,
  .add = FALSE,
  .drop = dplyr::group_by_drop_default(.data)
) {
  out <- dplyr::group_by(
    tibble::as_tibble(.data),
    ...,
    .add = .add,
    .drop = .drop
  )

  restore_env_schema(
    out,
    .data,
    validate = FALSE
  )
}

#' @export
ungroup.env_schema <- function(x, ...) {
  out <- dplyr::ungroup(
    tibble::as_tibble(x)
  )

  restore_env_schema(
    out,
    x,
    validate = FALSE
  )
}
