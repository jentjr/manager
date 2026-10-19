#' Compute prediction intervals
#'
#' @keywords internal
.compute_pred_int <- function(
  df,
  simultaneous = FALSE,
  method = "Bonferroni",
  n_mean = 1,
  k = 1,
  pi_type = "upper",
  conf_level = 0.95
) {
  .validate_est_dist_output(df)

  rlang::arg_match0(
    pi_type,
    c("upper", "lower", "two-sided")
  )

  checkmate::assert_int(n_mean, lower = 1)
  checkmate::assert_int(k, lower = 1)
  checkmate::assert_number(
    conf_level,
    lower = 0,
    upper = 1
  )

  pred_fun <- function(data, distribution) {
    values <- data$analysis_result

    switch(
      distribution,

      "Normal" = {
        if (simultaneous) {
          EnvStats::predIntNormSimultaneous(
            x = values,
            n.mean = n_mean,
            k = k,
            pi.type = pi_type,
            conf.level = conf_level
          )
        } else {
          EnvStats::predIntNorm(
            x = values,
            method = method,
            n.mean = n_mean,
            k = k,
            pi.type = pi_type,
            conf.level = conf_level
          )
        }
      },

      "Lognormal" = {
        if (simultaneous) {
          EnvStats::predIntLnormSimultaneous(
            x = values,
            n.geomean = n_mean,
            k = k,
            pi.type = pi_type,
            conf.level = conf_level
          )
        } else {
          EnvStats::predIntLnorm(
            x = values,
            method = method,
            n.geomean = n_mean,
            k = k,
            pi.type = pi_type,
            conf.level = conf_level
          )
        }
      },

      "Nonparametric" = {
        if (simultaneous) {
          EnvStats::predIntNparSimultaneous(
            x = values,
            pi.type = pi_type
          )
        } else {
          EnvStats::predIntNpar(
            x = values,
            pi.type = pi_type
          )
        }
      }
    )
  }

  df %>%
    dplyr::mutate(
      pred_int = purrr::map2(
        data,
        distribution,
        pred_fun
      )
    ) %>%
    dplyr::mutate(
      sample_size = purrr::map_dbl(
        pred_int,
        "sample.size"
      ),
      conf_level = purrr::map_dbl(
        pred_int,
        ~ .x$interval$conf.level
      ),
      lpl = purrr::map_dbl(
        pred_int,
        ~ unname(.x$interval$limits["LPL"])
      ),
      upl = purrr::map_dbl(
        pred_int,
        ~ unname(.x$interval$limits["UPL"])
      )
    ) %>%
    dplyr::select(-data, -pred_int)
}
