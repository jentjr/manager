#' Animate geochemical plots by date
#'
#' @param data Data frame.
#' @param plot_fn Plotting function.
#' @param sample_date Sample date column.
#' @param pause Delay between frames in seconds.
#' @param ...
#' Additional arguments passed to `plot_fn`.
#'
#' @return Invisibly returns NULL.
#'
#' @noRd
.animate_geochemical_plots <- function(
  data,
  plot_fn,
  sample_date = "sample_date",
  pause = 1,
  ...
) {
  dates <- sort(unique(data[[sample_date]]))

  plots <- purrr::map(
    dates,
    function(date) {
      plot_fn(
        data = dplyr::filter(
          data,
          .data[[sample_date]] == date
        ),
        sample_date = sample_date,
        ...
      )
    }
  )

  plots <- unlist(
    plots,
    recursive = FALSE
  )

  for (i in seq_along(plots)) {
    print(plots[[i]])

    readline(
      paste(
        "Frame",
        i,
        "of",
        length(plots),
        "- press Enter"
      )
    )
  }

  invisible(plots)
}

#' Save animated geochemical plots to HTML
#'
#' @param data Data frame.
#' @param plot_fn Plotting function.
#' @param sample_date Sample date column.
#' @param htmlfile Output HTML file.
#' @param title Animation title.
#' @param pause Delay between frames in seconds.
#' @param img.name Base image name.
#' @param ...
#' Additional arguments passed to `plot_fn`.
#'
#' @return Invisibly returns htmlfile.
#'
#' @noRd

.save_geochemical_animation <- function(
  data,
  plot_fn,
  sample_date = "sample_date",
  htmlfile,
  title,
  pause = 1,
  img.name = "Animation",
  ...
) {
  animation::saveHTML(
    expr = {
      .animate_geochemical_plots(
        data = data,
        plot_fn = plot_fn,
        sample_date = sample_date,
        pause = pause,
        ...
      )
    },
    img.name = img.name,
    htmlfile = htmlfile,
    autobrowse = TRUE,
    title = title
  )

  invisible(htmlfile)
}
