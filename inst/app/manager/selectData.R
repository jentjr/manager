selectData <- function(input, output, session, multiple = TRUE) {
  ns <- session$ns

  output$selectSites <- shiny::renderUI({
    shiny::selectInput(
      inputId = ns("sites"),
      label = "Sites",
      choices = site_list,
      selected = head(site_list, 1),
      multiple = TRUE
    )
  })

  get_locations <- shiny::reactive({
    shiny::req(input$sites)

    query |>
      dplyr::filter(name %in% input$sites) |>
      dplyr::distinct(location_id) |>
      dplyr::collect() |>
      dplyr::pull(location_id) |>
      sort()
  })

  output$selectLocations <- shiny::renderUI({
    locations <- get_locations()

    shiny::selectInput(
      inputId = ns("locations"),
      label = "Locations",
      choices = locations,
      selected = head(locations, 1),
      multiple = multiple
    )
  })

  get_constituents <- shiny::reactive({
    shiny::req(input$sites, input$locations)

    query |>
      dplyr::filter(
        name %in% input$sites,
        location_id %in% input$locations
      ) |>
      dplyr::distinct(param_name) |>
      dplyr::collect() |>
      dplyr::pull(param_name) |>
      sort()
  })

  output$selectConstituents <- shiny::renderUI({
    constituents <- get_constituents()

    shiny::selectInput(
      ns("constituents"),
      "Constituents",
      choices = constituents,
      selected = head(constituents, 1),
      multiple = multiple
    )
  })

  shiny::reactive({
    validate(
      need(input$sites != "", "Please submit a query")
    )

    shiny::req(
      input$sites,
      input$locations,
      input$constituents
    )

    query |>
      dplyr::filter(
        name %in% input$sites,
        location_id %in% input$locations,
        param_name %in% input$constituents
      ) |>
      dplyr::collect() |>
      dplyr::mutate(
        reporting_flag = .resolve_reporting_flag(
          flags,
          lt_measure
        )
      ) |>
      env_schema(
        location = location_id,
        sample_date = sample_date,
        analyte = param_name,
        result = analysis_result,
        unit = default_unit,
        sample_id = lab_id,
        reporting_flag = reporting_flag,

        reporting_limit = rl,
        detection_limit = detection_limit,

        achieved_mdc = achieved_mdc
      ) |>
      dplyr::mutate(
        .site_id = site_id,
        .site_name = name,

        .latitude = north_coordinate,
        .longitude = east_coordinate,
        .coordinate_system = coordinate_reference,

        .water_class = water_class,
        .location_class = location_class,
        .well_type = well_type,

        .source_flag = flags,
        .source_lt_measure = lt_measure
      ) |>
      set_source_metadata(
        c(
          ".site_id",
          ".site_name",
          ".latitude",
          ".longitude",
          ".coordinate_system",
          ".water_class",
          ".location_class",
          ".well_type",
          ".source_flag",
          ".source_lt_measure"
        )
      )
  })
}
