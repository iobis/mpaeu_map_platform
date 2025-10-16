########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
######################### Extra information section ############################

# Extra info (page) - SPECIES ----
wExtra <- waiter::Waiter$new(
  id = c("extraPlotA", "extraPlotB"),
  color = "white",
  html = htmltools::div(
    "Loading data...",
    htmltools::br(), htmltools::br(),
    waiter::spin_1(), style = "color: #8e929a; font-size: 18px; font-weight: 700"
  )
)

output$extraPlotA <- renderPlot({
  mdebug("Extra info triggered")
  wExtra$show()
  on.exit({
    wExtra$hide()
  })
  if (input$extraButton != 0 && input$extraButton %% 2 != 0) {
    tdf <- db_info$species |>
      select(-available_models) |>
      tidyr::unnest(files) |>
      filter(type == "biasmetrics") |>
      pull()
    tmpf <- tempfile(fileext = ".rds")
    download.file(
      tdf, tmpf, quiet = TRUE
    )
    bias <- readRDS(tmpf)
    k <- bias$k_stat
    l <- bias$l_stat
    # class(k) <- c("envelope", class(k))
    # class(l) <- c("envelope", class(l))
    par(mfrow = c(2, 1))
    plot(k, main = "Spatial bias - K-function")
    plot(l, main = "L-function")
  } else {
    plot.new()
  }
}) |>
  bindEvent(input$extraButton, ignoreInit = TRUE)

output$extraPlotB <- renderPlot({
  wExtra$show()
  on.exit({
    wExtra$hide()
  })
  if (input$extraButton != 0 && input$extraButton %% 2 != 0) {
    species_files <- db_info$species |>
      select(-available_models) |>
      tidyr::unnest(files)
    shape <- species_files |>
      filter(type == "shape") |>
      mutate(file = paste0("/vsicurl/", file)) |>
      pull(file) |>
      terra::rast() |>
      subset(input$extraPlotBSelect)
    mess <- species_files |>
      filter(type == "mess") |>
      mutate(file = paste0("/vsicurl/", file)) |>
      pull(file) |>
      terra::rast() |>
      subset(input$extraPlotBSelect)
    cl <- rev(c("#FDE725", "#B3DC2B", "#6DCC57", "#36B677", "#1F9D87", "#25818E", "#30678D", "#3D4988", "#462777", "#440154"))
    cl_mess <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#666666")
    par(mfrow = c(2, 1))
    plot(shape, main = "Extrapolation - SHAPE", col = cl)
    plot(mess, main = "Extrapolation - MESS", , col = cl_mess)
  } else {
    plot.new()
  }
}) |>
  bindEvent(input$extraButton, ignoreInit = TRUE)

output$speciesJsonLog <- shiny::renderUI({
  if (input$extraButton != 0 && input$extraButton %% 2 != 0) {
    if (!is.null(db_info$species)) {
      model_log <- db_info$species |>
        select(-available_models) |>
        tidyr::unnest(files) |>
        filter(type == "log") |>
        pull()
      listviewer::jsonedit(list(
                  `Model details` = jsonlite::fromJSON(model_log),
                  `What are the properties` = jsonlite::fromJSON("data/log_explanation.json")
              ), elementId = "json-explorer", width = "100%")
    }
  }
}) |>
  bindEvent(input$extraButton, ignoreInit = TRUE)