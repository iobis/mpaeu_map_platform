########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
######################### Main reactive for map change #########################

# Objects ------
# Palettes
main_palette <- RColorBrewer::brewer.pal(9, "Blues")
alt_palette <- rev(c("#7d1500", "#da4325", "#eca24e", "#e7e2bc", "#5cc3af", "#0a6265"))
diversity_palette <- ""

# Create waiters
wMap <- waiter::Waiter$new(
  id = "mainMap",
  color = "#ffffff00",
  html = htmltools::div(
    htmltools::tags$span("Preparing map", bsicons::bs_icon("globe-central-south-asia")),
    style = "color: #8e929a; font-size: 24px; font-weight: 700; white-space: nowrap; display: inline-block; background-color: rgba(255, 255, 255, 0.95); border-radius: 10px; padding: 20px;"
  )
)

# Create objects to hold info
boot <- reactiveValues(status = FALSE, legend = htmltools::HTML("Likelihood </br> of occurrence"))
files_inuse <- reactiveValues(file_a = NULL, file_b = NULL)
files_inuse_habdiv <- reactiveValues(file_habitat = NULL, file_diversity = NULL)

# Functions ------
# Init proxy object
init_proxy <- function(map_name = "mainMap") {
  leafletProxy(map_name) |>
    clearMarkers() |>
    clearShapes() |>
    clearImages() |>
    removeControl("legend") |>
    removeImage(layerId = "mapLayer1") |>
    removeImage(layerId = "mapLayer2") |>
    leafpm::removePmToolbar() |>
    leaflet.extras2::removeSidebyside("sidecontrols")
}

# Create the proxy with study area
clean_proxy <- function(proxy) {
  proxy |>
    leafem::addFeatures(starea, fillColor = "#184e77", fill = T)
}

# Function to add layers
add_layer_sp <- function(proxy, layer_1, layer_2 = NULL, min_range = 0) {
  session$sendCustomMessage("removeEye", "nothing")
  proxy |>
    addCircleMarkers(
      data = speciespts(),
      clusterOptions = NULL, # markerClusterOptions(),
      group = "Points",
      weight = 2,
      radius = 2,
      opacity = 1,
      fillOpacity = 0.1,
      color = "black"
    ) |>
    hideGroup("Points") |>
    addEasyButton(easyButton(
      icon = "fa-eye", title = "Activate/deactivate native mask",
      onClick = JS('
          function(btn, map) {
            var new_state = {id: "newstate", nonce: Math.random()};
            Shiny.onInputChange("jsMask", new_state);
          }
        ')
    ))

  if (!is.null(layer_2)) {
    proxy |>
      removeTiles("baseid") |>
      removeLayersControl() |>
      addTiles(
        urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png",
        group = "Open Street", layerId = "leftbaseid", options = pathOptions(pane = "left")
      ) |>
      addGeotiff(
        file = layer_1, layerId = "mapLayer1", opacity = 1,
        colorOptions = colorOptions(
          palette = main_palette,
          domain = c(min_range, 100), na.color = NA
        ),
        options = pathOptions(pane = "left"), autozoom = F
      ) |>
      addTiles(
        urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png",
        group = "Open Street B", layerId = "rightbaseid", options = pathOptions(pane = "right")
      ) |>
      addGeotiff(
        file = layer_2, opacity = 1, layerId = "mapLayer2",
        colorOptions = colorOptions(
          palette = main_palette,
          domain = c(min_range, 100), na.color = NA
        ),
        options = pathOptions(pane = "right"), autozoom = F
      ) |>
      addSidebyside(layerId = "sidecontrols", rightId = "rightbaseid", leftId = "leftbaseid")
  } else {
    proxy |>
      addGeotiff(
        file = layer_1, opacity = 1, layerId = "mapLayer1",
        colorOptions = colorOptions(
          palette = main_palette,
          domain = c(min_range, 100), na.color = NA
        ), autozoom = F
      ) |>
      leaflegend::addLegendNumeric(
        pal = colorNumeric(
          palette = main_palette,
          domain = c(0, 100), na.color = NA
        ),
        values = c(0, 100), title = boot$legend, layerId = "legend",
        orientation = "horizontal", fillOpacity = .7, width = 75,
        height = 15, position = "topright", labels = c("Low", "High")
      ) |>
      addPmToolbar(
        toolbarOptions = pmToolbarOptions(
          drawMarker = FALSE,
          drawPolyline = FALSE,
          drawCircle = FALSE,
          cutPolygon = FALSE,
          position = "topleft"
        ),
        drawOptions = pmDrawOptions(snappable = FALSE, allowSelfIntersection = FALSE),
        editOptions = pmEditOptions(preventMarkerRemoval = FALSE, draggable = TRUE)
      )
  }
}

# Function to pull info from the DBs
extract_sp <- function(.data, ty = "prediction", sc = "current", me = NULL, pe = NULL) {
  sld <- .data |>
    tidyr::unnest("files") |>
    filter(type == ty, scenario == sc, method == me)
  if (!is.null(pe) & sc != "current") {
    sld <- sld |> filter(period == pe)
  }
  return(sld |> pull(file))
}

# Function to get threshold
pull_thresh <- function(th_table, model, th) {
  thresholds <- th_table[grepl(substr(model, 1, 2), th_table$model),]
  switch(th,
    none = 0,
    p10 = round(as.numeric(thresholds$p10) * 100),
    maxsss = round(as.numeric(thresholds$max_spec_sens) * 100),
    mtp = round(as.numeric(thresholds$mtp) * 100)
  )
}

# Map observer -----
observe({
  # Debugging information
  mdebug("Executing map reactive")
  mdebug(active_tab$current)
  wMap$show()
  on.exit({
    wMap$hide() # Try without removing...
    session$sendCustomMessage("additionalInfoTrigger", "")
  })

  proxy <- init_proxy()

  # Species tab
  if (active_tab$current == "species") {
    if (select_params$species$species != "") {
      if (input$ecspBoot) {
        file_type <- "uncertainty"
        boot$status <- TRUE
        boot$legend <- htmltools::HTML("Uncertainty")
      } else {
        file_type <- "prediction"
        boot$status <- FALSE
        boot$legend <- htmltools::HTML("Likelihood </br> of occurrence")
      }

      min_val <- threshold_table() |>
        pull_thresh(select_params$species$model, input$ecspBin)

      layer_1 <- db_info$species |> 
        extract_sp(sc = select_params$species$scenario, pe = select_params$species$decade,
                   me = select_params$species$model, ty = file_type)
      if (input$sideSelect) {
        layer_1 <- db_info$species |> 
          extract_sp(me = select_params$species$model, ty = file_type)
        layer_2 <- db_info$species |> 
          extract_sp(sc = select_params$species$scenario, pe = select_params$species$decade,
                   me = select_params$species$model, ty = file_type)
      } else {
        layer_2 <- NULL
      }
      proxy |> add_layer_sp(layer_1, layer_2, min_val)
      files_inuse$file_a <- layer_1
      files_inuse$file_b <- layer_2
    } else {
      proxy |> clean_proxy()
    }
  }

  # Thermal tab

  # Habitat tab

  # Diversity tab

})