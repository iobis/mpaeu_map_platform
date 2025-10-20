# Functions for mapreactive.R -------
# Init proxy object
init_proxy <- function(map_name = "mainMap") {
  mdebug("Cleaning proxy")
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
add_layer_sp <- function(proxy, layer_1, layer_2 = NULL,
                         min_range = 0, band_1 = NULL, band_2 = NULL,
                         binary = FALSE, uncertainty = FALSE) {
  session$sendCustomMessage("removeEye", "nothing")

  if (!binary) {
    col_opt <- colorOptions(
      palette = main_palette,
      domain = c(min_range, 100), na.color = NA
    )
  } else {
    col_opt <- colorOptions(
      palette = binary_palette,
      domain = c(min_range, 1), na.color = NA
    )
  }

  if (uncertainty) {
    col_opt <- colorOptions(
      palette = uncertainty_palette, na.color = NA
    )
  }

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
        colorOptions = col_opt,
        options = pathOptions(pane = "left"), autozoom = F, bands = band_1
      ) |>
      addTiles(
        urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png",
        group = "Open Street B", layerId = "rightbaseid", options = pathOptions(pane = "right")
      ) |>
      addGeotiff(
        file = layer_2, opacity = 1, layerId = "mapLayer2",
        colorOptions = col_opt,
        options = pathOptions(pane = "right"), autozoom = F, bands = band_2
      ) |>
      addSidebyside(layerId = "sidecontrols", rightId = "rightbaseid", leftId = "leftbaseid")
  } else {
    proxy |>
      addGeotiff(
        file = layer_1, opacity = 1, layerId = "mapLayer1",
        colorOptions = col_opt, autozoom = F, bands = band_1
      )

    if (!binary) {
      proxy |>
        leaflegend::addLegendNumeric(
          pal = colorNumeric(
            palette = if (uncertainty) uncertainty_palette else main_palette,
            domain = c(0, 100), na.color = NA
          ),
          values = c(0, 100), title = boot$legend, layerId = "legend",
          orientation = "horizontal", fillOpacity = .7, width = 75,
          height = 15, position = "topright", labels = c("Low", "High")
        )
    } else {
      proxy |>
        leaflegend::addLegendFactor(
          pal = colorFactor(
            palette = binary_palette[2],
            domain = NULL
          ),
          values = c("Suitable"), title = boot$legend, layerId = "legend",
          orientation = "horizontal", fillOpacity = .7, width = 40,
          height = 15, position = "topright"
        )
    }

    proxy |>
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

add_layer_hab <- function(proxy, layer_1) {
  session$sendCustomMessage("removeEye", "nothing")
  maskstate(FALSE)

  col_opt <- colorOptions(
    palette = hab_palette, na.color = NA
  )

  pts_pal <- colorFactor("Blues", habitatpts()$species)

  # tr <- terra::rast(paste0("/vsicurl/", sel_habitat))
  # terra::setMinMax(tr)
  # lims <- terra::minmax(tr)[,1]
  proxy |>
    addGeotiff(
      file = layer_1, opacity = 1, layerId = "mapLayer1",
      colorOptions = col_opt, autozoom = F
    ) |>
    leaflegend::addLegendNumeric(
      pal = colorNumeric(
        domain = c(0, 1),
        palette = hab_palette,
        na.color = NA
      ), title = htmltools::HTML("Likelihood </br> of occurrence"), layerId = "legend", values = c(0, 1),
      orientation = "horizontal", fillOpacity = .7, width = 75,
      height = 15, position = "topright", labels = c("Low", "High")
    ) |>
    addCircleMarkers(
      data = habitatpts(),
      clusterOptions = NULL, # markerClusterOptions(),
      group = "Points",
      weight = 2,
      radius = 2,
      opacity = 1,
      fillOpacity = 0.1,
      color = pts_pal(habitatpts()$species), # "black",#~pts_pal(species),
      popup = ~species
    ) |>
    hideGroup("Points") |>
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

add_layer_div <- function(proxy, layer_1, legend) {
  session$sendCustomMessage("removeEye", "nothing")
  maskstate(FALSE)

  col_opt <- colorOptions(
    palette = div_palette, na.color = NA
  )

  # tr <- terra::rast(paste0("/vsicurl/", sel_habitat))
  # terra::setMinMax(tr)
  # lims <- terra::minmax(tr)[,1]
  proxy |>
    addGeotiff(
      file = layer_1, opacity = 1, layerId = "mapLayer1",
      colorOptions = col_opt, autozoom = F
    ) |>
    leaflegend::addLegendNumeric(
      pal = colorNumeric(
        domain = c(0, 1),
        palette = div_palette,
        na.color = NA
      ), title = htmltools::HTML(legend), layerId = "legend", values = c(0, 1),
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


# Function to pull info from the DBs
extract_sp <- function(.data, ty = "prediction", sc = "current", me = NULL, pe = NULL) {
  if (ty %in% c("prediction", "uncertainty")) {
    sld <- .data |>
      tidyr::unnest("files") |>
      filter(type == ty, scenario == sc, method == me)
  } else {
    sld <- .data |>
      tidyr::unnest("files") |>
      filter(type == ty)
  }
  if (!is.null(pe) & sc != "current") {
    sld <- sld |> filter(period == pe)
  }
  return(sld |> pull(file) |> (\(x) paste0("/vsicurl/", x))())
}

extract_hab <- function(.data, th = "p10", pt = "std", ty = "continuous", sc = "current", pe = NULL) {
  sld <- .data |>
    tidyr::unnest("files") |>
    filter(threshold == th, post_treatment == pt, type == ty, scenario == sc)
  if (!is.null(pe) & sc != "current") {
    sld <- sld |> filter(period == pe)
  }
  return(sld |> pull(file) |> (\(x) paste0("/vsicurl/", x))())
}

extract_div <- function(.data, gr = "all", th = "p10", pt = "std", ty = "continuous", sc = "current", pe = NULL, me = "richness") {
  if (me != "richness") {
    ty <- ""
  }
  if (ty == "raw") {
    sld <- .data |>
      tidyr::unnest("files") |>
      filter(group == gr, type == ty)
  } else {
    sld <- .data |>
      tidyr::unnest("files") |>
      filter(group == gr, threshold == th, post_treatment == pt, type == ty, scenario == sc)
  }
  if (!is.null(pe) & sc != "current" & ty != "raw") {
    sld <- sld |> filter(period == pe)
  }
  return(sld |> pull(file) |> (\(x) paste0("/vsicurl/", x))())
}

# Function to get threshold
pull_thresh <- function(th_table, model, th) {
  thresholds <- th_table[grepl(substr(model, 1, 2), th_table$model), ]
  switch(th,
    none = 0,
    p10 = round(as.numeric(thresholds$p10) * 100),
    maxsss = round(as.numeric(thresholds$max_spec_sens) * 100),
    mtp = round(as.numeric(thresholds$mtp) * 100)
  )
}