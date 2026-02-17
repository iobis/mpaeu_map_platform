# Functions for mapreactive.R -------
# Create state control object
previous_state <- reactiveValues(
  image_1 = FALSE,
  image_2 = FALSE,
  pm_toolbar = FALSE,
  markers = FALSE,
  study_area = TRUE,
  side_by_side = FALSE,
  atlas_vals = NULL
)

pstate_reset <- function() {
  previous_state$image_1 <- FALSE
  previous_state$image_2 <- FALSE
  previous_state$pm_toolbar <- FALSE
  previous_state$markers <- FALSE
  previous_state$study_area <- TRUE
  previous_state$side_by_side <- FALSE
  previous_state$atlas_vals <- NULL
  return(invisible())
}

pstate_side <- function() {
  previous_state$image_1 <- TRUE
  previous_state$image_2 <- TRUE
  previous_state$pm_toolbar <- TRUE
  previous_state$markers <- TRUE
  previous_state$study_area <- FALSE
  previous_state$side_by_side <- TRUE
  previous_state$atlas_vals <- NULL
  return(invisible())
}

pstate_single <- function() {
  previous_state$image_1 <- TRUE
  previous_state$image_2 <- FALSE
  previous_state$pm_toolbar <- TRUE
  previous_state$markers <- TRUE
  previous_state$study_area <- FALSE
  previous_state$side_by_side <- FALSE
  previous_state$atlas_vals <- NULL
  return(invisible())
}

pstate_atlas <- function(atlas_vals) {
  previous_state$image_1 <- FALSE
  previous_state$image_2 <- FALSE
  previous_state$pm_toolbar <- FALSE
  previous_state$markers <- FALSE
  previous_state$study_area <- FALSE
  previous_state$side_by_side <- FALSE
  previous_state$atlas_vals <- atlas_vals
  return(invisible())
}

state_debug <- function(debug = TRUE) {
  if (debug) {
    act <- function(x) if (x) "\033[46mactive\033[49m" else "\033[45minactive\033[49m"
    message(
      glue::glue(
        "image_1 is {act(previous_state$image_1)} / image_2 is {act(previous_state$image_2)} / pm_toolbar is {act(previous_state$pm_toolbar)}",
        "/ markers is {act(previous_state$markers)} / study_area is {act(previous_state$study_area)} / side_by_side is {act(previous_state$side_by_side)}"
      )
    )
  }
  return(invisible())
}

# Init proxy object
init_proxy <- function(image_1, image_2, pm_toolbar, markers, study_area, side_by_side, atlas_vals, map_name = "mainMap") {
  mdebug("Cleaning proxy")
  leafletProxy(map_name) |>
    (\(x) if (markers) x |> leaflet::clearGroup("Records") |> removeLayersControl() else x)() |>
    (\(x) if (!is.null(atlas_vals[1])) {
      for (idx in seq_along(atlas_vals)) {
        x <- x |> leaflet::clearGroup(atlas_vals[idx])
      }
      x |> removeLayersControl()
    } else {
      x
    })() |>
    (\(x) if (study_area) x |>  clearShapes() else x)() |>
    #clearImages() |>
    removeControl("legend") |>
    (\(x) if (image_1) x |> leaflet::clearGroup("geoLayers") else x)() |>
    #(\(x) if (image_1) x |> removeImage(layerId = "mapLayer1") else x)() |>
    #(\(x) if (image_2) x |> removeImage(layerId = "mapLayer2") else x)() |>
    (\(x) if (pm_toolbar) x |> leafpm::removePmToolbar() else x)() |>
    (\(x) if (side_by_side) x |> leaflet.extras2::removeSidebyside("sidecontrols") else x)()
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
    # addCircleMarkers(
    #   data = speciespts(),
    #   clusterOptions = markerClusterOptions(maxClusterRadius = 5),
    #   clusterId = "points_cluster",
    #   group = "Records",
    #   weight = 2,
    #   radius = 2,
    #   opacity = 1,
    #   fillOpacity = 0.1,
    #   color = "black"
    # ) |>
    (\(x) {
      sf_pt <- sf::st_as_sf(speciespts(), coords = c(1,2), crs = "EPSG:4326")
      leafgl::addGlPoints(x, data = sf_pt, group = "Records", weight = 2, radius = 8, opacity = 1, fillOpacity = 0.1, fillColor = "black", pane = "pointsPane") 
    })()|>
    addLayersControl(
      overlayGroups = c("Records"),
      #baseGroups = c("Open Street Maps"),
      options = layersControlOptions(collapsed = FALSE),
      position = "bottomright"
    ) |>
    #hideGroup("Records") |>
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
        colorOptions = col_opt, group = "geoLayers",
        options = pathOptions(pane = "left"), autozoom = F, bands = band_1, imagequery = FALSE
      ) |>
      addTiles(
        urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png",
        group = "Open Street B", layerId = "rightbaseid", options = pathOptions(pane = "right")
      ) |>
      addGeotiff(
        file = layer_2, opacity = 1, layerId = "mapLayer2",
        colorOptions = col_opt, group = "geoLayers",
        options = pathOptions(pane = "right"), autozoom = F, bands = band_2, imagequery = FALSE
      ) |>
      addSidebyside(layerId = "sidecontrols", rightId = "rightbaseid", leftId = "leftbaseid")
  } else {
    proxy |>
      addGeotiff(
        file = layer_1, opacity = 1, layerId = "mapLayer1", group = "geoLayers",
        colorOptions = col_opt, autozoom = F, bands = band_1, imagequery = FALSE
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

add_layer_hab <- function(proxy, layer_1, min_range = 0, max_range = 1) {
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
      file = layer_1, opacity = 1, layerId = "mapLayer1", group = "geoLayers",
      colorOptions = col_opt, autozoom = F, imagequery = FALSE
    ) |>
    leaflegend::addLegendNumeric(
      pal = colorNumeric(
        domain = c(0, 1),
        palette = hab_palette,
        na.color = NA
      ), title = htmltools::HTML("<span style='font-size: smaller;'>Sum of </br>species' likelihood </br>of occurrence</span>"), layerId = "legend", values = c(0, 1),
      orientation = "horizontal", fillOpacity = .7, width = 75, 
      labels = round(c(min_range, max_range), 0),
      height = 15, position = "topright"
    ) |>
    # addCircleMarkers(
    #   data = habitatpts(),
    #   clusterOptions = markerClusterOptions(maxClusterRadius = 5),
    #   clusterId = "points_cluster",
    #   group = "Records",
    #   weight = 2,
    #   radius = 2,
    #   opacity = 1,
    #   fillOpacity = 0.1,
    #   color = pts_pal(habitatpts()$species), # "black",#~pts_pal(species),
    #   popup = ~species
    # ) |>
    (\(x) {
      sf_pt <- sf::st_as_sf(habitatpts(), coords = c(1,2), crs = "EPSG:4326")
      leafgl::addGlPoints(x, data = sf_pt, group = "Records", weight = 2, radius = 8, opacity = 1, fillOpacity = 0.1, fillColor = pts_pal(habitatpts()$species), popup = habitatpts()$species, pane = "pointsPane") 
    })()|>
    addLayersControl(
      overlayGroups = c("Records"),
      #baseGroups = c("Open Street Maps"),
      options = layersControlOptions(collapsed = FALSE),
      position = "bottomright"
    ) |>
    #hideGroup("Records") |>
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

add_layer_div <- function(proxy, layer_1, legend, min_range = 0, max_range = 1) {
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
      file = layer_1, opacity = 1, layerId = "mapLayer1", group = "geoLayers",
      colorOptions = col_opt, autozoom = F, imagequery = FALSE
    ) |>
    leaflegend::addLegendNumeric(
      pal = colorNumeric(
        domain = c(0, 1),
        palette = div_palette,
        na.color = NA
      ), title = htmltools::HTML(legend), layerId = "legend", values = c(0, 1),
      orientation = "horizontal", fillOpacity = .7, width = 75,
      height = 15, position = "topright", labels = round(c(min_range, max_range), 0)
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

add_atlas_layer <- function(proxy, file, alpha, palette, group) {
  
  if (is.null(palette) || is.na(palette)) palette <- "Blues"
  if (is.null(alpha) || is.na(alpha)) alpha <- 1

  message(glue::glue(
    "File: {file} | Alpha: {alpha} | Palette: {palette}"
  ))

  sel_palette <- RColorBrewer::brewer.pal(palette, n = 9)
  
  col_opt <- colorOptions(
    palette = sel_palette, na.color = NA
  )

  if (grepl("\\.tif", file)) {
    proxy <- proxy |>
      addGeotiff(
        file = file, opacity = alpha, layerId = paste0("layer_atlas_", group),
        group = group,
        colorOptions = col_opt, autozoom = F, imagequery = FALSE
      )
  } else {
    if (grepl("points", file)) {
      proxy <- proxy |>
        (\(x) {
          sf_pt <- sf::st_read(file)
          content <- sf::st_drop_geometry(sf_pt[,1])[,1]
          pts_pal <- colorFactor(palette, content)
          leafgl::addGlPoints(x, data = sf_pt, group = group, weight = 2,
            radius = 8, opacity = alpha, fillOpacity = 0.1,
            fillColor = pts_pal(content),
            popup = content, pane = "pointsPane") 
      })()
    } else {
      proxy <- proxy |>
        (\(x) {
          sf_pt <- sf::st_read(file) #|> sf::st_cast("POLYGON")
          content <- sf::st_drop_geometry(sf_pt[,1])[,1]
          pts_pal <- colorFactor(palette, levels = unique(content))
          leafem::addFeatures(x, data = sf_pt, group = group,
            color = NA,
            fillOpacity = alpha,
            fillColor = pts_pal(content),
            popup = content, pane = "extraPane") 
      })()
    }
  }
    # |>
    # leaflegend::addLegendNumeric(
    #   pal = colorNumeric(
    #     domain = c(0, 1),
    #     palette = div_palette,
    #     na.color = NA
    #   ), title = htmltools::HTML(group), layerId = "legend", values = c(0, 1),
    #   orientation = "horizontal", fillOpacity = .7, width = 75,
    #   height = 15, position = "topright", labels = c("Low", "High")
    # )
  
  return(proxy)
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
  return(data.frame(
    layer = sld |> pull(file) |> (\(x) paste0("/vsicurl/", x))(),
    min_range = sld |> pull(range_min), max_range = sld |> pull(range_max)
  ))
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
  return(data.frame(
    layer = sld |> pull(file) |> (\(x) paste0("/vsicurl/", x))(),
    min_range = sld |> pull(range_min), max_range = sld |> pull(range_max)
  ))
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