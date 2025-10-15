########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
######################### Main reactive for map change #########################

# Start server code ------
# Update map with study area to begin
leafletProxy("mainMap") |>
  leafem::addFeatures(starea, fillColor = "#184e77", fill = T)

# Objects ------
# Palettes
main_palette <- RColorBrewer::brewer.pal(9, "Blues")
alt_palette <- rev(c("#7d1500", "#da4325", "#eca24e", "#e7e2bc", "#5cc3af", "#0a6265"))
hab_palette <- RColorBrewer::brewer.pal("PuRd", n = 9)
diversity_palette <- ""
binary_palette <- c("#f7fbff", "#08519c")

# Create waiters
wMap <- waiter::Waiter$new(
  id = "mainMap",
  color = "#ffffff00",
  html = htmltools::div(
    htmltools::tags$span("Preparing map", bsicons::bs_icon("globe-central-south-asia")),
    style = "color: #8e929a; font-size: 24px; font-weight: 700; white-space: nowrap; display: inline-block; background-color: rgba(255, 255, 255, 0.95); border-radius: 10px; padding: 20px;"
  )
)

wMask <- waiter::Waiter$new(
  id = "mainMap",
  color = "#ffffff00",
  html = htmltools::div(
    htmltools::tags$span("Loading mask", bsicons::bs_icon("eye-fill")),
    style = "color: #8e929a; font-size: 24px; font-weight: 700; white-space: nowrap; display: inline-block; background-color: rgba(255, 255, 255, 0.95); border-radius: 10px; padding: 20px;"
  )
)

wShapes <- waiter::Waiter$new(
  id = "mainMap",
  color = "#ffffff00",
  html = htmltools::div(
    htmltools::tags$span("Loading shape", bsicons::bs_icon("back")),
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
add_layer_sp <- function(proxy, layer_1, layer_2 = NULL,
                         min_range = 0, band_1 = NULL, band_2 = NULL,
                         binary = FALSE) {
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
            palette = main_palette,
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

add_layer_div <- function(proxy, layer_1) {
  session$sendCustomMessage("removeEye", "nothing")
  maskstate(FALSE)

  col_opt <- colorOptions(
    palette = hab_palette, na.color = NA
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
        palette = hab_palette,
        na.color = NA
      ), title = htmltools::HTML("Likelihood </br> of occurrence"), layerId = "legend", values = c(0, 1),
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
  message("ok extract")
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

# Map observer -----
observe({
  # Debugging information
  mdebug("Executing map reactive")
  mdebug(active_tab$current)
  wMap$show()
  on.exit({
    wMap$hide()
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
        extract_sp(
          sc = select_params$species$scenario, pe = select_params$species$decade,
          me = select_params$species$model, ty = file_type
        )
      if (input$sideSelect) {
        layer_1 <- db_info$species |>
          extract_sp(me = select_params$species$model, ty = file_type)
        layer_2 <- db_info$species |>
          extract_sp(
            sc = select_params$species$scenario, pe = select_params$species$decade,
            me = select_params$species$model, ty = file_type
          )
      } else {
        layer_2 <- NULL
      }
      proxy |> add_layer_sp(layer_1, layer_2, min_val)
      files_inuse$file_a <- layer_1
      files_inuse$file_b <- layer_2
    } else {
      proxy |> clean_proxy()
    }
    # Thermal tab
  } else if (active_tab$current == "thermal") {
    if (select_params$thermal$species_t != "") {
      file_type <- "thermenvelope"
      boot$status <- FALSE
      boot$legend <- htmltools::HTML("Thermal range")
      layer_2 <- layer_1 <- db_info$thermal |>
        extract_sp(ty = file_type)
      bands_list <- c(
        "current", paste(
          rep(paste0("ssp", c(126, 245, 370, 460, 585)), each = 2), c("dec50", "dec100"),
          sep = "_"
        )
      )
      # bands_list <- terra::describe(
      #   paste0("/vsicurl/", layer_1)
      # )
      # bands_list <- bands_list[grepl("Description = ", bands_list)]
      # bands_list <- gsub(" ", "", gsub("Description =", "", bands_list))
      if (input$sideSelect) {
        sel_band_1 <- which(bands_list == "current")
        sel_band_2 <- which(bands_list == paste(
          select_params$thermal$scenario_t,
          select_params$thermal$decade_t,
          sep = "_"
        ))
      } else {
        if (select_params$thermal$scenario_t == "current") {
          sel_band_1 <- which(bands_list == "current")
        } else {
          sel_band_1 <- which(bands_list == paste(
            select_params$thermal$scenario_t,
            select_params$thermal$decade_t,
            sep = "_"
          ))
        }
        sel_band_2 <- NULL
        layer_2 <- NULL
      }
      proxy |> add_layer_sp(layer_1, layer_2,
        band_1 = sel_band_1, band_2 = sel_band_2, binary = TRUE
      )
      files_inuse$file_a <- layer_1
      files_inuse$file_b <- layer_2
    } else {
      proxy |> clean_proxy()
    }
    # Habitat tab
  } else if (active_tab$current == "habitat") {
    if (select_params$habitat$habitat != "") {
      layer_1 <- db_info$habitat |>
        extract_hab(
          th = select_params$habitat$threshold_h,
          pt = select_params$habitat$model_h,
          ty = select_params$habitat$bintype_h,
          sc = select_params$habitat$scenario_h,
          pe = select_params$habitat$decade_h
        )

      proxy |> add_layer_hab(layer_1)
      files_inuse_habdiv$file_habitat <- layer_1
    } else {
      proxy |> clean_proxy()
    }
    # Diversity tab
  } else if (active_tab$current == "diversity") {
    if (select_params$diversity$metric != "") {
      layer_1 <- db_info$diversity |>
        extract_div(
          gr = select_params$diversity$group,
          th = select_params$diversity$threshold_d,
          pt = select_params$diversity$posttreat_d,
          ty = select_params$diversity$type_d,
          sc = select_params$diversity$scenario_d,
          pe = select_params$diversity$decade_d,
          me = select_params$diversity$metric
        )

      proxy |> add_layer_div(layer_1)
      files_inuse_habdiv$file_diversity <- layer_1
    } else {
      proxy |> clean_proxy()
    }
  } else if (active_tab$current == "atlas") {
    proxy |> clean_proxy()
  }
}) |>
  bindEvent(
    select_params$species,
    select_params$thermal,
    select_params$habitat,
    select_params$diversity,
    active_tab$current,
    input$ecspBoot,
    ignoreInit = TRUE
  )


# Mask layer ------
# Create a reactive with mask state
maskstate <- reactiveVal(TRUE)

# Observe changes on tab
observe({
  mdebug("Changing mask state based on tab")
  if (active_tab$current == "species" || active_tab$current == "thermal") {
    maskstate(TRUE)
  } else {
    maskstate(FALSE)
  }
}) |> bindEvent(active_tab$current)

# Observe changes via mask command (eye symbol)
observe({
  mdebug("Mask JS")
  maskstate(!maskstate())
}) |> bindEvent(input$jsMask)

# Turn mask on or off based on mask state
observe({
  mdebug("Processing mask")

  wMask$show()
  on.exit({
    wMask$hide()
  })

  proxy <- leafletProxy("mainMap")
  remove_mask <- FALSE
  mask_layer <- NULL

  if (active_tab$current %in% c("species", "thermal")) {
    if (active_tab$current == "species") {
      if (is.null(db_info$species)) {
        remove_mask <- TRUE
      } else {
        mask_layer <- db_info$species |>
          select(-available_models) |>
          tidyr::unnest(files) |>
          filter(type == "mask") |>
          pull()
      }
    } else if (active_tab$current == "thermal") {
      if (is.null(db_info$thermal)) {
        remove_mask <- TRUE
      } else {
        mask_layer <- db_info$thermal |>
          select(-available_models) |>
          tidyr::unnest(files) |>
          filter(type == "mask") |>
          pull()
      }
    }
  } else {
    remove_mask <- TRUE
  }

  if (remove_mask) {
    proxy |>
      removeImage(layerId = "mapMask") |>
      removeControl(layerId = "maskControl")
      session$sendCustomMessage("removeEye", "nothing")
  }
  
  if (!maskstate()) {
    mdebug("Mask deactivated")
    proxy |>
      removeImage(layerId = "mapMask") |>
      removeControl(layerId = "maskControl")
  } else {
    req(!is.null(mask_layer))
    mdebug("Mask activated")
    mask_layer <- gsub("what=what=", "what=", mask_layer) # TEMP - TO REMOVE
    avmasks <- c(
      "native_ecoregions", "fit_ecoregions", "fit_region",
      "fit_region_max_depth", "convex_hull", "minbounding_circle", "buffer100m"
    )
    sel_mask <- input$ecspMask
    which_band <- match(sel_mask, avmasks)
    proxy |>
      addGeotiff(
        file = mask_layer,
        opacity = 1,
        layerId = "mapMask",
        bands = which_band,
        options = pathOptions(pane = "maskPane"),
        colorOptions = colorOptions(
          palette = c("#d4dadc", "#0a626500"),
          domain = c(0, 1),
          na.color = NA
        ), autozoom = F
      ) |>
      addControl(tags$div(HTML('<span style="font-weight: bold; color: #184e77;">Mask active</span>')),
        position = "topright", layerId = "maskControl"
      )
  }
}) |>
  bindEvent(
    maskstate(),
    input$ecspMask,
    input$additionalInfo,
    ignoreInit = TRUE
  )


observe({
  mdebug("Processing realms")
  proxy <- leafletProxy("mainMap")

  if (input$ecspRealms) {
    wShapes$show()
    on.exit({
      wShapes$hide()
    })
    proxy |> leaflet::addPolygons(
      data = realms, color = "#454545", opacity = 0.3,
      popup = ~ as.character(Realm), fillColor = ~ colorQuantile("YlOrRd", Realm)(Realm),
      fillOpacity = 0.05, weight = 2, layerId = paste0("realmsShape", 1:nrow(realms)), options = pathOptions(pane = "extraPane")
    )
  } else {
    proxy |> leaflet::removeShape(layerId = paste0("realmsShape", 1:nrow(realms)))
  }
}) |>
  bindEvent(input$ecspRealms, ignoreInit = TRUE)

observe({
  mdebug("Processing EEZ")
  proxy <- leafletProxy("mainMap")

  if (input$ecspEEZ) {
    wShapes$show()
    on.exit({
      wShapes$hide()
    })
    proxy |> leaflet::addPolygons(
      data = eez, color = "#454545", opacity = 0.3,
      popup = ~EEZ, fillColor = "#0d7edb",
      fillOpacity = 0.05, weight = 2, layerId = paste0("eezShape", 1:nrow(eez)), options = pathOptions(pane = "extraPane")
    )
  } else {
    proxy |> leaflet::removeShape(layerId = paste0("eezShape", 1:nrow(eez)))
  }
}) |>
  bindEvent(input$ecspEEZ, ignoreInit = TRUE)