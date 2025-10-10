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
            rep(paste0("ssp", c(126, 245, 370, 460, 585)), each = 2), c("dec50", "dec100"), sep = "_"
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
            select_params$thermal$decade_t, sep = "_"
          ))
        } else {
          if (select_params$thermal$scenario_t == "current") {
            sel_band_1 <- which(bands_list == "current")
          } else {
            sel_band_1 <- which(bands_list == paste(
              select_params$thermal$scenario_t,
              select_params$thermal$decade_t, sep = "_"
            ))
          }
          sel_band_2 <- NULL
          layer_2 <- NULL
        }
        proxy |> add_layer_sp(layer_1, layer_2,
                              band_1 = sel_band_1, band_2 = sel_band_2, binary = TRUE)
        files_inuse$file_a <- layer_1
        files_inuse$file_b <- layer_2
      } else {
        proxy |> clean_proxy()
      }
    # Habitat tab
  } else if (active_tab$current == "habitat") {

    # Diversity tab
  } else if (active_tab$current == "diversity") {

  } else if (active_tab$current == "atlas") {

  }
}) |>
  bindEvent(
    select_params$species,
    active_tab$current,
    input$ecspBoot,
    ignoreInit = TRUE
  )