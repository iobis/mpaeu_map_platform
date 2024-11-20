########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
######################### Main reactive for map change #########################

# Create waiters
wMap <- waiter::Waiter$new(
  id = "mainMap",
  color = "#ffffff00",
  html = htmltools::div(
    htmltools::tags$span("Preparing map", bsicons::bs_icon("globe-central-south-asia")),
    style = "color: #8e929a; font-size: 24px; font-weight: 700; white-space: nowrap; display: inline-block; background-color: rgba(255, 255, 255, 0.95); border-radius: 10px; padding: 20px;"
  )
)

# Temporary solution for bootstrap (while not all available)
check_boot <- function() {
  nounc_mod <- shiny::modalDialog("Uncertainty not available for this species/model",
          title = NULL, footer = modalButton("Dismiss"), size = "s", easyClose = TRUE, fade = TRUE)
  if (input$ecspBoot) {
        bslib::update_switch("ecspBoot", value = FALSE)
        shiny::showModal(nounc_mod)
  }
  return(invisible(NULL))
}

boot <- reactiveValues(status = FALSE, legend = htmltools::HTML("Likelihood </br> of occurrence"))

observe({
  basepath <- ""#paste0("https://mpaeu-dist.s3.amazonaws.com/results/species/taxonid=", sp_info$spkey, "/model=", sp_info$acro, "/predictions/")
  if (active_tab$current == "species" && input$ecspBoot) {
    avf <- s3_list %>%
      filter(taxonID == sp_info$spkey) %>%
      collect()
    if (sp_info$scenario == "current") {
      uncert_file <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=", sp_info$acro, "", "_method=", sp_info$model, "_scen=current_what=bootcv_cog.tif")
      if (input$ecspBoot && any(grepl(uncert_file, avf$Key))) {
        boot$status <- TRUE
        boot$legend <- "Uncertainty"
      } else {
        boot$status <- FALSE
        boot$legend <- htmltools::HTML("Likelihood </br> of occurrence")
        check_boot()
      }
    } else {
      uncert_file <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=", sp_info$acro, "", "_method=", sp_info$model, "_scen=", sp_info$scenario, "_", sp_info$decade, "_what=bootcv_cog.tif")
      uncert_file_a <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=", sp_info$acro, "", "_method=", sp_info$model, "_scen=current_what=bootcv_cog.tif")
      uncert_file_b <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=", sp_info$acro, "", "_method=", sp_info$model, "_scen=", sp_info$scenario, "_", sp_info$decade, "_what=bootcv_cog.tif")
      if (input$sideSelect && input$ecspBoot && any(grepl(uncert_file_a, avf$Key)) && any(grepl(uncert_file_b, avf$Key))) {
        boot$status <- TRUE
        boot$legend <- "Uncertainty"
      } else if (input$ecspBoot && any(grepl(uncert_file, avf$Key))) {
        boot$status <- TRUE
        boot$legend <- "Uncertainty"
      } else {
        boot$status <- FALSE
        boot$legend <- htmltools::HTML("Likelihood </br> of occurrence")
        check_boot()
      }
    }
  } else {
    boot$status <- FALSE
    boot$legend <- htmltools::HTML("Likelihood </br> of occurrence")
  }
}) %>%
  bindEvent(input$ecspBoot)


# Create a reactive to hold the files in use
files_inuse <- reactiveValues(file_a = NULL, file_b = NULL)
files_inuse_habdiv <- reactiveValues(file_habitat = NULL, file_diversity = NULL)

# Observe changes and update the map accordingly
observe({
  
  # Debugging information
  mdebug("Executing map reactive")
  mdebug(active_tab$current)
  wMap$show()
  on.exit({
    wMap$hide() # Try without removing...
    session$sendCustomMessage("additionalInfoTrigger", "")
  })
  
  # Initialize a proxy for the leaflet map and clear existing layers
  proxy <- leafletProxy("mainMap") %>%
    clearMarkers() %>%
    clearShapes() %>%
    clearImages() %>%
    removeControl("legend") %>%
    removeImage(layerId = "mapLayer1") %>%
    removeImage(layerId = "mapLayer2") %>%
    leafpm::removePmToolbar() %>%
    leaflet.extras2::removeSidebyside("sidecontrols")
  
  # Send a custom message to remove an eye icon control
  session$sendCustomMessage("removeEye", "nothing")
  
  # Construct the base path for the map data files
  basepath <- paste0("https://mpaeu-dist.s3.amazonaws.com/results/species/taxonid=", sp_info$spkey, "/model=", sp_info$acro, "/predictions/")
  
  # If the active tab is "species"
  if (active_tab$current == "species") {
    mdebug("Executing species map")
    mdebug(paste("In use species", sp_info$species, sp_info$model, sp_info$scenario, collapse = ","))
    mdebug(paste0(basepath, "taxonid=", sp_info$spkey, "_model=", sp_info$acro, "", "_method=", sp_info$model, "_scen=", sp_info$scenario, "_", sp_info$decade, "_cog.tif"))
    
     # Get threshold
    if (length(sp_info$spkey) > 0 && sp_info$spkey != "") {
      thresholds <- arrow::read_parquet(paste0(
        "https://mpaeu-dist.s3.amazonaws.com/results/species/taxonid=", sp_info$spkey, "/model=", sp_info$acro, "/metrics/taxonid=", 
        sp_info$spkey, "_model=", sp_info$acro, "_what=thresholds.parquet"
     ))
      thresholds <- thresholds[grepl(substr(sp_info$model, 1, 2), thresholds$model),]
      min_range <- switch(input$ecspBin,
        none = 0,
        p10 = round(as.numeric(thresholds$p10) * 100),
        maxsss = round(as.numeric(thresholds$max_spec_sens) * 100),
        mtp = round(as.numeric(thresholds$mtp) * 100)
      )
    }

    # Determine which files to use based on the scenario and side selection
    side_select <- input$sideSelect
    if (sp_info$scenario == "current") {
      uncert_file <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=", sp_info$acro, "", "_method=", sp_info$model, "_scen=current_what=bootcv_cog.tif")
      if (boot$status) {
        file_a <- uncert_file
      } else {
        file_a <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=", sp_info$acro, "", "_method=", sp_info$model, "_scen=current_cog.tif")
      }
      file_b <- NULL
    } else {
      if (side_select) {
        uncert_file_a <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=", sp_info$acro, "", "_method=", sp_info$model, "_scen=current_what=bootcv_cog.tif")
        uncert_file_b <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=", sp_info$acro, "", "_method=", sp_info$model, "_scen=", sp_info$scenario, "_", sp_info$decade, "_what=bootcv_cog.tif")
        if (boot$status) {
          file_a <- uncert_file_a
          file_b <- uncert_file_b
        } else {
          file_a <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=", sp_info$acro, "", "_method=", sp_info$model, "_scen=current_cog.tif")
          file_b <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=", sp_info$acro, "", "_method=", sp_info$model, "_scen=", sp_info$scenario, "_", sp_info$decade, "_cog.tif")
        }
      } else {
        uncert_file <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=", sp_info$acro, "", "_method=", sp_info$model, "_scen=", sp_info$scenario, "_", sp_info$decade, "_what=bootcv_cog.tif")
        if (boot$status) {
          file_a <- uncert_file
        } else {
          file_a <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=", sp_info$acro, "", "_method=", sp_info$model, "_scen=", sp_info$scenario, "_", sp_info$decade, "_cog.tif")
        }
        file_b <- NULL
      }
    }
    
    # Store the file paths in the reactive values
    files_inuse$file_a <- file_a
    files_inuse$file_b <- file_b
    
    # If no species is selected, add a feature to the map
    if (sp_info$species == "") {
      proxy %>%
        leafem::addFeatures(starea, fillColor = "#184e77", fill = T)
    } else {
      # Add circle markers and a button to the map
      proxy <- proxy %>%
        addCircleMarkers(data = speciespts(),
                         clusterOptions = NULL, # markerClusterOptions(),
                         group = "Points",
                         weight = 2,
                         radius = 2,
                         opacity = 1,
                         fillOpacity = 0.1,
                         color = "black") %>% 
        hideGroup("Points") %>%
        addEasyButton(easyButton(
          icon = "fa-eye", title = "Activate/deactivate native mask",
          onClick = JS('
          function(btn, map) {
            var new_state = {id: "newstate", nonce: Math.random()};
            Shiny.onInputChange("jsMask", new_state);
          }
        '))
        )
      
      # Add side-by-side comparison if the scenario is not current and side selection is enabled
      if (sp_info$scenario != "current" & side_select) {
        proxy %>%
          removeTiles("baseid") %>%
          removeLayersControl() %>%
          addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png",
            group = "Open Street", layerId = "leftbaseid", options = pathOptions(pane = "left")) %>%
          addGeotiff(file = file_a, layerId = "mapLayer1", opacity = 1,
                     colorOptions = colorOptions(palette = rev(c("#7d1500", "#da4325", "#eca24e", "#e7e2bc", "#5cc3af", "#0a6265")),
                                                 domain = c(min_range, 100), na.color = NA),
                     options = pathOptions(pane = "left"), autozoom = F) %>%
          addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png",
            group = "Open Street B", layerId = "rightbaseid", options = pathOptions(pane = "right")) %>%
          addGeotiff(file = file_b, opacity = 1, layerId = "mapLayer2",
                     colorOptions = colorOptions(palette = rev(c("#7d1500", "#da4325", "#eca24e", "#e7e2bc", "#5cc3af", "#0a6265")),
                                                 domain = c(min_range, 100), na.color = NA),
                     options = pathOptions(pane = "right"), autozoom = F) %>%
          addSidebyside(layerId = "sidecontrols", rightId = "rightbaseid", leftId = "leftbaseid")
      } else {
        # Add a single GeoTIFF layer to the map and enable the toolbar for drawing and editing
        proxy %>%
          addGeotiff(file = file_a, opacity = 1, layerId = "mapLayer1",
                     colorOptions = colorOptions(palette = rev(c("#7d1500", "#da4325", "#eca24e", "#e7e2bc", "#5cc3af", "#0a6265")),
                                                 domain = c(min_range, 100), na.color = NA), autozoom = F) %>%
          leaflegend::addLegendNumeric(pal = colorNumeric(palette = rev(c("#7d1500", "#da4325", "#eca24e", "#e7e2bc", "#5cc3af", "#0a6265")),
                                                 domain = c(0, 100), na.color = NA),
                  values = c(0, 100), title = boot$legend, layerId = "legend",
                   orientation = 'horizontal', fillOpacity = .7, width = 75,
                   height = 15, position = 'topright', labels = c("Low", "High")) %>%
          addPmToolbar(toolbarOptions = pmToolbarOptions(drawMarker = FALSE,
                                                         drawPolyline = FALSE,
                                                         drawCircle = FALSE,
                                                         cutPolygon = FALSE,
                                                         position = "topleft"),
                       drawOptions = pmDrawOptions(snappable = FALSE, allowSelfIntersection = FALSE),
                       editOptions = pmEditOptions(preventMarkerRemoval = FALSE, draggable = TRUE))
      }
    }
  }
  
  # If the active tab is "thermal"
  if (active_tab$current == "thermal") {
    if (sp_info$species == "") {
      proxy %>%
        leafem::addFeatures(starea, fillColor = "#184e77", fill = T)
    } # With data state handled by thermaldata.R
  }
  
  # If the active tab is "habitat"
  if (active_tab$current == "habitat") {
    if (sp_info$habitat == "") {
      proxy %>%
        leafem::addFeatures(starea, fillColor = "#184e77", fill = T)
    } else {
      # Select the habitat file based on the scenario and decade
      # Example: "habitat=bivalves_beds_model=mpaeu_method=ensemble_scen=current_type=bin_threshold=max_spec_sens_cog.tif"
      sel_habitat <- paste0("https://mpaeu-dist.s3.amazonaws.com/results/habitat/habitat=", tolower(sp_info$habitat), "_model=", sp_info$acro_h,
                            "_method=", sp_info$model_h,
                            "_scen=", ifelse(sp_info$scenario_h == "current",
                                             "current", paste0(sp_info$scenario_h, "_", sp_info$decade_h)),
                            "_type=", sp_info$bintype_h, "_threshold=", sp_info$threshold_h,
                            "_cog.tif")
      mdebug(sel_habitat)
      files_inuse_habdiv$file_habitat <- sel_habitat
      # Disable the mask state
      maskstate(FALSE)

      pts_pal <- colorFactor("Blues", habitatpts()$species)
      
      # Add the habitat layer to the map and enable the toolbar for drawing and editing
      tr <- terra::rast(sel_habitat)
      terra::setMinMax(tr)
      lims <- terra::minmax(tr)[,1]
      proxy %>%
        addGeotiff(file = sel_habitat, opacity = 1, layerId = "mapLayer1",
                   colorOptions = colorOptions(palette = RColorBrewer::brewer.pal("PuRd", n = 9),
                                               na.color = NA), autozoom = F) %>%
        leaflegend::addLegendNumeric(
          pal = colorNumeric(
              domain = lims,
              palette = RColorBrewer::brewer.pal("PuRd", n = 9),
              na.color = NA
          ), title = htmltools::HTML("Likelihood </br> of occurrence"), layerId = "legend", values = lims,
          orientation = "horizontal", fillOpacity = .7, width = 75,
          height = 15, position = "topright", labels = c("Low", "High")
        ) %>%
        addCircleMarkers(data = habitatpts(),
                         clusterOptions = NULL, # markerClusterOptions(),
                         group = "Points",
                         weight = 2,
                         radius = 2,
                         opacity = 1,
                         fillOpacity = 0.1,
                         color = pts_pal(habitatpts()$species), #"black",#~pts_pal(species),
                         popup = ~species) %>% 
        hideGroup("Points") %>%
        addPmToolbar(toolbarOptions = pmToolbarOptions(drawMarker = FALSE,
                                                       drawPolyline = FALSE,
                                                       drawCircle = FALSE,
                                                       cutPolygon = FALSE,
                                                       position = "topleft"),
                     drawOptions = pmDrawOptions(snappable = FALSE, allowSelfIntersection = FALSE),
                     editOptions = pmEditOptions(preventMarkerRemoval = FALSE, draggable = TRUE))

    }
  }
  
  # If the active tab is "diversity"
  if (active_tab$current == "diversity") {
    if (sp_info$metric == "") {
      proxy %>%
        leafem::addFeatures(starea, fillColor = "#184e77", fill = T)
    } # With data state handled by diversitydata.R
  }
})
