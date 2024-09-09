########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
######################### Main reactive for map change #########################

# Create a reactive to hold the files in use
files_inuse <- reactiveValues(file_a = NULL, file_b = NULL)

# Observe changes and update the map accordingly
observe({
  
  # Debugging information
  mdebug("Executing map reactive")
  mdebug(active_tab$current)
  
  # Initialize a proxy for the leaflet map and clear existing layers
  proxy <- leafletProxy("mainMap") %>%
    clearMarkers() %>%
    clearShapes() %>%
    clearImages() %>%
    removeImage(layerId = "mapLayer1") %>%
    removeImage(layerId = "mapLayer2") %>%
    leafpm::removePmToolbar() %>%
    leaflet.extras2::removeSidebyside("sidecontrols")
  
  # Send a custom message to remove an eye icon control
  session$sendCustomMessage("removeEye", "nothing")
  
  # Construct the base path for the map data files
  basepath <- paste0("data/maps/taxonid=", sp_info$spkey, "/model=", sp_info$acro, "/predictions/")
  
  # If the active tab is "species"
  if (active_tab$current == "species") {
    mdebug("Executing species map")
    mdebug(paste("In use species", sp_info$species, sp_info$model, sp_info$scenario, collapse = ","))
    mdebug(paste0(basepath, "taxonid=", sp_info$spkey, "_model=", sp_info$acro, "", "_method=", sp_info$model, "_scen=", sp_info$scenario, "_", sp_info$decade, "_cog.tif"))
    
    # Determine which files to use based on the scenario and side selection
    side_select <- input$sideSelect
    if (sp_info$scenario == "current") {
      file_a <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=", sp_info$acro, "", "_method=", sp_info$model, "_scen=current_cog.tif")
      file_b <- NULL
    } else {
      if (side_select) {
        file_a <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=", sp_info$acro, "", "_method=", sp_info$model, "_scen=current_cog.tif")
        file_b <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=", sp_info$acro, "", "_method=", sp_info$model, "_scen=", sp_info$scenario, "_", sp_info$decade, "_cog.tif")
      } else {
        file_a <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=", sp_info$acro, "", "_method=", sp_info$model, "_scen=", sp_info$scenario, "_", sp_info$decade, "_cog.tif")
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
          addTiles(group = "Open Street", layerId = "leftbaseid", options = pathOptions(pane = "left")) %>%
          addGeotiff(file = file_a, layerId = "mapLayer1", opacity = 1,
                     colorOptions = colorOptions(palette = rev(c("#7d1500", "#da4325", "#eca24e", "#e7e2bc", "#5cc3af", "#0a6265")),
                                                 domain = c(0, 100), na.color = NA),
                     options = pathOptions(pane = "left"), autozoom = F) %>%
          addTiles(group = "Open Street B", layerId = "rightbaseid", options = pathOptions(pane = "right")) %>%
          addGeotiff(file = file_b, opacity = 1, layerId = "mapLayer2",
                     colorOptions = colorOptions(palette = rev(c("#7d1500", "#da4325", "#eca24e", "#e7e2bc", "#5cc3af", "#0a6265")),
                                                 domain = c(0, 100), na.color = NA),
                     options = pathOptions(pane = "right"), autozoom = F) %>%
          addSidebyside(layerId = "sidecontrols", rightId = "rightbaseid", leftId = "leftbaseid")
      } else {
        # Add a single GeoTIFF layer to the map and enable the toolbar for drawing and editing
        proxy %>%
          addGeotiff(file = file_a, opacity = 1, layerId = "mapLayer1",
                     colorOptions = colorOptions(palette = rev(c("#7d1500", "#da4325", "#eca24e", "#e7e2bc", "#5cc3af", "#0a6265")),
                                                 domain = c(0, 100), na.color = NA), autozoom = F) %>%
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
      sel_habitat <- paste0("data/habitats/habitat=", tolower(sp_info$habitat), "_model=mpaeu_scen=",
                            ifelse(sp_info$scenario == "current", "current", paste0(sp_info$scenario, "_", sp_info$decade)), "_cog.tif")
      
      # Disable the mask state
      maskstate(FALSE)
      
      # Add the habitat layer to the map and enable the toolbar for drawing and editing
      proxy %>%
        addGeotiff(file = sel_habitat, opacity = 1,
                   colorOptions = colorOptions(palette = rev(c("#7d1500", "#da4325", "#eca24e", "#e7e2bc", "#5cc3af", "#0a6265")),
                                               na.color = NA), autozoom = F) %>%
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
