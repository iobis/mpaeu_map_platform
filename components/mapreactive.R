# Map reactive
files_inuse <- reactiveValues(file_a = NULL,
                              file_b = NULL)
model_inuse <- reactiveValues(model = NULL)

speciesmap <- reactive({
  
  mdebug("Executing map")
  mdebug(active_tab$current)
  
  sp_info <- list(
    species = "",
    model = "",
    scenario = "",
    decade = "",
    spkey = ""
  )
  
  if (active_tab$current == "species") {
    sp_info$species <- input$speciesSelect
    sp_info$model <- input$modelSelect
    sp_info$scenario <- tolower(input$scenarioSelect)
    sp_info$decade <- ifelse(is.null(input$periodSelect), NULL,
                             ifelse(input$periodSelect == 2050, "dec50", "dec100"))
    sp_info$spkey <- speciesinfo$key[speciesinfo$species == input$speciesSelect]
  }
  if (active_tab$current == "thermal") {
    sp_info$species <- input$speciesSelectThermal
    sp_info$scenario <- tolower(input$scenarioSelectThermal)
    sp_info$decade <- ifelse(is.null(input$periodSelectThermal), NULL,
                             ifelse(input$periodSelectThermal == 2050, "dec50", "dec100"))
    sp_info$spkey <- speciesinfo$key[speciesinfo$species == input$speciesSelectThermal]
  }
  if (active_tab$current == "habitat") {
    sp_info$habitat <- input$habitatSelect
    sp_info$scenario <- tolower(input$scenarioSelectHabitat)
    sp_info$decade <- ifelse(is.null(input$periodSelectHabitat), NULL,
                             ifelse(input$periodSelectHabitat == 2050, "dec50", "dec100"))
  }
  
  basepath <- paste0("data/maps/taxonid=", sp_info$spkey,
                     "/model=inteval/predictions/")
  
  if (active_tab$current == "species") {
    mdebug("Executing species map")
    if (sp_info$species != "") {
      logf <- jsonlite::read_json(paste0("data/maps/taxonid=", sp_info$spkey,
                                         "/model=inteval/taxonid=", sp_info$spkey, "_model=inteval_what=log.json"))
      mod_names <- names(logf$model_posteval)[unlist(lapply(logf$model_posteval,
                                                            function(x) if (length(x) > 0) TRUE else FALSE))]
      available_models <- mod_names[!grepl("niche", mod_names)]
      available_models <- gsub("maxent", "maxnet", available_models)
      available_models <- gsub("rf", "rf_classification_ds", available_models)
      
      if (any(grepl(substr(sp_info$model,1,3), available_models))) {
        model_inuse$model <- sp_info$model
      } else {
        priority <- c("ensemble", "maxnet", "rf_classification_ds", "xgboost", "glm")
        model_inuse$model <- sp_info$model <- priority[priority %in% available_models][1]
      }
    }
    
    mdebug(paste("In use species", sp_info$species, sp_info$model,
                 sp_info$scenario, collapse = ","))
    mdebug(paste0(basepath, "taxonid=", sp_info$spkey, "_model=inteval", "_method=",
                  sp_info$model, "_scen=", sp_info$scenario, "_", sp_info$decade, "_cog.tif"))
    
    side_select <- input$sideSelect
    
    if (sp_info$scenario == "current") {
      file_a <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=inteval", 
                       "_method=", sp_info$model, "_scen=current_cog.tif")
      file_b <- NULL
    } else {
      if (side_select) {
        file_a <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=inteval", 
                         "_method=", sp_info$model, "_scen=current_cog.tif")
        file_b <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=inteval", 
                         "_method=", sp_info$model, "_scen=", sp_info$scenario, 
                         "_", sp_info$decade, "_cog.tif")
      } else {
        file_a <- paste0(basepath, "taxonid=", sp_info$spkey, "_model=inteval",
                         "_method=", sp_info$model, "_scen=", sp_info$scenario,
                         "_", sp_info$decade, "_cog.tif")
        file_b <- NULL
      }
    }
    
    files_inuse$file_a <- file_a
    files_inuse$file_b <- file_b
    
    if (sp_info$species == "") {
      m <- m %>%
        leafem::addFgb(file = "data/studyarea.fgb", fillColor = "#184e77", fill = T)
    } else {
      m <- m %>%
        addCircleMarkers(data = speciespts(),
                         clusterOptions = NULL,#markerClusterOptions(),
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
        ) %>%
        htmlwidgets::onRender('
                            LeafletWidget.methods.removeImage = function(layerId) {
                              this.layerManager.removeLayer(null, layerId);
                            }
                            ')
      
      if (sp_info$scenario != "current" & side_select) {
        
        m <- m %>%
          addMapPane("left", zIndex = 0) %>%
          addMapPane("right", zIndex = 0) %>%
          removeTiles("baseid") %>%
          removeLayersControl() %>%
          addTiles(group = "Open Street", layerId = "leftbaseid",
                   options = pathOptions(pane = "left")) %>%
          addGeotiff(file = file_a,
                     opacity = 1,
                     colorOptions = colorOptions(
                       palette = rev(c("#7d1500", "#da4325", "#eca24e", "#e7e2bc", "#5cc3af", "#0a6265")),
                       domain = c(0, 100),
                       na.color = NA
                     ),
                     options = pathOptions(pane = "left"), autozoom = F) %>%
          addTiles(group = "Open Street B", layerId = "rightbaseid",
                   options = pathOptions(pane = "right")) %>%
          addGeotiff(file = file_b,
                     opacity = 1,
                     colorOptions = colorOptions(
                       palette = rev(c("#7d1500", "#da4325", "#eca24e", "#e7e2bc", "#5cc3af", "#0a6265")),
                       domain = c(0, 100),
                       na.color = NA
                     ),
                     options = pathOptions(pane = "right"), autozoom = F) %>%
          addSidebyside(layerId = "sidecontrols",
                        rightId = "rightbaseid",
                        leftId = "leftbaseid")
        
      } else {
        
        m <- m %>%
          addGeotiff(file = file_a,
                     opacity = 1,
                     colorOptions = colorOptions(
                       palette = rev(c("#7d1500", "#da4325", "#eca24e", "#e7e2bc", "#5cc3af", "#0a6265")),
                       domain = c(0, 100),
                       na.color = NA
                     ), autozoom = F) %>%
          addPmToolbar(
            toolbarOptions = pmToolbarOptions(drawMarker = FALSE,
                                              drawPolyline = FALSE,
                                              drawCircle = FALSE,
                                              cutPolygon = FALSE,
                                              position = "topleft"),
            drawOptions = pmDrawOptions(snappable = FALSE, allowSelfIntersection = FALSE),
            editOptions = pmEditOptions(preventMarkerRemoval = FALSE, draggable = TRUE)
          )
      }
    }
  }
  if (active_tab$current == "thermal") {
    
    if (sp_info$species == "") {
      m <- m %>%
        leafem::addFgb(file = "data/studyarea.fgb", fillColor = "#184e77", fill = T)
    }
    
  }
  if (active_tab$current == "habitat") {
    if (sp_info$habitat == "") {
      m <- m %>%
        leafem::addFgb(file = "data/studyarea.fgb", fillColor = "#184e77", fill = T)
    } else {
      sel_habitat <- paste0("data/habitats/habitat=", tolower(sp_info$habitat), "_model=inteval_scen=", 
                            ifelse(sp_info$scenario == "current", "current",
                                   paste0(sp_info$scenario, "_", sp_info$decade)),
                            "_cog.tif")
      
      maskstate(FALSE)
      
      m <- m %>%
        addGeotiff(file = sel_habitat,
                   opacity = 1,
                   colorOptions = colorOptions(
                     palette = rev(c("#7d1500", "#da4325", "#eca24e", "#e7e2bc", "#5cc3af", "#0a6265")),
                     #domain = c(0, 100),
                     na.color = NA
                   ), autozoom = F) %>%
        addPmToolbar(
          toolbarOptions = pmToolbarOptions(drawMarker = FALSE,
                                            drawPolyline = FALSE,
                                            drawCircle = FALSE,
                                            cutPolygon = FALSE,
                                            position = "topleft"),
          drawOptions = pmDrawOptions(snappable = FALSE, allowSelfIntersection = FALSE),
          editOptions = pmEditOptions(preventMarkerRemoval = FALSE, draggable = TRUE)
        )
      
    }
  }
  
  # Return object
  m
  
})