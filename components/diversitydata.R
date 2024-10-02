########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
####################### Update map with diversity data ###########################


# For diversity data, we have either a shapefile or a raster file
# We load data according to what is chosen

# We load the data once, only if the user access this part
diversity_f_data <- reactive({
  mdebug("Loading diversity data")
  if (input$diversitySelect != "" & active_tab$current == "diversity") {
    
    # raw_diversity <- sfarrow::st_read_parquet(
    #   paste0("../mpaeu_sdm/results/taxonid=", sp_info$spkey_t, "/model=inteval/predictions/taxonid=",
    #          sp_info$spkey_t, "_model=inteval_what=thermenvelope.parquet")
    # )
    
    # modeled_diversity <- list.files("data/diversity", pattern = "\\.tif")
    modeled_diversity <- raw_diversity <- "data/maps/taxonid=1245747/model=mpaeu/predictions/taxonid=1245747_model=mpaeu_method=ensemble_scen=current_cog.tif"
    files_inuse_habdiv$file_diversity <- modeled_diversity
    
    list(raw = raw_diversity,
         modeled = modeled_diversity)
    
  } else {NULL}
  
}) %>%
  bindEvent(sp_info$metric, ignoreInit = T, once = TRUE)

diversity_data <- reactive({
  mdebug("Selecting diversity data")
  req(!is.null(diversity_f_data()))
  if (sp_info$model_d == "Raw") {
    # TO DO
  } else {
    # TO DO
  }
}) %>%
  bindEvent(sp_info$metric, sp_info$scenario_d, sp_info$decade_d, sp_info$model_d, diversity_f_data(), ignoreInit = T)

observe({
  
  mdebug("Processing diversity update")
  proxy <- leafletProxy("mainMap")
  
  if (sp_info$model_d == "Raw") {
    suppressWarnings({
      proxy <- proxy %>%
        clearMarkers() %>%
        clearShapes() %>%
        leafem::addFeatures(diversity_data(), layerId = "Current", group = "Current")
    })
  } else {
    suppressWarnings({
      proxy <- proxy %>%
        clearMarkers() %>%
        clearShapes() %>%
        addGeotiff(file = diversity_data(), opacity = 1,
                   colorOptions = colorOptions(palette = rev(c("#7d1500", "#da4325", "#eca24e", "#e7e2bc", "#5cc3af", "#0a6265")),
                                               na.color = NA), autozoom = F)
    })
  }
  
}) %>%
  bindEvent(diversity_data(), ignoreInit = F)