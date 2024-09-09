########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
####################### Update map with thermal data ###########################


# For thermal data, we have a shapefile. So, what we do is to just update the proxy map
thermal_data <- reactive({
  mdebug("Changing thermal reactive")
  if (input$speciesSelectThermal != "" & active_tab$current == "thermal") {
    thermal_envelope <- sfarrow::st_read_parquet(
      paste0("data/maps/taxonid=", sp_info$spkey_t, "/model=", sp_info$acro_t, "/predictions/taxonid=",
             sp_info$spkey_t, "_model=", sp_info$acro_t, "_what=thermenvelope.parquet")
    )
    
    thermal_envelope_current <- thermal_envelope[1,]
    
    if (tolower(input$scenarioSelectThermal) != "current") {
      nams_thermal_envelope <- c("current", paste0(
        rep(c("ssp126", "ssp245", "ssp370", "ssp460", "ssp585"), each = 2), "_", rep(c("dec50", "dec100"), 5)
      ))
      nams_thermal_envelope <- which(grepl(paste0(sp_info$scenario_t, "_", sp_info$decade_t), 
                                           nams_thermal_envelope))
      thermal_envelope_future <- thermal_envelope[nams_thermal_envelope,]
    } else {
      thermal_envelope_future <- NULL
    }
    
    list(current = thermal_envelope_current,
         future = thermal_envelope_future)
  } else {NULL}
  
}) %>%
  bindEvent(sp_info$spkey_t, sp_info$scenario_t, sp_info$decade_t, active_tab$current, ignoreInit = T)

observe({
  
  mdebug("Processing thermal update")
  proxy <- leafletProxy("mainMap")
  
  suppressWarnings({
    proxy <- proxy %>% 
      clearMarkers() %>%
      clearShapes() %>%
      leafem::addFeatures(thermal_data()$current, layerId = "Current", group = "Current")
  }) 
  
  if (!is.null(thermal_data()$future)) {
    suppressWarnings({
      proxy <- proxy %>% 
        leafem::addFeatures(thermal_data()$future, color = "#ffcc00", layerId = "Future", group = "Future") %>%
        addLayersControl(
          overlayGroups = c("Points", "Current", "Future"),
          baseGroups = c("Open Street Maps", "CartoDB", "CartoDB Dark"),
          options = layersControlOptions(collapsed = T),
          position = "bottomright"
        )
    })
  }
  
  proxy %>%
    addCircleMarkers(data = speciespts(),
                     clusterOptions = NULL,#markerClusterOptions(),
                     group = "Points",
                     weight = 2,
                     radius = 2,
                     opacity = 1,
                     fillOpacity = 0.1,
                     color = "black") %>%
    hideGroup("Points")
  
}) %>%
  bindEvent(thermal_data(), ignoreInit = F)