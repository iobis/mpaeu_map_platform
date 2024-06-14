# For thermal data, we have a shapefile. So, what we do is to just update the proxy map
thermal_data <- reactive({
  mdebug("Changing thermal reactive")
  if (input$speciesSelectThermal != "" & active_tab$current == "thermal") {
    scenario <- tolower(input$scenarioSelectThermal)
    decade <- ifelse(is.null(input$periodSelectThermal), NULL,
                     ifelse(input$periodSelectThermal == 2050, "dec50", "dec100"))
    spkey <- speciesinfo$key[speciesinfo$species == input$speciesSelectThermal]
    
    thermal_envelope <- sfarrow::st_read_parquet(
      paste0("../mpaeu_sdm/results/taxonid=", spkey, "/model=inteval/predictions/taxonid=", spkey, "_model=inteval_what=thermenvelope.parquet")
    )
    
    thermal_envelope_current <- thermal_envelope[1,]
    
    if (tolower(input$scenarioSelectThermal) != "current") {
      nams_thermal_envelope <- c("current", paste0(
        rep(c("ssp126", "ssp245", "ssp370", "ssp460", "ssp585"), each = 2), "_", rep(c("dec50", "dec100"), 5)
      ))
      nams_thermal_envelope <- which(grepl(paste0(scenario, "_", decade), nams_thermal_envelope))
      thermal_envelope_future <- thermal_envelope[nams_thermal_envelope,]
    } else {
      thermal_envelope_future <- NULL
    }
    
    list(current = thermal_envelope_current,
         future = thermal_envelope_future)
  } else {NULL}
  
})

observe({
  
  mdebug("Processing thermal update")
  proxy <- leafletProxy("mainMap")
  
  proxy <- proxy %>% 
    clearMarkers() %>%
    clearShapes() %>%
    leafem::addFeatures(thermal_data()$current, layerId = "Current", group = "Current") 
  
  if (!is.null(thermal_data()$future)) {
    proxy <- proxy %>% 
      leafem::addFeatures(thermal_data()$future, color = "#ffcc00", layerId = "Future", group = "Future") %>%
      addLayersControl(
        overlayGroups = c("Points", "Current", "Future"),
        baseGroups = c("Open Street Maps", "CartoDB", "CartoDB Dark"),
        options = layersControlOptions(collapsed = T),
        position = "bottomright"
      )
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