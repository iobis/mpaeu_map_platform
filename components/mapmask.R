maskstate <- reactiveVal(TRUE)
observe({
  mdebug("Mask JS")
  maskstate(!maskstate())
}) %>% bindEvent(input$jsMask)

# Turn mask on or off
observe({
  req(input$speciesSelect)
  mdebug("Processing mask")
  proxy <- leafletProxy("mainMap")
  spkey <- speciesinfo$key[speciesinfo$species == input$speciesSelect]
  mask_layer <- paste0("data/maps/taxonid=", spkey, "/model=inteval/predictions/taxonid=", spkey, "_model=inteval_mask_cog.tif")
  
  if (!maskstate()) {
    mdebug("Mask deactivated")
    proxy %>% removeImage(layerId = "mapMask") %>%
      removeControl(layerId = "maskControl")
  } else {
    mdebug("Mask activated")
    proxy %>% 
      addMapPane("maskPane", zIndex = 500) %>%
      addGeotiff(file = mask_layer,
                 opacity = 1,
                 #resolution = 50,
                 layerId = "mapMask",
                 bands = 1,
                 options = pathOptions(pane = "maskPane"),
                 colorOptions = colorOptions(
                   palette = c("#aad3df", "#0a626500"),
                   domain = c(0,1),
                   na.color = NA
                 ), autozoom = F) %>%
      addControl(tags$div(HTML('<span style="font-weight: bold; color: #184e77;">Mask active</span>')),
                 position = "topright", layerId = "maskControl")
  }
  
}) %>%
  bindEvent(maskstate(), input$speciesSelect, input$scenarioSelect, input$modelSelect, input$periodSelect, input$sideSelect)