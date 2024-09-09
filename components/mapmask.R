########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
########################### Control mask over map ##############################

# Create a reactive with mask state ----
maskstate <- reactiveVal(TRUE)

# Observe changes on tab ----
observe({
  mdebug("Changing mask state based on tab")
  if (active_tab$current == "species") {
    maskstate(TRUE)
  } else {
    maskstate(FALSE)
  }
}) %>% bindEvent(active_tab$current)

# Observe changes via mask command (eye symbol) ----
observe({
  mdebug("Mask JS")
  maskstate(!maskstate())
}) %>% bindEvent(input$jsMask)

# Turn mask on or off based on mask state ----
observe({
  req(input$speciesSelect)
  mdebug("Processing mask")
  proxy <- leafletProxy("mainMap")
  spkey <- speciesinfo$key[speciesinfo$species == input$speciesSelect]
  mask_layer <- paste0("data/maps/taxonid=", spkey, "/model=", sp_info$acro, "/predictions/taxonid=", spkey, "_model=", sp_info$acro, "_mask_cog.tif")
  avmasks <- c("native_ecoregions", "fit_ecoregions", "fit_region", "convex_hull", "minbounding_circle", "buffer100m")
  sel_mask <- input$ecspMask
  which_band <- match(sel_mask, avmasks)
  
  if (!maskstate()) {
    mdebug("Mask deactivated")
    proxy %>% removeImage(layerId = "mapMask") %>%
      removeControl(layerId = "maskControl")
  } else {
    mdebug("Mask activated")
    proxy %>% 
      addGeotiff(file = mask_layer,
                 opacity = 1,
                 layerId = "mapMask",
                 bands = which_band,
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
  bindEvent(maskstate(), input$ecspMask, input$speciesSelect, input$scenarioSelect, input$modelSelect, input$periodSelect, input$sideSelect)


observe({
  mdebug("Processing realms")
  proxy <- leafletProxy("mainMap")
  
  if (input$ecspRealms) {
    proxy %>% leaflet::addPolygons(data = realms, color = "#454545", opacity = 0.3,
      popup = ~as.character(Realm), fillColor = ~colorQuantile("YlOrRd", Realm)(Realm),
      fillOpacity = 0.05, weight = 2, layerId = paste0("realmsShape", 1:nrow(realms)), options = pathOptions(pane = "extraPane"))
  } else {
    proxy %>% leaflet::removeShape(layerId = paste0("realmsShape", 1:nrow(realms)))
  }
  
})

observe({
  mdebug("Processing EEZ")
  proxy <- leafletProxy("mainMap")
  
  if (input$ecspEEZ) {
    proxy %>% leaflet::addPolygons(data = eez, color = "#454545", opacity = 0.3,
      popup = ~EEZ, fillColor = "#0d7edb",
      fillOpacity = 0.05, weight = 2, layerId = paste0("eezShape", 1:nrow(eez)), options = pathOptions(pane = "extraPane"))
  } else {
    proxy %>% leaflet::removeShape(layerId = paste0("eezShape", 1:nrow(eez)))
  }
  
})