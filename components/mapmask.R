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