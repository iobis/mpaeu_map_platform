# Main renders
# Set reactive for the map ----
current_map <- reactiveVal()


# Start by ploting/printing placeholders ----
output$mainMap <- renderLeaflet({
  mdebug("Rendering Leaflet map")
  current_map()})

output$tableATitle <- renderText({ "Select a map to start" })

# Set the new rendered map
observe({
  mdebug("Setting new map")
  current_map(speciesmap())
}) %>%
  bindEvent(speciesmap())