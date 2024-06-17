########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
########################## Map render and placeholder ##########################

# Start by ploting/printing placeholders ----
output$mainMap <- renderLeaflet({
  mdebug("Rendering Leaflet map")
  m})

output$tableATitle <- renderText({ "Select a map to start" })
