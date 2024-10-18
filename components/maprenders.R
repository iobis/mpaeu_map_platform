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

shinyalert("Welcome", "This platform is still being prepared, and we are continuing to add new models. Some data may be unavailable, especially on the diversity and habitat tabs. If you encounter any issues, please try again later. You can track the project status here: <a href='https://github.com/iobis/mpaeu_map_platform/blob/main/NEWS.md'>github.com/iobis/mpaeu_map_platform/blob/main/NEWS.md</a>", type = "info", html = T)