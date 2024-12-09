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

welcome_message <- htmltools::div(
  htmltools::img(src = "www/disclaimer.png", width = "100%"),
  htmltools::span(
    htmltools::HTML(
      "Welcome to the MPA Europe map platform. Here, you can explore species and habitat range maps developed for the <a href='https://mpa-europe.eu/' target='_blank'>MPA Europe project</a>.
Model fitting is now complete, and maps are available for 12,684 species. We are currently in the <b>validation and improvement phase</b>, where we are assessing each modeling decision, comparing our results with other models, and inviting partners to review the results.
During this phase, improvements are expected as we implement alternative approaches and corrections. <b>The final launch will be accompanied by a scientific publication documenting all the methods used.</b> <br><br>
When using the models, it is important to explore all the details to evaluate the model quality. If you identify any errors, please email us at helpdesk@obis.org. You can track the project’s progress here: <a href='https://github.com/iobis/mpaeu_map_platform/blob/main/NEWS.md' target='_blank'>github.com/iobis/mpaeu_map_platform</a>."
    ), style = "text-align: left !important;"
  )
)

shinyalert("Welcome", welcome_message, type = "info", html = T, size = "m")