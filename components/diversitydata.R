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
  if (input$diversitySelect != "" && input$diversityGroup != "" && active_tab$current == "diversity") {
    scenario_f <- ifelse(sp_info$scenario_d == "current",
                        sp_info$scenario_d, paste0(sp_info$scenario_d, "_", sp_info$decade_d))

    glue::glue("data/diversity/metric={sp_info$metric}_model=mpaeu_method={sp_info$model_d}_scen={scenario_f}_group={sp_info$group}_type={sp_info$div_type}{sp_info$map_type}_cog.tif")
  } else {NULL}
  
}) %>%
  bindEvent(sp_info$metric, sp_info$scenario_d,
            sp_info$decade_d, sp_info$model_d, sp_info$map_type, sp_info$div_type, ignoreInit = T)

observe({
  
  mdebug("Processing diversity update")
  print(diversity_f_data())
  proxy <- leafletProxy("mainMap")
  
  suppressWarnings({
      proxy <- proxy %>%
        clearMarkers() %>%
        clearShapes() %>%
        addGeotiff(file = diversity_f_data(), opacity = 1,
                   colorOptions = colorOptions(palette = rev(c("#7d1500", "#da4325", "#eca24e", "#e7e2bc", "#5cc3af", "#0a6265")),
                                               na.color = NA), autozoom = F)
    })
  
}) %>%
  bindEvent(diversity_f_data(), ignoreInit = F)