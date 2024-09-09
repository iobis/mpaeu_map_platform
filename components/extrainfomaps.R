########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
###################### Extra information popup - maps ##########################

# Observe draw on map
observe({
  session$sendCustomMessage("showContext", "nothing")
}) %>%
  bindEvent(input$mainMap_draw_new_feature)

# Create list to hold information
continfo_leaf <- reactiveValues()

# Get information
observe({
  
  coords <- lapply(input$mainMap_draw_new_feature$geometry$coordinates[[1]], function(x){
    paste(x, collapse = " ")
  })
  
  wkt <- paste("POLYGON ((", paste(coords, collapse = ","),"))")
  
  vect_obj <- terra::vect(wkt)
  
  terra::crs(vect_obj) <- "EPSG:4326"
  
  if (active_tab$current == "species") {
    mdebug(paste("Active tab for data extraction:", active_tab$current))
    
    # sp_info <- list(
    #   species = input$speciesSelect,
    #   model = input$modelSelect,
    #   scenario = tolower(input$scenarioSelect),
    #   decade = ifelse(is.null(input$periodSelect), NULL,
    #                   ifelse(input$periodSelect == 2050, "dec50", "dec100")),
    #   spkey = speciesinfo$key[speciesinfo$species == input$speciesSelect]
    # )
    
    vals <- terra::extract(terra::rast(files_inuse$file_a), vect_obj)
    vals <- vals[,2]
    vals <- vals[!is.na(vals)]
    
    if (any(is.null(vals)) | any(is.na(vals))) {
      vals <- 0
    }
    
    p <- ggplot(data.frame(ROR = vals)) +
      geom_density(aes(x = ROR), fill = "#1a759f", color = NA) +
      xlab("Relative Occurrence Rate") +
      ylab(NULL) +
      theme_classic() +
      theme(axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank())
    
    continfo_leaf$density <- p
    
    continfo_leaf$text <- glue::glue(
      "<span style='font-size:larger;'><b>Selected area</b></span> <br> 
    <b>Mean Relative Occurrence Rate:</b> {round(mean(vals), 2)} <br>
    <b>Standard deviation ROR:</b> {round(sd(vals), 2)} <br>
    <b>Total area:</b> {round(terra::expanse(vect_obj, unit = 'km'), 2)}km² <br><br>"
    )
  }
  
  
}) %>%
  bindEvent(input$mainMap_draw_new_feature)

# Output contextual info
output$contextMap <- renderPlot({
  continfo_leaf$density
}, height = 200, width = 200) %>%
  bindEvent(continfo_leaf$density)

output$contextMapText <- renderText({
  continfo_leaf$text
}) %>%
  bindEvent(continfo_leaf$text)

outputOptions(output, "contextMap", suspendWhenHidden = FALSE)