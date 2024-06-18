########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
######################### Main information from input ##########################

# Reactive values to store species information and model in use
sp_info <- reactiveValues(species = "", # Species
                          model = "",
                          scenario = "",
                          decade = "",
                          spkey = "",
                          # Thermal
                          spkey_t = "",
                          scenario_t = "",
                          decade_t = "",
                          # Habitat
                          habitat = "",
                          # Diversity
                          metric = "",
                          scenario_d = "",
                          decade_d = "",
                          model_d = "")

model_inuse <- reactiveValues(model = NULL)

# Observe changes and update species information and model based on the active tab
observe({
  
  # When the active tab is "species"
  if (active_tab$current == "species") {
    # Update species information from input selections
    sp_info$species <- input$speciesSelect
    sp_info$model <- input$modelSelect
    sp_info$scenario <- tolower(input$scenarioSelect)
    sp_info$decade <- ifelse(is.null(input$periodSelect), NULL,
                             ifelse(input$periodSelect == 2050, "dec50", "dec100"))
    key <- speciesinfo$key[speciesinfo$species == input$speciesSelect]
    sp_info$spkey <- key
    
    # Check available models for the selected species
    if (input$speciesSelect != "") {
      logf <- jsonlite::read_json(paste0("data/maps/taxonid=", key,
                                         "/model=inteval/taxonid=", key, "_model=inteval_what=log.json"))
      mod_names <- names(logf$model_posteval)[unlist(lapply(logf$model_posteval,
                                                            function(x) if (length(x) > 0) TRUE else FALSE))]
      available_models <- mod_names[!grepl("niche", mod_names)]
      available_models <- gsub("maxent", "maxnet", available_models)
      available_models <- gsub("rf", "rf_classification_ds", available_models)
      
      # Set the model in use based on availability and priority
      if (any(grepl(substr(input$modelSelect, 1, 3), available_models))) {
        model_inuse$model <- input$modelSelect
      } else {
        priority <- c("ensemble", "maxnet", "rf_classification_ds", "xgboost", "glm")
        model_inuse$model <- sp_info$model <- priority[priority %in% available_models][1]
      }
    }
  }
  
  # When the active tab is "thermal"
  if (active_tab$current == "thermal") {
    # Update thermal species information from input selections
    sp_info$species <- input$speciesSelectThermal
    sp_info$scenario_t <- tolower(input$scenarioSelectThermal)
    sp_info$decade_t <- ifelse(is.null(input$periodSelectThermal), NULL,
                             ifelse(input$periodSelectThermal == 2050, "dec50", "dec100"))
    sp_info$spkey_t <- speciesinfo$key[speciesinfo$species == input$speciesSelectThermal]
  }
  
  # When the active tab is "habitat"
  if (active_tab$current == "habitat") {
    # Update habitat information from input selections
    sp_info$habitat <- input$habitatSelect
    sp_info$scenario <- tolower(input$scenarioSelectHabitat)
    sp_info$decade <- ifelse(is.null(input$periodSelectHabitat), NULL,
                             ifelse(input$periodSelectHabitat == 2050, "dec50", "dec100"))
  }
  
  # When the active tab is "diversity"
  if (active_tab$current == "diversity") {
    # Update diversity metric and model information from input selections
    sp_info$metric <- input$diversitySelect
    sp_info$model_d <- input$modelSelectDiversity
    sp_info$scenario_d <- tolower(input$scenarioSelectDiversity)
    sp_info$decade_d <- ifelse(is.null(input$periodSelectDiversity), NULL,
                             ifelse(input$periodSelectDiversity == 2050, "dec50", "dec100"))
  }
})
