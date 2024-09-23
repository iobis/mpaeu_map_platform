########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
########################## Update selectize options ############################

# Create species selectize ----
updateSelectizeInput(session, "speciesSelect", choices = sp_options, server = TRUE)
# Create thermal selectize ----
updateSelectizeInput(session, "speciesSelectThermal", choices = sp_options, server = TRUE)

# Change model options based on available models
observe({
  mdebug("Changing options")
  spkey <- speciesinfo$key[speciesinfo$species == input$speciesSelect]
  logf <- jsonlite::read_json(paste0("data/maps/taxonid=",spkey,"/model=mpaeu/taxonid=",spkey,"_model=mpaeu_what=log.json"))
  
  mod_names <- names(logf$model_posteval)[unlist(lapply(logf$model_posteval, function(x) if (length(x) > 0) TRUE else FALSE))]
  available_models <- mod_names[!grepl("niche", mod_names)]
  #available_models <- gsub("maxent", "maxnet", available_models)
  available_models <- gsub("rf", "rf_classification_ds", available_models)
  
  if (any(grepl(substr(input$modelSelect,1,3), available_models))) {
    model_inuse <- input$modelSelect
  } else {
    priority <- c("ensemble", "maxnet", "rf_classification_ds", "xgboost", "glm")
    model_inuse <- priority[priority %in% available_models][1]
  }
  
  names_options <- dplyr::case_when(
    available_models == "maxent" ~ "MAXENT",
    available_models == "rf_classification_ds" ~ "Random Forest",
    available_models == "glm" ~ "GLM",
    available_models == "xgboost" ~ "XGboost",
    available_models == "ensemble" ~ "Ensemble",
    .default = available_models
  )
  
  names(available_models) <- names_options
  updateSelectInput(session, "modelSelect", choices = available_models, selected = model_inuse)
}) %>% bindEvent(input$speciesSelect, ignoreInit = T)


##### Filter modals #####
source("scripts/filter_functions.R", local = TRUE)

# Species modal -----
observeEvent(input$filterSpecies, {
  updateSelectizeInput(session, "groupSelect", choices = sdm_groups, server = TRUE)
  updateSelectizeInput(session, "commonSelect", choices = common_names, server = TRUE)
  updateSelectizeInput(session, "seaSelect", choices = region_names, server = TRUE)
  updateSelectizeInput(session, "phylumSelect", choices = phylums, server = TRUE)
  updateSelectizeInput(session, "classSelect", choices = classes, server = TRUE)
  updateSelectizeInput(session, "orderSelect", choices = orders, server = TRUE)
  updateSelectizeInput(session, "familySelect", choices = families, server = TRUE)
  showModal(filterSpeciesModal())
})

observeEvent(input$speciesActionOK, {
  removeModal()
})

filtered_data <- reactiveValues(species = list(species = NULL, n = 0),
                                thermal = list(species = NULL, n = 0))

# Observe filtering
observe({
  filt_list <- filter_opts(speciesinfo, input$groupSelect, input$commonSelect, input$seaSelect, input$includeProjects,
    input$phylumSelect, input$classSelect, input$orderSelect, input$familySelect)

  filtered_data$species$species <- filt_list$species
  filtered_data$species$n <- nrow(filt_list)

  if (length(filt_list$species) > 0) {
    shiny::updateActionButton(inputId = "speciesActionOK", label = "OK", disabled = FALSE)
  } else {
    shiny::updateActionButton(inputId = "speciesActionOK", label = "OK", disabled = TRUE)
  }

})

output$filterN <- renderText({filtered_data$species$n})

observe({
  updateSelectizeInput(session, "speciesSelect", choices = filtered_data$species$species, server = TRUE)
}) %>% bindEvent(input$speciesActionOK)


# Thermal modal -----
observeEvent(input$filterThermalSpecies, {
  updateSelectizeInput(session, "groupThermalSelect", choices = sdm_groups, server = TRUE)
  updateSelectizeInput(session, "commonThermalSelect", choices = common_names, server = TRUE)
  updateSelectizeInput(session, "seaThermalSelect", choices = region_names, server = TRUE)
  updateSelectizeInput(session, "phylumThermalSelect", choices = phylums, server = TRUE)
  updateSelectizeInput(session, "classThermalSelect", choices = classes, server = TRUE)
  updateSelectizeInput(session, "orderThermalSelect", choices = orders, server = TRUE)
  updateSelectizeInput(session, "familyThermalSelect", choices = families, server = TRUE)
  showModal(filterThermalModal())
})

observeEvent(input$speciesThermalActionOK, {
  removeModal()
})

# Observe filtering
observe({
  filt_list <- filter_opts(speciesinfo, input$groupThermalSelect, input$commonThermalSelect,
    input$seaThermalSelect, input$includeThermalProjects,
    input$phylumThermalSelect, input$classThermalSelect, input$orderThermalSelect, input$familyThermalSelect)

  filtered_data$thermal$species <- filt_list$species
  filtered_data$thermal$n <- nrow(filt_list)

  if (length(filt_list$species) > 0) {
    shiny::updateActionButton(inputId = "speciesThermalActionOK", label = "OK", disabled = FALSE)
  } else {
    shiny::updateActionButton(inputId = "speciesThermalActionOK", label = "OK", disabled = TRUE)
  }

})

output$filterThermalN <- renderText({filtered_data$thermal$n})

observe({
  updateSelectizeInput(session, "speciesSelectThermal", choices = filtered_data$thermal$species, server = TRUE)
}) %>% bindEvent(input$speciesThermalActionOK)
