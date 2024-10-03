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
  req(input$speciesSelect != "")
  mdebug("Changing options")

  available_models <- speciesinfo$models[speciesinfo$species == input$speciesSelect]
  available_models <- unlist(strsplit(available_models, ";"))
  
  if (any(grepl(substr(input$modelSelect,1,3), available_models))) {
    model_inuse <- input$modelSelect
  } else {
    priority <- c("ensemble", "maxent", "rf_classification_ds", "xgboost", "glm")
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
  req(input$filterSpecies != 0)
  filt_list <- filter_opts(speciesinfo, input$groupSelect, input$commonSelect, input$seaSelect, input$includeProjects,
    input$phylumSelect, input$classSelect, input$orderSelect, input$familySelect)

  filtered_data$species$species <- filt_list$species
  filtered_data$species$n <- nrow(filt_list)

})

output$filterN <- renderText({filtered_data$species$n})

# Clear the selection before updating choices
# https://github.com/rstudio/shiny/issues/3966
observeEvent(input$speciesActionOK, {
  if (length(filtered_data$species$species) > 0) {
    updateSelectizeInput(
      inputId = "speciesSelect",
      selected = NULL,
      server = TRUE
    )
  }
})

observe({
 if (length(filtered_data$species$species) > 0) {
    updateSelectizeInput(session, "speciesSelect",
      choices = filtered_data$species$species,
      selected = NULL,#filtered_data$species$species[1],
      server = TRUE)
 }
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
  req(input$filterThermalSpecies != 0)
  filt_list <- filter_opts(speciesinfo, input$groupThermalSelect, input$commonThermalSelect,
    input$seaThermalSelect, input$includeThermalProjects,
    input$phylumThermalSelect, input$classThermalSelect, input$orderThermalSelect, input$familyThermalSelect)

  filtered_data$thermal$species <- filt_list$species
  filtered_data$thermal$n <- nrow(filt_list)

})

output$filterThermalN <- renderText({filtered_data$thermal$n})

# Clear the selection before updating choices
# https://github.com/rstudio/shiny/issues/3966
observeEvent(input$speciesThermalActionOK, {
  if (length(filtered_data$thermal$species) > 0) {
    updateSelectizeInput(
      inputId = "speciesSelectThermal",
      selected = NULL,
      server = TRUE
    )
  }
})

observe({
  if (length(filtered_data$thermal$species) > 0) {
      updateSelectizeInput(session, "speciesSelectThermal",
                           choices = filtered_data$thermal$species,
                           selected = NULL,#filtered_data$species$species[1],
                           server = TRUE)
  }
}) %>% bindEvent(input$speciesThermalActionOK)
