########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
########################## Update selectize options ############################

# Create species selectize ----
available_species <- list.files("data/maps/")
available_species_int <- list.files(paste0("data/maps/", available_species))
available_species <- available_species[which(grepl("mpaeu", available_species_int))]
available_species_int <- list.files(paste0("data/maps/", available_species, "/model=mpaeu"), pattern = "log.json")
available_ids <- unlist(regmatches(available_species_int, gregexpr("taxonid=\\d+_", available_species_int)))
available_ids <- gsub("_", "", gsub("taxonid=", "", available_ids))
available_species <- speciesinfo$scientificName[speciesinfo$taxonID %in% as.numeric(available_ids)]
sp_options <- c("", available_species)

updateSelectizeInput(session, "speciesSelect", choices = sp_options, server = TRUE)
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

# Change species according to group
observe({
  mdebug("Changing species options")
  
  sp_options_upd <- switch(input$groupSelect,
    all = sp_options,
    others = sp_options[sp_options %in% c("", speciesinfo$species[speciesinfo$sdm_group == "others"])],
    seabirds = sp_options[sp_options %in% c("", speciesinfo$species[speciesinfo$sdm_group == "seabirds"])],
    mammals = sp_options[sp_options %in% c("", speciesinfo$species[speciesinfo$sdm_group == "mammals"])],
    photosynthesizers = sp_options[sp_options %in% c("", speciesinfo$species[speciesinfo$sdm_group == "photosynthesizers"])]
  )
  
  updateSelectInput(session, "speciesSelect", choices = sp_options_upd, selected = sp_options_upd[2])
  
}) %>% bindEvent(input$groupSelect, ignoreInit = T, ignoreNULL = TRUE)