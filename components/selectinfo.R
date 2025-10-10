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
                          acro = "",
                          # Thermal
                          spkey_t = "",
                          scenario_t = "",
                          decade_t = "",
                          acro_t = "",
                          # Habitat
                          habitat = "",
                          acro_h = "mpaeu",
                          scenario_h = "",
                          decade_h = "",
                          model_h = "",
                          bintype_h = "",
                          threshold_h = "",
                          # Diversity
                          metric = "",
                          group = "",
                          acro_d = "mpaeu",
                          map_type = "",
                          div_type = "",
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
    index <- match(input$speciesSelect, speciesinfo$species, nomatch = 0)
    sp_info$spkey <- speciesinfo$key[index]
    sp_info$acro <- speciesinfo$acro[index]
    
    # Check available models for the selected species
    if (input$speciesSelect != "") {
      available_models <- speciesinfo$model[speciesinfo$species == input$speciesSelect]
      available_models <- strsplit(available_models, split = ";")[[1]]
      
      # Set the model in use based on availability and priority
      if (any(grepl(substr(input$modelSelect, 1, 3), available_models))) {
        model_inuse$model <- input$modelSelect
      } else {
        priority <- c("ensemble", "maxent", "rf_classification_ds", "xgboost", "glm")
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
    index <- match(input$speciesSelectThermal, speciesinfo$species, nomatch = 0)
    sp_info$spkey_t <- speciesinfo$key[index]
    sp_info$acro_t <- speciesinfo$acro[index]
  }
  
  # When the active tab is "habitat"
  if (active_tab$current == "habitat") {
    # Update habitat information from input selections
    sp_info$habitat <- input$habitatSelect
    sp_info$scenario_h <- tolower(input$scenarioSelectHabitat)
    sp_info$decade_h <- ifelse(is.null(input$periodSelectHabitat), NULL,
                             ifelse(input$periodSelectHabitat == 2050, "dec50", "dec100"))
    sp_info$model_h <- input$modelSelectHabitat
    sp_info$bintype_h <- ifelse(input$habitatBinaryFull, "bin", "cont")
    sp_info$threshold_h <- input$habitatBin
  }
  
  # When the active tab is "diversity"
  if (active_tab$current == "diversity") {
    # Update diversity metric and model information from input selections
    sp_info$metric <- input$diversitySelect
    sp_info$group <- tolower(input$diversityGroup)
    sp_info$model_d <- input$modelSelectDiversity
    sp_info$scenario_d <- tolower(input$scenarioSelectDiversity)
    sp_info$decade_d <- ifelse(is.null(input$periodSelectDiversity), NULL,
                             ifelse(input$periodSelectDiversity == 2050, "dec50", "dec100"))
    if (input$modelSelectDiversity == "raw") {
      sp_info$map_type <- ""
      sp_info$div_type <- input$diversityTypeRaw
    } else {
      sp_info$map_type <- paste0("_", input$diversityMode)
      sp_info$div_type <- input$diversityType
    }
  }
})


# To superseed in the future the sp_info object
global_acro <- "mpaeu"

db_info <- reactiveValues(
  species = NULL,
  thermal = NULL,
  habitat = NULL,
  diversity = NULL
)

species_db <- arrow::open_dataset("data/species_db.parquet")
habitat_db <- arrow::open_dataset("data/habitat_db.parquet")
diversity_db <- arrow::open_dataset("data/diversity_db.parquet")

observe({
  # When the active tab is "species"
  if (active_tab$current == "species") {
    # Check available models for the selected species
    if (input$speciesSelect != "") {
      sel_obj <- species_db |>
        select(-jsoncat) |>
        filter(scientificName == input$speciesSelect) |>
        collect()
      db_info$species <- sel_obj
    }
  }
  
  # When the active tab is "thermal"
  if (active_tab$current == "thermal") {
   if (input$speciesSelectThermal != "") {
      sel_obj <- species_db |>
        select(-jsoncat) |>
        filter(scientificName == input$speciesSelectThermal) |>
        collect()
      db_info$thermal <- sel_obj
    }
  }
  
  # When the active tab is "habitat"
  if (active_tab$current == "habitat") {
    if (input$habitatSelect != "") {
      sel_obj <- habitat_db |>
        select(-jsoncat) |>
        filter(habitat == input$habitatSelect) |>
        collect()
      db_info$habitat <- sel_obj
    }
  }
  
  # When the active tab is "diversity"
  if (active_tab$current == "diversity") {
    if (input$diversitySelect != "") {
      sel_obj <- diversity_db |>
        select(-jsoncat) |>
        filter(metric == input$diversitySelect) |>
        collect()
      db_info$diversity <- sel_obj
    }
  }
})

select_params <- reactiveValues(
  species = list(
    species = "",
    model = "",
    scenario = "",
    decade = "",
    spkey = "",
    acro = ""
  ),
  thermal = list(
    species_t = "",
    spkey_t = "",
    scenario_t = "",
    decade_t = "",
    acro_t = ""
  ),
  habitat = list(
    habitat = "",
    acro_h = "mpaeu",
    scenario_h = "",
    decade_h = "",
    model_h = "",
    bintype_h = "",
    threshold_h = ""
  ),
  diversity = list(
    metric = "",
    group = "",
    acro_d = "mpaeu",
    map_type = "",
    div_type = "",
    scenario_d = "",
    decade_d = "",
    model_d = ""
  )
)

observe({
  # Species
  if (!is.null(db_info$species$scientificName)) {
    select_params$species$species <- db_info$species$scientificName
  }
  select_params$species$spkey <- db_info$species$taxonid
  select_params$species$acro <- global_acro
  select_params$species$model <- input$modelSelect
  select_params$species$scenario <- tolower(input$scenarioSelect)
  select_params$species$decade <- ifelse(is.null(input$periodSelect), NULL,
    ifelse(input$periodSelect == 2050, "dec50", "dec100")
  )

  # Thermal
  if (!is.null(db_info$thermal$scientificName)) {
    select_params$thermal$species_t <- db_info$thermal$scientificName
  }
  select_params$thermal$spkey_t <- db_info$thermal$taxonid
  select_params$thermal$acro_t <- global_acro
  select_params$thermal$scenario_t <- tolower(input$scenarioSelectThermal)
  select_params$thermal$decade_t <- ifelse(is.null(input$periodSelectThermal), NULL,
    ifelse(input$periodSelectThermal == 2050, "dec50", "dec100")
  )

  # Habitat
  if (!is.null(db_info$habitat$habitat)) {
    select_params$habitat$habitat <- db_info$habitat$habitat
  }
  select_params$habitat$acro_h <- global_acro
  select_params$habitat$model_h <- input$modelSelectHabitat
  select_params$habitat$scenario_h <- tolower(input$scenarioSelectHabitat)
  select_params$habitat$decade_h <- ifelse(is.null(input$periodSelectHabitat), NULL,
    ifelse(input$periodSelectHabitat == 2050, "dec50", "dec100")
  )
  select_params$habitat$bintype_h <- ifelse(input$habitatBinaryFull, "bin", "cont")
  select_params$habitat$threshold_h <- input$habitatBin

  # Diversity
  if (!is.null(db_info$diversity$metric)) {
    select_params$diversity$metric <- db_info$diversity$metric
  }
  select_params$diversity$acro_d <- global_acro
  select_params$diversity$group <- tolower(input$diversityGroup)
  select_params$diversity$scenario_d <- tolower(input$scenarioSelectDiversity)
  select_params$diversity$decade_d <- ifelse(is.null(input$periodSelectDiversity), NULL,
    ifelse(input$periodSelectDiversity == 2050, "dec50", "dec100")
  )
  if (input$modelSelectDiversity == "raw") {
    select_params$diversity$map_type <- ""
    select_params$diversity$div_type <- input$diversityTypeRaw
  } else {
    select_params$diversity$map_type <- paste0("_", input$diversityMode)
    select_params$diversity$div_type <- input$diversityType
  }
})