########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
######################### Main information from input ##########################
db_info <- reactiveValues(
  species = NULL,
  thermal = NULL,
  habitat = NULL,
  diversity = NULL
)

# Objects *_db are loaded on serverstart.R

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
    acro = "",
    side_select = ""
  ),
  thermal = list(
    species_t = "",
    spkey_t = "",
    scenario_t = "",
    decade_t = "",
    acro_t = "",
    side_select_t = ""
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
    type_d = "",
    threshold_d = "",
    posttreat_d = "",
    scenario_d = "",
    decade_d = ""
  )
)

# Update side select to FALSE if tab changes
observe({
  updateCheckboxInput(session, inputId = "sideSelect", value = FALSE)
}) |>
  bindEvent(active_tab$current)

# Select parameters
observe({
  mdebug("Triggered select_params")
  # Species
  if (!is.null(db_info$species$scientificName)) {
    select_params$species$species <- db_info$species$scientificName
    mdebug("Changing species options")
    available_models <- db_info$species |>
      pull(available_models) |>
      unlist(use.names = F)

    if (any(grepl(substr(input$modelSelect, 1, 3), available_models))) {
      model_inuse <- input$modelSelect
    } else {
      priority <- c("ensemble", "maxent", "rf", "xgboost", "glm", "esm")
      model_inuse <- priority[priority %in% available_models][1]
    }

    names_options <- dplyr::case_when(
      available_models == "maxent" ~ "MAXENT",
      available_models == "rf" ~ "Random Forest",
      available_models == "glm" ~ "GLM",
      available_models == "xgboost" ~ "XGboost",
      available_models == "ensemble" ~ "Ensemble",
      available_models == "esm" ~ "ESM",
      .default = available_models
    )

    names(available_models) <- names_options
    updateSelectInput(session, "modelSelect", choices = available_models, selected = model_inuse)

    select_params$species$model <- model_inuse
  }
  select_params$species$spkey <- db_info$species$taxonid
  select_params$species$acro <- global_acro
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

  # Side select species AND thermal
  if (active_tab$current == "species") {
    if (input$scenarioSelect == "Current") {
      select_params$species$side_select <- FALSE
      updateCheckboxInput(session, inputId = "sideSelect", value = FALSE)
    } else {
      select_params$species$side_select <- input$sideSelect
    }
  } else if (active_tab$current == "thermal") {
    if (input$scenarioSelectThermal == "Current") {
      select_params$thermal$side_select_t <- FALSE
      updateCheckboxInput(session, inputId = "sideSelect", value = FALSE)
    } else {
      select_params$thermal$side_select_t <- input$sideSelect
    }
  } else {
    select_params$species$side_select <- FALSE
    select_params$thermal$side_select_t <- FALSE
    updateCheckboxInput(session, inputId = "sideSelect", value = FALSE)
  }

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
  select_params$habitat$bintype_h <- ifelse(input$habitatBinaryFull, "binary", "continuous")
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
  select_params$diversity$type_d <- input$diversityMode
  select_params$diversity$threshold_d <- input$diversityType
  select_params$diversity$posttreat_d <- input$diversityPostTreat
}) |>
  bindEvent(
    # Species
    db_info$species,
    input$modelSelect,
    input$scenarioSelect,
    input$periodSelect,
    # Thermal
    db_info$thermal,
    input$scenarioSelectThermal,
    input$periodSelectThermal,
    # Habitat
    db_info$habitat,
    input$modelSelectHabitat,
    input$scenarioSelectHabitat,
    input$periodSelectHabitat,
    input$habitatBinaryFull,
    input$habitatBin,
    # Diversity
    db_info$diversity,
    input$diversityGroup,
    input$scenarioSelectDiversity,
    input$periodSelectDiversity,
    input$diversityMode,
    input$diversityType,
    input$diversityPostTreat,
    # General
    input$sideSelect,
    # Options
    ignoreInit = TRUE
  )

# Triggers for contextual information
context_param <- reactiveValues(
  species = NULL,
  thermal = NULL,
  habitat = NULL,
  diversity = NULL,
  diversity_group = NULL
)

observe({
  if (active_tab$current == "species") {
    context_param$species <- select_params$species$species
  } else if (active_tab$current == "thermal") {
    context_param$thermal <- select_params$thermal$species_t
  } else if (active_tab$current == "habitat") {
    context_param$habitat <- select_params$habitat$habitat
  } else if (active_tab$current == "diversity") {
    context_param$diversity <- select_params$diversity$metric
    context_param$diversity_group <- select_params$diversity$group
  }
}) |>
  bindEvent(
    select_params$species,
    select_params$thermal,
    select_params$habitat,
    select_params$diversity,
    ignoreInit = TRUE
  )