########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
############################ Tab control/actions ###############################

# Set first tab
active_tab <- reactiveValues()
active_tab$current <- "species"

# Observe tab changes
observeEvent(input$jsValue, {
  mdebug("New tab active")
  # If there is a new value:
  if (active_tab$current != input$jsValue$id) {
    
    # Update current status
    active_tab$current <- input$jsValue$id
    
    # #Temporary workaround while other features will be added!
    # if (!active_tab$current %in% c("species", "thermal", "habitat", "diversity")) {
    #   shinyalert::shinyalert("Feature not available", "For now only species distribution/thermal range maps are available.", type = "info")
    #   active_tab$current <- "species"
    #   session$sendCustomMessage("backToTab", "nothing")
    # }
  }
})

# Observe first input change
input_state <- reactiveValues(status = 0)
bindEvent(observe({input_state$status <- 1}), input$speciesSelect,
          once = TRUE, ignoreInit = TRUE)
bindEvent(observe({input_state$status <- 2}), input$speciesSelectThermal,
          once = TRUE, ignoreInit = TRUE)
bindEvent(observe({input_state$status <- 3}), input$habitatSelect,
          once = TRUE, ignoreInit = TRUE)
bindEvent(observe({input_state$status <- 4}), input$diversitySelect,
          once = TRUE, ignoreInit = TRUE)
# bindEvent(observe({input_state$status <- 5}), input$diversitySelect,
#           once = TRUE, ignoreInit = TRUE)

# Create a reactive for titles
title_state <- reactiveValues()
title_state$current <- "empty"

base_list <- list(
  tableA = "Select a map to start",
  graph = "",
  tableB = "",
  modelTitle = ""
)

observe({
  if (title_state$current != active_tab$current) {
    session$sendCustomMessage("removeContext", "nothing")
    # Species condition
    if (active_tab$current == "species") {
      if (input$speciesSelect != "") {
        title_state$current <- "species"
        title_state$to_print <- list(
          tableA = "Model metrics",
          graph = "Response curves",
          tableB = "Variables importance",
          modelTitle = "Model explanation"
        )
      } else {
        title_state$to_print <- base_list
      }
    }
    
    # Thermal condition
    if (active_tab$current == "thermal") {
      if (input$speciesSelectThermal != "") {
        title_state$current <- "thermal"
        title_state$to_print <- list(
          tableA = "Thermal ranges",
          graph = "Thermal ranges (density)",
          tableB = "Area within thermal range",
          modelTitle = "Model explanation"
        )
      } else {
        title_state$to_print <- base_list
      }
    }
    
    # Habitat condition
    if (active_tab$current == "habitat") {
      if (input$habitatSelect != "") {
        title_state$current <- "habitat"
        title_state$to_print <- list(
          tableA = "Distribution by areas",
          graph = "Areas with known biogenic habitat occurrence",
          tableB = "Species information",
          modelTitle = "What is a biogenic habitat?"
        )
      } else {
        base_list_mod <- base_list
        base_list_mod$tableB <- base_list_mod$tableA
        base_list_mod$tableA <- ""
        title_state$to_print <- base_list_mod
      }
    }
    
    # Diversity condition
    if (active_tab$current == "diversity") {
      if (input$diversitySelect != "") {
        title_state$current <- "diversity"
        title_state$to_print <- list(
          tableA = "Diversity by areas",
          graph = "Protected areas",
          tableB = "Composition",
          modelTitle = "Metric explanation"
        )
      } else {
        base_list_mod <- base_list
        base_list_mod$tableB <- base_list_mod$tableA
        base_list_mod$tableA <- ""
        title_state$to_print <- base_list_mod
      }
    }

    # Atlas condition
    if (active_tab$current == "atlas") {
      if (FALSE) {
      #if (input$diversitySelect != "") {
        title_state$current <- "diversity"
        title_state$to_print <- list(
          tableA = "Diversity by areas",
          graph = "Protected areas",
          tableB = "Composition",
          modelTitle = "Metric explanation"
        )
      } else {
        base_list_mod <- base_list
        base_list_mod$tableB <- "Available soon"#base_list_mod$tableA
        base_list_mod$tableA <- ""
        title_state$to_print <- base_list_mod
      }
    }
  }
}) |>
  bindEvent(c(input_state$status, active_tab$current))