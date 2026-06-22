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
atlas_modal_shown <- reactiveVal(FALSE)
observeEvent(active_tab$current, {
  if (active_tab$current == "atlas" && !atlas_modal_shown()) {
    atlas_modal_shown(TRUE)
    atlas_message <- htmltools::tagList(
      htmltools::tags$style(htmltools::HTML("
        .front-card {
          border: solid;
          margin-top: 15px;
          margin-bottom: 15px;
          margin-right: 5px;
          margin-left: 5px;
          padding: 3px;
          border-radius: 10px;
          border-color: #bcbcbc;
          cursor: pointer;
          transition: background-color 0.2s ease;
        }
        .front-card:hover { background-color: #f0f0f0; }
        .card-icon {
          display: flex;
          flex-direction: row;
          height: 100%;
          align-items: center;
          justify-content: center;
          font-size: 2.1em;
        }
      ")),
      htmltools::div(class = "container",
        htmltools::div(class = "row",
          htmltools::div(class = "g-col-12",
            htmltools::p(htmltools::HTML(
              "Visit the new MPA Europe atlas, with an improved experience and better layer controls."
            ))
            # ,
            # bslib::card(
            #   bslib::layout_column_wrap(
            #     style = htmltools::css(grid_template_columns = "1fr 6fr"),
            #     htmltools::div(
            #       htmltools::tags$i(class = "fa-solid fa-map fa-2x", style = "color: #184e77;"),
            #       class = "card-icon"
            #     ),
            #     htmltools::div(
            #       htmltools::h4(htmltools::HTML(
            #         '<span style="color: #184e77;">MPA Europe Atlas</span> <i class="fa-solid fa-arrow-up-right-from-square" style="font-size: 0.85em; color: #b2b2b2;"></i>'
            #       ), style = "margin-top: 0px !important;"),
            #       htmltools::p("A dedicated app with more layers, advanced filtering, and a better overall experience."),
            #       style = "text-align: left;"
            #     )
            #   ),
            #   class = "front-card"
            # )
          )
        )
      )
    )
    shinyalert::shinyalert(
      "A new atlas is available",
      atlas_message,
      type = "info", html = TRUE, size = "m", closeOnClickOutside = TRUE,
      confirmButtonText = "Explore the new atlas", confirmButtonCol = "#184e77",
      showCancelButton = TRUE, cancelButtonText = "Stay here",
      callbackJS = "function(x) { if (x) { window.open('https://iobis.github.io/mpaeu_atlas/', '_blank'); } }"
    )
  }
}, ignoreInit = TRUE)

# Special case of atlas
atlas_active <- reactiveVal(FALSE)
bindEvent(observe({
  if (any(!unlist(lapply(input$atlasSelector, is.null), use.names = F))) {
    atlas_active(TRUE)
  }
}), input$atlasSelector, ignoreInit = TRUE, ignoreNULL = TRUE)

# Others
input_state <- reactiveValues(status = 0)
bindEvent(observe({input_state$status <- 1}), input$speciesSelect,
          once = TRUE, ignoreInit = TRUE)
bindEvent(observe({input_state$status <- 2}), input$speciesSelectThermal,
          once = TRUE, ignoreInit = TRUE)
bindEvent(observe({input_state$status <- 3}), input$habitatSelect,
          once = TRUE, ignoreInit = TRUE)
bindEvent(observe({input_state$status <- 4}), input$diversitySelect,
          once = TRUE, ignoreInit = TRUE)
bindEvent(observe({input_state$status <- 5}), atlas_active(),
          once = TRUE, ignoreInit = TRUE, ignoreNULL = TRUE)

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
      if (any(!unlist(lapply(input$atlasSelector, is.null), use.names = F))) {
        title_state$current <- "atlas"
        title_state$to_print <- list(
          tableA = "New",
          graph = "New",
          tableB = "Layer sources",
          modelTitle = "Additional information"
        )
      } else {
        base_list_mod <- base_list
        base_list_mod$tableB <- base_list_mod$tableA
        base_list_mod$tableA <- ""
        title_state$to_print <- base_list_mod
      }
    }
  }
}) |>
  bindEvent(c(input_state$status, active_tab$current))