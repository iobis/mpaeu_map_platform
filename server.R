# TODO: 
# Remove reprojection - do reprojection beforehand
# change selectInput for server-side selectize: https://shiny.posit.co/r/articles/build/selectize/
# improve code (clean up)
# Use geo optimized ploting (see draft)

#### Load needed packages ----
library(plotly)
library(geojsonsf)
library(ggplot2)
library(terra)
library(leaflet)
library(leafem)
library(leafpm)
#library(leaflet.extras)
library(leaflet.extras2)
library(leaflet.providers)
library(dplyr)



#### Server function ----
function(input, output, session, debug = TRUE) {
  
  # Create debug function
  mdebug <- function(text, toprint = debug) {
    if (toprint) {
      message(text)
    }
    return(invisible(NULL))
  }
  
  # Create base map ----
  # Load study area
  #study_area <- rjson::fromJSON(file = "data/studyarea.geojson")
  study_area <- sf::read_sf("data/studyarea.geojson")
  
  m <- leaflet() %>% 
    addTiles(group = "Open Street Maps") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark") %>%
    # addFullscreenControl() %>%
    # addResetMapButton() %>%
    # addDrawToolbar(
    #   targetGroup = "draw",
    #   singleFeature = T,
    #   markerOptions = FALSE,
    #   polylineOptions = FALSE,
    #   circleMarkerOptions = FALSE,
    #   editOptions = editToolbarOptions(
    #     edit = FALSE,
    #     selectedPathOptions = selectedPathOptions()
    #   )
    # )  %>%
    # addPmToolbar(
    #   toolbarOptions = pmToolbarOptions(drawMarker = FALSE,
    #                                     drawPolyline = FALSE,
    #                                     cutPolygon = FALSE,
    #                                     position = "topleft"),
    #   drawOptions = pmDrawOptions(snappable = FALSE, allowSelfIntersection = FALSE),
    #   editOptions = pmEditOptions(preventMarkerRemoval = FALSE, draggable = TRUE)
    # ) %>%
    addLayersControl(
      overlayGroups = c("draw"),
      baseGroups = c("Open Street Maps", "CartoDB", "CartoDB Dark"),
      #overlayGroups = c("Projection"),
      options = layersControlOptions(collapsed = T),
      position = "bottomright"
    ) %>%
    addEasyprint(options = easyprintOptions(
      title = 'Print map',
      position = 'bottomleft',
      exportOnly = TRUE)) %>%
    setView(lng = 0.35, lat = 65, zoom = 3)
  
  # Create a reactive to receive the map
  current_map <- reactiveVal(
    m %>%
      addPolygons(data = study_area, highlightOptions = F,
                  color = "#184e77")
  )
  
  
  # Start by ploting/printing placeholders ----
  output$mainMap <- renderLeaflet({
    mdebug("Rendering Leaflet map")
    current_map()}) %>%
    bindEvent(current_map())
  
  output$tableATitle <- renderText({ "Select a map to start" })
  
  
  
  # Tab actions ----
  # Set first tab
  active_tab <- reactiveValues()
  active_tab$current <- "species"
  
  # Observe tab changes
  observeEvent(input$jsValue, {
    mdebug("New tab active")
    # If there is a new value:
    if (active_tab$current != input$jsValue$id) {
      output$mainMap <- renderLeaflet({eval(parse(text = paste0("map_", 
                                                               input$jsValue$id)))})
      
      # Update current status
      active_tab$current <- input$jsValue$id
    }
  })
  
  # Observe first input change
  input_state <- reactiveValues(status = 0)
  bindEvent(observe({input_state$status <- 1}), input$speciesSelect,
            once = TRUE, ignoreInit = TRUE)
  bindEvent(observe({input_state$status <- 2}), input$speciesSelectThermal,
            once = TRUE, ignoreInit = TRUE)
  # bindEvent(observe({input_state$status <- 3}), input$speciesSelect,
  #           once = TRUE, ignoreInit = TRUE)
  # bindEvent(observe({input_state$status <- 4}), input$speciesSelectThermal,
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
    session$sendCustomMessage("showContext", "nothing")
  }) %>%
    bindEvent(input$mainMap_draw_new_feature)

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
            tableA = "Area",
            graph = "Response curves thermal",
            tableB = "Variables importance",
            modelTitle = "Model explanation"
          )
        } else {
          title_state$to_print <- base_list
        }
      }
      
      # Habitat condition
      if (active_tab$current == "habitat") {
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
      
      # Diversity condition
      if (active_tab$current == "diversity") {
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
    }
  }) %>%
    bindEvent(c(input_state$status, active_tab$current))
  
  
  
  # Print titles ----
  output$tableATitle <- renderText({title_state$to_print$tableA})
  output$graphTitle <- renderText({title_state$to_print$graph})
  output$tableBTitle <- renderText({title_state$to_print$tableB})
  output$textTitle <- renderText({title_state$to_print$modelTitle})
  
  
  
  # Add maps and content ----
  
  # Add title/text species
  speciesinfo <- read.csv("data/all_splist_20231017.csv")
  
  ## Species ----
  # Title/context text
  output$selectedSpecies <- renderText({
    input$speciesSelect
  }) %>%
    bindEvent(input$speciesSelect)
  
  output$contextSpecies <- renderText({
    selinf <- speciesinfo[speciesinfo$species == input$speciesSelect,]
    nrec <- 100
    neval <- 0
    glue::glue(
      "<b>Phylum:</b> {selinf$phylum} > <b>Order:</b> {selinf$order} > <b>Family:</b> {selinf$family} <br>
      <b>AphiaID:</b> <a style = 'text-decoration: none; color: #07A5F0;' target='_blank' href = 'https://www.marinespecies.org/aphia.php?p=taxdetails&id={selinf$key}'>{selinf$key}</a><br><br>
      <b>Number of records:</b> {nrec} <br>
      <b>Number of records for independent evaluation:</b> {neval}"
    )
  }) %>%
    bindEvent(input$speciesSelect)
  
  # Load raster (this will be used for all stuff)
  speciesrast <- reactiveValues(raster = NULL,
                                status = 0)
  
  observe({
    
    mdebug("Loading species raster")
    
    spkey <- speciesinfo$key[speciesinfo$species == input$speciesSelect]
    
    all_f <- list.files("data/maps/species",
                        recursive = TRUE, full.names = TRUE)
    all_f <- all_f[grepl(paste0("key", spkey), all_f)]
    all_f <- all_f[grepl("\\.tif", all_f)]
    all_f <- all_f[!grepl("aux", all_f)]
    
    spr <- rast(all_f)
    
    names(spr) <- gsub("\\.tif", "",  basename(all_f))
    
    spr <- projectRasterForLeaflet(spr, method = "bilinear")
    
    speciesrast$raster <- spr
    
    speciesrast$status <- speciesrast$status + 1
  }) %>%
    bindEvent(input$speciesSelect, ignoreInit = T)
  
  # Select the layer for current use
  speciesselrast <- reactive({
    mdebug("Selecting layer")
    
    model_sel <- input$modelSelect
    scenario_sel <- tolower(input$scenarioSelect)
    period_sel <- ifelse(scenario_sel == "current", "nothing",
                         ifelse(input$periodSelect == "2050", "dec50", "dec100"))
    
    sel_rast <- speciesrast$raster[[grepl(model_sel, names(speciesrast$raster))]]
    sel_rast <- sel_rast[[grepl(scenario_sel, names(sel_rast))]]
    if (period_sel != "nothing") {
      sel_rast <- sel_rast[[grepl(period_sel, names(sel_rast))]]
    }
    sel_rast
  })  %>%
    bindEvent(c(speciesrast$status, input$modelSelect, input$scenarioSelect, input$periodSelect),
              ignoreInit = TRUE)
  
  # Change the MAP reactive
  speciesmap <- reactive({
    mdebug(paste("In use layer", names(speciesselrast()), collapse = ","))
    
    m %>%
      addRasterImage(x = speciesselrast(), project = FALSE) %>%
      addPmToolbar(
        toolbarOptions = pmToolbarOptions(drawMarker = FALSE,
                                          drawPolyline = FALSE,
                                          drawCircle = FALSE,
                                          cutPolygon = FALSE,
                                          position = "topleft"),
        drawOptions = pmDrawOptions(snappable = FALSE, allowSelfIntersection = FALSE),
        editOptions = pmEditOptions(preventMarkerRemoval = FALSE, draggable = TRUE)
      )
    
  }) %>%
    bindEvent(speciesselrast())
  
  # Set the new rendered map
  observe({
    mdebug("Setting new map")
    current_map(speciesmap())
  }) %>%
    bindEvent(speciesmap())
  
  
  
  # Thermal
  output$selectedSpeciesThermal <- renderText({
    input$speciesSelectThermal
  }) %>%
    bindEvent(input$speciesSelectThermal, ignoreInit = T)
  
  
  # # Create reactives for species/tab info
  # react_species <- reactiveValues(status = 0)
  # react_thermal <- reactiveValues(status = 0)
  # react_habitat <- reactiveValues(status = 0)
  # react_diversity <- reactiveValues(status = 0)
  # 
  # observe({
  #   react_species$plot_raster <- rast("data/maps/species/key=100803/mv1_pr/lasso_naive/predictions/mv1_pr_lasso_naive_key100803_basevars_current.tif")
  #   react_species$status <- react_species$status + 1
  # }) %>%
  #   bindEvent(input$speciesSelect, ignoreInit = TRUE)
  
  # TODO: add habitat and diversity
  
 # context_leaf_info <- reactiveValues(haveinfo = FALSE)
  
  
  
  ## Contextual info ----
  continfo <- reactiveValues()
  
  observe({
    
    if (active_tab$current == "species") {
      
      spkey <- speciesinfo$key[speciesinfo$species == input$speciesSelect]
      
      # Table 1
      metrics <- read.csv(
        paste0("data/context/species/mv1_pr_", ifelse(input$modelSelect == "lassonaive", "lasso_naive", input$modelSelect),"_key", spkey, "_basevars_cvmetrics.csv")
      )
      metrics_mean <- metrics %>%
        mutate(across(1:ncol(metrics), mean)) %>%
        slice_head(n=1)
      metrics_mean <- metrics_mean %>%
        tidyr::pivot_longer(cols = 1:ncol(metrics_mean), names_to = "Metric", values_to = "Mean of 5 folds") %>%
        mutate(Metric = toupper(Metric))
      metrics_mean <- metrics_mean %>%
        filter(Metric %in% c("AUC", "CBI", "PR", "TSS_P10"))
      metrics_mean$`Mean of 5 folds` <- round(metrics_mean$`Mean of 5 folds`, 2)
      continfo$tableA <- metrics_mean
      
      # Graph
      response_curves <- read.csv(
        paste0("data/context/species/mv1_pr_", ifelse(input$modelSelect == "lassonaive", "lasso_naive", input$modelSelect),"_key", spkey, "_basevars_respcurves.csv")
      )
      response_curves <- response_curves %>%
        group_by(variable) %>%
        mutate(base = scale(base))
      pa <- ggplot(response_curves) +
        geom_line(aes(x = base, y = response, color = variable)) +
        theme_light() +
        xlab("Value (scaled)") + ylab("Response")
      
      continfo$plotA <- plotly::ggplotly(pa)
      
      # Text
      continfo$text[[1]] <- paste("Model explanation -", ifelse(
        input$modelSelect == "lassonaive", "Lasso", "Random Forest"
      ))
      
      continfo$text[[2]] <- ifelse(input$modelSelect == "lassonaive",
                                   "Given the complexity of the task, which involves generating distribution models for a large number 
                                   of species in a semi-automated manner...",
                                   "Given the complexity of the task, which involves generating distribution models for a large number 
                                   of species in a semi-automated manner... Random Forest...")
      
    }
    
  }) %>%
    bindEvent(c(input$speciesSelect, input$modelSelect), ignoreInit = TRUE)
  
  output$tableA <- DT::renderDT({
    continfo$tableA
  }) %>%
    bindEvent(continfo$tableA)
  
  output$plotA <- renderPlotly({
    continfo$plotA
  }) %>%
    bindEvent(continfo$plotA)
  
  output$tableB <- DT::renderDT({
    continfo$tableA
  }) %>%
    bindEvent(continfo$tableA)
  
  output$textTitle <- renderText({
    continfo$text[[1]]
  }) %>%
    bindEvent(continfo$text)
  
  output$textModel <- renderText({
    continfo$text[[2]]
  }) %>%
    bindEvent(continfo$text)
  
  ### Drawing context info ----
  continfo_leaf <- reactiveValues()
  
  observe({
    
    coords <- lapply(input$mainMap_draw_new_feature$geometry$coordinates[[1]], function(x){
      paste(x, collapse = " ")
    })
    
    wkt <- paste("POLYGON ((", paste(coords, collapse = ","),"))")
    
    vect_obj <- terra::vect(wkt)
    
    terra::crs(vect_obj) <- "EPSG:4326"
    
    if (active_tab$current == "species") {
      mdebug(paste("Active tab for data extraction:", active_tab$current))
      vals <- terra::extract(speciesselrast(), vect_obj)
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

  output$contextMap <- renderPlot({
    continfo_leaf$density
  }, height = 200, width = 200) %>%
    bindEvent(continfo_leaf$density)
  
  output$contextMapText <- renderText({
    continfo_leaf$text
  }) %>%
    bindEvent(continfo_leaf$text)
    
  outputOptions(output, "contextMap", suspendWhenHidden = FALSE)
  
}