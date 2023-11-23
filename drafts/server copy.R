#### Load needed packages ----
library(plotly)
library(geojsonsf)
library(terra)
library(leaflet)
library(leaflet.extras)

#### Plotly full screen data ----
# SVG icons
icons <- list()

# Fullscreen (source : https://fontawesome.com/icons/expand?f=classic&s=solid)
icons$expand <- "M32 32C14.3 32 0 46.3 0 64v96c0 17.7 14.3 32 32 32s32-14.3 32-32V96h64c17.7 0 32-14.3 32-32s-14.3-32-32-32H32zM64 352c0-17.7-14.3-32-32-32s-32 14.3-32 32v96c0 17.7 14.3 32 32 32h96c17.7 0 32-14.3 32-32s-14.3-32-32-32H64V352zM320 32c-17.7 0-32 14.3-32 32s14.3 32 32 32h64v64c0 17.7 14.3 32 32 32s32-14.3 32-32V64c0-17.7-14.3-32-32-32H320zM448 352c0-17.7-14.3-32-32-32s-32 14.3-32 32v64H320c-17.7 0-32 14.3-32 32s14.3 32 32 32h96c17.7 0 32-14.3 32-32V352z"

dirty_js <- function(x) {
  structure(x, class = unique(c("JS_EVAL", oldClass(x))))
}

button_fullscreen <- function() {
  list(
    name = "fullscreen",
    title = "Toggle fullscreen",
    icon = list(
      path = icons$expand,
      transform = 'matrix(1 0 0 1 0 -1) scale(0.03571429)'
    ),
    attr = "full_screen",
    val = "false",
    click = dirty_js(
      "function(gd, ev) {
         var button = ev.currentTarget;
         var astr = button.getAttribute('data-attr');
         var val = button.getAttribute('data-val') || false;
      
         if(astr === 'full_screen') {
           if(val === 'false') {
             button.setAttribute('data-val', 'true');
             gd.classList.add('full-screen');
             Plotly.Plots.resize(gd);
           } else {
             button.setAttribute('data-val', 'false');
             gd.classList.remove('full-screen');
             Plotly.Plots.resize(gd);
           }
         }
      }"
    )
  )
}

#### Server function ----
function(input, output, session) {
  
  # Create base map ----
  # Load study area
  study_area <- rjson::fromJSON(file = "data/studyarea.geojson")
  
  fig <- plot_ly() %>%
    add_trace(
      type="choroplethmapbox",
      geojson=study_area,
      locations=0,
      z=1,
      colorscale="Viridis",
      featureidkey="properties.FID",
      marker=list(line=list(
        width=0),
        opacity=0.5
      ),
      showlegend = F, 
      showscale = FALSE
    ) %>%
    # # Add full screen
    config(
      modeBarButtonsToAdd = list(
        list(button_fullscreen())
      )) %>%
    # Add mapbox
    layout(
      mapbox = list(
        style = 'open-street-map',
        zoom =2,
        center = list(lon = 0.35, lat = 65)),
      margin = list(l=0, r=0, t=0, b=0),
      showlegend = FALSE) %>% 
    style(hoverinfo = 'none')
  
  # Create an object for each plot
  map_diversity <- map_habitat <- map_thermal <- map_species <- fig
  
  
  
  # Start by ploting/printing placeholders ----
  #output$mainMap <- renderPlotly({fig})
  
  output$tableATitle <- renderText({ "Select a map to start" })
  
  
  
  # Tab actions ----
  # Set first tab
  active_tab <- reactiveValues()
  active_tab$current <- "species"
  
  # Observe tab changes
  observeEvent(input$jsValue, {
    # If there is a new value:
    if (active_tab$current != input$jsValue$id) {
      output$mainMap <- renderPlotly({eval(parse(text = paste0("map_", 
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
    if (title_state$current != active_tab$current) {
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
  # Create reactives for species/tab info
  react_species <- reactiveValues(status = 0)
  react_thermal <- reactiveValues(status = 0)
  react_habitat <- reactiveValues(status = 0)
  react_diversity <- reactiveValues(status = 0)
  
  observe({
    react_species$plot_raster <- rast("data/maps/species/key=100803/mv1_pr/lasso_naive/predictions/mv1_pr_lasso_naive_key100803_basevars_current.tif")
    react_species$status <- react_species$status + 1
  }) %>%
    bindEvent(input$speciesSelect, ignoreInit = TRUE)
  
  # TODO: add habitat and diversity

  # Update map if new status
  observe({
    
    # raster_data <- as.data.frame(react_species$plot_raster, xy = T)
    # colnames(raster_data)[3] <- "values"
    # 
    # map_species <- plot_ly() %>%
    #   add_trace(
    #     x = ~x, y = ~y,
    #     data = raster_data,
    #     type = 'heatmap',
    #     z = 'values'
    #   ) %>%
    #   # add_trace(
    #   #   type="choroplethmapbox",
    #   #   geojson=study_area,
    #   #   locations=0,
    #   #   z=1,
    #   #   colorscale="Magma",
    #   #   featureidkey="properties.FID",
    #   #   marker=list(line=list(
    #   #     width=0),
    #   #     opacity=0.5
    #   #   ),
    #   #   showlegend = F, 
    #   #   showscale = FALSE
    #   # ) %>%
    #   # # # Add full screen
    #   # config(
    #   #   modeBarButtonsToAdd = list(
    #   #     list(button_fullscreen())
    #   #   )) %>%
    #   # # Add mapbox
    #   layout(
    #     mapbox = list(
    #       style = 'open-street-map',
    #       zoom =2,
    #       center = list(lon = 0.35, lat = 65),
    #       images = list(
    #     list(
    #       source = img,
    #       x = 0,
    #       y = 1,
    #       sizex = 2,
    #       sizey = 2,
    #       xref = "x",
    #       yref = "y",
    #       opacity = 0.8,
    #       layer = "below"
    #     )
    #   )),
    #     margin = list(l=0, r=0, t=0, b=0),
    #     showlegend = FALSE) %>% 
    #   style(hoverinfo = 'none')
    # 
    # 
    # output$mainMap <- renderPlotly({map_species})
    
    output$mainMap <- renderLeaflet(
      leaflet() %>%
        addTiles() %>%
        # enableTileCaching() %>%
        # addTiles(
        #   "http://127.0.0.1:3746/map/t/{z}/{x}/{y}?level=900&time=2000-04-27 01:00:00&alpha=0.5",
        #   options = tileOptions(useCache = TRUE, crossOrigin = TRUE)
        # ) %>%
        setView(zoom = 3, lat = 30, lng = 30)
    )
    
  }) %>%
    bindEvent(react_species$status, ignoreInit = TRUE)
  
  
  #   #bindEvent(title_state$current)
  # 
  # output$tableATitle <- renderText({ "Select a map to start" })
  # output$graphTitle <- renderText({ "" })
  # output$tableBTitle <- renderText({ "" })
  # output$textTitle <- renderText({ "" })
  
  # output$textModel <- renderText({ "" })
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # the_color <- reactiveVal("red")
  # # Store in a convenience variable
  # cdata <- session$clientData
  # # Values from cdata returned as text
  # output$clientdataText <- renderText({
  #   cnames <- names(cdata)
  #   
  #   allvalues <- lapply(cnames, function(name) {
  #     paste(name, cdata[[name]], sep = " = ")
  #   })
  #   paste(allvalues, collapse = "\n")
  # })
  # 
  # observeEvent(input$jsValue, {
  #   cat("Changed \n")
  #   print(input$jsValue)
  #   the_color(sample(colors(), 1))
  # })
  # 
  # 
  # # You can access the value of the widget with input$text, e.g.
  # output$selectedSpecies <- renderText({ input$speciesSelect })
  # 
  # output$contextSpecies <- renderText({
  #   validate(
  #     need(input$speciesSelect != '', 'Select a species to begin.')
  #   )
  #   "Eastern Atlantic: southward from Trondheim Fjord (including North Sea and western Baltic) and Mediterranean Sea (including Sea of Marmara, Bosporus and southwestern Black Sea) (Ref. 4710). Elsewhere, southward to Senegal (Ref. 4710), including Cape Verde (Ref. 5304)."
  # })
  # 
  # output$selectedSpeciesThermal <- renderText({ input$speciesSelectThermal })
  # 
  # output$contextSpeciesThermal <- renderText({
  #   validate(
  #     need(input$speciesSelectThermal != '', 'Select a species to begin.')
  #   )
  #   "Eastern Atlantic: southward from Trondheim Fjord (including North Sea and western Baltic) and Mediterranean Sea (including Sea of Marmara, Bosporus and southwestern Black Sea) (Ref. 4710). Elsewhere, southward to Senegal (Ref. 4710), including Cape Verde (Ref. 5304)."
  # })
  # 
  # 
  # 
  # 
  # output$table <- DT::renderDataTable(DT::datatable({
  #   data <- ggplot2::mpg
  # }))
  # 
  # # Only triggered by the debounced reactive
  # output$distPlot <- renderPlot({
  #   x    <- faithful[, 1]
  #   bins <- seq(min(x), max(x), length.out = 5 + 1)
  #   hist(x, breaks = bins, col = the_color(), border = 'white',
  #        main = sprintf("Histogram of %s", 1))
  # })
  # 
  # output$tableATitle <- renderText({ "Model metrics" })
  # output$graphTitle <- renderText({ "Response curves" })
  # output$tableBTitle <- renderText({ "Variables importance" })
  # output$textTitle <- renderText({ "Model explanation" })
  # output$textModel <- renderText({ "This is a Maxent model.
  #   Are you curious.
  #   Let's talk about it." })
}