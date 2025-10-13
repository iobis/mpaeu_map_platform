########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
########################### Control mask over map ##############################

# Create waiters
wMask <- waiter::Waiter$new(
  id = "mainMap",
  color = "#ffffff00",
  html = htmltools::div(
    htmltools::tags$span("Loading mask", bsicons::bs_icon("eye-fill")),
    style = "color: #8e929a; font-size: 24px; font-weight: 700; white-space: nowrap; display: inline-block; background-color: rgba(255, 255, 255, 0.95); border-radius: 10px; padding: 20px;"
  )
)

wShapes <- waiter::Waiter$new(
  id = "mainMap",
  color = "#ffffff00",
  html = htmltools::div(
    htmltools::tags$span("Loading shape", bsicons::bs_icon("back")),
    style = "color: #8e929a; font-size: 24px; font-weight: 700; white-space: nowrap; display: inline-block; background-color: rgba(255, 255, 255, 0.95); border-radius: 10px; padding: 20px;"
  )
)

# Create a reactive with mask state ----
maskstate <- reactiveVal(TRUE)

# Observe changes on tab ----
observe({
  mdebug("Changing mask state based on tab")
  if (active_tab$current == "species" || active_tab$current == "thermal") {
    maskstate(TRUE)
  } else {
    maskstate(FALSE)
  }
}) |> bindEvent(active_tab$current)

# Observe changes via mask command (eye symbol) ----
observe({
  mdebug("Mask JS")
  maskstate(!maskstate())
}) |> bindEvent(input$jsMask)

# Turn mask on or off based on mask state ----
observe({
  #req(input$speciesSelect)
  mdebug("Processing mask")

  wMask$show()
  on.exit({
    wMask$hide()
  })

  proxy <- leafletProxy("mainMap")

  if (active_tab$current %in% c("species", "thermal")) {
    if (active_tab$current == "species") {
      req(!is.null(db_info$species))
      mask_layer <- db_info$species |>
        select(-available_models) |>
        tidyr::unnest(files) |>
        filter(type == "mask") |>
        pull()
    } else if (active_tab$current == "thermal") {
      if (is.null(db_info$thermal) & !is.null(db_info$species)) {
        proxy |> removeImage(layerId = "mapMask") |>
          removeControl(layerId = "maskControl")
        session$sendCustomMessage("removeEye", "nothing")
      }
      req(!is.null(db_info$thermal))
      mask_layer <- db_info$thermal |>
        select(-available_models) |>
        tidyr::unnest(files) |>
        filter(type == "mask") |>
        pull()
    }
    mask_layer <- gsub("what=what=", "what=", mask_layer) #TEMP - TO REMOVE
    avmasks <- c(
      "native_ecoregions", "fit_ecoregions", "fit_region",
      "fit_region_max_depth", "convex_hull", "minbounding_circle", "buffer100m"
    )
    sel_mask <- input$ecspMask
    which_band <- match(sel_mask, avmasks)
  }
  
  if (!maskstate()) {
    mdebug("Mask deactivated")
    proxy |> removeImage(layerId = "mapMask") |>
      removeControl(layerId = "maskControl")
  } else {
    mdebug("Mask activated")
    proxy |> 
      addGeotiff(file = mask_layer,
                 opacity = 1,
                 layerId = "mapMask",
                 bands = which_band,
                 options = pathOptions(pane = "maskPane"),
                 colorOptions = colorOptions(
                   palette = c("#d4dadc", "#0a626500"),
                   domain = c(0,1),
                   na.color = NA
                 ), autozoom = F) |>
      addControl(tags$div(HTML('<span style="font-weight: bold; color: #184e77;">Mask active</span>')),
                 position = "topright", layerId = "maskControl")
  }
  
}) |>
  bindEvent(maskstate(), input$ecspMask, 
  input$additionalInfo, ignoreInit = TRUE
  )


observe({
  mdebug("Processing realms")
  proxy <- leafletProxy("mainMap")
  
  if (input$ecspRealms) {
    wShapes$show()
    on.exit({
      wShapes$hide()
    })
    proxy |> leaflet::addPolygons(data = realms, color = "#454545", opacity = 0.3,
      popup = ~as.character(Realm), fillColor = ~colorQuantile("YlOrRd", Realm)(Realm),
      fillOpacity = 0.05, weight = 2, layerId = paste0("realmsShape", 1:nrow(realms)), options = pathOptions(pane = "extraPane"))
  } else {
    proxy |> leaflet::removeShape(layerId = paste0("realmsShape", 1:nrow(realms)))
  }
  
}) |>
  bindEvent(input$ecspRealms, ignoreInit = TRUE)

observe({
  mdebug("Processing EEZ")
  proxy <- leafletProxy("mainMap")
  
  if (input$ecspEEZ) {
    wShapes$show()
    on.exit({
      wShapes$hide()
    })
    proxy |> leaflet::addPolygons(data = eez, color = "#454545", opacity = 0.3,
      popup = ~EEZ, fillColor = "#0d7edb",
      fillOpacity = 0.05, weight = 2, layerId = paste0("eezShape", 1:nrow(eez)), options = pathOptions(pane = "extraPane"))
  } else {
    proxy |> leaflet::removeShape(layerId = paste0("eezShape", 1:nrow(eez)))
  }
  
}) |>
  bindEvent(input$ecspEEZ, ignoreInit = TRUE)