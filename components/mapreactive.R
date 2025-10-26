########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
######################### Main reactive for map change #########################

# Start server code ------
# Update map with study area to begin
leafletProxy("mainMap") |>
  leafem::addFeatures(starea, fillColor = "#184e77", fill = T)

# Objects ------
# Palettes
main_palette <- RColorBrewer::brewer.pal(9, "Blues")
alt_palette <- rev(c("#7d1500", "#da4325", "#eca24e", "#e7e2bc", "#5cc3af", "#0a6265"))
uncertainty_palette <- RColorBrewer::brewer.pal(9, "Oranges")
hab_palette <- RColorBrewer::brewer.pal("PuRd", n = 9)
div_palette <- RColorBrewer::brewer.pal("PuRd", n = 9)
binary_palette <- c("#f7fbff", "#08519c")
binary_palette_alt <- c("#f7fbff", "#db9f07")

# Create waiters
wMap <- waiter::Waiter$new(
  id = "mainMap",
  color = "#ffffff00",
  html = htmltools::div(
    htmltools::tags$span("Preparing map", bsicons::bs_icon("globe-central-south-asia")),
    style = "color: #8e929a; font-size: 24px; font-weight: 700; white-space: nowrap; display: inline-block; background-color: rgba(255, 255, 255, 0.95); border-radius: 10px; padding: 20px;"
  )
)

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

# Create objects to hold info
boot <- reactiveValues(status = FALSE, legend = htmltools::HTML("Likelihood </br> of occurrence"))
files_inuse <- reactiveValues(file_a = NULL, file_b = NULL)
files_inuse_habdiv <- reactiveValues(file_habitat = NULL, file_diversity = NULL)

# Functions ------
source("scripts/mapreactive_functions.R", local = TRUE)

# Map observer -----
observe({
  # Debugging information
  mdebug("Executing map reactive")
  mdebug(active_tab$current)
  wMap$show()
  on.exit({
    wMap$hide()
    session$sendCustomMessage("additionalInfoTrigger", "")
  })

  state_debug(FALSE)
  proxy <- init_proxy(
    image_1 = previous_state$image_1,
    image_2 = previous_state$image_2,
    pm_toolbar = previous_state$pm_toolbar,
    markers = previous_state$markers,
    study_area = previous_state$study_area,
    side_by_side = previous_state$side_by_side
  )

  # Species tab
  if (active_tab$current == "species") {
    if (select_params$species$species != "") {
      if (input$ecspBoot) {
        file_type <- "uncertainty"
        boot$status <- TRUE
        boot$legend <- htmltools::HTML("Uncertainty")
        unc <- TRUE
      } else {
        file_type <- "prediction"
        boot$status <- FALSE
        boot$legend <- htmltools::HTML("Likelihood </br> of occurrence")
        unc <- FALSE
      }

      min_val <- threshold_table() |>
        pull_thresh(select_params$species$model, input$ecspBin)

      layer_1 <- db_info$species |>
        extract_sp(
          sc = select_params$species$scenario, pe = select_params$species$decade,
          me = select_params$species$model, ty = file_type
        )
      if (select_params$species$side_select) {
        layer_1 <- db_info$species |>
          extract_sp(me = select_params$species$model, ty = file_type)
        layer_2 <- db_info$species |>
          extract_sp(
            sc = select_params$species$scenario, pe = select_params$species$decade,
            me = select_params$species$model, ty = file_type
          )
        pstate_side()
      } else {
        layer_2 <- NULL
        pstate_single()
      }
      proxy |> add_layer_sp(layer_1, layer_2, min_val, uncertainty = unc)
      files_inuse$file_a <- layer_1
      files_inuse$file_b <- layer_2
    } else {
      proxy |> clean_proxy()
      pstate_reset()
    }
    # Thermal tab
  } else if (active_tab$current == "thermal") {
    if (select_params$thermal$species_t != "") {
      maskstate(FALSE)
      file_type <- "thermenvelope"
      boot$status <- FALSE
      boot$legend <- htmltools::HTML("Thermal range")
      layer_2 <- layer_1 <- db_info$thermal |>
        extract_sp(ty = file_type)
      bands_list <- c(
        "current", paste(
          rep(paste0("ssp", c(126, 245, 370, 460, 585)), each = 2), c("dec50", "dec100"),
          sep = "_"
        )
      )
      if (select_params$thermal$side_select_t) {
        sel_band_1 <- which(bands_list == "current")
        sel_band_2 <- which(bands_list == paste(
          select_params$thermal$scenario_t,
          select_params$thermal$decade_t,
          sep = "_"
        ))
        pstate_side()
      } else {
        if (select_params$thermal$scenario_t == "current") {
          sel_band_1 <- which(bands_list == "current")
        } else {
          sel_band_1 <- which(bands_list == paste(
            select_params$thermal$scenario_t,
            select_params$thermal$decade_t,
            sep = "_"
          ))
        }
        sel_band_2 <- NULL
        layer_2 <- NULL
        pstate_single()
      }
      proxy |> add_layer_sp(layer_1, layer_2,
        band_1 = sel_band_1, band_2 = sel_band_2, binary = TRUE
      )
      files_inuse$file_a <- layer_1
      files_inuse$file_b <- layer_2
    } else {
      proxy |> clean_proxy()
      pstate_reset()
    }
    # Habitat tab
  } else if (active_tab$current == "habitat") {
    if (select_params$habitat$habitat != "") {
      layer_1 <- db_info$habitat |>
        extract_hab(
          th = select_params$habitat$threshold_h,
          pt = select_params$habitat$model_h,
          ty = select_params$habitat$bintype_h,
          sc = select_params$habitat$scenario_h,
          pe = select_params$habitat$decade_h
        )

      proxy |> add_layer_hab(layer_1)
      files_inuse_habdiv$file_habitat <- layer_1
      pstate_single()
    } else {
      proxy |> clean_proxy()
      pstate_reset()
    }
    # Diversity tab
  } else if (active_tab$current == "diversity") {
    if (select_params$diversity$metric != "") {
      layer_1 <- db_info$diversity |>
        extract_div(
          gr = select_params$diversity$group,
          th = select_params$diversity$threshold_d,
          pt = select_params$diversity$posttreat_d,
          ty = select_params$diversity$type_d,
          sc = select_params$diversity$scenario_d,
          pe = select_params$diversity$decade_d,
          me = select_params$diversity$metric
        )
      legend_val <- ifelse(select_params$diversity$metric == "richness",
                           "Number <br> of species", "LCBD")
      proxy |> add_layer_div(layer_1, legend = legend_val)
      files_inuse_habdiv$file_diversity <- layer_1
      pstate_single()
      previous_state$markers <- FALSE
    } else {
      proxy |> clean_proxy()
      pstate_reset()
    }
  } else if (active_tab$current == "atlas") {
    proxy |> clean_proxy()
  }
}) |>
  bindEvent(
    select_params$species,
    select_params$thermal,
    select_params$habitat,
    select_params$diversity,
    active_tab$current,
    input$ecspBoot,
    input$ecspBin,
    ignoreInit = TRUE
  )


# Mask layer ------
# Create a reactive with mask state
maskstate <- reactiveVal(TRUE)

# Observe changes on tab
observe({
  mdebug("Changing mask state based on tab")
  if (active_tab$current == "species" || active_tab$current == "thermal") {
    maskstate(TRUE)
  } else {
    maskstate(FALSE)
  }
}) |> bindEvent(active_tab$current)

# Observe changes via mask command (eye symbol)
observe({
  mdebug("Mask JS")
  maskstate(!maskstate())
}) |> bindEvent(input$jsMask)

# Turn mask on or off based on mask state
observe({
  mdebug("Processing mask")

  wMask$show()
  on.exit({
    wMask$hide()
  })

  proxy <- leafletProxy("mainMap")
  remove_mask <- FALSE
  mask_layer <- NULL

  if (active_tab$current %in% c("species", "thermal")) {
    if (active_tab$current == "species") {
      if (is.null(db_info$species)) {
        remove_mask <- TRUE
      } else {
        mask_layer <- db_info$species |>
          select(-available_models) |>
          tidyr::unnest(files) |>
          filter(type == "mask") |>
          pull()
      }
    } else if (active_tab$current == "thermal") {
      if (is.null(db_info$thermal)) {
        remove_mask <- TRUE
      } else {
        mask_layer <- db_info$thermal |>
          select(-available_models) |>
          tidyr::unnest(files) |>
          filter(type == "mask") |>
          pull()
      }
    }
  } else {
    remove_mask <- TRUE
  }

  if (remove_mask) {
    proxy |>
      removeImage(layerId = "mapMask") |>
      removeControl(layerId = "maskControl")
      session$sendCustomMessage("removeEye", "nothing")
  }
  
  if (!maskstate()) {
    mdebug("Mask deactivated")
    proxy |>
      removeImage(layerId = "mapMask") |>
      removeControl(layerId = "maskControl")
  } else {
    req(!is.null(mask_layer))
    mdebug("Mask activated")
    mask_layer <- gsub("what=what=", "what=", mask_layer) # TEMP - TO REMOVE
    avmasks <- c(
      "native_ecoregions", "fit_ecoregions", "fit_region",
      "fit_region_max_depth", "convex_hull", "minbounding_circle", "buffer100m"
    )
    sel_mask <- input$ecspMask
    which_band <- match(sel_mask, avmasks)
    proxy |>
      addGeotiff(
        file = mask_layer,
        opacity = 1,
        layerId = "mapMask",
        bands = which_band,
        options = pathOptions(pane = "maskPane"),
        colorOptions = colorOptions(
          palette = c("#d4dadc", "#0a626500"),
          domain = c(0, 1),
          na.color = NA
        ), autozoom = F
      ) |>
      addControl(tags$div(HTML('<span style="font-weight: bold; color: #184e77;">Mask active</span>')),
        position = "topright", layerId = "maskControl"
      )
  }
}) |>
  bindEvent(
    maskstate(),
    input$ecspMask,
    input$additionalInfo,
    ignoreInit = TRUE
  )


observe({
  mdebug("Processing realms")
  proxy <- leafletProxy("mainMap")

  if (input$ecspRealms) {
    wShapes$show()
    on.exit({
      wShapes$hide()
    })
    proxy |> leaflet::addPolygons(
      data = realms, color = "#454545", opacity = 0.3,
      popup = ~ as.character(Realm), fillColor = ~ colorQuantile("YlOrRd", Realm)(Realm),
      fillOpacity = 0.05, weight = 2, layerId = paste0("realmsShape", 1:nrow(realms)), options = pathOptions(pane = "extraPane")
    )
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
    proxy |> leaflet::addPolygons(
      data = eez, color = "#454545", opacity = 0.3,
      popup = ~EEZ, fillColor = "#0d7edb",
      fillOpacity = 0.05, weight = 2, layerId = paste0("eezShape", seq_len(nrow(eez))), options = pathOptions(pane = "extraPane")
    )
  } else {
    proxy |> leaflet::removeShape(layerId = paste0("eezShape", seq_len(nrow(eez))))
  }
}) |>
  bindEvent(input$ecspEEZ, ignoreInit = TRUE)