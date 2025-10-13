########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
##################### Add/update contextual information ########################

# Create waiters
w <- waiter::Waiter$new(
  id = c("tableA", "plotA", "tableB"),
  color = "white",
  html = htmltools::div(
    "Loading data...",
    htmltools::br(), htmltools::br(),
    waiter::spin_1(), style = "color: #8e929a; font-size: 18px; font-weight: 700"
  )
)

# Create a reactive list for contextual info
continfo <- reactiveValues()

# Observe input changes
observe({

  mdebug("Contextual info triggered")
  w$show()
  on.exit({
    w$hide()
  })

  if (active_tab$current == "species" & select_params$species$species == "") {
    continfo$text <- continfo$tableA <- continfo$tableB <- continfo$plotA <- NULL
  } else if (active_tab$current == "thermal" & select_params$thermal$species_t == "") {
    message('thermal aqui oh')
    continfo$text <- continfo$tableA <- continfo$tableB <- continfo$plotA <- NULL
  } else if (active_tab$current == "habitat" & select_params$habitat$habitat == "") {
    continfo$text <- continfo$tableA <- continfo$tableB <- continfo$plotA <- NULL
  } else if (active_tab$current == "diversity" & select_params$diversity$metric == "") {
    continfo$text <- continfo$tableA <- continfo$tableB <- continfo$plotA <- NULL
  }
  
  # If active is species
  if (active_tab$current == "species") {
    req(!is.null(db_info$species))
    
    species_files <- db_info$species |>
      select(-available_models) |>
      tidyr::unnest(files)

    # Table 1
    metrics <- species_files |>
      filter(type == "cvmetrics", method == select_params$species$model) |> #check model_inuse$model
      pull() |>
      arrow::read_parquet()
    if (select_params$species$model == "ensemble") {
      metrics <- metrics |> filter(what == "mean") |> filter(origin == "avg_fit") |> select(-origin, -what)
    }
    metrics_mean <- metrics |>
      summarise(across(1:ncol(metrics), function(x) {round(mean(x), 2)}))
    metrics_mean <- metrics_mean |>
      tidyr::pivot_longer(cols = 1:ncol(metrics_mean), names_to = "Metric", values_to = "Mean of 5 folds") |>
      mutate(Metric = toupper(Metric))
    metrics_sd <- metrics |>
      summarise(across(1:ncol(metrics), function(x) {round(sd(x), 2)}))
    metrics_sd <- metrics_sd |>
      tidyr::pivot_longer(cols = 1:ncol(metrics_sd), names_to = "Metric", values_to = "SD") |>
      mutate(Metric = toupper(Metric))
    metrics_mean <- left_join(metrics_mean, metrics_sd, by = "Metric")

    metrics_mean <- metrics_mean |> tidyr::separate_wider_delim(cols = Metric, delim = "_",
       names = c("Metric", "Threshold"),
       too_few = "align_start") |>
      mutate(Threshold = case_when(
        Threshold == "MAXSSS" ~ "Max. Sens. + Spec.",
        Threshold == "MTP" ~ "Min. train. pres.",
        Threshold == "P10" ~ "10th perc. train. pres."
      ))
    # metrics_mean <- metrics_mean |>
    #   filter(Metric %in% c("AUC", "CBI", "PR", "TSS_P10"))
    continfo$tableA <- metrics_mean
    
    # Table 2
    varimp <- species_files |>
      filter(type == "varimportance", method == select_params$species$model) |> #check model_inuse$model
      pull() |>
      arrow::read_parquet()

    varimp <- varimp |>
      tidyr::separate_wider_delim(cols = variable, delim = "_",
       names = c("variable", "variant"),
       too_few = "align_start") |>
      mutate(variable = case_when(
        variable == "tas" ~ "Air temperature",
        variable == "siconc" ~ "Sea ice concentration",
        variable == "thetao" ~ "Sea temperature",
        variable == "bathymetry" ~ "Bathymetry",
        variable == "distcoast" ~ "Distance to coast",
        variable == "sws" ~ "Sea water speed",
        variable == "wavefetch" ~ "Wavefetch",
        variable == "so" ~ "Salinity",
        variable == "no3" ~ "Nitrate",
        variable == "par" ~ "Photosynthetically Available Radiation",
        variable == "rugosity" ~ "Rugosity",
        variable == "o2" ~ "Oxygen concentration"
      )) |>
      mutate(variant = case_when(
        is.na(variant) ~ "",
        variant == "mean" ~ "Mean",
        variant == "max" ~ "Maximum",
        variant == "min" ~ "Minimum",
        variant == "range" ~ "Range"
      )) |>
      select(Variable = variable, Variant = variant, Mean = mean, SD = sd)

    continfo$tableB <- varimp
    
    # Graph
    response_curves <- species_files |>
      filter(type == "respcurves", method == select_params$species$model) |> #check model_inuse$model
      pull() |>
      arrow::read_parquet()
    
    continfo$plotA <- gen_plotly_resp(response_curves)
    
    # Text
    continfo$text[[1]] <- paste("Model explanation -",
                                switch(strtrim(select_params$species$model, 3),
                                       brt = "BRT",
                                       las = "LASSO",
                                       ela = "elasticnet",
                                       max = "MAXENT",
                                       rf_ = "Random Forest",
                                       glm = "GLM",
                                       gam = "GAM",
                                       xgb = "XGBoost",
                                       lgb = "LightGBM",
                                       ens = "Ensemble"))
    
    context_file <- jsonlite::read_json("www/context_info.json")
    continfo$text[[2]] <- switch(strtrim(select_params$species$model, 3),
                                 brt = unlist(context_file$models[["brt"]]),
                                 las = unlist(context_file$models[["lasso"]]),
                                 ela = unlist(context_file$models[["elasticnet"]]),
                                 max = unlist(context_file$models[["maxent"]]),
                                 rf_ = unlist(context_file$models[["rf"]]),
                                 glm = unlist(context_file$models[["glm"]]),
                                 gam = unlist(context_file$models[["gam"]]),
                                 xgb = unlist(context_file$models[["xgboost"]]),
                                 lgb = unlist(context_file$models[["lightgbm"]]),
                                 ens = unlist(context_file$models[["ensemble"]]))
    
  } else if (active_tab$current == "thermal") {   # If active tab is thermal
    
    req(!is.null(db_info$thermal))

    thermal_files <- db_info$thermal |>
      select(-available_models) |>
      tidyr::unnest(files)

    thermal_envelope <- thermal_files |>
      filter(type == "thermmetrics") |> #check model_inuse$model
      pull() |>
      jsonlite::read_json()
    
    # Table 1
    thermal_envelope$limits[[1]]$decade <- NA
    limits <- data.frame(do.call("rbind", thermal_envelope$limits))
    colnames(limits) <- c("Q5%", "Q50%", "Q95%", "Mean", "SD", "Scenario", "Decade")
    limits <- limits[,c(6,7,1:5)]
    limits$Scenario <- dplyr::case_when(
      limits$Scenario == "current" ~ "Current",
      limits$Scenario == "ssp126" ~ "SSP1 (2.6)",
      limits$Scenario == "ssp245" ~ "SSP2 (4.5)",
      limits$Scenario == "ssp370" ~ "SSP3 (7.0)",
      limits$Scenario == "ssp460" ~ "SSP4 (6.0)",
      limits$Scenario == "ssp585" ~ "SSP5 (8.5)"
    )
    limits$Decade <- dplyr::case_when(
      is.na(limits$Decade) ~ NA,
      limits$Decade == "dec50" ~ 2050,
      limits$Decade == "dec100" ~ 2100
    )
    
    # Table 2
    thermal_envelope$areas[[1]]$decade <- NA
    areas <- data.frame(do.call("rbind", thermal_envelope$areas))
    colnames(areas) <- c("Area (km²)", "Scenario", "Decade")
    areas <- areas[,c(2,3,1)]
    areas$Scenario <- limits$Scenario
    areas$Decade <- limits$Decade
    
    # Graph
    sst <- terra::rast(paste0("data/thetao_baseline_",thermal_envelope$sst_depth[[1]],"_mean_cog.tif"))
    sst_data <- terra::extract(sst, speciespts()[,1:2], ID = F)
    colnames(sst_data) <- "sst"
    
    limits_ob <- data.frame(
      what = c("Median current", "Q5%", "Q95%", "Median SSP1(2.6)-2100",
               "Median SSP2(4.5)-2100", "Median SSP3(7.0)-2100",
               "Median SSP4(6.0)-2100", "Median SSP5(8.5)-2100"),
      values = unlist(c(limits$`Q50%`[1],limits$`Q5%`[1], limits$`Q95%`[1],
                        limits$`Q50%`[c(3,5,7,9,11)]))
    )
    
    p <- ggplot() +
      geom_density(data = sst_data, aes(x = sst), fill = "grey90", color = "grey60") +
      geom_vline(data = limits_ob, aes(xintercept = values, color = what, linetype = what)) +
      scale_linetype_manual(values = c(rep(1,6), 2,2), name = NULL) +
      scale_color_manual(values = c(c("#E01156", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594"),
                                    "grey50", "grey70"), name = NULL) +
      xlab("Sea temperature (°C)") + ylab("Density") +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0.1,0)) +
      labs(caption = "Median values extracted using the current distribution.") +
      theme_light() +
      theme(
        legend.key.spacing.y = ggplot2::unit(0.2, "lines"),
        panel.grid = element_blank()
      )
    
    continfo$tableA <- limits
    continfo$tableB <- areas
    continfo$plotA <- plotly::ggplotly(p)
    
    # Text
    continfo$text[[1]] <- "How this data is extracted?"
    continfo$text[[2]] <- "Thermal ranges are extracted based on the occurrence data and SST data from Bio-ORACLE v3.0 (https://www.bio-oracle.org/). For each occurrence record we extract the temperature and then calculates a kernel density. This follow the method developed on the 'speedy' package (https://github.com/iobis/speedy)."
  } else if (active_tab$current == "habitat") { # If active tab is habitat
  
    req(!is.null(db_info$habitat))
    
    # Table 1
    # hab_eez <- arrow::read_parquet(paste0(
    #   "https://mpaeu-dist.s3.amazonaws.com/results/habitat/habitat=", sp_info$habitat,
    #   "_model=", sp_info$acro_h, 
    #   "_what=eezstats.parquet"
    # ))
    hab_eez <- data.frame(value = NA)
    # hab_eez <- hab_eez |>
    #   filter(method == sp_info$model_h) |>
    #   filter(scen == ifelse(sp_info$scenario_h == "current",
    #                                          "current", paste0(sp_info$scenario_h, "_", sp_info$decade_h))) |>
    #   filter(threshold == input$threshold_h)
    
    # Table 2
    hab_sel_species <- data.frame(value = NA)
    # hab_file <- jsonlite::read_json(paste0("https://mpaeu-dist.s3.amazonaws.com/results/habitat/habitat=", sp_info$habitat, 
    #   "_model=", sp_info$acro_h, "_what=log.json"))
    # hab_file_sp <- unlist(hab_file$species)
    # hab_sel_species <- speciesinfo_full |> # Change to speciesinfo in next version!
    #   select(AphiaID, scientificName, kingdom, phylum, class, order,
    #   family, genus, authority, gbif_speciesKey, gbif_scientificName, common_names) |>
    #   filter(AphiaID %in% hab_file_sp)
    # colnames(hab_sel_species)[10:12] <- c("GBIF speciesKey", "GBIF scientificName", "Common names")

    # Graph
    # base <- rnaturalearth::ne_countries(returnclass = "sf")
    # hab_true_data <- "data/teste.pnd"
    # if (file.exists(hab_true_data)) {
    #   hab_true_data <- arrow::read_parquet(hab_true_data)
    #   p <- ggplot() +
    #     geom_sf(data = base, color = "grey70", fill = "grey70") +
    #     geom_point(data = hab_true_data, aes(x = x, y = y)) +
    #     xlab(NULL) + ylab(NULL) +
    #     coord_sf() +
    #     theme_light()
    # } else {
    #   p <- ggplot() +
    #     geom_sf(data = base, color = "grey70", fill = "grey70") +
    #     geom_text(data = data.frame(label = "No data available", x = 0, y = 0), aes(x = x, y = y, label = label)) +
    #     xlab(NULL) + ylab(NULL) +
    #     coord_sf() +
    #     theme_light()
    # }
    
    continfo$tableA <- hab_eez
    continfo$tableB <- hab_sel_species
    continfo$plotA <- plotly::ggplotly(ggplot2::ggplot(data.frame(x = 1, y = 1)) + ggplot2::geom_point(aes(x = x, y = y)))
    
    # Text
    continfo$text[[1]] <- "What is a biogenic habitat?"
    continfo$text[[2]] <- "A biogenic marine habitat is an environment created by living organisms, such as corals, seagrasses, mangroves, or oysters, that form complex structures in marine ecosystems. These habitats provide shelter, feeding grounds, and breeding areas for various marine species, enhancing biodiversity. They are crucial for ecosystem functions, such as nutrient cycling and shoreline protection. Examples include coral reefs, kelp forests, and oyster beds. Biogenic habitats are sensitive to environmental changes and human activities, making their conservation vital for maintaining marine biodiversity."
  } else if (active_tab$current == "diversity") { # If active tab is diversity
    
    req(!is.null(db_info$diversity))
    
    # Table 1
    # if (sp_info$group != "all" & input$modelSelectDiversity != "raw") {
    #   scenario_f <- ifelse(sp_info$scenario_d == "current",
    #                     sp_info$scenario_d, paste0(sp_info$scenario_d, "_", sp_info$decade_d))
    #   eez_f <- glue::glue("https://mpaeu-dist.s3.amazonaws.com/results/diversity/metric={sp_info$metric}_model=mpaeu_method={sp_info$model_d}_scen={scenario_f}_group={sp_info$group}_type={sp_info$div_type}_area=eez.txt")
    #   table_eez <- read.table(eez_f, header = T)
    #   colnames(table_eez) <- c("EEZ/Protected area code", "Number of species")

    #   mpa_f <- glue::glue("https://mpaeu-dist.s3.amazonaws.com/results/diversity/metric={sp_info$metric}_model=mpaeu_method={sp_info$model_d}_scen={scenario_f}_group={sp_info$group}_type={sp_info$div_type}_area=mpa.txt")
    #   table_mpa <- read.table(mpa_f)
    #   colnames(table_mpa) <- c("EEZ/Protected area code", "Number of species")

    #   table_a <- rbind(table_eez, table_mpa)
    # } else {
    #   table_a <- data.frame()
    # }
    table_a <- data.frame(value = NA)
    
    # Table 2
    if (sp_info$group == "all") {
      table_b <- div_sp_list |> collect()
    } else {
      table_b <- div_sp_list |>
        filter(tolower(group) == sp_info$group) |>
        collect()
    }
    if (sp_info$model_d != "raw") {
      table_b <- table_b[table_b[[sp_info$model_d]], ]
    }
    table_b <- table_b[, c("taxonID", "scientificName", "group")]
    
    # Graph
    # base <- rnaturalearth::ne_countries(returnclass = "sf")
    # p <- ggplot() +
    #   geom_sf(data = base, color = "grey70", fill = "grey70") +
    #   geom_text(data = data.frame(x = 0, y = 0, label = "Information not available"), aes(x = x, y = y, label = label)) +
    #   #geom_point(data = data.frame(x = rnorm(10), y = rnorm(10)), aes(x = x, y = y)) +
    #   xlab(NULL) + ylab(NULL) +
    #   coord_sf() +
    #   theme_light()

    
    continfo$tableA <- table_a
    continfo$tableB <- table_b
    continfo$plotA <- plotly::ggplotly(ggplot2::ggplot(data.frame(x = 1, y = 1)) + ggplot2::geom_point(aes(x = x, y = y)))
    
    # Text
    continfo$text[[1]] <- paste("What is", ifelse(
      select_params$diversity$metric == "lcbd", "LCBD", stringr::str_to_title(select_params$diversity$metric)
    ))
    continfo$text[[2]] <- "Species richness refers to the number of different species present in a specific area or ecosystem. It is a measure of biodiversity, indicating how many unique species are found in a given habitat, without considering their abundance. High species richness suggests a diverse ecosystem, while low species richness may indicate a more homogeneous or disturbed environment. It is commonly used in ecological studies to assess the health and complexity of ecosystems."
  }
  
}) |>
  bindEvent(
    db_info$species,
    db_info$thermal,
    db_info$habitat,
    db_info$diversity,
    active_tab$current,
    ignoreInit = TRUE)