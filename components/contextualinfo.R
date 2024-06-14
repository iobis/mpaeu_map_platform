# Contextual info
continfo <- reactiveValues()

observe({
  
  if (active_tab$current == "species") {
    req(!is.null(model_inuse$model))
    spkey <- speciesinfo$key[speciesinfo$species == input$speciesSelect]
    
    basepath <- paste0("data/maps/taxonid=", spkey, "/model=inteval/metrics/")
    
    # Table 1
    metrics <- arrow::read_parquet(
      paste0(basepath, "taxonid=", spkey, "_model=inteval_method=", model_inuse$model,#input$modelSelect, 
             "_what=cvmetrics.parquet")
    )
    if (model_inuse$model == "ensemble") {
      metrics <- metrics %>% filter(what == "mean") %>% filter(origin == "avg_fit") %>% select(-origin, -what)
    }
    metrics_mean <- metrics %>%
      summarise(across(1:ncol(metrics), function(x) {round(mean(x), 2)}))
    metrics_mean <- metrics_mean %>%
      tidyr::pivot_longer(cols = 1:ncol(metrics_mean), names_to = "Metric", values_to = "Mean of 5 folds") %>%
      mutate(Metric = toupper(Metric))
    metrics_sd <- metrics %>%
      summarise(across(1:ncol(metrics), function(x) {round(sd(x), 2)}))
    metrics_sd <- metrics_sd %>%
      tidyr::pivot_longer(cols = 1:ncol(metrics_sd), names_to = "Metric", values_to = "SD") %>%
      mutate(Metric = toupper(Metric))
    metrics_mean <- left_join(metrics_mean, metrics_sd, by = "Metric")
    # metrics_mean <- metrics_mean %>%
    #   filter(Metric %in% c("AUC", "CBI", "PR", "TSS_P10"))
    continfo$tableA <- metrics_mean
    
    # Table 2
    varimp <- arrow::read_parquet(
      paste0(basepath, "taxonid=", spkey, "_model=inteval_method=", model_inuse$model,#input$modelSelect,
             "_what=varimportance.parquet")
    )
    continfo$tableB <- varimp
    
    # Graph
    response_curves <- arrow::read_parquet(
      paste0(basepath, "taxonid=", spkey, "_model=inteval_method=", model_inuse$model,#input$modelSelect,
             "_what=respcurves.parquet")
    )
    # response_curves <- response_curves %>%
    #   group_by(variable) %>%
    #   mutate(base = scale(base))
    # pa <- ggplot(response_curves) +
    #   geom_line(aes(x = base, y = response, color = variable)) +
    #   theme_light() +
    #   xlab("Value (scaled)") + ylab("Response")
    
    continfo$plotA <- gen_plotly_resp(response_curves)
    
    # Text
    continfo$text[[1]] <- paste("Model explanation -",
                                switch(strtrim(input$modelSelect, 3),
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
    continfo$text[[2]] <- switch(strtrim(input$modelSelect, 3),
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
    
  }
  
  if (active_tab$current == "thermal") {
    
    spkey <- speciesinfo$key[speciesinfo$species == input$speciesSelectThermal]
    
    thermal_envelope <- jsonlite::read_json(
      paste0("../mpaeu_sdm/results/taxonid=", spkey, "/model=inteval/metrics/taxonid=", spkey, "_model=inteval_what=thermmetrics.json"))
    
    thermal_envelope$limits[[1]]$decade <- NA
    limits <- data.frame(do.call("rbind", thermal_envelope$limits))
    colnames(limits) <- c("Q5%", "Q50%", "Q95%", "Mean", "SD", "Scenario", "Decade")
    limits <- limits[,c(6,7,1:5)]
    
    thermal_envelope$areas[[1]]$decade <- NA
    areas <- data.frame(do.call("rbind", thermal_envelope$areas))
    colnames(areas) <- c("Area (km²)", "Scenario", "Decade")
    areas <- areas[,c(2,3,1)]
    
    sst <- rast(paste0("data/thetao_baseline_",thermal_envelope$sst_depth[[1]],"_mean_cog.tif"))
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
    continfo$text[[1]] <- "How this data is extracted?"
    continfo$text[[2]] <- "Short explanation."
  }
  
}) %>%
  bindEvent(c(input$speciesSelect, input$modelSelect,
              input$speciesSelectThermal), ignoreInit = TRUE)