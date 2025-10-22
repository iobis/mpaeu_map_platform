gen_plotly_resp <- function(respcurves) {
  
  vars <- unique(respcurves$variable)
  n_vars <- length(vars)
  
  resp_list <- split(respcurves, respcurves$variable)
  resp_list <- resp_list[vars]
  
  traces <- vector("list", n_vars)
  for (i in seq_len(n_vars)) {
    dat <- resp_list[[i]]
    traces[[i]] <- list(
      y = dat$response,
      x = dat$base,
      name = dat$variable[1],
      type = "scatter",
      mode = "lines",
      visible = if (i == 1) TRUE else FALSE
    )
  }
  
  visible_matrix <- matrix(FALSE, nrow = n_vars, ncol = n_vars)
  diag(visible_matrix) <- TRUE
  
  buttons <- vector("list", n_vars)
  for (i in seq_len(n_vars)) {
    buttons[[i]] <- list(
      method = "update",
      args = list(
        list(visible = as.list(visible_matrix[, i])),
        list(xaxis = list(title = vars[i]))
      ),
      label = vars[i]
    )
  }
  
  p <- plot_ly()
  for (i in seq_len(n_vars)) {
    p <- do.call(add_trace, c(list(p), traces[[i]]))
  }
  
  p %>%
    layout(
      xaxis = list(domain = c(0.1, 1), zeroline = FALSE),
      yaxis = list(title = "Response", zeroline = FALSE),
      showlegend = FALSE,
      updatemenus = list(list(y = 1.1, x = 0.4, buttons = buttons))
    )
}

# Generate quarto report
gen_quarto_report <- function(folder, basepath, species_aphia, model, sp_name, acronym, db_files) {
  
  tg_species <- species_aphia

  outfolder <- folder
  
  base_f <- readLines("scripts/map_output_model.qmd", warn = F)
  
  pred_base <- '

```{r}
#| echo: false
#| fig-width: 8
#| fig-height: 6
rastobj <- paste0("/vsicurl/", "SCEN_FILE") |>
    rast(lyrs = 1) |>
    mask(masks) |>
    crop(e)
plot_map(rastobj, wrld_pol = wrld, title = "TITLE")
```

'
  
  pred_list <- data.frame(scenario = c("current", rep(paste0("ssp", c(126, 245, 370, 460, 585)), each = 2)),
                          scenario_t = c("current", rep(paste0("ssp", c("1 (2.6)", "2 (4.5)", 
                                                                        "3 (7.0)", "4 (6.0)", 
                                                                        "5 (8.5)")), each = 2)),
                          period = c(NA, rep(c("dec50", "dec100"), 5)))
  pred_list$name_code <- paste0(pred_list$scenario, ifelse(is.na(pred_list$period), "", paste0("_", pred_list$period)))
  db_files$name_code <- paste0(db_files$scenario, ifelse(is.na(db_files$period), "", paste0("_", db_files$period)))

  pred_files <- db_files |>
    filter(type == "prediction") |>
    filter(method == model) |>
    mutate(name_code = paste0(scenario, ifelse(is.na(period), "", paste0("_", period)))) |>
    select(name_code, file)
  pred_list <- dplyr::left_join(pred_list, pred_files, by = "name_code")

  sp_code <- as.numeric(tg_species)

  # Construct predictions
  pred_code <- c()
  for (z in seq_len(nrow(pred_list))) {
    scen <- paste(pred_list[z,c(1,3)], collapse = "_")
    scen_title <- paste(pred_list[z,c(2,3)], collapse = "_")
    if (grepl("NA", scen)) {
      scen <- gsub("_NA", "", scen)
      scen_title <- gsub("_NA", "", scen_title)
      scen_title <- stringr::str_to_title(scen)
    } else {
      scen_title <- toupper(gsub("_", " - ", scen_title))
      scen_title <- gsub("DEC50", "2050", scen_title)
      scen_title <- gsub("DEC100", "2100", scen_title)
    }
    pred_base_t <- gsub("TITLE", scen_title, pred_base)
    pred_base_t <- gsub("SCEN_FILE", pred_list$file[z], pred_base_t)
    pred_code <- c(pred_code, pred_base_t)
  }
  
  base_f_new <- gsub("\\{SPECIES_NAME}", sp_name, base_f)
  base_f_new <- gsub("\\{ACRO_CODE}", acronym, base_f_new)
  base_f_new <- gsub("\\{SPECIES_CODE}", sp_code, base_f_new)
  base_f_new <- gsub("\\{MODEL_CODE}", model, base_f_new)
  base_f_new <- gsub("\\{WD_PATH}", basepath, base_f_new)

  # Files
  base_f_new <- db_files |>
    filter(type == "mask") |>
    pull(file) |>
    (\(x)  gsub("\\{MASK_FILE}", gsub("what=what=", "what=", x), base_f_new))()
  base_f_new <- db_files |>
    filter(type == "log") |>
    pull(file) |>
    (\(x)  gsub("\\{LOG_FILE}", x, base_f_new))()
  base_f_new <- db_files |>
    filter(type == "fitocc") |>
    pull(file) |>
    (\(x)  gsub("\\{PTS_FILE}", x, base_f_new))()
  base_f_new <- db_files |>
    filter(type == "cvmetrics", method == model) |>
    pull(file) |>
    (\(x)  gsub("\\{CV_FILE}", x, base_f_new))()
  base_f_new <- db_files |>
    filter(type == "varimportance", method == model) |>
    pull(file) |>
    (\(x)  gsub("\\{VARIMP_FILE}", x, base_f_new))()
  base_f_new <- db_files |>
    filter(type == "respcurves", method == model) |>
    pull(file) |>
    (\(x)  gsub("\\{RC_FILE}", x, base_f_new))()
  
  pred_l <- which(grepl("\\{PREDICTIONS}", base_f_new))
  
  top <- 1:(pred_l - 1)
  
  down <- (pred_l + 1):length(base_f_new)
  
  new_code_cont <- c(base_f_new[top], pred_code, base_f_new[down])
  
  new_file <- paste0(outfolder, "/taxonid=", sp_code, "_model=", acronym, "_what=report.qmd")
  
  writeLines(new_code_cont, new_file)

  quarto::quarto_render(new_file, output_format = "html", quiet = F)
  
  return(gsub("qmd", "html", new_file))
  
}


verify_posteval <- function(log_obj) {
  
  nrec <- unlist(log_obj$model_fit_points)
  neval <- unlist(log_obj$model_eval_points)
  
  av_models <- names(log_obj$model_posteval)[!grepl("niche", names(log_obj$model_posteval))]
  mod_cont <- lapply(av_models, function(x){
    if (length(log_obj$model_posteval[[x]]) > 0) {
      TRUE
    } else {
      FALSE
    }
  })
  av_models <- av_models[unlist(mod_cont)]
  
  if (length(av_models) > 1) {
    priority <- c("ensemble", "maxnet", "rf_classification_ds", "xgboost", "glm")
    av_models <- priority[priority %in% av_models][1]
  }
  
  if (length(av_models) > 0) {
    tenvstat <- unlist(log_obj$model_posteval[[av_models]]$thermal_envelope[[1]]$status)
    tenvstat <- ifelse(grepl("inside", tenvstat), "Yes", "No")
    tenvval <- unlist(log_obj$model_posteval[[av_models]]$thermal_envelope[[1]]$percentage)
  } else {
    tenvstat <- tenvval <- "Not available"
  }
  
  return(list(
    nrec = nrec, neval = neval,
    tenvstat = tenvstat, tenvval = tenvval,
    model = av_models
  ))
  
}


s3_exists <- function(file) {
  bucket <- sub("https://([^.]+)\\.s3\\.amazonaws\\.com.*", "\\1", file)
  object <- sub("https://[^/]+\\.s3\\.amazonaws\\.com/", "", file)
  aws.s3::object_exists(object, bucket = bucket)
}


gen_context_boxes <- function(model_quality = "Not assessed",
                             reviewed = "No",
                             conservation_status = "Not available",
                             type = "species") {

  if (is.null(model_quality) || is.na(model_quality) || length(model_quality) < 1) {
    return("")
  }

  if (model_quality == "Good") {
    mqc <- "#199651"
  } else if (model_quality == "Average") {
    mqc <- "#C18820"
  } else if (model_quality == "Poor"){
    mqc <- "#a51c3c"
  } else {
    mqc <- "#B4B4B4"
  }

  if (reviewed == "not_evaluated") {
    rec <- "#a51c3c"
    reviewed <- "No"
  } else {
    rec <- "#199651"
    reviewed <- "Yes"
  }

  if (conservation_status == "Not available") {
    csc <- "#adadad"
  } else if (conservation_status == "DD") {
    csc <- "#727272"
  } else if (conservation_status == "LC") {
    csc <- "#51bc1d"
  } else if (conservation_status == "NT") {
    csc <- "#97c115"
  } else if (conservation_status == "VU") {
    csc <- "#ffc90e"
  } else if (conservation_status == "EN") {
    csc <- "#f28533"
  } else if (conservation_status == "CR") {
    csc <- "#c52412"
  } else if (conservation_status == "EW") {
    csc <- "#85618d"
  } else {
    csc <- "white"
  }

  tooltip_1 <- bslib::tooltip("Model quality",
    "This takes into account both the peer-review of the models and the metrics.",
    placement = "auto")
  tooltip_2 <- bslib::tooltip("Reviewed",
    "Model revision done by specialists.",
    placement = "auto")
  tooltip_3 <- bslib::tooltip("Threatened status",
    "According to IUCN Red List.",
    placement = "auto")
  
  if (type == "species") {
    html_content <- glue::glue(
      '
<div id="quality-box" style="display: flex; margin-top: 14px; margin-bottom: 6px; gap: 20px;">
  <div>
    <span style="font-weight: bold;">Expert review</span>
    <div id="review-box" style=" display: flex; border: solid 0.1em; border-radius: 5px; padding: 3px; border-color: #cccccc;">
      <div style="display: flex; flex-direction: column; padding-left: 5px; padding-right: 10px;">
        <span style="text-align: center; font-size: smaller;">{tooltip_2}</span> <span style="background-color: {rec}; border-radius: 5px; padding: 3px; color: white; text-align: center;">{reviewed}</span>
      </div>
      <div style="display: flex; flex-direction: column; padding-right: 5px;">
        <span style="text-align: center; font-size: smaller;">{tooltip_1}</span> <span style="background-color: {mqc}; border-radius: 5px; padding: 3px; color: white; text-align: center;">{model_quality}</span>
      </div>
    </div>
  </div>
  <div>
    <span>&nbsp;</span>
    <div id="redlist-box" style=" display: flex; border: solid 0.1em; border-radius: 5px; padding: 3px; border-color: #cccccc00;">
      <div style="display: flex; flex-direction: column; padding-right: 10px;">
        <span style="text-align: center; font-size: smaller;">{tooltip_3}</span> <span style="background-color: {csc}; border-radius: 5px; padding: 3px; color: white; text-align: center;">{conservation_status}</span>
      </div>
    </div>
  </div>
</div>
      '
    )
    modal_link <- shiny::actionLink("modelQualityButton", htmltools::HTML(
      ifelse(
        model_quality != "Not assessed",
        '<i class="bi bi-clipboard-data"></i> Model evaluation details',
        '<i class="bi bi-clipboard-data"></i> Help to evaluate this model'
      )
    ), style = "color: #07A5F0 !important;")
    #modal_link <- gsub("><i class", ' style="color: #07A5F0 !important;"><i class', modal_link)
    html_content <- glue::glue(
      '<div style="display: flex; flex-direction: column;">{html_content}{modal_link}</div>'
    )
  } else if (type == "habitat") {
    html_content <- ""
  } else {
    html_content <- ""
  }

  return(html_content)
}


citation_mod <- function(spkey, cit_species_ds, cit_general_ds) {

  sp_data <- cit_species_ds |> 
    filter(key == spkey) |> 
    collect()

  sp_context <- cit_general_ds |> 
    select(-source, -description) |> 
    filter(dataset_id %in% sp_data$dataset_id) |> 
    collect()

  sp_data <- left_join(sp_data, sp_context, by = "dataset_id")

  colnames(sp_data) <- c("AphiaID", "Dataset ID", "Records", "Source", "Title", "Citation", "Nodes", "DOI")

  sp_data$Source <- toupper(sp_data$Source)

  nodes_info <- strsplit(sp_data$Nodes, "\\|")
  nodes_info <- lapply(nodes_info, function(x){
    x <- strsplit(x, ";")
    xe <- unlist(lapply(x, function(x) x[2]), use.names = F)
    paste(xe, collapse = ", ")
  })
  sp_data$Nodes <- unlist(nodes_info, use.names = F)

  reactable::reactable(sp_data, searchable = T)
  
}

evaluation_modal <- function(evalcontent) {
  if (is.null(evalcontent) || evalcontent$status == "not_evaluated") {
    "We are preparing a tool to enable peer-review of models by users. Meanwhile, if you spotted something that needs immediate action, contact helpdesk@obis.org"
  } else {
    htmltools::tags$div(
      reactable::reactable(evalcontent$evaluations, searchable = T),
      htmltools::tags$span(htmltools::tags$br(), "We are extremely thankful to the evaluator(s) who voluntarily provided time to improve this tool.", htmltools::tags$br(), htmltools::tags$hr()),
      htmltools::tags$div(htmltools::HTML(
        "<p><strong>Evaluation summary</strong></p>",
        paste("<p>Average score:", evalcontent$summary$average_score, " (from 1-5, being 1 the best)</p>"),
        paste("<p>Best evaluator score:", evalcontent$summary$best_score_n, " (equivalent to question 3)</p>"),
        paste("<p>Best model CBI score:", evalcontent$summary$cbi_score, " (this CBI score is converted to 1-5 scale, being 1 the best, as follows: >=0.7 - 1 = 1, >=0.6 - 0.7 = 2, >=0.5 - 0.6 = 3, >=0.4 - 0.5 = 4, >=0.3 - 0.4 = 5)</p>"),
        paste("<p>Average model CBI score ± SD:", evalcontent$summary$average_cbi, "±", evalcontent$summary$sd_cbi, "</p>"),
        paste("<p>Best model:", toupper(evalcontent$summary$best_cbi), "</p>")
      )),
      style = "font-size: smaller;"
    )
  }
}