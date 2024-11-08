gen_plotly_resp <- function(respcurves) {
  
  vars <- unique(respcurves$variable)
  
  obj <- plot_ly()
  
  for (i in vars) {
    #print(i)
    if (i == vars[1]) {
      obj <- obj %>%
        add_trace(y = respcurves$response[respcurves$variable == i],
                  x = respcurves$base[respcurves$variable == i], name = i,
                  type = 'scatter', mode = 'lines')
    } else {
      obj <- obj %>%
        add_trace(y = respcurves$response[respcurves$variable == i],
                  x = respcurves$base[respcurves$variable == i], name = i,
                  visible = FALSE, type = 'scatter', mode = 'lines')
    }
  } 
  
  lobjs <- lapply(vars, function(i){
    eval(parse(text = glue::glue(
      'list(method = "update",
               args = list(list(visible = list({paste(rep(FALSE, length(vars)), collapse = ",")})),
                           list(xaxis = list(title = "{i}"))),
               label = "{i}")'
    )))
  })
  
  for (i in 1:length(vars)) {
    lobjs[[i]]$args[[1]]$visible[[i]] <- TRUE
  }
  
  
  
  po_final <- obj %>%
    layout(
      xaxis = list(domain = c(0.1, 1),
                   zeroline = F),
      yaxis = list(title = "Response",
                   zeroline = F),
      showlegend = FALSE,
      updatemenus = list(
        list(
          y = 1.1, x = 0.4,
          buttons = lobjs)
      )
    )
  
  return(po_final)
  
}


# Generate quarto report
gen_quarto_report <- function(folder, basepath, species_aphia, model, sp_name, acronym) {
  
  tg_species <- species_aphia
  
  base_f <- readLines("scripts/map_output_model.qmd", warn = F)
  
  outfolder <- folder
  
  pred_base <- '

```{r echo=FALSE, message=FALSE, warning=FALSE}
which_scenario <- SCENARIO
r <- retrieve(sp, paste0("method=", which_model, "_scen=", which_scenario), acro = model_acro, results_folder = outfolder)
r <- r[[1]]

r <- terra::mask(r, masks[[1]])
r <- terra::crop(r, ecoreg_sel)
r <- r/100
r <- terra::aggregate(r, fact = 10)
r <- as.data.frame(r, xy=T)
colnames(r)[3] <- "vals"

ext_dat <- terra::ext(ecoreg_sel)

ggplot() +
  geom_sf(data = wrld, color = "grey70", fill = "grey80") +
  geom_raster(data = r, aes(x = x, y = y, fill = vals)) +
  sca(c(0, 1), "ROC") +
  ggtitle("TITLE") +
  ylab(NULL) + xlab(NULL) +
  coord_sf(crs = "EPSG:4326", xlim = c(ext_dat[1:2]), ylim = c(ext_dat[3:4])) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90"),
        legend.position = "bottom",
        legend.title = element_text(hjust = 0.5))
```

'
  
  pred_list <- data.frame(scenario = c("current", rep(paste0("ssp", c(126, 245, 370, 460, 585)), each = 2)),
                          scenario_t = c("current", rep(paste0("ssp", c("1 (2.6)", "2 (4.5)", 
                                                                        "3 (7.0)", "4 (6.0)", 
                                                                        "5 (8.5)")), each = 2)),
                          period = c(NA, rep(c("dec50", "dec100"), 5)))
  
  sp_code <- as.numeric(tg_species)

  # Construct predictions
  pred_code <- c()
  for (z in 1:nrow(pred_list)) {
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
    pred_base_t <- gsub("SCENARIO", paste0("'", scen, "'"), pred_base_t)
    pred_code <- c(pred_code, pred_base_t)
  }
  
  base_f_new <- gsub("\\{SPECIES_NAME}", sp_name, base_f)
  base_f_new <- gsub("\\{ACRO_CODE}", acronym, base_f_new)
  base_f_new <- gsub("\\{SPECIES_CODE}", sp_code, base_f_new)
  base_f_new <- gsub("\\{MODEL_CODE}", model, base_f_new)
  base_f_new <- gsub("\\{WD_PATH}", basepath, base_f_new)
  
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