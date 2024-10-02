########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
########################## Manage download requests ############################

# Species report ----
output$downloadSpeciesReport <- downloadHandler(
  filename = function() {
    spkey <- speciesinfo$key[speciesinfo$species == input$speciesSelect]
    paste0("taxonid=", spkey, "_date=", format(Sys.time(), "%Y-%m-%d_%H-%M%Z"), "_report.pdf")
  },
  content = function(file) {
    # showModal(modalDialog("Preparing report. This might take a while.", footer=NULL))
    shinyalert(
      title = "Preparing your download",
      text = "This may take a while. You can keep using the app.",
      size = "xs", closeOnEsc = TRUE, closeOnClickOutside = TRUE, html = FALSE, type = "info",
      showConfirmButton = TRUE, showCancelButton = FALSE, confirmButtonText = "OK", confirmButtonCol = "#184E77",
      timer = 0, imageUrl = "", animation = TRUE
    )
    # on.exit(removeModal())
    on.exit(shinyalert::closeAlert())
    spkey <- speciesinfo$key[speciesinfo$species == input$speciesSelect]
    model <- input$modelSelect
    tdir <- tempdir()
    outfolder <- fs::dir_create(paste0(tdir, "/temp_aphiaid_", spkey, "_", sample(1:10000, 1)))
    mdebug("getting wd")
    basepath <- getwd()
    mdebug(basepath)
    rep_result <- try(gen_quarto_report(file, outfolder, basepath, spkey, model))
    if (inherits(rep_result, "try-error")) {
      pdf(file)
      plot.new()
      text(x = 0.5, y = 0.5, labels = "Report failed. Try again or contact the OBIS team: helpdesk@obis.org")
      dev.off()
    }
    if (dir.exists(outfolder)) fs::dir_delete(outfolder)
  }
)


# Species data download ------
source("scripts/download_handlers_species.R", local = TRUE)

observe({
  showModal(
    species_download_modal(
      species = sp_info$species,
      key = sp_info$spkey,
      model = sp_info$model,
      scenario = sp_info$scenario,
      decade = sp_info$decade,
      acro = sp_info$acro
  )
  )
}) %>%
  bindEvent(input$downloadDataSpecies)

output$downloadSpeciesAction <- downloadHandler(
  filename = function() {
    pf <- paste0("taxonid=", sp_info$spkey)
    if (input$speciesDownloadType == "selected") {
      pf <- paste0(pf, "_model=", sp_info$acro, "_method=", sp_info$model, "_scenario=", sp_info$scenario,
      "_decade=", sp_info$decade, ".zip")
    } else {
      pf <- paste0(pf, ".zip")
    }
    pf
  },
  content = function(file) {
    all_files <- list.files(paste0("data/maps/taxonid=", sp_info$spkey, "/model=", sp_info$acro),
     recursive = T, full.names = T)
    if (input$speciesDownloadType == "selected") {
      model_f <- all_files[grepl(sp_info$model, all_files)]
      model_scen_f <- model_f[grepl(sp_info$scenario, model_f)]
      model_scen_f <- model_scen_f[grepl(sp_info$decade, model_scen_f)]
      model_f <- model_f[grepl("cvmetrics|respcurves|fullmetrics|varimp", model_f)]
      other_f <- all_files[grepl("shape_|mess_|thresholds|log.j", all_files)]
      sel_files <- c(model_scen_f, model_f, other_f)
    } else {
      sel_files <- all_files
    }
    on.exit(removeModal())
    zip::zip(file, sel_files, mode = "cherry-pick")
  }
)


# Thermal data download ------
output$downloadDataThermal <- downloadHandler(
  filename = function() {
    paste0("taxonid=", sp_info$spkey_t, "_model=", sp_info$acro_t, "_what=thermenvelope.parquet")
  },
  content = function(file) {
    shinyalert(
      title = "Preparing your download",
      text = paste("Downloading", file),
      size = "xs", closeOnEsc = TRUE, closeOnClickOutside = TRUE, html = FALSE, type = "info",
      showConfirmButton = TRUE, showCancelButton = FALSE, confirmButtonText = "OK", confirmButtonCol = "#184E77",
      timer = 0, imageUrl = "", animation = TRUE
    )
    on.exit(shinyalert::closeAlert())

    if (input$speciesSelectThermal != "" & active_tab$current == "thermal") {
      thermal_envelope <- paste0(
          "data/maps/taxonid=", sp_info$spkey_t, "/model=", sp_info$acro_t, "/predictions/taxonid=",
          sp_info$spkey_t, "_model=", sp_info$acro_t, "_what=thermenvelope.parquet"
        )
      p <- sfarrow::st_read_parquet(thermal_envelope)
      sfarrow::st_write_parquet(p, file)
    } else {
      NULL
    }
  }
)


# Habitat data download ------
output$downloadDataHabitat <- downloadHandler(
  filename = function() {
    basename(files_inuse_habdiv$file_habitat)
  },
  content = function(file) {
    shinyalert(
      title = "Preparing your download",
      text = paste("Downloading", basename(files_inuse_habdiv$file_habitat)),
      size = "xs", closeOnEsc = TRUE, closeOnClickOutside = TRUE, html = FALSE, type = "info",
      showConfirmButton = TRUE, showCancelButton = FALSE, confirmButtonText = "OK", confirmButtonCol = "#184E77",
      timer = 0, imageUrl = "", animation = TRUE
    )
    on.exit(shinyalert::closeAlert())
    r <- terra::rast(files_inuse_habdiv$file_habitat)
    writeRaster(r, file, overwrite = T)
  }
)

# Diversity data download ------
output$downloadDataDiversity <- downloadHandler(
  filename = function() {
    basename(files_inuse_habdiv$file_diversity)
  },
  content = function(file) {
    shinyalert(
      title = "Preparing your download",
      text = paste("Downloading", basename(files_inuse_habdiv$file_diversity)),
      size = "xs", closeOnEsc = TRUE, closeOnClickOutside = TRUE, html = FALSE, type = "info",
      showConfirmButton = TRUE, showCancelButton = FALSE, confirmButtonText = "OK", confirmButtonCol = "#184E77",
      timer = 0, imageUrl = "", animation = TRUE
    )
    on.exit(shinyalert::closeAlert())
    r <- terra::rast(files_inuse_habdiv$file_diversity)
    writeRaster(r, file, overwrite = T)
  }
)


# output$downloadModelContent <- downloadHandler(
#     filename = function() {
#       spkey <- speciesinfo$key[speciesinfo$species == input$speciesSelect]
#       paste0("taxonid=", spkey,"_date=", format(Sys.time(), "%Y-%m-%d-%H:%M%Z"), "_report.pdf")
#     },
#     content = function(file) {
#       # showModal(modalDialog("Preparing report. This might take a while.", footer=NULL))
#       shinyalert(
#             title = "Preparing your download",
#             text = "This may take a while. You can keep using the app.",
#             size = "xs",  closeOnEsc = TRUE, closeOnClickOutside = TRUE, html = FALSE, type = "info",
#             showConfirmButton = TRUE, showCancelButton = FALSE, confirmButtonText = "OK", confirmButtonCol = "#184E77",
#             timer = 0, imageUrl = "", animation = TRUE)
#       #on.exit(removeModal())
#       on.exit(shinyalert::closeAlert())
#       spkey <- speciesinfo$key[speciesinfo$species == input$speciesSelect]
#       model <- input$modelSelect
#       outfolder <- fs::dir_create(paste0("temp_aphiaid_", tg_species, "_", sample(1:10000, 1)))
#       rep_result <- try(gen_quarto_report(file, outfolder, spkey, model))
#       if (inherits(rep_result, "try-error")) {
#         pdf(file)
#         plot.new()
#         text(x = 0.5, y = 0.5, labels = "Report failed. Try again or contact the OBIS team: helpdesk@obis.org")
#         dev.off()
#       }
#       if (dir.exists(outfolder)) fs::dir_delete(outfolder)
#     }
#   )
#
# output$downloadModelCode <- downloadHandler(
#     filename = function() {
#       spkey <- speciesinfo$key[speciesinfo$species == input$speciesSelect]
#       paste0("taxonid=", spkey,"_date=", format(Sys.time(), "%Y-%m-%d-%H:%M%Z"), "_report.pdf")
#     },
#     content = function(file) {
#       # showModal(modalDialog("Preparing report. This might take a while.", footer=NULL))
#       shinyalert(
#             title = "Preparing your download",
#             text = "This may take a while. You can keep using the app.",
#             size = "xs",  closeOnEsc = TRUE, closeOnClickOutside = TRUE, html = FALSE, type = "info",
#             showConfirmButton = TRUE, showCancelButton = FALSE, confirmButtonText = "OK", confirmButtonCol = "#184E77",
#             timer = 0, imageUrl = "", animation = TRUE)
#       #on.exit(removeModal())
#       on.exit(shinyalert::closeAlert())
#       spkey <- speciesinfo$key[speciesinfo$species == input$speciesSelect]
#       model <- input$modelSelect
#       outfolder <- fs::dir_create(paste0("temp_aphiaid_", tg_species, "_", sample(1:10000, 1)))
#       rep_result <- try(gen_quarto_report(file, outfolder, spkey, model))
#       if (inherits(rep_result, "try-error")) {
#         pdf(file)
#         plot.new()
#         text(x = 0.5, y = 0.5, labels = "Report failed. Try again or contact the OBIS team: helpdesk@obis.org")
#         dev.off()
#       }
#       if (dir.exists(outfolder)) fs::dir_delete(outfolder)
#     }
#   )
