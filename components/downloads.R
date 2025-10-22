########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
########################## Manage download requests ############################

# Species report ----
report_download_modal <- function(species, key, model) {
  model_ed <- ifelse(grepl("rf", model), "RandomForest", toupper(model))
  modalDialog(
    bslib::layout_column_wrap(
      width = 1, heights_equal = "row",
      bslib::card(
        bslib::card_header("Download species report"),
        bslib::card_body(
          htmltools::span(
            htmltools::HTML(
              glue::glue("The current species selected is <i><b>{species}</b></i> (AphiaID <b>{key}</b>) for model <b>{model_ed}</b>")
            ),
            style = "color: #097da5"
          ),
          shiny::uiOutput("speciesDownloadStatus"),
          full_screen = F
        )
      ),
      bslib::card(
        bslib::card_body(
           htmltools::h5("Note"),
          "The report takes a few minutes to generate. During this time, it is not possible to use the app.",
          htmltools::h5("Tips"),
          "The report is downloaded in html format. You can convert it to
                         PDF by opening it in your browser and then printing/saving as a PDF. You can also use the
                         function `pagedown::chrome_print()` on R to convert the document to PDF.",
          full_screen = F
        )
      )
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("startSpeciesReport", label = "Generate report")
    ), size = "m"
  )
}

observe({
   showModal(
    report_download_modal(select_params$species$species, select_params$species$spkey, select_params$species$model)
   )
}) |>
  bindEvent(input$speciesReportAction)

download_result <- reactiveValues(original = NULL, copy = NULL)

observe({
  removeModal()
  showModal(
    modalDialog(
      htmltools::div(
        htmltools::span("Generating species report, that may take a few minutes...",
          style = "color: #097da5; padding-top: 10px; font-size: 25px"
        ), htmltools::br(), htmltools::br(),
        htmltools::span(class="loader"), 
        style = "display: flex; flex-direction: column; justify-content: center; align-items: center;"
      ),
      footer = NULL
    )
  )

  file <- paste0("taxonid=", select_params$species$spkey, "_date=", format(Sys.time(), "%Y-%m-%d_%H-%M%Z"), "_report.html")

  mdebug("Starting report download")

  tdir <- tempdir()
  outfolder <- fs::dir_create(paste0(tdir, "/temp_aphiaid_", select_params$species$spkey, "_", sample(1:10000, 1)))

  basepath <- getwd()

  mdebug("Trying report generation")
  species_files <- db_info$species |>
    select(-available_models) |>
    tidyr::unnest(files)
  rep_result <- try(gen_quarto_report(
    outfolder, basepath,
    select_params$species$spkey, select_params$species$model, select_params$species$species, select_params$species$acro,
    species_files
  ))

  if (inherits(rep_result, "try-error")) {
    print(rep_result)
    rep_result <- NULL
  } else {
    mdebug(paste("File created:", rep_result, "Output:", file))
  }

  download_result$original <- rep_result
  download_result$copy <- file

  removeModal()
  showModal(
    modalDialog(
      htmltools::span("Your report is ready.",
        style = "color: #097da5; padding-top: 10px; font-size: 25px"
      ),
      footer = downloadButton("downloadSpeciesReport")
    )
  )
}) |>
  bindEvent(input$startSpeciesReport)

output$downloadSpeciesReport <- downloadHandler(
  filename = function() {
    download_result$copy
  },
  content = function(file) {
    on.exit(removeModal())
    if (is.null(download_result$original)) {
      pdf(gsub("html", "pdf", file))
      plot.new()
      text(x = 0.5, y = 0.5, labels = "Report failed. Try again or contact the OBIS team: helpdesk@obis.org")
      dev.off()
    } else {
      file.copy(download_result$original, file)
    }
  }
)


# Species data download ------
source("scripts/download_handlers_species.R", local = TRUE)
source("scripts/create_local_fit_file.R", local = TRUE)

observe({
  showModal(
    species_download_modal(
      species = select_params$species$species,
      key = select_params$species$spkey,
      model = select_params$species$model,
      scenario = select_params$species$scenario,
      decade = select_params$species$decade,
      acro = select_params$species$acro
  )
  )
}) |>
  bindEvent(input$downloadDataSpecies)

downloadFiles <- reactiveValues(files = NULL)

observe({
  showModal(
    modalDialog(
      htmltools::div(
          htmltools::span("Your download is being prepared, please wait.",
            style = "color: #097da5; padding-top: 10px; font-size: 25px"
          ), htmltools::br(), htmltools::br(),
          htmltools::span(class="loader"), 
          style = "display: flex; flex-direction: column; justify-content: center; align-items: center;"
        ),
      footer = NULL
    )
  )

  # Prepare download files
  all_files <- db_info$species |>
      select(-available_models) |>
      tidyr::unnest(files)
  tfold <- tempdir(check = TRUE)
  all_files <- all_files$file
  if (input$speciesDownloadType == "selected") {
    model_f <- all_files[grepl(paste0("method=", select_params$species$model), all_files)]
    model_scen_f <- model_f[grepl(select_params$species$scenario, model_f)]
    if (select_params$species$scenario != "current") {
      model_scen_f <- model_scen_f[grepl(select_params$species$decade, model_scen_f)]
    }
    model_f <- model_f[grepl("cvmetrics|respcurves|fullmetrics|varimp", model_f)]
    other_f <- all_files[grepl("shape_|mess_|mask_|thresholds|log.j|fitocc", all_files)]
    sel_files <- c(model_scen_f, model_f, other_f)
  } else {
    sel_files <- all_files
  }
  addr <- "https://obis-maps.s3.us-east-1.amazonaws.com/"
  sel_files_ed <- gsub(addr, "", sel_files)
  directories <- dirname(sel_files_ed)
  directories <- unique(gsub("sdm/species/", "", directories))
  fs::dir_create(file.path(tfold, directories))
  for (k in seq_along(sel_files)) {
    download.file(sel_files[k], file.path(tfold, gsub("sdm/species/", "", sel_files_ed[k])))
  }
  #download.file(paste0(addr, sel_files), file.path(tfold, sel_files), method = "libcurl")
  sel_files <- list.files(tfold, full.names = T)
  sel_files <- sel_files[!grepl("vscode-R", sel_files)]
  sel_files <- sel_files[grepl(paste0("taxonid=", select_params$species$spkey), sel_files)]

  # Assign to list
  downloadFiles$files <- sel_files

  removeModal()

  showModal(modalDialog(
    htmltools::span("The data is ready for download.",
            style = "color: #097da5; padding-top: 10px; font-size: 25px"
    ),
    footer = downloadButton("downloadSpeciesAction", label = "Download")
  ))
}) |>
  bindEvent(input$downloadSpeciesInterm)

output$downloadSpeciesAction <- downloadHandler(
  filename = function() {
    pf <- paste0("taxonid=", select_params$species$spkey)
    if (input$speciesDownloadType == "selected") {
      pf <- paste0(pf, "_model=", select_params$species$acro, "_method=", select_params$species$model, "_scenario=", select_params$species$scenario,
      "_decade=", select_params$species$decade, ".zip")
    } else {
      pf <- paste0(pf, ".zip")
    }
    pf
  },
  content = function(file) {
    on.exit({
      removeModal()
      fs::dir_delete(downloadFiles$files)
    })
    zip::zip(file, downloadFiles$files, mode = "cherry-pick")
  }
)

# Species code download ------
observe({
  showModal(
    species_code_download(
      species = select_params$species$species,
      key = select_params$species$spkey,
      model = select_params$species$model
  )
  )
}) |>
  bindEvent(input$runModelSpecies)

output$downloadCodeSpeciesAction <- downloadHandler(
  filename = function() {
    pf <- paste0("taxonid=", select_params$species$spkey, "_modelcode")
    pf <- paste0(pf,
                 ifelse(input$speciesCodeDownloadType == "ipynb", ".ipynb", ".qmd"))
    pf
  },
  content = function(file) {
    if (input$speciesCodeDownloadType == "ipynb") {
      model_code_f <- get_local_file(select_params$species$species, select_params$species$spkey, select_params$species$model)
    } else {
      model_code_f <- get_local_file(select_params$species$species, select_params$species$spkey, select_params$species$model, type = "qmd")
    }
    on.exit(removeModal())
    writeLines(model_code_f, file)
  }
)


# Thermal data download ------
output$downloadDataThermal <- downloadHandler(
  filename = function() {
    paste0("taxonid=", select_params$species$spkey_t, "_model=", select_params$species$acro_t, "_what=thermenvelope.parquet")
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
          "https://mpaeu-dist.s3.amazonaws.com/", "results/species/taxonid=", select_params$species$spkey_t, "/model=", select_params$species$acro_t, "/predictions/taxonid=",
          select_params$species$spkey_t, "_model=", select_params$species$acro_t, "_what=thermenvelope.parquet"
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
