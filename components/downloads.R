# Downloads

# Species report ----
output$downloadSpeciesReport <- downloadHandler(
  filename = function() {
    spkey <- speciesinfo$key[speciesinfo$species == input$speciesSelect]
    paste0("taxonid=", spkey,"_date=", format(Sys.time(), "%Y-%m-%d_%H-%M%Z"), "_report.pdf")
  },
  content = function(file) {
    # showModal(modalDialog("Preparing report. This might take a while.", footer=NULL))
    shinyalert(
      title = "Preparing your download",
      text = "This may take a while. You can keep using the app.",
      size = "xs",  closeOnEsc = TRUE, closeOnClickOutside = TRUE, html = FALSE, type = "info",
      showConfirmButton = TRUE, showCancelButton = FALSE, confirmButtonText = "OK", confirmButtonCol = "#184E77",
      timer = 0, imageUrl = "", animation = TRUE)
    #on.exit(removeModal())
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