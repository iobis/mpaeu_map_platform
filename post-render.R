# Post-render function
cat("Saving build file... \n")
log_file <- list(
    platform_version = "1.0.1",
    platform_assemble_version = Sys.Date(),
    acronyms_available = c("mpaeu"),
    quarto_version = as.character(quarto::quarto_version()),
    shiny_version = as.character(packageVersion("shiny"))
)

jsonlite::write_json(log_file, "data/platform_build.json", pretty = T)
