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

cat("Updating navbar icons")
htmlf <- readLines("index.html", warn = FALSE)
tr_line <- htmlf[grepl("SPECIES", htmlf)]
tr_line <- gsub("SPECIES", '<i <i class="fa-solid fa-shrimp"></i> <span>SPECIES</span>', tr_line)
tr_line <- gsub("THERMAL RANGE", '<i class="fa-solid fa-temperature-half"></i> <span>THERMAL RANGE</span>', tr_line)
tr_line <- gsub("HABITAT", '<i class="fa-solid fa-layer-group"></i> <span>HABITAT</span>', tr_line)
tr_line <- gsub("DIVERSITY", '<i class="fa-solid fa-chart-simple"></i> <span>DIVERSITY</span>', tr_line)
tr_line <- gsub("ATLAS", '<i class="fa-solid fa-earth-europe"></i> <span>ATLAS</span>', tr_line)
htmlf[grepl("SPECIES", htmlf)] <- tr_line
writeLines(htmlf, "index.html")