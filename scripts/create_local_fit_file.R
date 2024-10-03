get_local_file <- function(species_name, aphia_id, algo_name, type = "ipynb") {
    if (type == "ipynb") {
        source_file <- readLines("scripts/fit_locally_model.ipynb")
    } else {
        source_file <- readLines("scripts/fit_locally_model.qmd", warn = F)
    }

    edited_file <- gsub("SPECIES_NAME", species_name, source_file)
    edited_file <- gsub("APHIA_ID", aphia_id, edited_file)

    if (grepl("rf", algo_name)) {
        algo_name <- "rf"
    }

    edited_file <- gsub("ALGO_NAME", algo_name, edited_file)
    algo_package <- switch(algo_name,
                    rf = "RandomForest",
                    maxent = "maxnet",
                    xgboost = "xgboost",
                    ensemble = "maxnet",
                    esm = "maxnet")
    edited_file <- gsub("ALGO_PACKAGE", algo_package, edited_file)

    return(edited_file)
}