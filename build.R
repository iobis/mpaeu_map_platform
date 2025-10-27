download_local_files <- function() {
    cat("Downloading diversity lists\n")
    download.file(
        "https://obis-maps.s3.amazonaws.com/sdm/diversity/model=mpaeu/metric=richness_model=mpaeu_what=splist.parquet",
        "data/metric=richness_model=mpaeu_what=splist.parquet"
    )
    cat("Downloading citation lists\n")
    download.file(
        "https://obis-maps.s3.amazonaws.com/sdm/source/model=mpaeu/reg_datasets_context.parquet",
        "data/reg_datasets_context.parquet"
    )
    download.file(
        "https://obis-maps.s3.amazonaws.com/sdm/source/model=mpaeu/reg_datasets_species.parquet",
        "data/reg_datasets_species.parquet"
    )
    cat("Download concluded.\n")
}

download_local_files()

build_catalogue <- function(cleanup = TRUE) {

    message("Downloading the STAC catalogue\n\n")
    system("aws s3 sync --no-sign-request s3://obis-maps/sdm/stac/ ./stac/")

    species_cat <- jsonlite::read_json(
        "stac/species-catalog/species-mpaeu/species-mpaeu-collection/collection.json"
    )
    av_species <- lapply(species_cat[["links"]], \(x) if (x[["rel"]] == "item") x[["href"]] else NULL)
    av_species <- unlist(av_species)
    taxon_ids <- as.integer(gsub("\\.json", "", gsub(".*taxonid=", "", av_species)))
    av_species <- file.path(
        "stac/species-catalog/species-mpaeu/species-mpaeu-collection",
        gsub("^\\.\\/", "", av_species)
    )

    species_table <- tibble::tibble(
        taxonid = taxon_ids, scientificName = NA, jsoncat = av_species, fit_n = NA,
        files = list(NA), available_models = list(NA)
    )

    message("Building the species database")
    pb <- progress::progress_bar$new(total = length(taxon_ids))
    for (ts in seq_along(taxon_ids)) {
        pb$tick()
        spj <- jsonlite::read_json(av_species[ts])
        cont <- spj[["assets"]]
        cont <- lapply(seq_along(cont), \(x) {
            nam <- names(cont)[x]
            obj <- cont[[x]]
            scenario <- obj[["scenario"]]
            period <- NA
            if (!is.null(scenario) && scenario != "current") {
                period <- strsplit(scenario, "_")[[1]][2]
                scenario <- strsplit(scenario, "_")[[1]][1]
            }
            href <- gsub("s3://obis-maps", "https://obis-maps.s3.us-east-1.amazonaws.com", obj[["href"]])
            if (grepl("prediction|uncertainty", nam)) {
                me <- ifelse(obj[["method"]] == "rf_classification_ds", "rf", obj[["method"]])
                data.frame(type = obj[["class"]], scenario = scenario, period = period,
                           method = me, file = href)
            } else {
                type <- gsub("^.*what=", "", nam)
                if (grepl("method", nam)) {
                    me <- gsub("method=", "", gsub("_what=.*", "", nam))
                    me <- ifelse(me == "rf_classification_ds", "rf", me)
                    # me <- obj[["method"]] # When added to STAC in next version
                } else {
                    me <- NA
                }
                data.frame(type = type, scenario = NA, period = NA,
                           method = me, file = href)
            }
        })
        cont <- tibble::tibble(dplyr::bind_rows(cont))
        species_table$files[[ts]] <- cont
        species_table$scientificName[ts] <- spj[["properties"]][["scientificName"]]
        species_table$fit_n[ts] <- spj[["properties"]][["fit_n_points"]]
        av_methods <- spj[["properties"]][["methods"]]
        if (is.numeric(av_methods)) { # Kept only for back compatibility, solved
            av_methods <- tibble::tibble(method = c("esm"))
        } else {
            av_methods <- tibble::tibble(method = unlist(av_methods, use.names = FALSE))
        }
        species_table$available_models[[ts]] <- av_methods
    }

    # Save species_db
    arrow::write_parquet(species_table, "data/species_db.parquet")


    hab_cat <- jsonlite::read_json(
        "stac/habitat-catalog/habitat-mpaeu/habitat-mpaeu-collection/collection.json"
    )
    av_hab <- lapply(hab_cat[["links"]], \(x) if (x[["rel"]] == "item") x[["href"]] else NULL)
    av_hab <- unlist(av_hab)
    hab_ids <- gsub("\\.json", "", gsub(".*habitat=", "", av_hab))
    av_hab <- file.path(
        "stac/habitat-catalog/habitat-mpaeu/habitat-mpaeu-collection",
        gsub("^\\.\\/", "", av_hab)
    )

    habitat_table <- tibble::tibble(
        habitat = hab_ids, habitat_name = NA, jsoncat = av_hab,
        files = list(NA)
    )

    message("Building the habitat database")
    for (ts in seq_along(hab_ids)) {
        spj <- jsonlite::read_json(av_hab[ts])
        cont <- spj[["assets"]]
        cont <- lapply(seq_along(cont), \(x) {
            obj <- cont[[x]]
            scenario <- obj[["scenario"]]
            period <- NA
            if (!is.null(scenario) && scenario != "current") {
                period <- strsplit(scenario, "_")[[1]][2]
                scenario <- strsplit(scenario, "_")[[1]][1]
            }
            href <- gsub("s3://obis-maps", "https://obis-maps.s3.us-east-1.amazonaws.com", obj[["href"]])
            if (obj[["what"]] == "fitocc") {
                scenario <- obj[["post_treatment"]] <- obj[["threshold"]] <- NA
            }
            range_vals <- unlist(obj$values_range, use.names = FALSE)
            range_min <- range_max <- NA
            if (!is.null(range_vals[1])) {
                range_min <- range_vals[1]
                range_max <- range_vals[2]
            }
            data.frame(threshold = obj[["threshold"]], post_treatment = obj[["post_treatment"]],
                       type = obj[["what"]],
                       scenario = scenario, period = period, file = href,
                       range_min = range_min, range_max = range_max)
        })
        cont <- tibble::tibble(dplyr::bind_rows(cont))
        habitat_table$files[[ts]] <- cont
        habitat_table$habitat_name[ts] <- spj[["properties"]][["habitat"]]
    }

    # Save habitat_db
    arrow::write_parquet(habitat_table, "data/habitat_db.parquet")


    div_cat <- jsonlite::read_json(
        "stac/diversity-catalog/diversity-mpaeu/diversity-mpaeu-collection/collection.json"
    )
    av_div <- lapply(div_cat[["links"]], \(x) if (x[["rel"]] == "item") x[["href"]] else NULL)
    av_div <- unlist(av_div)
    div_ids <- gsub("\\.json", "", gsub(".*metric=", "", av_div))
    av_div <- file.path(
        "stac/diversity-catalog/diversity-mpaeu/diversity-mpaeu-collection",
        gsub("^\\.\\/", "", av_div)
    )

    div_table <- tibble::tibble(
        metric = div_ids, metric_name = NA, jsoncat = av_div,
        files = list(NA)
    )

    message("Building the diversity database")
    for (ts in seq_along(div_ids)) {
        spj <- jsonlite::read_json(av_div[ts])
        cont <- spj[["assets"]]
        cont <- lapply(seq_along(cont), \(x) {
            obj <- cont[[x]]
            if (obj$what == "splist") return(NULL)
            scenario <- obj[["scenario"]]
            period <- NA
            if (!is.null(scenario) && scenario != "current") {
                period <- strsplit(scenario, "_")[[1]][2]
                scenario <- strsplit(scenario, "_")[[1]][1]
            }
            href <- gsub("s3://obis-maps", "https://obis-maps.s3.us-east-1.amazonaws.com", obj[["href"]])
            range_vals <- unlist(obj$values_range, use.names = FALSE)
            range_min <- range_max <- NA
            if (!is.null(range_vals[1])) {
                if (obj[["what"]] == "continuous") {
                    range_vals <- range_vals/100
                }
                range_min <- range_vals[1]
                range_max <- range_vals[2]
            }
            data.frame(group = obj[["group"]],
                       threshold = obj[["threshold"]], post_treatment = obj[["post_treatment"]],
                       type = obj[["what"]],
                       scenario = scenario, period = period, file = href,
                       range_min = range_min, range_max = range_max)
        })
        cont <- tibble::tibble(dplyr::bind_rows(cont))
        div_table$files[[ts]] <- cont
        div_table$metric_name[ts] <- dplyr::case_when(
            spj[["properties"]][["metric"]] == "richness" ~ "Richness",
            spj[["properties"]][["metric"]] == "lcbd" ~ "LCBD",
            .default = spj[["properties"]][["metric"]]
        )
    }

    # Save diversity_db
    arrow::write_parquet(div_table, "data/diversity_db.parquet")

    if (cleanup) {
        message("Erasing STAC local databaset")
        fs::dir_delete("stac")
    }

    message("Concluded building process.")
    return(invisible())
}

build_catalogue()

# Get world shape for plots
get_world <- function() {
    message("Getting world shapefile")
    suppressPackageStartupMessages(require(terra))
    base_rast <- rast("data/thetao_baseline_depthsurf_mean_cog.tif")
    base_rast[is.na(base_rast)] <- -999999
    base_rast[base_rast != -999999] <- NA
    base_rast <- as.polygons(base_rast)
    base_rast <- base_rast[,-1]
    writeVector(base_rast, "data/world_shape.gpkg", overwrite = TRUE)
    message("Concluded")
    return(invisible())
}

get_world()

# Unzip bundle
message("Unzipping app bundle")
zip::unzip(
    "app_bundle.zip",
    exdir = "data/"
)
