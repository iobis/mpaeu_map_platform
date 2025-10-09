get_s3_list <- function(bucket = "mpaeu-dist", folder = "results") {
    
    cat("Retrieving S3 list, this may take a while...\n")

    require(dplyr)

    bucket_list <- aws.s3::get_bucket_df(
        bucket = bucket,
        prefix = folder,
        use_https = TRUE,
        max = Inf
    )

    cat("Processing...\n")

    bucket_list <- bucket_list[grepl("results", bucket_list$Key),]

    category <- sub("results/*/", "\\1", bucket_list$Key)
    category <- sub("*/.*", "\\1", category)

    bucket_list$category <- category

    taxonid <- unlist(
        lapply(1:nrow(bucket_list), function(x) {
            m <- regmatches(bucket_list$Key[x], regexpr("(?<=taxonid=)\\d+", bucket_list$Key[x], perl = TRUE))
            if (length(m) > 0) {
                m
            } else {
                NA
            }
        })
    )

    bucket_list$taxonID <- as.integer(taxonid)

    model_acro <- regmatches(bucket_list$Key, regexpr("model=[^/]+", bucket_list$Key))
    model_acro <- sub("^(model=[^_]+(?:_[^_=]+)*)_.*", "\\1", model_acro)
    model_acro <- gsub("model=", "", model_acro)
    bucket_list$model_acro <- model_acro

    method <- unlist(lapply(1:nrow(bucket_list), function(x) {
        m <- regmatches(bucket_list$Key[x], regexpr("method=[^/]+", bucket_list$Key[x]))
        if (length(m) > 0) {
            m
        } else {
            NA
        }
    }))
    method <- sub("^(method=[^_]+(?:_[^_=]+)*)_.*", "\\1", method)
    method <- gsub("method=", "", method)
    
    bucket_list$models <- method

    bucket_list <- bucket_list %>%
        group_by(taxonID, models) %>%
        mutate(is_boot = ifelse(grepl("bootcv", Key), TRUE, FALSE))

    arrow::write_parquet(bucket_list, "data/s3_list.parquet")

    species_data <- bucket_list %>%
        filter(category == "species") %>%
        group_by(taxonID) %>%
        summarise(models = paste0(unique(na.omit(models)), collapse = ";"),
                  acro = model_acro[1])

    species_thermal <- bucket_list %>%
        filter(category == "species") %>%
        group_by(taxonID) %>%
        summarise(with_thermal = ifelse(
            any(grepl("thermenvelope", Key)), TRUE, FALSE
        ))

    species_data <- left_join(species_data, species_thermal)

    arrow::write_parquet(species_data, "data/s3_species_data.parquet")

    div_hab_json <- list(
        diversity = NA,
        habitat = NA,
        diversity_sp_list = NA
    )

    diversity_groups <- bucket_list %>%
        filter(category == "diversity")

    div_hab_json$diversity_sp_list <- diversity_groups$Key[grepl("what=splist", diversity_groups$Key)]

    diversity_groups <- diversity_groups[!grepl("what=splist", diversity_groups$Key), ]

    diversity_groups$group <- gsub("group=", "", regmatches(diversity_groups$Key, regexpr("group=[^_]+", diversity_groups$Key)))

    methods_group <- diversity_groups %>%
        group_by(group) %>%
        distinct(models)

    div_hab_json$diversity <- list(
        groups = unique(diversity_groups$group),
        methods = methods_group
    )

    habitat_groups <- bucket_list %>%
        filter(category == "habitat")

    habitat_groups$habitat <- gsub("_model=$", "",
            gsub("habitat=", "", sub(".*(habitat=[^_]+(?:_[^_]+)*_model=).*", "\\1", habitat_groups$Key)))

    methods_habitat <- habitat_groups %>%
        group_by(habitat) %>%
        distinct(models) %>%
        filter(!is.na(models))

    div_hab_json$habitat <- list(
        habitats = unique(habitat_groups$habitat),
        methods = methods_habitat
    )

    jsonlite::write_json(div_hab_json, path = "data/s3_divhab_data.json", pretty = T)

    cat("Saved!")

    return(invisible(NULL))

}

get_s3_list()

download_local_files <- function() {
    cat("Downloading diversity lists\n")
    download.file(
        "https://mpaeu-dist.s3.amazonaws.com/results/diversity/metric=richness_model=mpaeu_what=splist.parquet",
        "data/metric=richness_model=mpaeu_what=splist.parquet"
    )
    cat("Downloading citation lists\n")
    download.file(
        "https://mpaeu-dist.s3.amazonaws.com/source/citations/reg_datasets_context.parquet",
        "data/reg_datasets_context.parquet"
    )
    download.file(
        "https://mpaeu-dist.s3.amazonaws.com/source/citations/reg_datasets_species.parquet",
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
                data.frame(type = obj[["class"]], scenario = scenario, period = period, file = href)
            } else {
                type <- gsub("^.*what=", "", nam)
                data.frame(type = type, scenario = NA, period = NA, file = href)
            }
        })
        cont <- tibble::tibble(dplyr::bind_rows(cont))
        species_table$files[[ts]] <- cont
        species_table$scientificName[ts] <- spj[["properties"]][["scientificName"]]
        species_table$fit_n[ts] <- spj[["properties"]][["fit_n_points"]]
        av_methods <- spj[["properties"]][["methods"]]
        if (is.numeric(av_methods)) {
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
            data.frame(threshold = obj[["threshold"]], post_treatment = obj[["post_treatment"]],
                       type = obj[["what"]],
                       scenario = scenario, period = period, file = href)
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
            scenario <- obj[["scenario"]]
            period <- NA
            if (!is.null(scenario) && scenario != "current") {
                period <- strsplit(scenario, "_")[[1]][2]
                scenario <- strsplit(scenario, "_")[[1]][1]
            }
            href <- gsub("s3://obis-maps", "https://obis-maps.s3.us-east-1.amazonaws.com", obj[["href"]])
            data.frame(group = obj[["group"]],
                       threshold = obj[["threshold"]], post_treatment = obj[["post_treatment"]],
                       type = obj[["what"]],
                       scenario = scenario, period = period, file = href)
        })
        cont <- tibble::tibble(dplyr::bind_rows(cont))
        div_table$files[[ts]] <- cont
        div_table$metric_name[ts] <- dplyr::case_when(
            spj[["properties"]][["metric"]] == "richness" ~ "Richness",
            spj[["properties"]][["metric"]] == "lcbd" ~ "LCBD",
            .default = spj[["properties"]][["metric"]]
        )
    }

    # Save divitat_db
    arrow::write_parquet(div_table, "data/diversity_db.parquet")

    if (cleanup) {
        message("Erasing STAC local databaset")
        fs::dir_delete("stac")
    }

    message("Concluded building process.")
    return(invisible())
}

species_table |> filter(taxonid == 100801) |> pull(available_models) |> unlist(use.names = F)
