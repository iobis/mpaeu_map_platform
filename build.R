get_s3_list <- function(bucket = "mpaeu-dist", folder = "results") {
    
    cat("Retrieving S3 list, this may take a while...")

    bucket_list <- aws.s3::get_bucket_df(
        bucket = bucket,
        prefix = folder,
        use_https = TRUE,
        max = Inf
    )

    cat("Processing...")

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

    require(dplyr)

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
        habitat = NA
    )

    diversity_groups <- bucket_list %>%
        filter(category == "diversity")

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