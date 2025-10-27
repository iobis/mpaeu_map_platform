# Pre-render code
library(dplyr)
library(h3jsr)
library(arrow)
sf::sf_use_s2(FALSE)

force <- FALSE
if (!file.exists("data/app_splist.rds") || force) {
    cat("Preparing species list...")

    mpaeu_list <- read.csv("data/all_splist_20240724.csv")

    # Join different tables and add project ----
    joint_list <- bind_rows(
        cbind(mpaeu_list, study = "mpaeurope")
    )

    # Get common names ----
    if (!file.exists("data/vernacular_names.txt")) {
        mpaeu_list_aid <- split(mpaeu_list$AphiaID, ceiling(seq_along(mpaeu_list$AphiaID) / 100))
        common_names <- lapply(
            mpaeu_list_aid, function(x) {
                r <- worrms::wm_common_id_(x)
                Sys.sleep(3)
                r
            }
        )

        common_names <- bind_rows(common_names)

        common_names <- common_names %>%
            filter(language_code == "eng") %>%
            rename(AphiaID = id, common_names = vernacular) %>%
            select(AphiaID, common_names) %>%
            group_by(AphiaID) %>%
            mutate(common_names = paste0(common_names, collapse = "; "))

        common_names <- common_names %>%
            distinct(AphiaID, .keep_all = T) %>%
            mutate(AphiaID = as.numeric(AphiaID))

        final_common <- left_join(mpaeu_list, common_names) %>% select(AphiaID, common_names)
        final_common$common_names <- ifelse(is.na(final_common$common_names),
            "not_available", final_common$common_names)

        write.table(final_common, "data/vernacular_names.txt")
    }

    vernacular <- read.table("data/vernacular_names.txt")

    joint_list_vern <- left_join(joint_list, vernacular)

    if (any(is.na(joint_list_vern$common_names))) {

        na_info <- joint_list_vern$AphiaID[is.na(joint_list_vern$common_names)]

        nainfo_aid <- split(na_info, ceiling(seq_along(na_info) / 100))
        common_names <- lapply(
            nainfo_aid, function(x) {
                r <- worrms::wm_common_id_(x)
                Sys.sleep(3)
                r
            }
        )

        common_names <- bind_rows(common_names)

        common_names <- common_names %>%
            filter(language_code == "eng") %>%
            rename(AphiaID = id, common_names = vernacular) %>%
            select(AphiaID, common_names) %>%
            group_by(AphiaID) %>%
            mutate(common_names = paste0(common_names, collapse = "; "))

        common_names <- common_names %>%
            distinct(AphiaID, .keep_all = T) %>%
            mutate(AphiaID = as.numeric(AphiaID)) %>%
            select(AphiaID, common_names)

        vernacular <- read.table("data/vernacular_names.txt")
        vernacular <- bind_rows(common_names, vernacular)

        write.table(vernacular, "data/vernacular_names.txt")

        joint_list_vern <- left_join(joint_list, vernacular)
    }

    # Add group information
    joint_list_group <- obissdm::get_listbygroup(joint_list_vern, "data/sdm_conf.yml")

    # Add sea information
    if (!file.exists("data/seas_and_regions.rds")) {

        # Change those two paths if working in a different setup
        sp_data_path <- "../mpaeu_sdm/data/species" # path to species folder
        # Path to IHO SEAS (marineregions.org)
        iho_seas_path <- "../mpaeu_sandbox/data/World_Seas_IHO_v3/World_Seas_IHO_v3.shp"

        if (!file.exists("data/iho_h3.rds")) {
            iho_seas <- sf::st_read(iho_seas_path)
            iho_seas <- iho_seas[,"NAME"]
            h3_list <- lapply(1:nrow(iho_seas), function(x) NULL)
            names(h3_list) <- iho_seas$NAME
            for (z in 1:nrow(iho_seas)) {
                cat("Processing", z, "out of", nrow(iho_seas), "\n")
                h3_list[[z]] <- try(unlist(polygon_to_cells(iho_seas[z,], res = 5), use.names = F))
                if (inherits(h3_list[[z]], "try-error")) {
                    split_grid <- sf::st_make_grid(iho_seas[z,], n = c(2,2))
                    split_grid <- sf::st_intersection(iho_seas[z,], split_grid)
                    parts <- lapply(1:4, function(x) unlist(polygon_to_cells(split_grid[x,], res = 5), use.names = F))
                    h3_list[[z]] <- unlist(parts, use.names = F)
                }
            }
            iho_h3 <- lapply(h3_list, function(x) data.frame(h3_05 = x))
            names(iho_h3) <- names(h3_list)
            iho_h3 <- bind_rows(iho_h3, .id = "region_name")
            saveRDS(iho_h3, "data/iho_h3.rds")
        } else {
            iho_h3 <- readRDS("data/iho_h3.rds")
        }

        to_h3 <- function(df) {
            df <- as.data.frame(df)
            unique_sp <<- unique_sp-1
            print(unique_sp)
            df$h3_05 <- point_to_cell(df[,1:2], res = 5)
            df <- left_join(df, iho_h3)
            df$region_name <- ifelse(is.na(df$region_name), "Other", df$region_name)
            tfip <- paste0(unique(df$region_name), collapse = "; ")
            data.frame(AphiaID = df$AphiaID[1], region_name = tfip)
        }

        all_sp_data <- open_dataset(sp_data_path)
        unique_sp <- all_sp_data %>% distinct(taxonID) %>% count() %>% collect()
        unique_sp <- unique_sp$n
        all_sp_data <- all_sp_data %>%
            select(decimalLongitude, decimalLatitude, taxonID, data_type) %>%
            filter(data_type == "fit_points") %>%
            rename(AphiaID = taxonID) %>%
            group_by(AphiaID) %>%
            map_batches(~ as_record_batch(to_h3(.))) %>%
            select(AphiaID, region_name) %>%
            collect()

        saveRDS(all_sp_data, "data/seas_and_regions.rds")

    }

    sea_info <- readRDS("data/seas_and_regions.rds")

    joint_list_sea <- left_join(joint_list_group, sea_info)

    if (any(is.na(joint_list_sea$region_name))) {

        iho_h3 <- readRDS("data/iho_h3.rds")

        to_h3 <- function(df) {
            df <- as.data.frame(df)
            unique_sp <<- unique_sp-1
            print(unique_sp)
            df$h3_05 <- point_to_cell(df[,1:2], res = 5)
            df <- left_join(df, iho_h3)
            df$region_name <- ifelse(is.na(df$region_name), "Other", df$region_name)
            tfip <- paste0(unique(df$region_name), collapse = "; ")
            data.frame(AphiaID = df$AphiaID[1], region_name = tfip)
        }

        to_do_ids <- joint_list_sea$AphiaID[is.na(joint_list_sea$region_name)]

        sp_data_path <- "../mpaeu_sdm/data/species"
        all_sp_data <- open_dataset(sp_data_path)
        unique_sp <- all_sp_data %>% distinct(taxonID) %>% collect()
        valid_ids <- to_do_ids[to_do_ids %in% unique_sp$taxonID]
        unique_sp <- nrow(unique_sp)

        if (length(valid_ids) > 0) {
            all_sp_data <- all_sp_data %>%
                filter(taxonID %in% valid_ids) %>%
                select(decimalLongitude, decimalLatitude, taxonID, data_type) %>%
                filter(data_type == "fit_points") %>%
                rename(AphiaID = taxonID) %>%
                group_by(AphiaID) %>%
                map_batches(~ as_record_batch(to_h3(.))) %>%
                select(AphiaID, region_name) %>%
                collect()

            sea_info <- bind_rows(sea_info, all_sp_data)

            saveRDS(sea_info, "data/seas_and_regions.rds")

            joint_list_sea <- left_join(joint_list_group, sea_info)

        }
    }

    joint_list_sea$region_name[is.na(joint_list_sea$region_name)] <- "Other"

    # Add red list status
    if (!file.exists("data/redlist.csv")) {
        # Code from the eDNA dashboard pipeline
        suppressPackageStartupMessages(library(rredlist))
        suppressPackageStartupMessages(library(dplyr))

        redlist <- data.frame()

        page <- 0

        while (TRUE) {
            res <- rl_sp(page, key = "a936c4f78881e79a326e73c4f97f34a6e7d8f9f9e84342bff73c3ceda14992b9")$result
            if (length(res) == 0) {
                break
            }
            redlist <- bind_rows(redlist, res)
            page <- page + 1
        }

        redlist <- redlist %>%
            filter(is.na(population)) %>%
            select(species = scientific_name, category)

        write.csv(redlist, "data/redlist.csv", row.names = FALSE, quote = TRUE)
    }

    redlist <- read.csv("data/redlist.csv")
    colnames(redlist) <- c("scientificName", "redlist_category")

    joint_list_sea <- left_join(joint_list_sea, redlist)
    joint_list_sea$redlist_category[is.na(joint_list_sea$redlist_category)] <- "Not available"
    joint_list_sea$redlist_category[grepl("LR", joint_list_sea$redlist_category)] <- "LR"

    if (file.exists("data/sdm_review.csv")) { # Deprecated, to remove in next version
        sdm_status <- read.csv("data/sdm_review.csv")
        joint_list_sea <- left_join(joint_list_sea, sdm_status)
    } else {
        joint_list_sea$sdm_quality <- "Not assessed"
        joint_list_sea$sdm_reviewed <- "No"
    }

    saveRDS(joint_list_sea, "data/app_splist.rds")

    # Prepare bundle for serverstart.R
    zip::zip(
        "app_bundle.zip",
        c("data/app_splist.rds",
          "data/studyarea.fgb",
          "data/EEZ_IHO_simp_edited.parquet",
          "data/world_shape.gpkg"),
        mode = "cherry-pick"
    )

} else {
    cat("List already prepared. To force preparing set `force=TRUE` on `pre-render.R`")
}
