########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
#################### Information loaded on server start ########################

# Load needed packages ----
library(leaflet)
library(leafem)
library(leafpm)
library(leaflet.extras2)
library(leaflet.providers)
library(dplyr)
library(terra)
library(plotly)
library(ggplot2)
library(spatstat.explore)
library(shinyalert)

# Source functions
source("scripts/functions.R")

# Create debug function
mdebug <- function(text, toprint = debug) {
  if (toprint) {
    message(text)
  }
  return(invisible(NULL))
}

# Create leaflet object ----
m <- leaflet() %>% 
  addTiles(group = "Open Street Maps", layerId = "baseid") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark") %>%
  addLayersControl(
    overlayGroups = c("Points"),
    baseGroups = c("Open Street Maps", "CartoDB", "CartoDB Dark"),
    options = layersControlOptions(collapsed = T),
    position = "bottomright"
  ) %>%
  addEasyprint(options = easyprintOptions(
    title = 'Print map',
    position = 'bottomleft',
    exportOnly = TRUE)) %>%
  setView(lng = 0.35, lat = 65, zoom = 3) %>%
  addMapPane("left", zIndex = 0) %>%
  addMapPane("right", zIndex = 0) %>%
  addMapPane("maskPane", zIndex = 500) %>%
  addMapPane("extraPane", zIndex = 600)

m$dependencies <- c(m$dependencies, leafpm::pmDependencies())

m <- m %>%
  htmlwidgets::onRender('
                            LeafletWidget.methods.removeImage = function(layerId) {
                              this.layerManager.removeLayer(null, layerId);
                            }
                            ')

# Add title/text species
# speciesinfo <- read.csv("data/all_splist_20240724.csv")
# speciesinfo$key <- speciesinfo$taxonID
# speciesinfo$species <- speciesinfo$scientificName

# speciesinfo <- obissdm::get_listbygroup(speciesinfo, conf_file = "data/sdm_conf.yml")
# available_groups <- c("all", unique(speciesinfo$sdm_group))
# names(available_groups) <- stringr::str_to_title(available_groups)

# New data loading and options setting
speciesinfo <- readRDS("data/app_splist.rds")
speciesinfo$key <- speciesinfo$taxonID
speciesinfo$species <- speciesinfo$scientificName
speciesinfo <- speciesinfo[order(speciesinfo$scientificName),]

# Create a "copy" for other uses
# TO BE REMOVED IN NEXT VERSION
speciesinfo_full <- speciesinfo

avmodels <- lapply(speciesinfo$taxonID, function(id) {
  jfile <- paste0("data/maps/taxonid=", id, "/model=mpaeu/taxonid=", id,
                  "_model=mpaeu_what=log.json")

  if (file.exists(jfile)) {
    logf <- jsonlite::read_json(jfile)

    mod_names <- names(logf$model_posteval)[unlist(lapply(logf$model_posteval, function(x) if (length(x) > 0) TRUE else FALSE))]
    available_models <- mod_names[!grepl("niche", mod_names)]
    # available_models <- gsub("maxent", "maxnet", available_models)
    available_models <- gsub("rf", "rf_classification_ds", available_models)
    available_models <- paste0(available_models, collapse = ";")
  } else {
    available_models <- NA
  }
  return(available_models)
})
speciesinfo$models <- unlist(avmodels)

available_groups <- c("all", unique(speciesinfo$sdm_group))
names(available_groups) <- stringr::str_to_title(available_groups)

common_names <- stringr::str_split(speciesinfo$common_names, pattern = "; ")
common_names <- unique(unlist(common_names, use.names = F))
#common_names <- stringr::str_to_sentence(common_names)
common_names <- c("All" = "all", common_names)

region_names <- stringr::str_split(speciesinfo$region_name, pattern = "; ")
region_names <- unique(unlist(region_names, use.names = F))
region_names <- c("All" = "all", region_names)

sdm_groups <- c("All" = "all", unique(speciesinfo$sdm_group))
phylums <- c("All" = "all", unique(speciesinfo$phylum))
classes <- c("All" = "all", unique(speciesinfo$class))
orders <- c("All" = "all", unique(speciesinfo$order))
families <- c("All" = "all", unique(speciesinfo$family))

available_species <- list.files("data/maps/")
available_ids <- gsub("taxonid=", "", available_species)
speciesinfo <- speciesinfo[speciesinfo$taxonID %in% as.numeric(available_ids), ]
sp_options <- c("", speciesinfo$scientificName)

# Verify most recent acronym
build_json <- jsonlite::read_json("data/platform_build.json")
if (length(unlist(build_json$acronyms_available)) > 1) {
  av_keys <- gsub("taxonid=", "", list.files("data/maps"))
  recent_acro <- lapply(av_keys, function(x){
    re <- suppressMessages(obissdm::recent_file(paste0("data/maps/taxonid=", x, "/"), "*"))
    ifelse(is.null(re), "NA", basename(re))
  })
  recent_acro <- unlist(recent_acro)
  acros_df <- data.frame(key = as.numeric(av_keys), acro = gsub("model=", "", recent_acro))
  speciesinfo <- dplyr::left_join(speciesinfo, acros_df)
} else {
  speciesinfo$acro <- unlist(build_json$acronyms_available)[1]
}

# Load study area
starea <- sf::read_sf("data/studyarea.fgb")

# Load additional data
eez <- sfarrow::st_read_parquet("data/EEZ_IHO_simp_edited.parquet")
realms <- sf::st_read("data/MarineRealms_BO.shp")

# See available diversity groups
av_div_groups <- list.files("data/diversity")
av_div_groups <- av_div_groups[!grepl("txt", av_div_groups)]
av_div_groups <- gsub(".*group=([^_]+)_type.*", "\\1", av_div_groups)
av_div_groups <- unique(av_div_groups)
names(av_div_groups) <- stringr::str_to_title(av_div_groups)
