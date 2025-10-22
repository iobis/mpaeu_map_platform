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
setGDALconfig("AWS_NO_SIGN_REQUEST", "YES")
setGDALconfig("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")

# Source functions
source("scripts/general_functions.R")

# Create debug function
mdebug <- function(text, toprint = debug) {
  if (toprint) {
    message(text)
  }
  return(invisible(NULL))
}

# Global settings ------
global_acro <- "mpaeu"

# Create leaflet object ----
m <- leaflet() %>% 
  addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png", layerId = "baseid") %>%
  # addTiles(group = "Open Street Maps", layerId = "baseid") %>%
  # addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
  # addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark") %>%
  addLayersControl(
    overlayGroups = c("Points"),
    baseGroups = c("Open Street Maps"),
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

# Load data ------
# DB objects -----
species_db <- arrow::open_dataset("data/species_db.parquet")
habitat_db <- arrow::open_dataset("data/habitat_db.parquet")
diversity_db <- arrow::open_dataset("data/diversity_db.parquet")

# Species info -----
speciesinfo <- readRDS("data/app_splist.rds")
speciesinfo$key <- speciesinfo$taxonID
speciesinfo$species <- speciesinfo$scientificName
speciesinfo <- speciesinfo[order(speciesinfo$scientificName),]

#available_groups <- c("all", unique(speciesinfo$sdm_group))
#names(available_groups) <- stringr::str_to_title(available_groups)

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

available_ids <- species_db |>
  select(taxonid) |>
  distinct() |>
  collect() |>
  pull(taxonid)
speciesinfo <- speciesinfo[speciesinfo$taxonID %in% as.numeric(available_ids), ]
sp_options <- c("", speciesinfo$scientificName)
sp_options_thermal <- sp_options

# Load study area
starea <- sf::read_sf("data/studyarea.fgb")

# Load additional data
eez <- sfarrow::st_read_parquet("data/EEZ_IHO_simp_edited.parquet")
realms <- sf::st_read("data/MarineRealms_BO.shp")

# See available diversity groups
av_div_groups <- diversity_db |>
  filter(metric == "richness") |>
  collect() |>
  tidyr::unnest(files) |>
  distinct(group) |>
  pull(group)
names(av_div_groups) <- stringr::str_to_title(av_div_groups)

# Load citation info
cit_species_ds <- arrow::open_dataset("data/reg_datasets_species.parquet")
cit_general_ds <- arrow::open_dataset("data/reg_datasets_context.parquet")

# Load diversity species list
div_sp_list <- arrow::open_dataset("data/metric=richness_model=mpaeu_what=splist.parquet")