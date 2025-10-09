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

# New data loading and options setting
s3_list <- arrow::open_dataset("data/s3_list.parquet")
s3_species <- arrow::read_parquet("data/s3_species_data.parquet")
s3_divhab <- jsonlite::read_json("data/s3_divhab_data.json")

speciesinfo <- readRDS("data/app_splist.rds")
speciesinfo$key <- speciesinfo$taxonID
speciesinfo$species <- speciesinfo$scientificName
speciesinfo <- speciesinfo[order(speciesinfo$scientificName),]

# Create a "copy" for other uses
# TO BE REMOVED IN NEXT VERSION
speciesinfo_full <- speciesinfo

speciesinfo <- left_join(speciesinfo, s3_species[,1:3], by = "taxonID")

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

available_ids <- unique(s3_species$taxonID)
speciesinfo <- speciesinfo[speciesinfo$taxonID %in% as.numeric(available_ids), ]
sp_options <- c("", speciesinfo$scientificName)

thermal_ok <- s3_species$taxonID[s3_species$with_thermal]
sp_options_thermal <- c("", speciesinfo$scientificName[speciesinfo$taxonID %in% thermal_ok])
rm(thermal_ok)

# Load study area
starea <- sf::read_sf("data/studyarea.fgb")

# Load additional data
eez <- sfarrow::st_read_parquet("data/EEZ_IHO_simp_edited.parquet")
realms <- sf::st_read("data/MarineRealms_BO.shp")

# See available diversity groups
av_div_groups <- unlist(s3_divhab$diversity$groups)
names(av_div_groups) <- stringr::str_to_title(av_div_groups)

# Load citation info
cit_species_ds <- arrow::open_dataset("data/reg_datasets_species.parquet")
cit_general_ds <- arrow::open_dataset("data/reg_datasets_context.parquet")

# Load diversity species list
div_sp_list <- arrow::open_dataset("data/metric=richness_model=mpaeu_what=splist.parquet")
