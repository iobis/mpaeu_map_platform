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
  addMapPane("maskPane", zIndex = 500)

m$dependencies <- c(m$dependencies, leafpm::pmDependencies())

m <- m %>%
  htmlwidgets::onRender('
                            LeafletWidget.methods.removeImage = function(layerId) {
                              this.layerManager.removeLayer(null, layerId);
                            }
                            ')

# Add title/text species
speciesinfo <- read.csv("data/all_splist_20231017.csv")

# Load study area
starea <- sf::read_sf("data/studyarea.fgb")