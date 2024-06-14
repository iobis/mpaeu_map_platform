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
  setView(lng = 0.35, lat = 65, zoom = 3)

# Add title/text species
speciesinfo <- read.csv("data/all_splist_20231017.csv")