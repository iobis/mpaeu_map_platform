########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
############################ Load accessory data ###############################

# Load species points ----
speciespts <- reactive({
  if (active_tab$current == "species") {
    sel_species <- input$speciesSelect
  }
  if (active_tab$current == "thermal") {
    sel_species <- input$speciesSelectThermal
  }
  spkey <- speciesinfo$key[speciesinfo$species == sel_species]
  pts <- arrow::read_parquet(paste0("data/maps/taxonid=", spkey, "/model=inteval/taxonid=", spkey, "_model=inteval_what=fitocc.parquet"))[,1:2]
  colnames(pts) <- c("longitude", "latitude")
  pts
})