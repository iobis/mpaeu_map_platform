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
    req(!is.null(input$speciesSelect) && input$speciesSelect != "")
    sel_species <- input$speciesSelect
  }
  if (active_tab$current == "thermal") {
    req(!is.null(input$speciesSelectThermal) && input$speciesSelectThermal != "")
    sel_species <- input$speciesSelectThermal
  }
  spkey <- speciesinfo$key[speciesinfo$species == sel_species]
  sel_acro <- speciesinfo$acro[speciesinfo$species == sel_species]
  pts <- arrow::read_parquet(paste0("https://mpaeu-dist.s3.amazonaws.com/results/species/taxonid=", spkey, "/model=", sel_acro, "/taxonid=", spkey, "_model=", sel_acro, "_what=fitocc.parquet"))[,1:2]
  colnames(pts) <- c("longitude", "latitude")
  pts
})

habitatpts <- reactive({
  pts <- arrow::read_parquet(paste0("https://mpaeu-dist.s3.amazonaws.com/results/habitat/habitat=", sp_info$habitat,
   "_model=", sp_info$acro_h, "_what=points.parquet"))
  colnames(pts)[1:2] <- c("longitude", "latitude")
  pts$species <- as.factor(pts$species)
  pts
})

threshold_table <- reactive({
  thresholds <- arrow::read_parquet(paste0(
    "https://obis-maps.s3.amazonaws.com/sdm/species/taxonid=", sp_info$spkey, "/model=", sp_info$acro, "/metrics/taxonid=", 
    sp_info$spkey, "_model=", sp_info$acro, "_what=thresholds.parquet"
  ))
})