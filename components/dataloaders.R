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
    req(!is.null(db_info$species))
    sel_obj <- db_info$species
  }
  if (active_tab$current == "thermal") {
    req(!is.null(db_info$thermal))
    sel_obj <- db_info$thermal
  }
  pts <- sel_obj |>
    tidyr::unnest("files") |>
    filter(type == "fitocc") |>
    pull(file) |>
    arrow::read_parquet()
  colnames(pts) <- c("longitude", "latitude")
  pts
})

habitatpts <- reactive({
  pts <- db_info$habitat |>
    tidyr::unnest("files") |>
    filter(type == "fitocc") |>
    pull(file) |>
    arrow::read_parquet()
  colnames(pts)[1:2] <- c("longitude", "latitude")
  pts$species <- as.factor(pts$species)
  pts
})

threshold_table <- reactive({
  req(!is.null(db_info$species))
  db_info$species |>
    tidyr::unnest("files") |>
    filter(type == "thresholds") |>
    pull(file) |>
    arrow::read_parquet()
})