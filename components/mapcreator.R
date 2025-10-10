map_data <- reactiveValues(
    tab  = "species",
    layer_1 = NULL,
    layer_2 = NULL,
    mask = NULL,
    mask_type = NULL,
    mask_state = FALSE,
    filter = NULL,
    boot_1 = NULL,
    boot_2 = NULL,
    realms = FALSE,
    eez = FALSE,
    mpas = FALSE,
    eunis = FALSE
)

extract_sp <- function(.data, ty = "prediction", sc = "current", pe = NULL) {
    sld <- .data |>
        filter(type == ty, scenario == sc)
    if (!is.null(pe)) {
        sld <- sld |> filter(period == pe)
    }
    return(sld |> pull(files))
}

observe({
    map_data$tab <- active_tab$current
    if (active_tab$current == "species") {
        sel_species <- species_db |>
            filter(taxonid == sp_info$spkey, model == sp_info$acro, method = sp_info$model) |>
            tidyr::unnest(files)

        map_data$layer_1 <- sel_species |> extract_sp()
        map_data$boot_1 <- sel_species |> extract_sp(ty = "uncertainty")
        if (sp_info$scenario == "current") {
            map_data$layer_2 <- NULL
            map_data$boot_2 <- NULL
        } else {
            map_data$layer_2 <- sel_species |> 
                extract_sp(sc = sp_info$scenario, pe = sp_info$decade)
            map_data$boot_2 <- sel_species |> 
                extract_sp(ty = "uncertainty", sc = sp_info$scenario, pe = sp_info$decade)
        }

        map_data$mask <- sel_species |> 
                extract_sp(ty = "mask")
        map_data$mask_state <- maskstate()


        

        sel_species <- species_db |>
            filter(taxonid == sp_info$spkey, model == sp_info$acro) |>
            tidyr::unnest(files)
    } else if (active_tab$current == "thermal") {

    } else if (active_tab$current == "habitat") {

    } else if (active_tab$current == "diversity") {

    }
})



### Tests
sp_info <- list(acro = "mpaeu", spkey = 1022)
species_db <- tibble::tibble(
    taxonid = 1022,
    model = "mpaeu",
    files = tibble::tibble(
        type = c(rep("uncertainty", 3), rep("prediction", 3)),
        scenario = rep(c("current", "ssp1", "ssp2"), 2),
        period = rep(c(NA, 2100, 2100), 2),
        files = paste0("file", 1:6)
    )
)

library(leaflet);library(leafem)
tictoc::tic("normal")
leaflet() %>% 
  addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png", layerId = "baseid") |>
  addGeotiff(
    file = "https://mpaeu-dist.s3.amazonaws.com/results/species/taxonid=100599/model=mpaeu/predictions/taxonid=100599_model=mpaeu_method=esm_scen=current_cog.tif"
  )
tictoc::toc()

tictoc::tic("cog")
leaflet() %>% 
  addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png", layerId = "baseid") |>
  addCOG(
    url = "https://mpaeu-dist.s3.amazonaws.com/results/species/taxonid=100599/model=mpaeu/predictions/taxonid=100599_model=mpaeu_method=esm_scen=current_cog.tif"
  )
tictoc::toc()