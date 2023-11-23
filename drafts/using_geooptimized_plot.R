library(leaflet)
library(leafem)
library(terra)
library(stars)

r <- terra::rast("../mpaeu_sdm/results/species/key=100803/mv1_pr/lasso_naive/predictions/mv1_pr_lasso_naive_key100803_basevars_current.tif")
r_b <- stars::read_stars("../mpaeu_sdm/results/species/key=100803/mv1_pr/lasso_naive/predictions/mv1_pr_lasso_naive_key100803_basevars_current.tif")

leaflet() %>%
  addTiles() %>%
  addGeotiff(
    file = "data/maps/species/cogmv1pr_lassonaive_key100803_basevars_current.tif"
    , opacity = 0.9
    , colorOptions = colorOptions(
      palette = hcl.colors(256, palette = "inferno")
      , na.color = "transparent"
    )
  )


leaflet() %>%
  addTiles() %>%
  addRasterImage(x = r)

leaflet() %>%
  addTiles() %>%
  addGeoRaster(x = r_b, opacity = 0.9, colorOptions = colorOptions(
                 palette = hcl.colors(256, palette = "inferno"), na.color = "transparent"))



bench::mark({
  leaflet() %>%
    addTiles() %>%
    addGeotiff(
      file = "data/maps/species/cogmv1pr_lassonaive_key100803_basevars_current.tif"
      , opacity = 0.9
      , colorOptions = colorOptions(
        palette = hcl.colors(256, palette = "inferno")
        , na.color = "transparent"
      )
    )
},
{
  r <- terra::rast("../mpaeu_sdm/results/species/key=100803/mv1_pr/lasso_naive/predictions/mv1_pr_lasso_naive_key100803_basevars_current.tif")
  
  leaflet() %>%
    addTiles() %>%
    addRasterImage(x = r)
},
{
  r_b <- stars::read_stars("../mpaeu_sdm/results/species/key=100803/mv1_pr/lasso_naive/predictions/mv1_pr_lasso_naive_key100803_basevars_current.tif")
  leaflet() %>%
    addTiles() %>%
    addGeoRaster(x = r_b, opacity = 0.9, colorOptions = colorOptions(
      palette = hcl.colors(256, palette = "inferno"), na.color = "transparent"))
  
}, check = F)
 