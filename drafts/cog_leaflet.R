library(leaflet)
library(leafem)
library(stars)

tif = "https://obis-shared.s3.amazonaws.com/silas/cogmv1pr_lassonaive_key100803_basevars_current.tif"
x1 = stars::read_stars(tif)

leaflet() %>%
  addTiles() %>%
  leafem:::addGeoRaster(
    x1
    , opacity = 1
    , colorOptions = colorOptions(
      palette = grey.colors(256)
    )
  )

"data/maps/species/cogmv1pr_lassonaive_key100803_basevars_current.tif"

teste <- "https://shiny.obis.org/distmaps/data/maps/species/cogmv1pr_lassonaive_key100803_basevars_current.tif"

leaflet() %>%
  addTiles() %>%
  leafem::addCOG(
    url = teste#"~/Research/mpa_europe/mpaeu_map_platform/data/maps/species/cogmv1pr_lassonaive_key100803_basevars_current.tif"
    # , opacity = 1
    # , colorOptions = colorOptions(
    #   palette = grey.colors(256)
    )

base_url = "https://sentinel-cogs.s3.us-west-2.amazonaws.com"
image_url = "sentinel-s2-l2a-cogs/46/X/DG/2022/8/S2B_46XDG_20220829_0_L2A/L2A_PVI.tif"
url = sprintf("%s/%s", base_url, image_url)





library(leaflet)
library(leafem)

base_url = "https://sentinel-cogs.s3.us-west-2.amazonaws.com"
image_url = "sentinel-s2-l2a-cogs/46/X/DG/2022/8/S2B_46XDG_20220829_0_L2A/L2A_PVI.tif"
url = sprintf("%s/%s", base_url, image_url)



min_scale = 0; max_scale = 1
js_scale = paste0("function (values) {
                    var scale = chroma.scale(['#7d1500', '#da4325', '#eca24e', '#e7e2bc', '#5cc3af', '#0a6265']).domain([", min_scale, ",", max_scale, "]);
                    var val = values[0];
                    if (isNaN(val)) return '#22c7e800';
                    if (val === 0) return;
                    if (val < 0) return;
                    return scale(val).hex();
                    }")

leaflet() |>
  addTiles() |>
  leafem:::addCOG(
    url = "https://obis-shared.s3.amazonaws.com/silas/cogmv1pr_lassonaive_key100803_basevars_current.tif",
    opacity = 1,
    colorOptions = colorOptions(
      palette = rev(c("#7d1500", "#da4325", "#eca24e", "#e7e2bc", "#5cc3af", "#0a6265")),
      domain = c(0, 1),
      na.color = NA
    ), autozoom = F, pixelValuesToColorFn = JS(js_scale)
  )




pal = hcl.colors(256, "viridis")
brks = seq(0, 500, 50)

myCustomJSFunc = htmlwidgets::JS(
  "
    pixelValuesToColorFn = (raster, colorOptions) => {
      const cols = colorOptions.palette;
      var scale = chroma.scale(cols);

      if (colorOptions.breaks !== null) {
        scale = scale.classes(colorOptions.breaks);
      }
      var pixelFunc = values => {
        let clr = scale.domain([raster.mins, raster.maxs]);
        if (isNaN(values)) return colorOptions.naColor;
        if (values < 120) return colorOptions.naColor;
        return clr(values).hex();
      };
      return pixelFunc;
    };
  "
)


leaflet() |>
  addTiles() |>
  leafem:::addCOG(
    url = "https://obis-shared.s3.amazonaws.com/silas/cogmv1pr_lassonaive_key100803_basevars_current.tif",
    opacity = 1,
    colorOptions =  leafem:::colorOptions(
      palette = pal #viridisLite::cividis
      , breaks = brks
      , na.color = "transparent"
    ), autozoom = F, pixelValuesToColorFn = JS(myCustomJSFunc)
  )
