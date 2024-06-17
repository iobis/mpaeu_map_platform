########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
######################### Extra information section ############################

# Extra info (page) - SPECIES ----
output$extraPlotA <- renderPlot({
  if (input$extraButton != 0 && input$extraButton %% 2 != 0) {
    bias <- readRDS("data/maps/taxonid=100803/model=inteval/metrics/taxonid=100803_model=inteval_what=biasmetrics.rds")
    k <- bias$k_stat
    l <- bias$l_stat
    # class(k) <- c("envelope", class(k))
    # class(l) <- c("envelope", class(l))
    par(mfrow = c(2, 1))
    plot(k, main = "Spatial bias - K-function")
    plot(l, main = "L-function")
  } else {
    plot.new()
  }
})

output$extraPlotB <- renderPlot({
  if (input$extraButton != 0 && input$extraButton %% 2 != 0) {
    shape <- terra::rast("data/maps/taxonid=100803/model=inteval/predictions/taxonid=100803_model=inteval_what=shape_cog.tif")[[1]]
    mess <- terra::rast("data/maps/taxonid=100803/model=inteval/predictions/taxonid=100803_model=inteval_what=mess_cog.tif")[[1]]
    cl <- c("#FDE725", "#B3DC2B", "#6DCC57", "#36B677", "#1F9D87", "#25818E", "#30678D", "#3D4988", "#462777", "#440154")
    cl_mess <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#666666")
    par(mfrow = c(2, 1))
    plot(shape, main = "Extrapolation - SHAPE", col = cl)
    plot(mess, main = "Extrapolation - MESS", , col = cl_mess)
  } else {
    plot.new()
  }
})