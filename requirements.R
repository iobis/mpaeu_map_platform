############################## MPA Europe project ##############################
########### WP3 - Species and biogenic habitat distributions (UNESCO) ##########
# March of 2024
# Authors: Silas C. Principe, Pieter Provoost
# Contact: s.principe@unesco.org
############################ Project Requirements ##############################

# Needed packages on CRAN
req_packages <- c(
  "leaflegend",
  "listviewer",
  "plotly",
  "leaflet.extras2",
  "waiter",
  "bsicons",
  "reactable",
  "quarto",
  "leafgl",
  "spatstat",
  "shinyalert",
  "arrow",
  "sfarrow",
  "leaflegend"
)

# Needed packages on GitHub
git_packages <- c("obissdm")
git_packages_source <- c(
  "iobis/mpaeu_msdm"
)

# Install Python packages
# reticulate::py_install(c("rio-cogeo", "xarray", "zarr"), pip = TRUE)