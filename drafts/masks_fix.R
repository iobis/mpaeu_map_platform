# Correct masks
# Temporary solution
library(terra)

cogeo_optim_alt <- function(id, verbose = TRUE) {
  
  if (verbose) cli::cli_alert_info("Optimizing {.path {id}} to COG using {.var rio}")
  outid <- id
  
  out <- sys::exec_internal("rio",
                            c("cogeo", "create", id, outid),
                            error = F)
  if (out$status == 1) {
    if (verbose) cli::cli_alert_danger("Optimization failed")
    to_return <- data.frame(file = id, status = "optimization-failed")
  } else {
    # Validate
    out <- sys::exec_internal("rio",
                              c("cogeo", "validate", outid),
                              error = F)
    if (out$status == 1) {
      to_return <- data.frame(file = id, status = "optimization-failed")
      if (verbose) cli::cli_alert_danger("Optimization failed")
    } else {
      out <- sys::as_text(out$stdout)
      if (grepl("is a valid cloud optimized GeoTIFF", out)) {
        to_return <- data.frame(file = id, status = "optimization-succeeded")
        if (verbose) cli::cli_alert_success("Optimization succeeded. Output file: {.path {outid}}")
      } else {
        to_return <- data.frame(file = id, status = "optimization-gen-failed")
        if (verbose) cli::cli_alert_danger("Optimization is invalid")
      }
    }
  }
  return(to_return)
}


all_masks <- list.files("data/maps/", recursive = T, pattern = "mask")
all_masks <- all_masks[!grepl("aux", all_masks)]

base <- rast("data/maps/taxonid=100803/model=inteval/predictions/taxonid=100803_model=inteval_method=maxnet_scen=current_cog.tif")
base[!is.na(base)] <- 0

for (i in 1:length(all_masks)) {
  r <- rast(paste0("data/maps/", all_masks)[i])
  r_base <- base
  r_base <- mask(r_base, r, updatevalue = 1, inverse = T)
  names(r_base) <- names(r)
  r_base <- as.int(r_base)
  writeRaster(r_base, filename = paste0("data/maps/", all_masks)[i], overwrite = T, datatype = "INT1U")
  result <- cogeo_optim_alt(paste0("data/maps/", all_masks)[i])
}