for (i in 1:length(lf)) {
  r <- rast(lf[i])
  
  for (z in 1:nlyr(r)) {
    nam <- names(r)[z]
    
    r_temp <- r[[z]] * 100
    
    r_temp <- as.int(r_temp)
    
    outf <- lf[i]
    outf <- gsub("_cog.tif", paste0("_scen=", nam, "_cog.tif"), outf)
    
    terra::writeRaster(r_temp, outf, datatype = "INT1U")
  }
  
  fs::file_delete(lf[i])
  
}