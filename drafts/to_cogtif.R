infile <- list.files("data/maps/species", full.names = T)

outfile <- gsub("mv1_pr", "cogmv1pr", infile)
outfile <- gsub("lasso_naive", "lassonaive", outfile)

for (i in 1:length(infile)) {
  system(paste("rio cogeo create", infile[i], outfile[i]))
  #file.remove(infile[i])
}