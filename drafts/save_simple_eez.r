library(sf)

sh <- read_sf("data/Intersect_EEZ_IHO_v4_2020.shp")
sh

sh <- sh[,c("MRGID", "EEZ")]
sh

sf_use_s2(FALSE)
sh_simp <- st_simplify(st_make_valid(sh[!is.na(sh$EEZ),]), dTolerance = 0.1)

sfarrow::st_write_parquet(sh_simp, "data/EEZ_IHO_simp_edited.parquet")
