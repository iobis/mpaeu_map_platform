# Post rendering code

files <- list.files()

files <- files[!files %in% c("_extensions", "backup", "drafts",
                             "mpaeu_map_platform.Rproj", "readme_files",
                             "README.md", "out_bundle", "index_v1.qmd",
                             "index_data", "emodnet_usecase.png", "masks_fix.R")]

# Temp:
#files <- files[!files %in% c("data")]

fs::dir_delete("out_bundle")
fs::dir_create("out_bundle")
file.copy(files, "out_bundle", recursive = T)




