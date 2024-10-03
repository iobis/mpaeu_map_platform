# Post rendering code

files <- list.files()

files <- files[!files %in% c("_extensions", "backup", "drafts",
                             "mpaeu_map_platform.Rproj", "readme_files",
                             "README.md", "out_bundle", "index_v1.qmd",
                             "index_data", "emodnet_usecase.png", "masks_fix.R", "data")]

files_data <- list.files("data", full.names = T)
files_data <- files_data[grepl("all_splist_2024|app_splist|sdm_conf",files_data)]

# Temp:
#files <- files[!files %in% c("data")]

fs::dir_delete("out_bundle")
fs::dir_create("out_bundle")
file.copy(files, "out_bundle", recursive = T)
fs::dir_create("out_bundle/data")
file.copy(files_data, paste0("out_bundle/", files_data))





