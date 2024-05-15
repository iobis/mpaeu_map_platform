# Post rendering code

files <- list.files()

files <- files[!files %in% c("_extensions", "_quarto.yml", "backup", "drafts", "mpaeu_map_platform.Rproj", "readme_files", "README.md", "out_bundle", "index_v1.qmd")]

fs::dir_create("out_bundle")
file.copy(files, "out_bundle", recursive = T)




