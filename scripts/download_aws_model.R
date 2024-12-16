library(aws.s3)

output_folder <- "species"
fs::dir_create(output_folder)

bucket <- "mpaeu-dist"
s3_folder <- "results/species"

# Create a function to download
download_objs <- function(s3_objects, local_folder) {
    i <- 0
    total <- length(s3_objects)
    for (obj in s3_objects) {
        i <- i + 1
        cat("Downloading", i, "out of", total, "\n")
        s3_key <- obj$Key
        local_file <- file.path(local_folder, s3_key)

        if (!endsWith(s3_key, "/")) {
            save_object(
                object = s3_key,
                bucket = bucket,
                file = local_file,
                region = "",
                use_https = TRUE 
            )
            message(paste("Downloaded:", s3_key, "to", local_file))
        }
    }
    return(invisible(NULL))
}

# CODE 1: faster, look only at the target species
species <- c(1005391, 100599) # List the AphiaIDs of the species you want to download

for (sp in species) {

    s3_objects <- get_bucket(
        bucket = bucket,
        prefix = paste0(s3_folder, "/taxonid=", sp),
        use_https = TRUE, max = Inf
    )

    download_objs(s3_objects, file.path(output_folder, paste0("taxonid=", sp)))

}



# CODE 2: takes longer, will look for all objects

# THIS WILL TAKE A LONG TIME!
# All data will be more than 1TB, make sure you have space!
if (readline("Do you want to download all data? Y/N\n") == "Y") {
    s3_objects <- get_bucket(bucket = bucket, prefix = s3_folder, use_https = TRUE, max = Inf)

    download_objs(s3_objects, output_folder)
}

# FOR HABITATS:
# Change s3_folder for "results/habitat" and use code 2

# FOR DIVERSITY
# Change s3_folder for "results/diversity" and use code 2