# Download species data
cli::cli_progress_step("Syncing species data with S3 bucket")
system(
    "aws s3 sync --no-sign-request s3://mpaeu-dist/results/species ./data/maps"
)

# Download habitat data
cli::cli_progress_step("Syncing habitat data with S3 bucket")
system(
    "aws s3 sync --no-sign-request s3://mpaeu-dist/results/habitat ./data/habitats"
)

# Download diversity data
cli::cli_progress_step("Syncing diversity data with S3 bucket")
system(
    "aws s3 sync --no-sign-request s3://mpaeu-dist/results/diversity ./data/diversity"
)

cli::cli_progress_done()
