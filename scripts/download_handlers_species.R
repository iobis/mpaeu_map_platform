species_download_modal <- function(species, key, model, scenario, decade, acro) {
    if (species == "") {
        modalDialog(
            bslib::layout_column_wrap(
                width = 1, heights_equal = "row",
                bslib::card("Select a species before downloading data."),
                bslib::card(
                    bslib::card_body(
                        htmltools::h5("Want to download all available data?"),
                        htmltools::span(htmltools::HTML(
                            "<p>You can also download through our 
                            <a href='https://mpaeu-dist.s3.amazonaws.com/index.html' target='_blank'>
                            AWS S3 bucket directly</a>
            or using <a href='https://github.com/iobis/mpaeu_map_platform/blob/main/scripts/download_aws_model.R'
             target='_blank'>this R script.</a>"
                        )),
                        full_screen = F
                    )
                )
            ),
            footer = tagList(
                modalButton("Cancel")
            ), size = "l"
        )
    } else {
        scenario_ed <- ifelse(grepl("current", scenario), "Current", toupper(scenario))
        model_ed <- ifelse(grepl("rf", model), "RandomForest", toupper(model))

        modalDialog(
            bslib::layout_column_wrap(
                width = 1, heights_equal = "row",
                bslib::card(
                    bslib::card_header("Download species data"),
                    bslib::card_body(
                        "What do you want to download?",
                        shiny::radioButtons(
                            "speciesDownloadType",
                            label = "",
                            choices = c("Selected species/scenario" = "selected",
                                        "Full data for this species" = "full"),
                            inline = T
                        ),
                        htmltools::span(
                            htmltools::HTML(
                                paste0(
    glue::glue("The current species selected is <i><b>{species}</b></i> (AphiaID <b>{key}</b>) for model <b>{model_ed}</b>, "),
    glue::glue("in the scenario <b>{scenario_ed}</b> and decade <b>{ifelse(grepl(50, decade), 2050, 2100)}</b>."),
    "<br>A partial download is approximately 2MB and a full download 140MB."
                                )
                            ),
                            style = "color: #097da5"
                        ),
                        full_screen = F
                    )
                ),
                bslib::card(
                    bslib::card_body(
                        htmltools::h5("Choose the full data if..."),
                        htmltools::tags$ul(
                            htmltools::tags$li("You want to do further analysis with the results"),
                            htmltools::tags$li("You want to produce other visualizations"),
                            htmltools::tags$li("You want to run the model locally or 
                            using a JupyterHub instance (e.g. GoogleColab®)")
                        ),
                        full_screen = F
                    )
                ),
                bslib::card(
                    bslib::card_body(
                        htmltools::h5("Want to download all available data?"),
                        htmltools::span(htmltools::HTML(
                            "<p>You can also download through our 
                            <a href='https://mpaeu-dist.s3.amazonaws.com/index.html' target='_blank'>
                            AWS S3 bucket</a>
            or using <a href='https://github.com/iobis/mpaeu_map_platform/blob/main/scripts/download_aws_model.R' 
            target='_blank'>this R script.</a>"
                        )),
                        full_screen = F
                    )
                )
            ),
            footer = tagList(
                modalButton("Cancel"),
                downloadButton("downloadSpeciesAction", label = "Download")
            ), size = "l"
        )
    }
}

species_code_download <- function(species, key, model) {
    if (species == "") {
        modalDialog(
            bslib::layout_column_wrap(
                width = 1, heights_equal = "row",
                bslib::card("Select a species before downloading the code."),
                bslib::card(
                    bslib::card_body(
                        htmltools::h5("Want to see all the framework?"),
                        htmltools::span(htmltools::HTML(
                            "<p>You can also explore our approach through 
                            <a href='https://mpaeu-dist.s3.amazonaws.com/index.html' target='_blank'>
                            the GitHub repository</a>"
                        )),
                        full_screen = F
                    )
                )
            ),
            footer = tagList(
                modalButton("Cancel")
            ), size = "l"
        )
    } else {

        model_ed <- ifelse(grepl("rf", model), "RandomForest", toupper(model))

        modalDialog(
            bslib::layout_column_wrap(
                width = 1, heights_equal = "row",
                bslib::card(
                    bslib::card_header("Download code to run model locally"),
                    bslib::card_body(
                        "What type of code do you want?",
                        shiny::radioButtons(
                            "speciesCodeDownloadType",
                            label = "",
                            choices = c("Jupyter Notebook" = "ipynb",
                                        "Quarto document" = "qmd"),
                            inline = T
                        ),
                        htmltools::span(
                            htmltools::HTML(
    glue::glue("The current species selected is <i><b>{species}</b></i> (AphiaID <b>{key}</b>) for model <b>{model_ed}</b>.")
                            ),
                            style = "color: #097da5"
                        ),
                        full_screen = F
                    )
                ),
                bslib::card(
                    bslib::card_body(
                        htmltools::h5("What type of code do I need?"),
                        "Choose 'Quarto document' if you use RStudio as your IDE. 
                        If you use VSCode or if you want to run the code in a Jupyter instance (e.g. GoogleColab®), then choose 'Jupyter Notebook'",
                        full_screen = F
                    )
                ),
                bslib::card(
                    bslib::card_body(
                        htmltools::h5("Remember: you also need the data"),
                        htmltools::span(htmltools::HTML(
                            "<p>After you download the code, remember to download the data by clicking on 'Download the data' in the app. 
                            You can also download all data through our 
                            <a href='https://mpaeu-dist.s3.amazonaws.com/index.html' target='_blank'>
                            AWS S3 bucket</a>
            or using <a href='https://github.com/iobis/mpaeu_map_platform/blob/main/scripts/download_aws_model.R' 
            target='_blank'>this R script.</a>"
                        )),
                        full_screen = F
                    )
                )
            ),
            footer = tagList(
                modalButton("Cancel"),
                downloadButton("downloadCodeSpeciesAction", label = "Download")
            ), size = "l"
        )
    }
}