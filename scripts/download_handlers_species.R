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
                            using a JupyterHub environment (e.g. GoogleColab®)")
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
                            AWS S3 bucket directly</a>
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
