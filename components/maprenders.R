########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
########################## Map render and placeholder ##########################

# Start by ploting/printing placeholders ----
output$mainMap <- renderLeaflet({
  mdebug("Rendering Leaflet map")
  m})

output$tableATitle <- renderText({ "Select a map to start" })

# welcome_message <- htmltools::div(
#   htmltools::img(src = "www/disclaimer.png", width = "100%"),
#   htmltools::span(
#     htmltools::HTML(
#       "Welcome to the MPA Europe map platform. Here, you can explore species and habitat range maps developed for the <a href='https://mpa-europe.eu/' target='_blank'>MPA Europe project</a>.
# Model fitting is now complete, and maps are available for 12,684 species. We are currently in the <b>validation and improvement phase</b>, where we are assessing each modeling decision, comparing our results with other models, and inviting partners to review the results.
# During this phase, improvements are expected as we implement alternative approaches and corrections. <b>The final launch will be accompanied by a scientific publication documenting all the methods used.</b> <br><br>
# When using the models, it is important to explore all the details to evaluate the model quality. If you identify any errors, please email us at helpdesk@obis.org. You can track the project’s progress here: <a href='https://github.com/iobis/mpaeu_map_platform/blob/main/NEWS.md' target='_blank'>github.com/iobis/mpaeu_map_platform</a>."
#     ), style = "text-align: left !important;"
#   )
# )

welcome_message <- htmltools::tagList(
  htmltools::tags$style(htmltools::HTML("
    .card {
      border: solid;
      margin-top: 15px;
      margin-bottom: 15px;
      margin-right: 5px;
      margin-left: 5px;
      padding: 3px;
      border-radius: 10px;
      border-color: #bcbcbc;
      cursor: pointer;
      transition: background-color 0.2s ease;
    }
    .card:hover {
      background-color: #f0f0f0;
    }
    .card-icon {
      display: flex;
      flex-direction: row;
      height: 100%;
      align-items: center;
      justify-content: center;
      font-size: 2.1em;
    }
  ")),
  
  htmltools::div(class = "container",
    htmltools::div(class = "row",
      htmltools::div(class = "g-col-12",
        htmltools::p(
          htmltools::HTML("Welcome to the MPA Europe map platform. Here, you can explore species and habitat range maps developed for the <a href='https://mpa-europe.eu/' target='_blank'>MPA Europe project</a>.")
        ),
        # Card 1
        bslib::card(
          bslib::layout_column_wrap(
            style = htmltools::css(grid_template_columns = "1fr 6fr"),
            htmltools::div(
              htmltools::tags$i(class = "fa-solid fa-chalkboard-user fa-2x", style="color: #184e77;"),
              class = "card-icon"
            ),
            htmltools::div(
              htmltools::h4(htmltools::HTML('<span style="color: #184e77;">Learn how to interpret range maps</span> <i class="fa-solid fa-arrow-up-right-from-square" style="font-size: 0.85em; color: #b2b2b2;"></i>'), style = "margin-top: 0px !important;"),
              htmltools::p("Species distribution models (SDMs) are valuable tools, but it's important to understand how to interpret their results correctly."),
              style = "text-align: left;")
          ),
          class = "card",
          onclick = "window.open('https://iobis.github.io/mpaeu_docs/understanding.html', '_blank')" 
        ),
        # Card 2
        bslib::card(
          bslib::layout_column_wrap(
            style = htmltools::css(grid_template_columns = "1fr 6fr"),
            htmltools::div(
              htmltools::tags$i(class = "fa-solid fa-book fa-2x", style="color: #1e6091;"),
              class = "card-icon"
            ),
            htmltools::div(
              htmltools::h4(htmltools::HTML('<span style="color: #1e6091;">Read the documentation <i class="fa-solid fa-arrow-up-right-from-square" style="font-size: 0.85em; color: #b2b2b2;"></i>'), style = "margin-top: 0px !important;"),
              htmltools::p("Explore the full details of the framework used to produce the range maps."),
              style = "text-align: left;")
          ),
          class = "card",
          onclick = "window.open('https://iobis.github.io/mpaeu_docs/', '_blank')" 
        )#,
        # Card 3
        # bslib::card(
        #   bslib::layout_column_wrap(
        #     style = htmltools::css(grid_template_columns = "1fr 6fr"),
        #     htmltools::div(
        #       htmltools::tags$i(class = "fa-solid fa-binoculars fa-2x", style="color: #1a759f;"),
        #       class = "card-icon"
        #     ),
        #     htmltools::div(
        #       htmltools::h4("Preview version 2", style = "margin-top: 0px !important; color: #1a759f;"),
        #       htmltools::p("We're finalizing version 2 of the models (expected by the end of July), but you can already preview results for some species."),
        #       style = "text-align: left;")
        #   ),
        #   class = "card", id = "version_card",
        #   onclick = "Shiny.setInputValue('card_click', 'clicked', {priority: 'event'});"  # Example click handler
        # )
      )
    )
  )
)


shinyalert("Welcome", welcome_message, type = "info", html = T, size = "m", closeOnClickOutside = T,
confirmButtonText = "Go to the maps", confirmButtonCol = "#184e77")