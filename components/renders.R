########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
###################### Contextual information renders ##########################

# Print titles ----
output$tableATitle <- renderText({title_state$to_print$tableA})
output$graphTitle <- renderText({title_state$to_print$graph})
output$tableBTitle <- renderText({title_state$to_print$tableB})
output$textTitle <- renderText({title_state$to_print$modelTitle})

# Context info ----
output$tableA <- reactable::renderReactable({
  reactable::reactable(continfo$tableA,
                       pagination = TRUE,
                       defaultPageSize = 5, filterable = TRUE, highlight = TRUE)
}) |>
  bindEvent(continfo$tableA)

output$plotA <- renderPlotly({
  continfo$plotA
}) |>
  bindEvent(continfo$plotA)

output$tableB <- reactable::renderReactable({
  if (active_tab$current == "habitat") {
    reactable::reactable(continfo$tableB,
                       pagination = TRUE,
                       defaultPageSize = 5, filterable = TRUE, highlight = TRUE,
                       resizable = TRUE, wrap = FALSE, columns = list(
                            AphiaID = reactable::colDef(cell = function(value) {
                              htmltools::tags$a(href = paste0("https://obis.org/taxon/", value),
                                                target = "_blank", as.character(value))
                            }),
                            scientificName = reactable::colDef(minWidth = 220),
                            `Regions of occurrence` = reactable::colDef(minWidth = 350)
                          ))
  } else if (active_tab$current == "diversity") {
    reactable::reactable(continfo$tableB,
                          pagination = TRUE,
                          defaultPageSize = 5, filterable = TRUE, highlight = TRUE,
                          resizable = TRUE, columns = list(
                            AphiaID = reactable::colDef(cell = function(value) {
                              htmltools::tags$a(href = paste0("https://obis.org/taxon/", value),
                                                target = "_blank", as.character(value))
                            }),
                            scientificName = reactable::colDef(minWidth = 180)
                          ))
  } else {
    reactable::reactable(continfo$tableB,
                       pagination = TRUE,
                       defaultPageSize = 5, filterable = TRUE, highlight = TRUE)
  }
}) |>
  bindEvent(continfo$tableB)

output$textTitle <- renderText({
  continfo$text[[1]]
}) |>
  bindEvent(continfo$text)

output$textModel <- renderText({
  continfo$text[[2]]
}) |>
  bindEvent(continfo$text)