# Info renders

# Thermal
output$selectedSpeciesThermal <- renderText({
  input$speciesSelectThermal
}) %>%
  bindEvent(input$speciesSelectThermal, ignoreInit = T)

# Print titles ----
output$tableATitle <- renderText({title_state$to_print$tableA})
output$graphTitle <- renderText({title_state$to_print$graph})
output$tableBTitle <- renderText({title_state$to_print$tableB})
output$textTitle <- renderText({title_state$to_print$modelTitle})

# Context info ----
output$tableA <- DT::renderDT({
  #continfo$tableA
  DT::datatable(continfo$tableA, options = list(paging =TRUE, pageLength = 5))
}) %>%
  bindEvent(continfo$tableA)

output$plotA <- renderPlotly({
  continfo$plotA
}) %>%
  bindEvent(continfo$plotA)

output$tableB <- DT::renderDT({
  DT::datatable(continfo$tableB, options = list(paging =TRUE, pageLength = 5))
}) %>%
  bindEvent(continfo$tableB)

output$textTitle <- renderText({
  continfo$text[[1]]
}) %>%
  bindEvent(continfo$text)

output$textModel <- renderText({
  continfo$text[[2]]
}) %>%
  bindEvent(continfo$text)