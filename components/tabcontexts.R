########################### MPA Europe - Map platform ##########################
########################## SDMs created by WP3 - OBIS ##########################
# June of 2024
# Authors: Silas Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
######################### Tab contextual information ###########################

# Species ----
# Species title
output$selectedSpecies <- renderText({
  input$speciesSelect
})

# Context info
output$contextSpecies <- renderText({
  selinf <- speciesinfo[speciesinfo$species == input$speciesSelect,]
  if (input$speciesSelect != "") {
    spmodinfo <- jsonlite::read_json(glue::glue("https://mpaeu-dist.s3.amazonaws.com/results/species/taxonid={selinf$key}/model={selinf$acro}/taxonid={selinf$key}_model={selinf$acro}_what=log.json"))
  } else {
    spmodinfo <- NULL
  }
  
  modinfo <- verify_posteval(spmodinfo)
  
  nrec <- modinfo$nrec
  neval <- modinfo$neval
  tenvstat <- modinfo$tenvstat
  tenvval <- modinfo$tenvval
  context_tags <- gen_context_boxes(
    model_quality = selinf$sdm_quality,
    reviewed = selinf$sdm_reviewed,
    conservation_status = selinf$redlist_category
  )
  data_sources <- shiny::actionLink("dataSourcesButton", "see data sources.")
  data_sources <- gsub(">see", 'style="color: #07A5F0 !important;">see', data_sources)
  glue::glue(
    "<b>Phylum:</b> {selinf$phylum} > <b>Order:</b> {selinf$order} > <b>Family:</b> {selinf$family} <br>
      <b>AphiaID:</b> <a style = 'text-decoration: none; color: #07A5F0;' target='_blank' href = 'https://www.marinespecies.org/aphia.php?p=taxdetails&id={selinf$key}'>{selinf$key}</a><br><br>
    <b>Number of records:</b> {nrec} - {data_sources}<br>
    <b>Number of records for independent evaluation:</b> {neval} <br><br>
    <b>Additional info:</b> All inside thermal envelope? {tenvstat} ({tenvval}%)
    {context_tags}"
  )
})

# Data source modal
observe({
  showModal(modalDialog(
    citation_mod(select_params$species$spkey, cit_species_ds, cit_general_ds),
    size = "xl", easyClose = T
  ))
}) |>
  bindEvent(input$dataSourcesButton)

# Species evaluation modal
observe({
  showModal(modalDialog(
    evaluation_modal("example"),
    size = "xl", easyClose = T
  ))
}) |>
  bindEvent(input$modelQualityButton)

# Thermal ----
# Species title
output$selectedSpeciesThermal <- renderText({
  input$speciesSelectThermal
}) |>
  bindEvent(input$speciesSelectThermal, ignoreInit = T)

# Context info
output$contextSpeciesThermal <- renderText({
  selinf <- speciesinfo[speciesinfo$species == input$speciesSelectThermal,]
  if (input$speciesSelectThermal != "") {
    spmodinfo <- jsonlite::read_json(glue::glue("https://mpaeu-dist.s3.amazonaws.com/results/species/taxonid={selinf$key}/model={selinf$acro}/taxonid={selinf$key}_model={selinf$acro}_what=log.json"))
  } else {
    spmodinfo <- NULL
  }
  nrec <- unlist(spmodinfo$model_fit_points)
  glue::glue(
    "<b>Phylum:</b> {selinf$phylum} > <b>Order:</b> {selinf$order} > <b>Family:</b> {selinf$family} <br>
      <b>AphiaID:</b> <a style = 'text-decoration: none; color: #07A5F0;' target='_blank' href = 'https://www.marinespecies.org/aphia.php?p=taxdetails&id={selinf$key}'>{selinf$key}</a><br><br>
    <b>Number of records:</b> {nrec} <br>"
  )
})

# Habitat ----
# Species title
output$selectedHabitat <- renderText({
  gsub("_", " ", stringr::str_to_title(input$habitatSelect))
}) |>
  bindEvent(input$habitatSelect, ignoreInit = T)

# Context info
output$contextHabitat <- renderText({
  if (input$habitatSelect != "") {
    context_file <- jsonlite::read_json("www/context_info.json")
    HTML(
      switch(input$habitatSelect,
        seagrass = unlist(context_file$habitats[["seagrass"]]),
        kelp = unlist(context_file$habitats[["kelp"]]),
        polychaete_reefs = unlist(context_file$habitats[["polychaete_reefs"]]),
        maerl = unlist(context_file$habitats[["maerl"]]),
        bivalves_beds = unlist(context_file$habitats[["bivalves_beds"]]),
        corals = unlist(context_file$habitats[["corals"]])
      )
    )
  }
})

# Diversity ----
# Species title
output$selectedMetric <- renderText({
  if (input$diversitySelect == "lcbd") {
    toupper(input$diversitySelect)
  } else {
    stringr::str_to_title(input$diversitySelect)
  }
}) |>
  bindEvent(input$diversitySelect, ignoreInit = T)

# Context info
output$contextMetric <- renderText({
  if (input$diversitySelect != "") {
    switch(input$diversitySelect,
        richness = "Number of species (SDMs or raw data). Use the 'Valids' model to obtain the SDM-based richness with all species."
      )
  }
})