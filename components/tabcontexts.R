# Tab contexts
output$selectedSpecies <- renderText({
  input$speciesSelect
})

output$contextSpecies <- renderText({
  selinf <- speciesinfo[speciesinfo$species == input$speciesSelect,]
  if (input$speciesSelect != "") {
    spmodinfo <- jsonlite::read_json(glue::glue("data/maps/taxonid={selinf$key}/model=inteval/taxonid={selinf$key}_model=inteval_what=log.json"))
  } else {
    spmodinfo <- NULL
  }
  
  modinfo <- verify_posteval(spmodinfo)
  
  nrec <- modinfo$nrec
  neval <- modinfo$neval
  tenvstat <- modinfo$tenvstat
  tenvval <- modinfo$tenvval
  glue::glue(
    "<b>Phylum:</b> {selinf$phylum} > <b>Order:</b> {selinf$order} > <b>Family:</b> {selinf$family} <br>
      <b>AphiaID:</b> <a style = 'text-decoration: none; color: #07A5F0;' target='_blank' href = 'https://www.marinespecies.org/aphia.php?p=taxdetails&id={selinf$key}'>{selinf$key}</a><br><br>
    <b>Number of records:</b> {nrec} <br>
    <b>Number of records for independent evaluation:</b> {neval} <br><br>
    <b>Additional info:</b> All inside thermal envelope? {tenvstat} ({tenvval}%)"
  )
})

output$contextSpeciesThermal <- renderText({
  selinf <- speciesinfo[speciesinfo$species == input$speciesSelectThermal,]
  if (input$speciesSelectThermal != "") {
    spmodinfo <- jsonlite::read_json(glue::glue("data/maps/taxonid={selinf$key}/model=inteval/taxonid={selinf$key}_model=inteval_what=log.json"))
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