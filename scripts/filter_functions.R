# Filter modals - Species and thermal range tabs

# Species modal -----
filterSpeciesModal <- function(failed = FALSE) {
  modalDialog(
    bslib::layout_column_wrap(
      width = 1 / 2, heights_equal = "row",
      bslib::card(
        bslib::card_body(
        selectInput(
          inputId = "groupSelect",
          label = "Group",
          choices = c("All" = "all")
        ),
        selectInput(
          inputId = "commonSelect",
          label = "Common name (EN)",
          choices = c("All" = "all")
        ),
        selectInput(
          inputId = "seaSelect",
          label = "Presence in sea/region",
          choices = c("All" = "all")
        ),
        checkboxInput(
          inputId = "includeProjects",
          label = "Include from other projects?",
          value = FALSE
        ),
        full_screen = F
      )),
      bslib::card(
        bslib::card_body(
        selectInput(
          inputId = "phylumSelect",
          label = "Phylum",
          choices = c("All" = "all")
        ),
        selectInput(
          inputId = "classSelect",
          label = "Class",
          choices = c("All" = "all")
        ),
        selectInput(
          inputId = "orderSelect",
          label = "Order",
          choices = c("All" = "all")
        ),
        selectInput(
          inputId = "familySelect",
          label = "Family",
          choices = c("All" = "all")
        ),
        full_screen = F
      ))
    ),
    footer = tagList(
      htmltools::span("Total:", textOutput("filterN"),
        style = "position: absolute; left: 0px; margin-left: 10px; display: flex;"),
      modalButton("Cancel"),
      actionButton(inputId = "speciesActionOK", label = "OK")
    )
  )
}

filter_opts <- function(species_list, group = NULL, common = NULL,
  sea = NULL, include = NULL, phylum = NULL, class = NULL, order = NULL, family = NULL) {

    if (!is.null(include) && !include) {
      species_list <- species_list[species_list$study == "mpaeurope",]
    }
    if (!is.null(group) && group != "all") {
      species_list <- species_list[species_list$sdm_group == group,]
    }
    if (!is.null(common) && common != "all") {
      species_list <- species_list[grepl(common, species_list$common_names),]
    }
    if (!is.null(sea) && sea != "all") {
      species_list <- species_list[grepl(sea, species_list$region_name),]
    }
    if (!is.null(phylum) && phylum != "all") {
      species_list <- species_list[species_list$phylum == phylum,]
    }
    if (!is.null(class) && class != "all") {
      species_list <- species_list[species_list$class == class,]
    }
    if (!is.null(order) && order != "all") {
      species_list <- species_list[species_list$order == order,]
    }
    if (!is.null(family) && family != "all") {
      species_list <- species_list[species_list$family == family,]
    }

    in_common <- stringr::str_split(species_list$common_names, pattern = "; ")
    in_common <- unique(unlist(in_common, use.names = F))
    in_common <- c("All" = "all", in_common)

    in_region <- stringr::str_split(species_list$region_name, pattern = "; ")
    in_region <- unique(unlist(in_region, use.names = F))
    in_region <- c("All" = "all", in_region)

    updateSelectizeInput(session, "groupSelect",
     choices = c("All" = "all", unique(species_list$sdm_group)),
     selected = group, server = TRUE)
    updateSelectizeInput(session, "commonSelect",
     choices = c("All" = "all", in_common), selected = common, server = TRUE)
    updateSelectizeInput(session, "seaSelect", 
     choices = c("All" = "all", in_region), selected = sea, server = TRUE)
    updateSelectizeInput(session, "phylumSelect",
     choices = c("All" = "all", unique(species_list$phylum)),
     selected = phylum, server = TRUE)
    updateSelectizeInput(session, "classSelect",
     choices = c("All" = "all", unique(species_list$class)),
     selected = class, server = TRUE)
    updateSelectizeInput(session, "orderSelect",
     choices = c("All" = "all", unique(species_list$order)),
     selected = order, server = TRUE)
    updateSelectizeInput(session, "familySelect",
     choices = c("All" = "all", unique(species_list$family)),
     selected = family, server = TRUE)
    
    return(species_list)
  }



# Thermal modal -----
filterThermalModal <- function(failed = FALSE) {
  modalDialog(
    bslib::layout_column_wrap(
      width = 1 / 2, heights_equal = "row",
      bslib::card(
        bslib::card_body(
        selectInput(
          inputId = "groupThermalSelect",
          label = "Group",
          choices = c("All" = "all")
        ),
        selectInput(
          inputId = "commonThermalSelect",
          label = "Common name (EN)",
          choices = c("All" = "all")
        ),
        selectInput(
          inputId = "seaThermalSelect",
          label = "Presence in sea/region",
          choices = c("All" = "all")
        ),
        checkboxInput(
          inputId = "includeThermalProjects",
          label = "Include from other projects?",
          value = FALSE
        ),
        full_screen = F
      )),
      bslib::card(
        bslib::card_body(
        selectInput(
          inputId = "phylumThermalSelect",
          label = "Phylum",
          choices = c("All" = "all")
        ),
        selectInput(
          inputId = "classThermalSelect",
          label = "Class",
          choices = c("All" = "all")
        ),
        selectInput(
          inputId = "orderThermalSelect",
          label = "Order",
          choices = c("All" = "all")
        ),
        selectInput(
          inputId = "familyThermalSelect",
          label = "Family",
          choices = c("All" = "all")
        ),
        full_screen = F
      ))
    ),
    footer = tagList(
      htmltools::span("Total:", textOutput("filterThermalN"),
        style = "position: absolute; left: 0px; margin-left: 10px; display: flex;"),
      modalButton("Cancel"),
      actionButton(inputId = "speciesThermalActionOK", label = "OK")
    )
  )
}

filter_opts_thermal <- function(species_list, group = NULL, common = NULL,
  sea = NULL, include = NULL, phylum = NULL, class = NULL, order = NULL, family = NULL) {

    if (!is.null(include) && !include) {
      species_list <- species_list[species_list$study == "mpaeurope",]
    }
    if (!is.null(group) && group != "all") {
      species_list <- species_list[species_list$sdm_group == group,]
    }
    if (!is.null(common) && common != "all") {
      species_list <- species_list[grepl(common, species_list$common_names),]
    }
    if (!is.null(sea) && sea != "all") {
      species_list <- species_list[grepl(sea, species_list$region_name),]
    }
    if (!is.null(phylum) && phylum != "all") {
      species_list <- species_list[species_list$phylum == phylum,]
    }
    if (!is.null(class) && class != "all") {
      species_list <- species_list[species_list$class == class,]
    }
    if (!is.null(order) && order != "all") {
      species_list <- species_list[species_list$order == order,]
    }
    if (!is.null(family) && family != "all") {
      species_list <- species_list[species_list$family == family,]
    }

    in_common <- stringr::str_split(species_list$common_names, pattern = "; ")
    in_common <- unique(unlist(in_common, use.names = F))
    in_common <- c("All" = "all", in_common)

    in_region <- stringr::str_split(species_list$region_name, pattern = "; ")
    in_region <- unique(unlist(in_region, use.names = F))
    in_region <- c("All" = "all", in_region)

    updateSelectizeInput(session, "groupThermalSelect",
     choices = c("All" = "all", unique(species_list$sdm_group)),
     selected = group, server = TRUE)
    updateSelectizeInput(session, "commonThermalSelect",
     choices = c("All" = "all", in_common), selected = common, server = TRUE)
    updateSelectizeInput(session, "seaThermalSelect", 
     choices = c("All" = "all", in_region), selected = sea, server = TRUE)
    updateSelectizeInput(session, "phylumThermalSelect",
     choices = c("All" = "all", unique(species_list$phylum)),
     selected = phylum, server = TRUE)
    updateSelectizeInput(session, "classThermalSelect",
     choices = c("All" = "all", unique(species_list$class)),
     selected = class, server = TRUE)
    updateSelectizeInput(session, "orderThermalSelect",
     choices = c("All" = "all", unique(species_list$order)),
     selected = order, server = TRUE)
    updateSelectizeInput(session, "familyThermalSelect",
     choices = c("All" = "all", unique(species_list$family)),
     selected = family, server = TRUE)
    
    return(species_list)
  }