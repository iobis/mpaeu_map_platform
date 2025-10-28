layerStyleInput <- function(inputId, label = NULL, layers, 
                           default_alpha = 0.7, 
                           default_palette = "Set1") {
  
  # Get all available RColorBrewer palettes
  all_palettes <- rownames(RColorBrewer::brewer.pal.info)
  
  # Generate palette previews as data attributes
  palette_data <- lapply(all_palettes, function(pal) {
    info <- RColorBrewer::brewer.pal.info[pal, ]
    n_colors <- min(info$maxcolors, 9)
    colors <- RColorBrewer::brewer.pal(n_colors, pal)
    list(name = pal, colors = paste(colors, collapse = ","))
  })

  # path <- normalizePath("./scripts/custom_inputs")
  
  # # Create the HTML dependency
  # styleDependency <- htmltools::htmlDependency(
  #   name = "layerStyleInput",
  #   version = "1.0.0",
  #   src = c(file = path),
  #   script = "layerStyleInput.js",
  #   stylesheet = "layerStyleInput.css"
  # )
  
  # Generate HTML for each layer
  style_html <- htmltools::tags$div(
    class = "layer-style-input",
    id = inputId,
    if (!is.null(label)) htmltools::tags$label(label, class = "control-label"),
    htmltools::tags$div(
      class = "style-container",
      lapply(layers, function(layer) {
        layer_id <- gsub("[^[:alnum:]]", "_", layer)
        
        htmltools::tags$div(
          class = "layer-style-item",
          `data-layer` = layer,
          
          # Layer name header
          htmltools::tags$div(
            class = "layer-name",
            layer
          ),
          
          # Alpha slider
          htmltools::tags$div(
            class = "style-control",
            htmltools::tags$label(
              class = "control-label-small",
              "Opacity:"
            ),
            htmltools::tags$input(
              type = "range",
              class = "alpha-slider",
              min = "0.5",
              max = "1",
              step = "0.05",
              value = default_alpha,
              `data-layer` = layer
            ),
            htmltools::tags$span(
              class = "alpha-value",
              sprintf("%.2f", default_alpha)
            )
          ),
          
          # Palette selector
          htmltools::tags$div(
            class = "style-control",
            htmltools::tags$label(
              class = "control-label-small",
              "Palette:"
            ),
            htmltools::tags$select(
              class = "palette-selector",
              `data-layer` = layer,
              lapply(all_palettes, function(pal) {
                htmltools::tags$option(
                  value = pal,
                  selected = if (pal == default_palette) NA else NULL,
                  `data-colors` = paste(RColorBrewer::brewer.pal(min(RColorBrewer::brewer.pal.info[pal, "maxcolors"], 9), pal), collapse = ","),
                  pal
                )
              })
            ),
            htmltools::tags$div(
              class = "palette-preview",
              `data-layer` = layer
            )
          )
        )
      })
    ),
    
    # Apply button
    htmltools::tags$div(
      class = "apply-button-container",
      htmltools::tags$button(
        type = "button",
        class = "btn btn-primary apply-style-btn",
        id = paste0(inputId, "_apply"),
        "Apply Style"
      )
    )
  )
  
  # Attach the dependency and return
  #htmltools::attachDependencies(style_html, styleDependency)
  style_html
}