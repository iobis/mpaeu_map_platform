customTreeInput <- function(inputId, label = NULL, choices) {

  # path <- normalizePath("./scripts/custom_inputs")
  
  # treeDependency <- htmltools:::htmlDependency(
  #   name = "customTreeInput",
  #   version = "1.0.0",
  #   src = c(file = path),
  #   script = "customTreeInput.js",
  #   stylesheet = "customTreeInput.css"
  # )

  tree_html <- htmltools::tags$div(
    class = "custom-tree-input",
    id = inputId,
    if (!is.null(label)) htmltools::tags$label(label, class = "control-label"),
    htmltools::tags$div(
      class = "tree-container",
      lapply(names(choices), function(parent) {
        parent_id <- paste0(inputId, "_", gsub("[^[:alnum:]]", "_", parent))
        htmltools::tags$div(
          class = "tree-node",
          htmltools::tags$div(
            class = "node-label",
            `data-parent` = parent,
            parent
          ),
          htmltools::tags$div(
            class = "node-children collapsed", # <-- ADDED 'collapsed' for default state
            lapply(seq_along(choices[[parent]]), function(i) {
              child <- choices[[parent]][i]
              child_id <- paste0(parent_id, "_child_", i)
              htmltools::tags$div(
                class = "tree-child",
                htmltools::tags$input(
                  type = "checkbox",
                  name = parent_id,
                  class = "tree-checkbox",
                  id = child_id,
                  value = child,
                  `data-parent` = parent
                ),
                htmltools::tags$label(`for` = child_id, child)
              )
            })
          )
        )
      })
    )
  )
  
  #htmltools::attachDependencies(tree_html, treeDependency)
  tree_html
}
