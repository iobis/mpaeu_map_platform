# PLotly full screen code
#### Plotly full screen data ----
# SVG icons
icons <- list()

# Fullscreen (source : https://fontawesome.com/icons/expand?f=classic&s=solid)
icons$expand <- "M32 32C14.3 32 0 46.3 0 64v96c0 17.7 14.3 32 32 32s32-14.3 32-32V96h64c17.7 0 32-14.3 32-32s-14.3-32-32-32H32zM64 352c0-17.7-14.3-32-32-32s-32 14.3-32 32v96c0 17.7 14.3 32 32 32h96c17.7 0 32-14.3 32-32s-14.3-32-32-32H64V352zM320 32c-17.7 0-32 14.3-32 32s14.3 32 32 32h64v64c0 17.7 14.3 32 32 32s32-14.3 32-32V64c0-17.7-14.3-32-32-32H320zM448 352c0-17.7-14.3-32-32-32s-32 14.3-32 32v64H320c-17.7 0-32 14.3-32 32s14.3 32 32 32h96c17.7 0 32-14.3 32-32V352z"

dirty_js <- function(x) {
  structure(x, class = unique(c("JS_EVAL", oldClass(x))))
}

button_fullscreen <- function() {
  list(
    name = "fullscreen",
    title = "Toggle fullscreen",
    icon = list(
      path = icons$expand,
      transform = 'matrix(1 0 0 1 0 -1) scale(0.03571429)'
    ),
    attr = "full_screen",
    val = "false",
    click = dirty_js(
      "function(gd, ev) {
         var button = ev.currentTarget;
         var astr = button.getAttribute('data-attr');
         var val = button.getAttribute('data-val') || false;
      
         if(astr === 'full_screen') {
           if(val === 'false') {
             button.setAttribute('data-val', 'true');
             gd.classList.add('full-screen');
             Plotly.Plots.resize(gd);
           } else {
             button.setAttribute('data-val', 'false');
             gd.classList.remove('full-screen');
             Plotly.Plots.resize(gd);
           }
         }
      }"
    )
  )
}