gen_plotly_resp <- function(respcurves) {
  
  vars <- unique(respcurves$variable)
  
  obj <- plot_ly()
  
  for (i in vars) {
    print(i)
    if (i == vars[1]) {
      obj <- obj %>%
        add_trace(y = respcurves$response[respcurves$variable == i],
                  x = respcurves$base[respcurves$variable == i], name = i,
                  type = 'scatter', mode = 'lines')
    } else {
      obj <- obj %>%
        add_trace(y = respcurves$response[respcurves$variable == i],
                  x = respcurves$base[respcurves$variable == i], name = i,
                  visible = FALSE, type = 'scatter', mode = 'lines')
    }
  } 
  
  lobjs <- lapply(vars, function(i){
    eval(parse(text = glue::glue(
      'list(method = "update",
               args = list(list(visible = list({paste(rep(FALSE, length(vars)), collapse = ",")})),
                           list(xaxis = list(title = "{i}"))),
               label = "{i}")'
    )))
  })
  
  for (i in 1:length(vars)) {
    lobjs[[i]]$args[[1]]$visible[[i]] <- TRUE
  }
  
  
  
  po_final <- obj %>%
    layout(
      xaxis = list(domain = c(0.1, 1),
                   zeroline = F),
      yaxis = list(title = "Response",
                   zeroline = F),
      showlegend = FALSE,
      updatemenus = list(
        list(
          y = 1.1, x = 0.4,
          buttons = lobjs)
      )
    )
  
  return(po_final)
  
}