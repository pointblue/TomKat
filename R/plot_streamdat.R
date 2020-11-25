
plot_streamdat = function(dat, colors, ytitle, ymin = 0, ymax,
                          type = 'scatter', mode = 'lines',
                          selector = FALSE, slider = FALSE) {
  sdat <- dat %>% split(.$name)

  p <- purrr::map(c(1:length(sdat)),
             function(x) {
               plot_ly(data = sdat[[x]], x = ~date, y = ~value, 
                       type = type, mode = mode,
                       color = ~I(colors[x]),
                       text = paste0(ytitle[x], ': ', 
                                     htmlTable::txtRound(sdat[[x]]$value, 
                                                         digits = 2)),
                       hoverinfo = 'x+text',
                       hoverlabel = standardfonts) %>%
                 layout(yaxis = list(title = ytitle[x],
                                     range = c(ymin[x], ymax[x]),
                                     gridcolor = 'white',
                                     titlefont = standardfonts,
                                     tickfont = standardfonts,
                                     automargin = TRUE))
             }) %>% 
    subplot(nrows = length(sdat), shareX = TRUE, titleY = TRUE) %>% 
    layout(xaxis = list(title = NA, type = 'date',
                        tickfont = standardfonts,
                        gridcolor = 'gray60'),
           showlegend = FALSE,
           hovermode = 'x',
           dragmode = 'pan',
           margin = list(r = 0, b = 10, t = 10)) %>% 
    config(displaylogo = FALSE, showTips = FALSE,
           modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d', 
                                         'zoomIn2d', 'zoomOut2d', 
                                         'pan2d', 'toggleSpikelines'))
  
  if (selector) {
    p <- p %>%
      layout(xaxis = list(
        rangeselector = list(
          buttons = list(list(count = 1,
                              label = "1 yr",
                              step = "year",
                              stepmode = "backward"),
                         list(count = 3,
                              label = "3 yrs",
                              step = "year",
                              stepmode = "backward"),
                         list(step = "all")),
          font = c(standardfonts, color = 'white'),
          bgcolor = tk.palette[2],
          activecolor = tk.palette[5])))
  }
  if (slider) {
    p <- p %>% 
      rangeslider(max(dat$date)-365, max(dat$date), thickness = 0.05)
  }
  
  return(p)
}



