##'@title Plot stream data
##'@description function for generating interacting plot_ly figures for TomKat
##'  stream flow data, with subplots for temperature, flow, and depth. Used for
##'  both daily stats (line graphs) and monthly differences from normal (bar
##'  graphs).
##'@param dat data frame containing stream flow stats. Expects fields "name"
##'  (stat name), "date", and "value", with stats ordered as: temp, flow, depth.
##'@param colors character vector of colors to be used for temp, flow, and
##'  depth, respectively
##'@param ytitle vector of strings to be used as y-axis titles for temp, flow,
##'  and depth subplots, respectively
##'@param ymin vector of minimum y-axis values to be used for subplots
##'@param ymax vector of maximum y-axis values to be used for subplots
##'@param type passed to plot_ly; defaults to "scatter"
##'@param mode passed to plot_ly; defaults to "lines"
##'@param selector logical, whether or not to include selector buttons at top
##'  left for showing plot from last year, last 3 years, or all time; defaults
##'  to FALSE
##'@param slider logical, whether or not to include a range slider at the bottom
##'  of the plot; defaults to FALSE
##'
##'@export
##'@author Kristen Dybala
##'
##'  
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



