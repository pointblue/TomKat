##'@title plot_birdtrend
##'@description functions for generating interacting plot_ly figures for TomKat
##'  trend data
##'@param dat data frame containing trend data. Expects fields "group"
##'  (identifier for each trend line; ok if there's only one group), "year",
##'  "value", "minus" and "plus" (negative and positive error differences from
##'  value, passed to plot_ly's arrayminus and array). Optional: "label_text"
##'  field containing string to be displayed when hovering over a data point.
##'@param colors character vector of colors to be used for temp, flow, and
##'  depth, respectively
##'@param ytitle strings to be used as y-axis title
##'@param yrange vector of min and max y-axis values to be shown 
##'
##'@export
##'@author Kristen Dybala
##'
##'  

plotly_trend = function(dat, colors, yrange, ytitle) {
  # Estimate rough trend estimates:
  #   assume relatively even effort, random selection of points per year
  # --> Note: this ignores error in richness estimates, so is very rough
  
  mod = glm(round(value, digits = 0) ~ year * group, dat, family = poisson)
  print(summary(mod))
  
  mod2 = glm(round(value, digits = 0) ~ year + group, dat, family = poisson)
  print(summary(mod2))
  
  datlist = dat %>% split(.$group)
  
  
  # initiate plotly figure
  p = plot_ly(x = ~year) %>%
    layout(yaxis = list(title = ytitle,
                        font = list(size = 14),
                        showline = TRUE,
                        ticks = 'outside',
                        tick0 = 0,
                        range = yrange,
                        showgrid = FALSE,
                        automargin = TRUE),
           xaxis = list(title = NA,
                        showline = TRUE,
                        ticks = 'outside',
                        showgrid = FALSE),
           legend = list(x = 0.01, xanchor = 'left', y = 1, yanchor = 'top',
                         bordercolor = ~I('black'), borderwidth = 1),
           hovermode = 'x',
           margin = list(r = 0, b = 10, t = 10)) %>%
    config(displaylogo = FALSE, showTips = FALSE,
           modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d', 
                                         'zoomIn2d', 'zoomOut2d', 
                                         'pan2d', 'toggleSpikelines'))
  
  # add traces for each group in dat
  for (i in c(1:length(datlist))) {
    p = p %>% 
      add_trace(data = datlist[[i]],
                y = ~value, 
                type = 'scatter', 
                mode = 'lines+markers',
                error_y = list(type = 'data',
                               symmetric = FALSE,
                               arrayminus = ~minus,
                               array = ~plus,
                               color = colors[i]),
                line = list(color = colors[i]),
                marker = list(color = colors[i], 
                              size = 10),
                text = ~label_text,
                hoverinfo = 'x+text', 
                name = names(datlist)[i])
  }
  return(p)
}
