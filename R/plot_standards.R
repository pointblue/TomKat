##'@title plot_data
##'@description functions for generating interacting plot_ly figures for TomKat
##'  data, with supporting function to save widgets, render Rmds, and access
##'  point blue logo & palette
##'
##'@export
##'@author Kristen Dybala
##'
##'  # PLOTTING STANDARDS

pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa', '#bfd730',
                       '#a7a9ac', '#666666')

logo <- 'https://data.pointblue.org/apps/assets/images/pb-logo-full.png'

save_widget = function(widget, pathout, selfcontained = TRUE, title) {
  htmlwidgets::saveWidget(widget,
                          here::here(pathout),
                          selfcontained = selfcontained,
                          title = title)
}

render_Rmd = function(pathin, pathout) {
  rmarkdown::render(pathin, 
                    output_file = here::here(pathout))
}

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
