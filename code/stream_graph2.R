# README----------------------
# Produce figure of daily stream stats time series

## packages
library(tidyverse)
library(plotly)

## input files
masterdat <- 'data_master/TK_stream_monthly_stats.csv'

## output files
graph2 <- 'docs/widget/stream_graph2.html'


pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa',
                       '#bfd730', '#a7a9ac', '#666666')

tk.palette <- c('#3b4035', '#9c8755', '#61655c',
                '#d1bc8b', '#40696f', '#2e5150',
                '#5f5131', '#9e513a')


# DATA SET UP----------------

dat <- read_csv(here::here(masterdat), col_types = cols()) 


# PLOTS----------------

standardfonts <- list(
  family = 'Lato, sans-serif',
  size = 14
)

p1 <- plot_ly(data = dat, x = ~date, y = ~diff_temp.C, type = 'bar',
              color = ~I(pointblue.palette[3]), 
              text = paste0('Temp (C): ', 
                            htmlTable::txtRound(dat$diff_temp.C, digits = 2)),
              hoverinfo = 'x+text',
              hoverlabel = standardfonts) %>%
  layout(yaxis = list(title = 'Temperature (C)',
                      range = c(-5, 5),
                      gridcolor = 'white',
                      titlefont = standardfonts,
                      tickfont = standardfonts,
                      automargin = TRUE))

p2 <- plot_ly(data = dat, x = ~date, y = ~diff_flow.cfs, type = 'bar', 
              color = ~I(pointblue.palette[2]),
              text = paste0('Flow (cfs): ', 
                            htmlTable::txtRound(dat$diff_flow.cfs, digits = 2)),
              hoverinfo = 'x+text',
              hoverlabel = standardfonts) %>%
  layout(yaxis = list(title = 'Flow (cfs)',
                      range = c(-5, 10),
                      gridcolor = 'white',
                      titlefont = standardfonts,
                      tickfont = standardfonts,
                      automargin = TRUE))

p3 <- plot_ly(data = dat, x = ~date, y = ~diff_stage.ft, type = 'bar',
              color = ~I(pointblue.palette[4]),
              text = paste0('Temp (C): ', 
                            htmlTable::txtRound(dat$diff_stage.ft, digits = 2)),
              hoverinfo = 'x+text',
              hoverlabel = standardfonts) %>%
  layout(yaxis = list(title = 'Depth (ft)',
                      range = c(-1, 1),
                      gridcolor = 'white',
                      titlefont = standardfonts,
                      tickfont = standardfonts,
                      automargin = TRUE))

plot2 <- subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
  layout(xaxis = list(title = NA, type = 'date',
                      font = list(family = 'sans-serif', size = 14),
                      gridcolor = 'gray60'),
         showlegend = FALSE,
         hovermode = 'x',
         dragmode = 'pan',
         margin = list(r = 0, b = 10, t = 10)) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d', 
                                       'zoomIn2d', 'zoomOut2d', 
                                       'pan2d', 'toggleSpikelines'))

htmlwidgets::saveWidget(plot2,
                        here::here(graph2),
                        selfcontained = TRUE,
                        title = 'Honsinger Creek Monthly Stats')
