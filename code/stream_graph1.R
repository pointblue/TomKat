# README----------------------
# Produce figure of daily stream stats time series

## packages
library(tidyverse)
library(plotly)

## input files
masterdat <- 'data_master/TK_stream_daily_stats.csv'

## output files
graph1 <- 'stream_graph1.html'


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

p1 <- plot_ly(data = dat, x = ~date, y = ~temp.C_mean, 
              type = 'scatter', mode = 'lines',
              line = list(color = pointblue.palette[3]),
              text = paste0('Temp (C): ', 
                            htmlTable::txtRound(dat$temp.C_mean, digits = 2)),
              hoverinfo = 'x+text') %>%
  layout(yaxis = list(title = 'Temperature (C)',
                      range = c(0, 20),
                      gridcolor = 'white',
                      titlefont = standardfonts,
                      tickfont = standardfonts))

p2 <- plot_ly(data = dat, x = ~date, y = ~flow.cfs_mean, 
              type = 'scatter', mode = 'lines',
              line = list(color = pointblue.palette[2]),
              text = paste0('Flow (cfs): ', 
                            htmlTable::txtRound(dat$flow.cfs_mean, digits = 2)),
              hoverinfo = 'x+text') %>%
  layout(yaxis = list(title = 'Flow (cfs)',
                      range = c(0, 100),
                      gridcolor = 'white',
                      titlefont = standardfonts,
                      tickfont = standardfonts))

p3 <- plot_ly(data = dat, x = ~date, y = ~stage.ft_mean, 
              type = 'scatter', mode = 'lines',
              line = list(color = pointblue.palette[4]),
              text = paste0('Temp (C): ', 
                            htmlTable::txtRound(dat$stage.ft_mean, digits = 2)),
              hoverinfo = 'x+text') %>%
  layout(yaxis = list(title = 'Depth (ft)',
                      range = c(0, 5),
                      gridcolor = 'white',
                      titlefont = standardfonts,
                      tickfont = standardfonts))

plot1 <- subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
  layout(xaxis = list(title = NA, type = 'date',
                      font = list(family = 'sans-serif', size = 14),
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
                                       activecolor = tk.palette[5])),
         showlegend = FALSE,
         hovermode = 'x',
         dragmode = 'pan') %>%
    rangeslider('2017-08-16', '2018-08-16', thickness = 0.05) %>%
  config(collaborate = FALSE, displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d', 
                                       'zoomIn2d', 'zoomOut2d', 
                                       'pan2d', 'toggleSpikelines'))

htmlwidgets::saveWidget(plot1,
                        here::here(graph1),
                        selfcontained = TRUE,
                        title = 'Honsinger Creek Daily Stats')
