# README----------------------
# Produce drought indices figure

## packages
library(tidyverse)
library(plotly)

## input files
pdsi <- 'data_master/CA_Palmer_Drought_Severity_Index.csv'
zndx <- 'data_master/CA_Palmer_Z_Index.csv'

## output files
graph3 <- 'weather_graph3.html'
graph4 <- 'weather_graph4.html'


pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa',
                       '#bfd730', '#a7a9ac', '#666666')


tk.palette <- c('#3b4035', '#9c8755', '#61655c',
                '#d1bc8b', '#40696f', '#2e5150',
                '#5f5131', '#9e513a')

# drought palette
pal = colorRampPalette(c(pointblue.palette[4], '#ffffff', pointblue.palette[3]))
palette = pal(7)


# PDSI----------------

pdat <- read_csv(here::here(pdsi)) %>%
  filter(date > '2010-07-01') %>% #start with July 2010 as with other weather data
  gather(PDSI.01:PDSI.07, key = 'division', value = 'value') %>%
  filter(division %in% c('PDSI.04', 'PDSI.01', 'PDSI.06')) %>%
  mutate(color = cut(value, breaks = c(-20, -4, -3, -2, 2, 3, 4, 20),
                     labels = rev(palette)),
         division = factor(division, levels = c('PDSI.04', 'PDSI.01', 'PDSI.06')),
         label = case_when(division == 'PDSI.04' ~ 'Central coast',
                           division == 'PDSI.01' ~ 'North coast',
                           division == 'PDSI.06' ~ 'South coast'),
         text = paste0('<b>', label, ': ', value)) %>%
  # change date format
  mutate(date = as.Date(paste0(substr(date, 1, 7), '-01')))

## Climate divisions within California:
## 01: North Coast Drainage
## 02: Sacramento Drainage
## 03: Northeast Interior Basins
## 04: Central Coast **TomKat**
## 05: San Joaquin Drainage
## 06: South Coast Drainage
## 07: Southeast Desert Basin

# PDSI drought categories:
dcat <- tibble(group = c('A', 'A', 'B', 'B', 'C', 'C', 'D', 'D', 'E', 'E', 'F', 'F'),
               xlimit = rep(c(as.Date('2010-01-01'), max(pdat$date)), 6),
               ylimit = c(4, 4, 3, 3, 2, 2, -2, -2, -3, -3, -4, -4),
               label = c(rep('Extremely moist', 2),
                         rep('Very moist', 2),
                         rep('Moderately moist', 2),
                         rep('Moderate drought', 2),
                         rep('Severe drought', 2),
                         rep('Extreme drought', 2)),
               color = c(rep(palette[1], 2),
                         rep(palette[2], 2),
                         rep(palette[3], 2),
                         rep(palette[5], 2),
                         rep(palette[6], 2),
                         rep(palette[7], 2)))

hline <- function(y = 0, color = "blue") {
  list(type = "line", 
       x0 = 0, x1 = 1, xref = "paper",
       y0 = y, y1 = y, line = list(color = color))
}
  
plot3 <- plot_ly() %>%
  add_trace(data = pdat %>% filter(label == 'North coast'), x = ~date, y = ~value,
            type = 'scatter', mode = 'lines+markers', name = 'North coast',
            text = ~text, hoverinfo = 'x+text',
            line = list(color = pointblue.palette[7], width = 0.5),
            marker = list(color = ~I(color), size = 5, symbol = 'square',
                          line = list(color = pointblue.palette[7], width = 0.5))) %>%
  add_trace(data = pdat %>% filter(label == 'Central coast'), x = ~date, y = ~value,
            type = 'scatter', mode = 'lines+markers', name = 'Central coast',
            text = ~text, hoverinfo = 'x+text',
            line = list(color = 'black', width = 1),
            marker = list(color = ~I(color), size = 6, 
                          line = list(color = 'black', width = 1))) %>%
  add_trace(data = pdat %>% filter(label == 'South coast'), x = ~date, y = ~value,
            type = 'scatter', mode = 'lines+markers',
            text = ~text, hoverinfo = 'x+text', name = 'South coast',
            line = list(color = pointblue.palette[7], dash = 'dot', width = 0.5),
            marker = list(color = ~I(color), size = 5, symbol = 'diamond',
                          line = list(color = pointblue.palette[7], width = 0.5))) %>%
  layout(shapes = list(hline(2, color = palette[3]),
                       hline(-2, color = palette[5])),
         yaxis = list(title = NA, showgrid = FALSE, zeroline = FALSE,
                      font = list(family = 'sans-serif', size = 14),
                      automargin = TRUE),
         xaxis = list(title = NA, type = 'date',
                      font = list(family = 'sans-serif', size = 14)),
         legend = list(x = 1, xanchor = 'right', y = 1, yanchor = 'top',
                       bgcolor = I('white'), 
                       bordercolor = I('black'), borderwidth = 1,
                       font = list(size = 14, family = 'sans-serif')),
         hovermode = 'x',
         dragmode = 'pan',
         margin = list(l = 0, r = 0, b = 50, t = 10)) %>%
  layout(annotations = list(xref = 'paper', x = 0.01, xanchor = 'left',
                           yref = 'y', y = 4, yanchor = 'bottom',
                           text = ~paste('Extremely moist'),
                           font = list(family = 'sans-serif', size = 12,
                                       color = palette[1]),
                           showarrow = FALSE)) %>%
  layout(annotations = list(xref = 'paper', x = 0.01, xanchor = 'left',
                            yref = 'y', y = 3, yanchor = 'bottom',
                            text = ~paste('Very moist'),
                            font = list(family = 'sans-serif', size = 12,
                                        color = palette[2]),
                            showarrow = FALSE)) %>%
  layout(annotations = list(xref = 'paper', x = 0.01, xanchor = 'left',
                            yref = 'y', y = 2, yanchor = 'bottom',
                            text = ~paste('Moderately moist'),
                            font = list(family = 'sans-serif', size = 12,
                                        color = palette[3]),
                            showarrow = FALSE)) %>%
  layout(annotations = list(xref = 'paper', x = 0.01, xanchor = 'left',
                            yref = 'y', y = -2, yanchor = 'top',
                            text = ~paste('Moderate drought'),
                            font = list(family = 'sans-serif', size = 12,
                                        color = palette[5]),
                            showarrow = FALSE)) %>%
  layout(annotations = list(xref = 'paper', x = 0.01, xanchor = 'left',
                            yref = 'y', y = -3, yanchor = 'top',
                            text = ~paste('Severe drought'),
                            font = list(family = 'sans-serif', size = 12,
                                        color = palette[6]),
                            showarrow = FALSE)) %>%
  layout(annotations = list(xref = 'paper', x = 0.01, xanchor = 'left',
                            yref = 'y', y = -4, yanchor = 'top',
                            text = ~paste('Extreme drought'),
                            font = list(family = 'Arial', size = 12,
                                        color = palette[7]),
                            showarrow = FALSE)) %>%
  # rangeslider('2010-01-01', thickness = 0.1) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d', 
                                       'zoomIn2d', 'zoomOut2d', 
                                       'pan2d', 'toggleSpikelines'))

htmlwidgets::saveWidget(plot3,
                        here::here(graph3),
                        selfcontained = TRUE,
                        title = 'Palmer Drought Severity Index')

# PZI---------------
zdat <- read_csv(here::here(zndx), col_types = cols()) %>%
  filter(date > '2010-07-01') %>% #start with July 2010 as with other weather data
  gather(PZI.01:PZI.07, key = 'division', value = 'value') %>%
  filter(division %in% c('PZI.04', 'PZI.01', 'PZI.06')) %>%
  mutate(color = cut(value, breaks = c(-20, -2.75, -2, -1.25, 1, 2.5, 3.5, 20),
                     labels = rev(pal(7))),
         division = factor(division, levels = c('PZI.04', 'PZI.01', 'PZI.06')),
         label = case_when(division == 'PZI.04' ~ 'Central coast',
                           division == 'PZI.01' ~ 'North coast',
                           division == 'PZI.06' ~ 'South coast'),
         text = paste0('<b>', label, ': ', value)) %>%
  # change date format
  mutate(date = as.Date(paste0(substr(date, 1, 7), '-01')))

# PZI drought categories:
zcat <- tibble(group = c('A', 'A', 'B', 'B', 'C', 'C', 'D', 'D', 'E', 'E', 'F', 'F'),
               xlimit = rep(c(as.Date('2010-01-01'), max(pdat$date)), 6),
               ylimit = c(3.5, 3.5, 2.5, 2.5, 1, 1, -1.25, -1.25, -2, -2, -2.75, -2.75),
               label = c(rep('Extremely moist', 2),
                         rep('Very moist', 2),
                         rep('Moderately moist', 2),
                         rep('Moderate drought', 2),
                         rep('Severe drought', 2),
                         rep('Extreme drought', 2)),
               color = c(rep(palette[1], 2),
                         rep(palette[2], 2),
                         rep(palette[3], 2),
                         rep(palette[5], 2),
                         rep(palette[6], 2),
                         rep(palette[7], 2)))



plot4 <- plot_ly(zdat, x = ~date) %>%
  add_trace(data = zdat %>% filter(division == 'PZI.01'), x = ~date, y = ~value,
            type = 'scatter', mode = 'lines+markers', name = 'North coast',
            text = ~text, hoverinfo = 'x+text',
            line = list(color = pointblue.palette[7], width = 0.5),
            marker = list(color = ~I(color), size = 5, symbol = 'square',
                          line = list(color = pointblue.palette[7], width = 0.5))) %>%
  add_trace(data = zdat %>% filter(division == 'PZI.04'), x = ~date, y = ~value,
          type = 'scatter', mode = 'lines+markers', name = 'Central coast',
          text = ~text, hoverinfo = 'x+text',
          line = list(color = 'black', width = 1),
          marker = list(color = ~I(color), size = 6, 
                        line = list(color = 'black', width = 1))) %>%
  add_trace(data = zdat %>% filter(division == 'PZI.06'), x = ~date, y = ~value,
            type = 'scatter', mode = 'lines+markers', name = 'South coast',
            text = ~text, hoverinfo = 'x+text',
            line = list(color = pointblue.palette[7], dash = 'dot', width = 0.5),
            marker = list(color = ~I(color), size = 5, symbol = 'diamond',
                          line = list(color = pointblue.palette[7], width = 0.5))) %>%
  layout(shapes = list(hline(1, color = palette[3]),
                       hline(-1.25, color = palette[5])),
         yaxis = list(title = NA, showgrid = FALSE, zeroline = FALSE,
                      font = list(family = 'sans-serif',
                                  size = 14),
                      automargin = TRUE),
         xaxis = list(title = NA, type = 'date',
                      font = list(family = 'sans-serif', size = 14)),
         legend = list(x = 1, xanchor = 'right', y = 1, yanchor = 'top',
                       bgcolor = ~I('white'), 
                       bordercolor = ~I('black'), borderwidth = 1,
                       font = list(family = 'sans-serif', size = 14)),
         hovermode = 'x',
         dragmode = 'pan',
         margin = list(l = 0, r = 0, b = 50, t = 10)) %>%
  layout(annotations = list(xref = 'paper', x = 0.01, xanchor = 'left',
                            yref = 'y', y = 3.5, yanchor = 'bottom',
                            text = ~paste('Extremely moist'),
                            font = list(family = 'sans-serif', size = 12,
                                        color = palette[1]),
                            showarrow = FALSE)) %>%
  layout(annotations = list(xref = 'paper', x = 0.01, xanchor = 'left',
                            yref = 'y', y = 2.5, yanchor = 'bottom',
                            text = ~paste('Very moist'),
                            font = list(family = 'sans-serif', size = 12,
                                        color = palette[2]),
                            showarrow = FALSE)) %>%
  layout(annotations = list(xref = 'paper', x = 0.01, xanchor = 'left',
                            yref = 'y', y = 1, yanchor = 'bottom',
                            text = ~paste('Moderately moist'),
                            font = list(family = 'sans-serif', size = 12,
                                        color = palette[3]),
                            showarrow = FALSE)) %>%
  layout(annotations = list(xref = 'paper', x = 0.01, xanchor = 'left',
                            yref = 'y', y = -1.25, yanchor = 'top',
                            text = ~paste('Moderate drought'),
                            font = list(family = 'sans-serif', size = 12,
                                        color = palette[5]),
                            showarrow = FALSE)) %>%
  layout(annotations = list(xref = 'paper', x = 0.01, xanchor = 'left',
                            yref = 'y', y = -2, yanchor = 'top',
                            text = ~paste('Severe drought'),
                            font = list(family = 'sans-serif', size = 12,
                                        color = palette[6]),
                            showarrow = FALSE)) %>%
  layout(annotations = list(xref = 'paper', x = 0.01, xanchor = 'left',
                            yref = 'y', y = -2.75, yanchor = 'top',
                            text = ~paste('Extreme drought'),
                            font = list(family = 'sans-serif', size = 12,
                                        color = palette[7]),
                            showarrow = FALSE)) %>%
  # rangeslider('2010-01-01', thickness = 0.1) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d', 
                                       'zoomIn2d', 'zoomOut2d', 
                                       'pan2d', 'toggleSpikelines'))

htmlwidgets::saveWidget(plot4,
                        here::here(graph4),
                        selfcontained = TRUE,
                        title = 'Palmer Z Index')

