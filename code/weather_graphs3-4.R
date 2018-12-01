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


pointblue.palette <-
  c('#4495d1',
    '#74b743',
    '#f7941d',
    '#005baa',
    '#bfd730',
    '#a7a9ac',
    '#666666')


# PDSI----------------
pal = colorRampPalette(c(pointblue.palette[4], '#ffffff', pointblue.palette[3]))

pdat <- read_csv(here::here(pdsi)) %>%
  filter(date > '2010-07-01' & date < '2018-11-15') %>% #start with July 2010 as with other weather data
  gather(PDSI.01:PDSI.07, key = 'division', value = 'value') %>%
  filter(division %in% c('PDSI.04', 'PDSI.01', 'PDSI.06')) %>%
  mutate(color = cut(value, breaks = c(-20, -4, -3, -2, 2, 3, 4, 20),
                     labels = rev(pal(7))),
         division = factor(division, levels = c('PDSI.04', 'PDSI.01', 'PDSI.06')),
         div = case_when(division == 'PDSI.04' ~ 'Central coast',
                         division == 'PDSI.01' ~ 'North coast',
                         division == 'PDSI.06' ~ 'South coast'),
         text = paste0('<b>', div, ': ', value))

## Climate divisions within California:
## 01: North Coast Drainage
## 02: Sacramento Drainage
## 03: Northeast Interior Basins
## 04: Central Coast **TomKat**
## 05: San Joaquin Drainage
## 06: South Coast Drainage
## 07: Southeast Desert Basin

plot3 <- plot_ly(x = ~date, y = ~value) %>%
  add_trace(data = pdat %>% filter(division == 'PDSI.01'), 
            type = 'scatter', mode = 'lines+markers', text = ~text, hoverinfo = 'x+text',
            line = list(color = pointblue.palette[6], dash = 'dash'),
            marker = list(color = ~I(color), size = 6, line = list(color = pointblue.palette[6], width = 1))) %>%
  add_trace(data = pdat %>% filter(division == 'PDSI.06'), 
            type = 'scatter', mode = 'lines+markers', text = ~text, hoverinfo = 'x+text',
            line = list(color = pointblue.palette[6], dash = 'dot'),
            marker = list(color = ~I(color), size = 6, line = list(color = pointblue.palette[6], width = 1))) %>%
  add_trace(data = pdat %>% filter(division == 'PDSI.04'), 
            type = 'scatter', mode = 'lines+markers', text = ~text, hoverinfo = 'x+text',
            line = list(color = 'black'),
            marker = list(color = ~I(color), size = 7, line = list(color = 'black', width = 2))) %>%
  layout(yaxis = list(title = NA),
         xaxis = list(title = NA),
         showlegend = F,
         hovermode = 'x',
         dragmode = 'pan') %>%
  config(collaborate = FALSE, displaylogo = FALSE)

htmlwidgets::saveWidget(plot3,
                        here::here(graph3),
                        selfcontained = TRUE,
                        title = 'Palmer Drought Severity Index')

# PZI---------------
zdat <- read_csv(here::here(zndx)) %>%
  filter(date > '2010-07-01' & date < '2018-11-15') %>% #start with July 2010 as with other weather data
  gather(PZI.01:PZI.07, key = 'division', value = 'value') %>%
  filter(division %in% c('PZI.04', 'PZI.01', 'PZI.06')) %>%
  mutate(color = cut(value, breaks = c(-20, -2.75, -2, -1.25, 1, 2.5, 3.5, 20),
                     labels = rev(pal(7))),
         division = factor(division, levels = c('PZI.04', 'PZI.01', 'PZI.06')),
         div = case_when(division == 'PZI.04' ~ 'Central coast',
                         division == 'PZI.01' ~ 'North coast',
                         division == 'PZI.06' ~ 'South coast'),
         text = paste0('<b>', div, ': ', value))

plot4 <- plot_ly(x = ~date, y = ~value) %>%
  add_trace(data = zdat %>% filter(division == 'PZI.01'), 
            type = 'scatter', mode = 'lines+markers', text = ~text, hoverinfo = 'x+text',
            line = list(color = pointblue.palette[6], dash = 'dash'),
            marker = list(color = ~I(color), size = 6, line = list(color = pointblue.palette[6], width = 1))) %>%
  add_trace(data = zdat %>% filter(division == 'PZI.06'), 
            type = 'scatter', mode = 'lines+markers', text = ~text, hoverinfo = 'x+text',
            line = list(color = pointblue.palette[6], dash = 'dot'),
            marker = list(color = ~I(color), size = 6, line = list(color = pointblue.palette[6], width = 1))) %>%
  add_trace(data = zdat %>% filter(division == 'PZI.04'), 
            type = 'scatter', mode = 'lines+markers', text = ~text, hoverinfo = 'x+text',
            line = list(color = 'black'),
            marker = list(color = ~I(color), size = 7, line = list(color = 'black', width = 2))) %>%
  layout(yaxis = list(title = NA),
         xaxis = list(title = NA),
         showlegend = F,
         hovermode = 'x',
         dragmode = 'pan') %>%
  config(collaborate = FALSE, displaylogo = FALSE)

htmlwidgets::saveWidget(plot4,
                        here::here(graph4),
                        selfcontained = TRUE,
                        title = 'Palmer Z Index')

