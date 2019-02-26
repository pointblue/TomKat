# README----------------------
# Script to produce vegetation trend graphs (accounting for different sizes of 
# each pasture, total area of pastures surveyed each year, and some missing data)
# 1: ranch-wide trend in % cover of major veg types
# 2: comparison of native, annual, perennial grasses

## packages
library(tidyverse)
library(sf)
library(plotly)

## input files
masterveg <- 'data_master/TK_veg_master.csv'
poly <- 'TK_veg_fields' ## shapefile

## output files
graph1 <- 'vegetation_graph1.html'
graph2 <- 'vegetation_graph2.html'

pointblue.palette <-
  c('#4495d1',
    '#74b743',
    '#f7941d',
    '#005baa',
    '#bfd730',
    '#a7a9ac',
    '#666666')

# DATA SET UP-------------
shp_poly <- st_read(here::here('GIS'), poly, quiet = TRUE) 

dat <- read_csv(here::here(masterveg), col_types = cols()) %>%
  filter(Year > 2011) %>%
  mutate(Pasture = as.factor(Pasture)) %>%
  # get pasture area from polygons
  full_join(shp_poly %>% st_set_geometry(NULL), by = 'Pasture') %>%
  # calculate cover area
  mutate(cover.area = cover / 100 * area_ha) %>%
  # summarize totals by year (area surveyed and covered area)
  filter(!is.na(cover)) %>%
  group_by(Year, vegtype) %>%
  summarize(area_ha = sum(area_ha),
            cover.area = sum(cover.area, na.rm = T),
            Cover = cover.area / area_ha) %>%
  ungroup() %>%
  mutate(y_round = paste0(round(Cover * 100, digits = 0), '%'),
         fullname = recode(vegtype, AnnualGr = 'Annual Grasses',
                           BareGround = 'Bare Ground',
                           Grass = 'All Grasses',
                           NativeGr = 'Native Grasses',
                           PereGr = 'Perennial Grasses',
                           Weeds = 'Invasive Weeds'))


# ESTIMATE TRENDS---------------
# model ranch-wide trend over time in each major veg class:
mod = lm(qlogis(Cover) ~ vegtype * Year, 
         dat %>% filter(!(vegtype %in% c('PereGr', 'NativeGr', 'AnnualGr')))) 
summary(mod)
# no significant changes in any major veg class

mod.alt = betareg::betareg(Cover ~ Year * vegtype, 
                       dat %>% filter(!(vegtype %in% c('PereGr', 'NativeGr', 'AnnualGr')))) 
summary(mod.alt)

# model ranch-wide trend over time within grasses:
mod2 = lm(qlogis(Cover) ~ Year * vegtype, 
          dat %>% filter(vegtype %in% c('PereGr', 'NativeGr', 'AnnualGr')))
summary(mod2)
# no significant changes


# PLOT 1-------------
# Overview: all grasses, shrubs, forbs, bare ground, weeds

plot1 <- plot_ly(x = ~Year) %>%
  add_trace(data = dat %>% filter(vegtype == 'Grass'), 
            y = ~Cover * 100, 
            type = 'scatter', 
            mode = 'markers+lines',
            line = list(color = pointblue.palette[1]),
            marker = list(color = pointblue.palette[1], 
                          size = 10),
            text = ~y_round,
            hoverinfo = 'x+text', 
            name = 'All Grasses') %>%
  add_trace(data = dat %>% filter(vegtype == 'BareGround'), 
            y = ~Cover * 100, 
            type = 'scatter', 
            mode = 'lines+markers',
            line = list(color = pointblue.palette[2]),
            marker = list(color = pointblue.palette[2], 
                          size = 10),
            text = ~y_round,
            hoverinfo = 'x+text', 
            name = 'Bare Ground') %>%
  add_trace(data = dat %>% filter(vegtype == 'Forbs'), 
            y = ~Cover * 100, 
            type = 'scatter', 
            mode = 'lines+markers',
            line = list(color = pointblue.palette[3]),
            marker = list(color = pointblue.palette[3], 
                          size = 10),
            text = ~y_round,
            hoverinfo = 'x+text', 
            name = 'Forbs') %>%
  add_trace(data = dat %>% filter(vegtype == 'Weeds'), 
            y = ~Cover * 100, 
            type = 'scatter', 
            mode = 'lines+markers',
            line = list(color = pointblue.palette[4]),
            marker = list(color = pointblue.palette[4], 
                          size = 10),
            text = ~y_round,
            hoverinfo = 'x+text', 
            name = 'Invasive Weeds') %>%
  add_trace(data = dat %>% filter(vegtype == 'Shrubs'), 
            y = ~Cover * 100, 
            type = 'scatter', 
            mode = 'lines+markers',
            line = list(color = pointblue.palette[6]),
            marker = list(color = pointblue.palette[6], 
                          size = 10),
            text = ~y_round,
            hoverinfo = 'x+text', 
            name = 'Shrubs') %>%
  layout(yaxis = list(title = '% Cover',
                      font = list(size = 14),
                      showline = TRUE,
                      ticks = 'outside',
                      tick0 = 0,
                      range = c(0, 80),
                      showgrid = FALSE,
                      automargin = TRUE),
         xaxis = list(title = NA,
                      showline = TRUE,
                      ticks = 'outside',
                      showgrid = FALSE),
         legend = list(x = 0.01, xanchor = 'left', y = 1, yanchor = 'top',
                       bordercolor = ~I('black'), borderwidth = 1,
                       font = list(family = 'sans-serif',
                                   size = 14)),
         hovermode = 'x',
         margin = list(r = 0, b = 10, t = 10)) %>%
  config(collaborate = FALSE, displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d', 
                                       'zoomIn2d', 'zoomOut2d', 
                                       'pan2d', 'toggleSpikelines'))

htmlwidgets::saveWidget(plot1,
                        here::here(graph1),
                        selfcontained = TRUE,
                        title = 'TomKat Vegetation Trends')

# PLOT 2-------------
# Native vs. Perennial vs. Annual Grasses

plot2 <- plot_ly(x = ~Year) %>%
  add_trace(data = dat %>% filter(vegtype == 'AnnualGr'), 
            y = ~Cover * 100, 
            type = 'scatter', 
            mode = 'markers+lines',
            line = list(color = pointblue.palette[1]),
            marker = list(color = pointblue.palette[1], 
                          size = 10),
            text = ~y_round,
            hoverinfo = 'x+text', 
            name = 'Annual Grasses') %>%
  add_trace(data = dat %>% filter(vegtype == 'PereGr'), 
            y = ~Cover * 100, 
            type = 'scatter', 
            mode = 'lines+markers',
            line = list(color = pointblue.palette[3]),
            marker = list(color = pointblue.palette[3], 
                          size = 10),
            text = ~y_round,
            hoverinfo = 'x+text', 
            name = 'Perennial Grasses') %>%
  add_trace(data = dat %>% filter(vegtype == 'NativeGr'), 
            y = ~Cover * 100, 
            type = 'scatter', 
            mode = 'lines+markers',
            line = list(color = pointblue.palette[2]),
            marker = list(color = pointblue.palette[2], 
                          size = 10),
            text = ~y_round,
            hoverinfo = 'x+text', 
            name = 'Native Grasses')%>%
  layout(yaxis = list(title = '% Cover',
                      font = list(size = 14),
                      showline = TRUE,
                      ticks = 'outside',
                      tick0 = 0,
                      range = c(0, 80),
                      showgrid = FALSE,
                      automargin = TRUE),
         xaxis = list(title = NA,
                      showline = TRUE,
                      ticks = 'outside',
                      showgrid = FALSE),
         legend = list(x = 0.01, xanchor = 'left', y = 1, yanchor = 'top',
                       bordercolor = ~I('black'), borderwidth = 1,
                       font = list(family = 'sans-serif',
                                   size = 14)),
         hovermode = 'x',
         margin = list(r = 0, b = 10, t = 10)) %>%
  config(collaborate = FALSE, displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d', 
                                       'zoomIn2d', 'zoomOut2d', 
                                       'pan2d', 'toggleSpikelines'))

htmlwidgets::saveWidget(plot2,
                        here::here(graph2),
                        selfcontained = TRUE,
                        title = 'TomKat Grass Trends')

