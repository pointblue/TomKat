# README----------------------
# Script to produce vegetation trend graphs (accounting for different sizes of 
# each pasture, total area of pastures surveyed each year, and some missing data)
# 1: ranch-wide trend in % cover of major veg types
# 2: comparison of native, annual, perennial grasses
# Outputs: html for webpages and jpg for powerpoint

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

ppt1 <- 'figs/vegetation_graph1_ppt.jpg'
ppt2 <- 'figs/vegetation_graph2_ppt.jpg'

pointblue.palette <-
  c('#4495d1',
    '#74b743',
    '#f7941d',
    '#005baa',
    '#bfd730',
    '#a7a9ac',
    '#666666')

# theme for presentation figs (larger font sizes)
theme_presentation <- theme_classic() + 
  theme(legend.background = element_blank(),
        legend.position = c(0.01, 1), 
        legend.justification = c(0, 1),
        legend.title = element_text(size = 28),
        legend.text = element_text(size = 24), 
        plot.title = element_text(size = 36, face = 'bold', vjust = 1, hjust = 0.5),
        axis.text = element_text(size = 24, color = 'black', face = 'plain'), 
        axis.title = element_text(size = 28, face = 'plain', vjust = 0),
        strip.text = element_text(size = 28, face = 'bold', hjust = 0), 
        strip.background = element_blank())

# formatting sizes for powerpoint template
ppt.width = 10 #inches
ppt.height = 7.5 #inches
ppt.width.wide = 13.33 #inches

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
         vegtype = factor(vegtype, levels = c('Grass', 'Shrubs', 'Forbs', 
                                              'Weeds', 'BareGround', 'AnnualGr', 
                                              'PereGr', 'NativeGr', 'Trees', 
                                              'Misc')),
         fullname = recode(vegtype, 
                           AnnualGr = 'Annual Grasses',
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

# mod.alt = betareg::betareg(Cover ~ Year * vegtype, 
#                        dat %>% filter(!(vegtype %in% c('PereGr', 'NativeGr', 'AnnualGr')))) 
# summary(mod.alt)

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
  config(displaylogo = FALSE, showTips = FALSE,
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
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d', 
                                       'zoomIn2d', 'zoomOut2d', 
                                       'pan2d', 'toggleSpikelines'))

htmlwidgets::saveWidget(plot2,
                        here::here(graph2),
                        selfcontained = TRUE,
                        title = 'TomKat Grass Trends')


# POWERPOINT FIGURES ------------------------------------------------------

## overview plot

dat %>% 
  filter(vegtype %in% c('Grass', 'Shrubs', 'Forbs', 'BareGround', 'Weeds')) %>% 
  select(fullname, vegtype, Year, Cover) %>% 
  # nest(data = c(Year, Cover)) %>% 
  # mutate(mod = purrr::map(data, loess, formula = qlogis(Cover) ~ Year, span = 0.99),
  #        smooth = purrr::map(mod, `[[`, 'fitted')) %>% 
  # select(-mod) %>% 
  # unnest(cols = c(data, smooth)) %>% 
  # mutate(smooth = plogis(smooth)) %>% 
  mutate(Cover = Cover * 100) %>% 
  ggplot(aes(Year, Cover, color = fullname)) + 
  geom_point(size = 3) + 
  geom_smooth(size = 1.5, se = FALSE, level = 0.5, span = 0.99) +
  scale_color_manual(values = pointblue.palette[c(1, 6, 3, 4, 2)]) +
  labs(x = NULL, y = "% Cover", color = NULL) +
  ylim(0, 70) +
  theme_presentation

ggsave(ppt1, width = ppt.width, height = ppt.height, units = 'in')


## grasses

dat %>% 
  filter(vegtype %in% c('PereGr', 'NativeGr', 'AnnualGr')) %>% 
  select(fullname, vegtype, Year, Cover) %>% 
  mutate(Cover = Cover * 100) %>% 
  ggplot(aes(Year, Cover, color = fullname)) + 
  geom_point(size = 3) + 
  geom_smooth(size = 1.5, se = FALSE, level = 0.5, span = 0.99) +
  scale_color_manual(values = pointblue.palette[c(1, 3, 2)]) +
  labs(x = NULL, y = "% Cover", color = NULL) +
  ylim(0, 50) +
  theme_presentation

ggsave(ppt2, width = ppt.width, height = ppt.height, units = 'in')

