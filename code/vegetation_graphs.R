# README----------------------
# Script to produce vegetation trend graphs (accounting for different sizes of 
# each pasture, total area of pastures surveyed each year, and some missing data)
# 1: ranch-wide trend in % cover of major veg types
# 2: comparison of native, annual, perennial grasses

## packages
library(tidyverse)
library(sf)

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
shp_poly <- st_read(here::here('GIS'), poly) 

dat <- read_csv(here::here(masterveg)) %>%
  filter(Year > 2011) %>%
  # get pasture area from polygons
  full_join(shp_poly %>% st_set_geometry(NULL), by = 'Pasture') %>%
  # calculate cover area
  mutate(cover.area = cover / 100 * area_ha) %>%
  # summarize totals by year (area surveyed and covered area)
  filter(!is.na(cover)) %>%
  group_by(Year, vegtype) %>%
  summarize(area_ha = sum(area_ha),
            cover.area = sum(cover.area, na.rm = T)) %>%
  mutate(Cover = cover.area / area_ha)
  
# test models:
mod = betareg::betareg(Cover ~ Year * vegtype, dat) 
summary(mod)
# significant increase in NativeGr, PereGr

mod = lm(log(Cover) ~ Year * vegtype, dat) 
summary(mod)
# significant increase in NativeGr

dat_final <- dat %>%
  mutate(Cover = round(Cover * 100, digits = 0),
         vegtype = recode(vegtype, AnnualGr = 'Annual Grasses',
                          BareGround = 'Bare Ground',
                          Grass = 'All Grasses',
                          NativeGr = 'Native Grasses',
                          PereGr = 'Perennial Grasses',
                          Weeds = 'Invasive Weeds'))

# PLOT 1-------------
# Overview: all grasses, shrubs, forbs, bare ground, weeds

theme_custom = theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, vjust = 1, face = 'plain'),
    axis.line.y = element_line(color = "black"),
    axis.line.x = element_line(color = "black")
  )

plot1 <- dat_final %>% 
  filter(vegtype %in% c(
    'All Grasses', 'Shrubs', 'Forbs', 'Bare Ground', 'Invasive Weeds')) %>%
  ggplot(aes(x = Year, y = Cover, color = vegtype)) +
  geom_line() + geom_point(pch = 19) + ylab('% Cover') + xlab(NULL) +
  ylim(0, 80) + scale_color_manual(values = pointblue.palette[c(1:4, 6)]) +
  theme_custom

plot1 <- plot1 %>%
  plotly::ggplotly() %>% 
  plotly::layout(legend = list(x = 0, y = 1, tracegroupgap = 5)) 

htmlwidgets::saveWidget(plot1, graph1, selfcontained = TRUE, 
                        title = 'TomKat Vegetation Trends')

# PLOT 2-------------
# Native vs. Perennial vs. Annual Grasses

plot2 <- dat_final %>%
  filter(vegtype %in% c('Native Grasses', 'Annual Grasses', 'Perennial Grasses')) %>%
  ggplot(aes(x = Year, y = Cover, color = vegtype)) + 
  geom_line() + geom_point(pch = 19) + ylab('% Cover') + xlab(NULL) + 
  ylim(0, 80) + scale_color_manual(values = pointblue.palette[1:3]) +
  theme_custom

plot2 <-  plot2 %>% 
  plotly::ggplotly() %>% 
  plotly::layout(legend = list(x = 0, y = 1, tracegroupgap = 5))

htmlwidgets::saveWidget(plot2, graph2, selfcontained = TRUE, 
                        title = 'TomKat Vegetation Trends')

