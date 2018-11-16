# README----------------------
# Script to produce vegetation graph 1: ranch-wide trend in % cover
# Accounting for different sizes of each pasture, total area of pastures 
# surveyed each year, and some missing data.
#
# From RStudio Viewer: Export as webpage "docs/vegetation_map1.html"

## packages
library(tidyverse)
library(sf)

## input files
masterveg <- 'data_master/TK_veg_master.csv'

## shapefile
poly <- 'TK_veg_fields'

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

plot1 <- ggplot(dat_final %>%
                  filter(
                    vegtype %in% c('All Grasses', 'Shrubs', 'Forbs', 'Bare Ground', 'Invasive Weeds')
                  ),
                aes(x = Year, y = Cover, color = vegtype)) +
  geom_line() + geom_point(pch = 19) + ylab('% Cover') + xlab(NULL) +
  ylim(0, 80) + scale_color_manual(values = pointblue.palette[c(1:4, 6)]) +
  theme_custom 
  
plotly::ggplotly(plot1) %>% 
  plotly::layout(legend = list(x = 0, y = 1, tracegroupgap = 5)) 

# PLOT 2-------------
# Native vs. Perennial vs. Annual Grasses

plot2 <- ggplot(dat_final %>%
                  filter(
                    vegtype %in% c('Native Grasses', 'Annual Grasses', 'Perennial Grasses')
                  ),
                aes(x = Year, y = Cover, color = vegtype)) + 
  geom_line() + geom_point(pch = 19) + ylab('% Cover') + xlab(NULL) + 
  ylim(0, 80) + scale_color_manual(values = pointblue.palette[1:3]) +
  theme_custom

plotly::ggplotly(plot2) %>% 
  plotly::layout(legend = list(x = 0, y = 1, tracegroupgap = 5))
