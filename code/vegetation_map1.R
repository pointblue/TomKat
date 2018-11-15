# README----------------------
# Script to produce vegetation map 1: current veg map

## packages
library(tidyverse)
library(sf)
library(htmlTable)
library(leaflet)
library(mapview)

## input files
masterveg <- 'data_master/TK_veg_master.csv'

## shapefiles
poly <- 'TK_veg_fields'
ranch <- 'TomKat_ranch_boundary'


logo <- "http://www.pointblue.org/logos/pb-logo-full.png" 
logo <- 'figs/PB_logo_RGB_Full_Color_cs.png'

pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa', '#bfd730', 
                       '#a7a9ac', '#666666')

# DATA SET UP-------------
dat <- read_csv(here::here(masterveg)) %>%
  filter(vegtype != 'Trees') %>% #inconsistent treatment; not included in map
  mutate(cover = round(cover, digits = 1),
         vegtype = as.factor(vegtype),
         Pasture = as.factor(Pasture))

## current year's data
dat_yr <- dat %>% filter(Year == max(Year)) %>%
  spread(key = vegtype, value = cover)


# POPUP HTML TABLES-------------

dat_yr_lab <- dat_yr %>%
  mutate(label_PereGr = map(dat_yr$Pasture, 
                            ~ dat_yr %>% filter(Pasture == .x) %>% 
                              select(PereGr) %>% 
                              htmlTable(header = '% Cover', 
                                        rnames = 'Perennial Grasses',
                                        caption = paste0('<b>Pasture ', .x, '</b>'))),
         label_AnnualGr = map(dat_yr$Pasture, 
                            ~ dat_yr %>% filter(Pasture == .x) %>% 
                              select(AnnualGr) %>% 
                              htmlTable(header = '% Cover', 
                                        rnames = 'Annual Grasses',
                                        caption = paste0('<b>Pasture ', .x, '</b>'))),
         label_NativeGr = map(dat_yr$Pasture, 
                            ~ dat_yr %>% filter(Pasture == .x) %>% 
                              select(NativeGr) %>% 
                              htmlTable(header = '% Cover', 
                                        rnames = 'Native Grasses',
                                        caption = paste0('<b>Pasture ', .x, '</b>'))),
         label_Grass = map(dat_yr$Pasture, 
                            ~ dat_yr %>% filter(Pasture == .x) %>% 
                              select(Grass) %>% 
                              htmlTable(header = '% Cover', 
                                        rnames = 'All Grasses',
                                        caption = paste0('<b>Pasture ', .x, '</b>'))),
         label_Shrubs = map(dat_yr$Pasture, 
                            ~ dat_yr %>% filter(Pasture == .x) %>% 
                              select(Shrubs) %>% 
                              htmlTable(header = '% Cover', 
                                        rnames = 'Shrubs',
                                        caption = paste0('<b>Pasture ', .x, '</b>'))),
         label_Forbs = map(dat_yr$Pasture, 
                            ~ dat_yr %>% filter(Pasture == .x) %>% 
                              select(Forbs) %>% 
                              htmlTable(header = '% Cover', 
                                        rnames = 'Forbs',
                                        caption = paste0('<b>Pasture ', .x, '</b>'))),
         label_Weeds = map(dat_yr$Pasture, 
                            ~ dat_yr %>% filter(Pasture == .x) %>% 
                              select(Weeds) %>% 
                              htmlTable(header = '% Cover', 
                                        rnames = 'Invasive Weeds',
                                        caption = paste0('<b>Pasture ', .x, '</b>'))),
         label_BareGround = map(dat_yr$Pasture, 
                            ~ dat_yr %>% filter(Pasture == .x) %>% 
                              select(BareGround) %>% 
                              htmlTable(header = '% Cover', 
                                        rnames = 'Bare Ground',
                                        caption = paste0('<b>Pasture ', .x, '</b>'))))


# SHAPEFILES SET UP------

shp_poly <- st_read(here::here('GIS'), poly) %>%
  st_transform('+proj=longlat +datum=WGS84') %>%
  full_join(dat_yr_lab, by = 'Pasture')

shp_ranch <- st_read(here::here('GIS'), ranch) %>%
  st_transform('+proj=longlat +datum=WGS84')


# COLOR PALETTE-----------
## Define color palette for % cover data, grouped into bins: <1%, 1-5%, 5-10%, 10-20%, and >20%
## This one goes from white to Point Blue's dark blue, with dark gray as the NA color
pal <- colorBin(palette = colorRamp(colors = c('#ffffff', pointblue.palette[4])), 
                domain = c(0, 100), 
                bins = c(0, 1, 5, 10, 20, 50, 100), 
                na.color = pointblue.palette[6])


# MAP----------------------

leaflet(height = 500) %>% setView(lng = -122.3598, lat = 37.26693, zoom = 14) %>%
  ## background terrain
  addProviderTiles("Stamen.Terrain", 
                   options = providerTileOptions(minzoom = 14, maxzoom = 15)) %>% 
  
  # ranch boundary
  addPolygons(data = shp_ranch, color='black', fill=F, weight=2.5) %>%
  
  ## perennial grasses
  addPolygons(
    data = shp_poly, color = 'black', fillOpacity = 1, weight = 1.5,
    fillColor = ~pal(PereGr), group = 'Perennial Grasses', 
    popup = ~label_PereGr) %>%
  
  ## native grasses
  addPolygons(
    data = shp_poly, color = 'black', fillOpacity = 1, weight = 1.5, 
    fillColor = ~pal(NativeGr), group = 'Native Grasses', 
    popup = ~label_NativeGr) %>%
  
  ## annual grasses
  addPolygons(
    data = shp_poly, color = 'black', fillOpacity = 1, weight = 1.5, 
    fillColor = ~pal(AnnualGr), group = 'Annual Grasses', 
    popup = ~label_AnnualGr) %>%
  
  ## all grasses
  addPolygons(
    data = shp_poly, color = 'black', fillOpacity = 1, weight = 1.5, 
    fillColor = ~pal(Grass), group = 'All Grasses', 
    popup = ~label_Grass) %>%
  
  ## shrubs
  addPolygons(
    data = shp_poly, color = 'black', fillOpacity = 1, weight = 1.5, 
    fillColor = ~pal(Shrubs), group = 'Shrubs', 
    popup = ~label_Shrubs) %>%
  
  ## forbs
  addPolygons(
    data = shp_poly, color = 'black', fillOpacity = 1, weight = 1.5, 
    fillColor = ~pal(Forbs), group = 'Forbs', popup = ~label_Forbs) %>%

  ## weeds
  addPolygons(
    data = shp_poly, color = 'black', fillOpacity = 1, weight = 1.5, 
    fillColor = ~pal(Weeds), group = 'Invasive Weeds', 
    popup = ~label_Weeds) %>%
  
  ## bare ground
  addPolygons(
    data = shp_poly, color = 'black', fillOpacity = 1, weight = 1.5, 
    fillColor = ~pal(BareGround), group = 'Bare Ground', 
    popup = ~label_BareGround) %>%
  
  ## legend
  addLegend(
    position = 'topright', opacity = 1, title = '% Cover', pal = pal, 
    values = dat %>% filter(Year == max(Year)) %>% select(cover),
    labFormat = labelFormat(suffix = '%'), 
    na.label = 'No data') %>%
  
  ## toggles
  addLayersControl(
    baseGroups = c(
      'Perennial Grasses', 'Native Grasses', 'Annual Grasses', 'All Grasses',
      'Shrubs', 'Forbs', 'Invasive Weeds', 'Bare Ground'),
    options = layersControlOptions(collapsed = F), position = 'bottomleft') %>%
  
  ## logo
  addLogo(
    logo, url = 'http://www.pointblue.org', 
    width = 174, height = 90, offset.y = -5)
