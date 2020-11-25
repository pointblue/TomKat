# README----------------------
# Script to produce vegetation map 1: current veg map

## packages
library(tidyverse)
library(sf)
library(htmlTable)
library(leaflet)
library(mapview)

## input files
masterveg <- 'data_clean/TK_veg_main.csv'

## output files
output1 <- 'docs/widget/vegetation_map1.html'

## shapefiles
poly <- 'TK_veg_fields'
ranch <- 'TomKat_ranch_boundary'

logo <- 'https://data.pointblue.org/apps/assets/images/pb-logo-full.png'

pointblue.palette <-
  c('#4495d1',
    '#74b743',
    '#f7941d',
    '#005baa',
    '#bfd730',
    '#a7a9ac',
    '#666666')

# DATA SET UP-------------
dat <- read_csv(here::here(masterveg), col_types = cols()) %>%
  filter(vegtype != 'Trees') %>% #inconsistent treatment; not included in map
  mutate(cover = round(cover, digits = 1),
         vegtype = as.factor(vegtype),
         Pasture = as.factor(Pasture)) %>% 
  filter(Year == max(Year)) %>%
  spread(key = vegtype, value = cover)


# POPUP HTML TABLES-------------

dat_lab <- dat %>%
  mutate(
    label_PereGr = map(
      dat$Pasture,
      ~ dat %>% filter(Pasture == .x) %>%
        select(PereGr) %>%
        htmlTable(
          header = '% Cover',
          rnames = 'Perennial Grasses',
          caption = paste0('<b>Pasture ', .x, '</b>')
        )
    ),
    label_AnnualGr = map(
      dat$Pasture,
      ~ dat %>% filter(Pasture == .x) %>%
        select(AnnualGr) %>%
        htmlTable(
          header = '% Cover',
          rnames = 'Annual Grasses',
          caption = paste0('<b>Pasture ', .x, '</b>')
        )
    ),
    label_NativeGr = map(
      dat$Pasture,
      ~ dat %>% filter(Pasture == .x) %>%
        select(NativeGr) %>%
        htmlTable(
          header = '% Cover',
          rnames = 'Native Grasses',
          caption = paste0('<b>Pasture ', .x, '</b>')
        )
    ),
    label_Grass = map(
      dat$Pasture,
      ~ dat %>% filter(Pasture == .x) %>%
        select(Grass) %>%
        htmlTable(
          header = '% Cover',
          rnames = 'All Grasses',
          caption = paste0('<b>Pasture ', .x, '</b>')
        )
    ),
    label_Shrubs = map(
      dat$Pasture,
      ~ dat %>% filter(Pasture == .x) %>%
        select(Shrubs) %>%
        htmlTable(
          header = '% Cover',
          rnames = 'Shrubs',
          caption = paste0('<b>Pasture ', .x, '</b>')
        )
    ),
    label_Forbs = map(
      dat$Pasture,
      ~ dat %>% filter(Pasture == .x) %>%
        select(Forbs) %>%
        htmlTable(
          header = '% Cover',
          rnames = 'Forbs',
          caption = paste0('<b>Pasture ', .x, '</b>')
        )
    ),
    label_Weeds = map(
      dat$Pasture,
      ~ dat %>% filter(Pasture == .x) %>%
        select(Weeds) %>%
        htmlTable(
          header = '% Cover',
          rnames = 'Invasive Weeds',
          caption = paste0('<b>Pasture ', .x, '</b>')
        )
    ),
    label_BareGround = map(
      dat$Pasture,
      ~ dat %>% filter(Pasture == .x) %>%
        select(BareGround) %>%
        htmlTable(
          header = '% Cover',
          rnames = 'Bare Ground',
          caption = paste0('<b>Pasture ', .x, '</b>')
        )
    )
  )


# SHAPEFILES SET UP------

shp_poly <- st_read(here::here('GIS'), poly, quiet = TRUE) %>%
  st_transform('+proj=longlat +datum=WGS84') %>%
  full_join(dat_lab, by = 'Pasture')

shp_ranch <- st_read(here::here('GIS'), ranch, quiet = TRUE) %>%
  st_transform('+proj=longlat +datum=WGS84')


# COLOR PALETTE-----------
## Define color palette for % cover data, grouped into bins: <1%, 1-5%, 5-10%, 10-20%, and >20%
## This one goes from white to Point Blue's dark blue, with dark gray as the NA color
pal <-
  colorBin(
    palette = colorRamp(colors = c('#ffffff', pointblue.palette[4])),
    domain = c(0, 100),
    bins = c(0, 1, 5, 10, 20, 50, 100),
    na.color = pointblue.palette[6]
  )


# MAP----------------------

map1 <- leaflet(shp_poly, height = 500) %>% 
  setView(lng = -122.3598, lat = 37.26693, zoom = 14) %>%
  
  ## background terrain
  addProviderTiles("Esri.WorldStreetMap",
                   options = providerTileOptions(minzoom = 14, maxzoom = 15)) %>%
  
  # ranch boundary
  addPolygons(data = shp_ranch, color = 'black', fill = F, weight = 3) %>%
  
  ## perennial grasses
  addPolygons(fillColor = ~ pal(PereGr),
              group = 'Perennial Grasses',
              popup = ~ label_PereGr,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## native grasses
  addPolygons(fillColor = ~ pal(NativeGr),
              group = 'Native Grasses',
              popup = ~ label_NativeGr,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%

  
  ## annual grasses
  addPolygons(fillColor = ~ pal(AnnualGr),
              group = 'Annual Grasses',
              popup = ~ label_AnnualGr,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%

  ## all grasses
  addPolygons(fillColor = ~ pal(Grass),
              group = 'All Grasses',
              popup = ~ label_Grass,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## shrubs
  addPolygons(fillColor = ~ pal(Shrubs),
              group = 'Shrubs',
              popup = ~ label_Shrubs,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%

  ## forbs
  addPolygons(fillColor = ~ pal(Forbs),
              group = 'Forbs',
              popup = ~ label_Forbs,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## weeds
  addPolygons(fillColor = ~ pal(Weeds),
              group = 'Invasive Weeds',
              popup = ~ label_Weeds,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%

  ## bare ground
  addPolygons(fillColor = ~ pal(BareGround),
              group = 'Bare Ground',
              popup = ~ label_BareGround,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## legend
  addLegend(position = 'topright', 
            pal = pal,
            values = dat %>% gather(AnnualGr:Weeds, key = 'key', value = 'value') %>% pull(value),
            labFormat = labelFormat(suffix = '%'),
            na.label = 'No data',
            opacity = 1, 
            title = '% Cover') %>%
  
  ## toggles
  addLayersControl(baseGroups = c('Perennial Grasses', 'Native Grasses', 
                                  'Annual Grasses', 'All Grasses', 'Shrubs',
                                  'Forbs', 'Invasive Weeds', 'Bare Ground'),
    options = layersControlOptions(collapsed = F),
    position = 'bottomleft'
  ) %>%
  
  ## logo
  leafem::addLogo(img = logo, src = 'remote', url = 'http://www.pointblue.org',
          width = 174, height = 90, offset.y = -5)

# add CSS
map1$dependencies <- c(map1$dependencies, 
                       list(
                         htmltools::htmlDependency(
                           name = 'tomkat-leaflet',
                           version = '1.0.0',
                           src = here::here('Rmd'),
                           stylesheet = 'tk_leaflet.css'
                         )
                       ))

title <- paste0('TomKat Vegetation Map ', max(dat$Year))

htmlwidgets::saveWidget(map1,
                        here::here(output1),
                        selfcontained = TRUE,
                        title = title)
