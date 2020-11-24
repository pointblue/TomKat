# README----------------------
# Script to produce vegetation map 3: current vegetation species diversity

## packages
library(tidyverse)
library(sf)
library(htmlTable)
library(leaflet)
library(mapview)

## input files
masterveg <- 'data_master/TK_veg_master_div.csv'

## output files
output3 <- 'docs/widget/vegetation_map3.html'

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
# calculate "recent" average

## use most recent 3 years of data as "recent"
maxyear = read_csv(here::here(masterveg), col_types = cols()) %>% 
  pull(year) %>% max()
minyear = maxyear - 3

dat <- read_csv(here::here(masterveg), col_types = cols()) %>%
  filter(!(group %in% c('totaldiv', 'herbdiv'))) %>% 
  filter(year > minyear) %>%
  group_by(pasture, group) %>%
  summarize(species = mean(species)) %>%
  group_by(pasture) %>%
  summarize(species = round(sum(species), digits = 0)) %>%
  ungroup() %>%
  mutate(pasture = toupper(pasture))

# POPUP HTML TABLES-------------

dat_lab <- dat %>%
  mutate(
    label = map(unique(dat$pasture),
                       ~ dat %>% 
                         filter(pasture == .x) %>%
                         select(species) %>%
                         htmlTable(header = c('Average #<br>species'), 
                                   rnames = F,
                                   # align = 'r', total = F,
                                   caption = paste0('<b>Pasture ', .x, '</b>'))))


# SHAPEFILES SET UP------

shp_poly <- st_read(here::here('GIS'), poly, quiet = TRUE) %>%
  st_transform('+proj=longlat +datum=WGS84') %>%
  full_join(dat_lab, by = c('Pasture' = 'pasture'))

shp_ranch <- st_read(here::here('GIS'), ranch, quiet = TRUE) %>%
  st_transform('+proj=longlat +datum=WGS84')


# COLOR PALETTE-----------
# ## Define color palette for % cover data, grouped into bins: <1%, 1-5%, 5-10%, 10-20%, and >20%
# ## This one goes from white to Point Blue's dark blue, with dark gray as the NA color
# pal <-
#   colorBin(
#     palette = colorRamp(colors = c(
#       pointblue.palette[3], '#ffffff', pointblue.palette[4]
#     )),
#     domain = c(-100, 100),
#     bins = c(-100, -75, -50, -25, -10, 10, 25, 50, 75, 100),
#     na.color = pointblue.palette[7]
#   )

## Alternate palette for proportional change:
pal <- colorBin(palette = colorRamp(colors = c('#ffffff', pointblue.palette[4])),
                domain = c(0, max(dat$species)),
                na.color = pointblue.palette[7],
                bins = c(0, 20, 40, 60, 80))

# MAP----------------------

map3 <- leaflet(shp_poly, height = 500) %>% 
  setView(lng = -122.3598, lat = 37.26693, zoom = 14) %>%
  
  ## background terrain
  addProviderTiles("Esri.WorldStreetMap",
                   options = providerTileOptions(minzoom = 14, maxzoom = 15)) %>%
  
  # ranch boundary
  addPolygons(data = shp_ranch, color = 'black', fill = F, weight = 3) %>%
  
  ## species richness
  addPolygons(fillColor = ~ pal(species),
              popup = ~ label,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## legend
  addLegend(position = 'topright',
            title = 'Average #<br>species',
            pal = pal,
            values = dat$species,
            # labFormat = function(type, cuts, p) {
            #   n = length(cuts)
            #   paste0(cuts[-n], " &ndash; ", cuts[-1])
            # },
            na.label = 'No data',
            opacity = 1) %>%

  ## logo
  leafem::addLogo(img = logo, src = 'remote', url = 'http://www.pointblue.org',
          width = 174, height = 90, offset.y = -5)

# add CSS
map3$dependencies <- c(map3$dependencies, 
                       list(
                         htmltools::htmlDependency(
                           name = 'tomkat-leaflet',
                           version = '1.0.0',
                           src = here::here('Rmd'),
                           stylesheet = 'tk_leaflet.css'
                         )
                       ))

title <- paste0('TomKat Vegetation Diversity ', minyear+1, '-', maxyear)

htmlwidgets::saveWidget(map3,
                        here::here(output3),
                        selfcontained = TRUE,
                        title = title)
