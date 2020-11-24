# README----------------------
# Script to produce vegetation map 1: current veg map

## packages
library(tidyverse)
library(sf)
library(htmlTable)
library(leaflet)
library(mapview)

## input files
masterdat <- 'data_master/TK_mgmt_stats.csv'

## output files
output1 <- 'docs/widget/mgmt_map1.html'
output2 <- 'docs/widget/mgmt_map2.html'

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
# get full list of fields from shapefile:
fields <- st_read(here::here('GIS'), poly, quiet = TRUE) %>%
  st_set_geometry(NULL)

dat <- read_csv(here::here(masterdat), col_types = cols()) %>%
  select(field, season, ndays, ADA) %>%
  mutate(field = factor(field, levels = fields$Pasture),
         season = factor(season, levels = c('growing', 'dormant', 'total')),
         ndays_round = txtRound(ndays, digits = 1),
         ADA_round = txtRound(ADA, digits = 1)) %>%
  complete(field, season, fill = list(ndays = 0, 
                                      ADA = 0, 
                                      ndays_round = '0.0', 
                                      ADA_round = '0.0')) %>%
  arrange(field, season)

# POPUP HTML TABLES-------------

dat_lab <- dat %>%
  select(-ndays_round, -ADA_round) %>%
  gather(ndays:ADA, key = 'var', value = 'value') %>%
  unite('var', var, season) %>%
  spread(key = 'var', value = 'value') %>%
  mutate(label_ada = map(dat %>% filter(season == 'total') %>% pull(field),
                         ~ dat %>% filter(field == .x) %>% select(ADA_round) %>%
                           htmlTable(header = 'ADA',
                                     align = 'r',
                                     rnames = c('Growing (Nov 2017 - Jun 2018)',
                                                'Dormant (Jul 2018 - Oct 2018)',
                                                'Total (Nov 2017 - Oct 2018)'),
                                     total = T,
                                     caption = paste0('<b>Pasture ', .x, '</b>'))),
         label_days = map(dat %>% filter(season == 'total') %>% pull(field),
                         ~ dat %>% filter(field == .x) %>% select(ndays_round) %>%
                           htmlTable(header = 'Days',
                                     align = 'r',
                                     rnames = c('Growing (Nov 2017 - Jun 2018)',
                                                'Dormant (Jul 2018 - Oct 2018)',
                                                'Total (Nov 2017 - Oct 2018)'),
                                     total = T,
                                     caption = paste0('<b>Pasture ', .x, '</b>'))))


# SHAPEFILES SET UP------

shp_poly <- st_read(here::here('GIS'), poly, quiet = TRUE) %>%
  st_transform('+proj=longlat +datum=WGS84') %>%
  full_join(dat_lab, by = c('Pasture' = 'field'))

shp_ranch <- st_read(here::here('GIS'), ranch, quiet = TRUE) %>%
  st_transform('+proj=longlat +datum=WGS84')


# COLOR PALETTE-----------
## Define color palette for ADA range, grouped into bins:
pal1 <- colorBin(palette = colorRamp(colors = c('#ffffff', pointblue.palette[4])),
                domain = c(0, max(dat$ADA)),
                bins = c(0, 0.01, 25, 50, 100, 150),
                na.color = pointblue.palette[6])

## color palette for ndays range
pal2 <- colorBin(palette = colorRamp(colors = c('#ffffff', pointblue.palette[2])),
                 domain = c(0, max(dat$ADA)),
                 bins = c(0, 0.01, 25, 50, 100, 150),
                 na.color = pointblue.palette[6])

# MAP 1----------------------
# animal days per acre

map1 <- leaflet(shp_poly, height = 500) %>% 
  setView(lng = -122.3598, lat = 37.26693, zoom = 14) %>%
  
  ## background terrain
  addProviderTiles("Esri.WorldStreetMap",
                   options = providerTileOptions(minzoom = 14, maxzoom = 15)) %>%
  
  # ranch boundary
  addPolygons(data = shp_ranch, color = 'black', fill = F, weight = 3) %>%
  
  ## growing
  addPolygons(fillColor = ~ pal1(ADA_growing),
              group = 'Growing (Nov 2017 - Jun 2018)',
              popup = ~ label_ada,
              color = 'black', fillOpacity = 1, weight = 1.5) %>%
  
  ## dormant
  addPolygons(fillColor = ~ pal1(ADA_dormant),
              group = 'Dormant (Jul 2018 - Oct 2018)',
              popup = ~ label_ada,
              color = 'black', fillOpacity = 1, weight = 1.5) %>%

  
  ## total
  addPolygons(fillColor = ~ pal1(ADA_total),
              group = 'Total (Nov 2017 - Oct 2018',
              popup = ~ label_ada,
              color = 'black', fillOpacity = 1, weight = 1.5) %>%
  
  ## legend
  addLegend(position = 'topright',
            colors = pal1(c(0, 15, 30, 75, 125)),
            labels = c('0', '< 25', '25 - 50', '50 - 100', '> 100'),
            na.label = 'No data',
            opacity = 1, 
            title = 'Animal days<br>per acre') %>%
  
  ## toggles
  addLayersControl(baseGroups = c('Growing (Nov 2017 - Jun 2018)',
                                  'Dormant (Jul 2018 - Oct 2018)',
                                  'Total (Nov 2017 - Oct 2018)'),
    options = layersControlOptions(collapsed = F),
    position = 'bottomleft'
  ) %>%
  
  ## logo
  leafem::addLogo(img = logo, src = 'remote', url = 'https://www.pointblue.org',
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

title <- 'TomKat Grazing ADA Map 2018'

htmlwidgets::saveWidget(map1,
                        here::here(output1),
                        selfcontained = TRUE,
                        title = title)

# MAP 2 --------------------------
# total days

map2 <- leaflet(shp_poly, height = 500) %>% 
  setView(lng = -122.3598, lat = 37.26693, zoom = 14) %>%
  
  ## background terrain
  addProviderTiles("Esri.WorldStreetMap",
                   options = providerTileOptions(minzoom = 14, maxzoom = 15)) %>%
  
  # ranch boundary
  addPolygons(data = shp_ranch, color = 'black', fill = F, weight = 3) %>%
  
  ## growing
  addPolygons(fillColor = ~ pal2(ndays_growing),
              group = 'Growing (Nov 2017 - Jun 2018)',
              popup = ~ label_days,
              color = 'black', fillOpacity = 1, weight = 1.5) %>%
  
  ## dormant
  addPolygons(fillColor = ~ pal2(ndays_dormant),
              group = 'Dormant (Jul 2018 - Oct 2018)',
              popup = ~ label_days,
              color = 'black', fillOpacity = 1, weight = 1.5) %>%
  
  
  ## total
  addPolygons(fillColor = ~ pal2(ndays_total),
              group = 'Total (Nov 2017 - Oct 2018',
              popup = ~ label_days,
              color = 'black', fillOpacity = 1, weight = 1.5) %>%
  
  ## legend
  addLegend(position = 'topright',
            colors = pal2(c(0, 15, 30, 75, 125)),
            labels = c('0', '< 25', '25 - 50', '50 - 100', '> 100'),
            na.label = 'No data',
            opacity = 1, 
            title = 'Total<br>grazing days') %>%
  
  ## toggles
  addLayersControl(baseGroups = c('Growing (Nov 2017 - Jun 2018)',
                                  'Dormant (Jul 2018 - Oct 2018)',
                                  'Total (Nov 2017 - Oct 2018)'),
                   options = layersControlOptions(collapsed = F),
                   position = 'bottomleft'
  ) %>%
  
  ## logo
  leafem::addLogo(img = logo, src = 'remote', url = 'https://www.pointblue.org',
          width = 174, height = 90, offset.y = -5)

# add CSS
map2$dependencies <- c(map2$dependencies, 
                       list(
                         htmltools::htmlDependency(
                           name = 'tomkat-leaflet',
                           version = '1.0.0',
                           src = here::here('Rmd'),
                           stylesheet = 'tk_leaflet.css'
                         )
                       ))

title2 <- 'TomKat Grazing Days Map 2018'

htmlwidgets::saveWidget(map2,
                        here::here(output2),
                        selfcontained = TRUE,
                        title = title2)
