# README----------------------
# Script to produce soil map 3: current soil nutrients map
# - bulk density, carbon at 2 depths, water infiltration, & overall "soil health" score

## packages
library(tidyverse)
library(sf)
library(htmlTable)
library(leaflet)
library(mapview)

## input files
masterdat <- 'data_clean/TK_soil_clean.csv'

## output files
output3 <- 'docs/widget/soil_map3.html'

## shapefiles
poly <- 'TK_veg_fields'
ranch <- 'TomKat_ranch_boundary'
pts <- 'TOKA_point_count_grid'

logo <- 'https://data.pointblue.org/apps/assets/images/pb-logo-full.png'

pointblue.palette <-
  c('#4495d1',
    '#74b743',
    '#f7941d',
    '#005baa',
    '#bfd730',
    '#a7a9ac',
    '#666666',
    '#456d28', #add a few more complementary colors
    '#b74374', 
    '#5e2a84',
    '#d2c921')

tk.palette <- c('#3b4035', '#9c8755', '#61655c',
                '#d1bc8b', '#40696f', '#2e5150',
                '#5f5131', '#9e513a')

# DATA SET UP-------------

dat <- read_csv(here::here(masterdat), col_types = cols()) %>%
  rename(Name = 'Point Name') %>%
  select(Name, Year, `Total NitrogenA`:`Total NitrogenB`, PotassiumA:PotassiumB, 
         SodiumA:SodiumB, MagnesiumA:MagnesiumB, CalciumA:CalciumB, pHA:pHB) %>%
  gather(key = var, value = value, `Total NitrogenA`:pHB) %>%
  filter(!is.na(value)) %>%
  filter(Year == max(Year)) %>%
  mutate(y_round = case_when(!(var %in% c('pHA', 'pHB')) ~ 
                               txtRound(value, digits = 2, txt.NA = 'NA'),
                             TRUE ~ txtRound(value, digits = 1, txt.NA = 'NA')))


# POPUP HTML TABLES-------------

dat_lab <- dat %>%
  select(-y_round) %>%
  spread(key = var, value = value) %>%
  mutate(
    label_Nitrogen = map(
      Name,
      ~ dat %>% filter(Name == .x & var %in% c('Total NitrogenA', 'Total NitrogenB')) %>%
        select(y_round) %>%
        htmlTable(header = 'Total<br>Nitrogen',
                  align = 'r',
                  rnames = c('0-10 cm', '10-40 cm'),
                  caption = paste0('<b>', .x, '</b>'))),
    label_Potassium = map(
      Name,
      ~ dat %>% filter(Name == .x & var %in% c('PotassiumA', 'PotassiumB')) %>%
        select(y_round) %>%
        htmlTable(header = 'Potassium',
                  align = 'r',
                  rnames = c('0-10 cm', '10-40 cm'),
                  caption = paste0('<b>', .x, '</b>'))),
    label_Sodium = map(
      Name,
      ~ dat %>% filter(Name == .x & var %in% c('SodiumA', 'SodiumB')) %>%
        select(y_round) %>%
        htmlTable(header = 'Sodium',
                  align = 'r',
                  rnames = c('0-10 cm', '10-40 cm'),
                  caption = paste0('<b>', .x, '</b>'))),
    label_Magnesium = map(
      Name,
      ~ dat %>% filter(Name == .x & var %in% c('MagnesiumA', 'MagnesiumB')) %>%
        select(y_round) %>%
        htmlTable(header = 'Magnesium',
                  align = 'r',
                  rnames = c('0-10 cm', '10-40 cm'),
                  caption = paste0('<b>', .x, '</b>'))),
    label_Calcium = map(
      Name,
      ~ dat %>% filter(Name == .x & var %in% c('CalciumA', 'CalciumB')) %>%
        select(y_round) %>%
        htmlTable(header = 'Calcium',
                  align = 'r',
                  rnames = c('0-10 cm', '10-40 cm'),
                  caption = paste0('<b>', .x, '</b>'))),
    label_pH = map(
      Name,
      ~ dat %>% filter(Name == .x & var %in% c('pHA', 'pHB')) %>%
        select(y_round) %>%
        htmlTable(header = 'pH',
                  align = 'r',
                  rnames = c('0-10 cm', '10-40 cm'),
                  caption = paste0('<b>', .x, '</b>'))))

# SHAPEFILES SET UP------
shp_pts <- st_read(here::here('GIS'), pts, quiet = TRUE) %>%
  st_transform('+proj=longlat +datum=WGS84') %>%
  right_join(dat_lab, by = 'Name')

shp_poly <- st_read(here::here('GIS'), poly, quiet = TRUE) %>%
  st_transform('+proj=longlat +datum=WGS84')

shp_ranch <- st_read(here::here('GIS'), ranch, quiet = TRUE) %>%
  st_transform('+proj=longlat +datum=WGS84')


# COLOR PALETTE-----------
## Define color palette for each metric:
pal1 <- colorBin(palette = colorRamp(colors = c('#ffffff', pointblue.palette[4])),
                 domain = c(dat_lab$`Total NitrogenA`, dat_lab$`Total NitrogenB`),
                 bins = c(0, 0.2, 0.3, 0.4, 0.7),
                 na.color = pointblue.palette[6])

pal2 <- colorBin(palette = colorRamp(colors = c('#ffffff', pointblue.palette[3])),
                 domain = c(dat_lab$PotassiumA, dat_lab$PotassiumB),
                 bins = c(0, 0.5, 1, 1.5, 3),
                 na.color = pointblue.palette[6])

pal0 <- colorBin(palette = colorRamp(colors = c('#ffffff', pointblue.palette[10])),
                 domain = c(dat_lab$SodiumA, dat_lab$SodiumB),
                 bins = c(0, 0.25, 0.5, 0.75, 1),
                 na.color = pointblue.palette[6])

pal3 <- colorBin(palette = colorRamp(colors = c('#ffffff', tk.palette[8])),
                 domain = c(dat_lab$MagnesiumA, dat_lab$MagnesiumB),
                 bins = c(0, 5, 8, 11, 15),
                 na.color = pointblue.palette[6])

pal4 <- colorBin(palette = colorRamp(colors = c('#ffffff', tk.palette[6])),
                 domain = c(dat_lab$CalciumA, dat_lab$CalciumB),
                 bins = c(0, 10, 15, 20, 25),
                 na.color = pointblue.palette[6])

pal5 <- colorBin(palette = colorRamp(colors = c('#ffffff', tk.palette[7])),
                 domain = c(dat_lab$pHA, dat_lab$pHB),
                 bins = c(5, 5.5, 6, 6.5),
                 na.color = pointblue.palette[6])


# MAP-----------

map3 <- leaflet(shp_pts, height = 500) %>% 
  setView(lng = -122.3598,
          lat = 37.26693,
          zoom = 14) %>% 
  
  # background terrain
  addProviderTiles("Esri.WorldStreetMap",
                   options = providerTileOptions(minzoom = 14, maxzoom = 15)) %>%
  
  # pasture boundaries:
  addPolygons(data = shp_poly, fillColor = pointblue.palette[2], 
              color = 'black', weight = 1, fillOpacity = 0.1) %>% 
  
  # ranch boundary
  addPolygons(data = shp_ranch, color = 'black', fill = F, weight = 3) %>%
  
  # nitrogen: (overlapping circles for two depths)
  addCircleMarkers(fillColor =  ~ pal1(`Total NitrogenB`),
                   popup =  ~ label_Nitrogen,
                   group = 'Total Nitrogen (N)',
                   radius = 9,
                   fillOpacity = 1,
                   opacity = 1,
                   color = 'black',
                   weight = ~ifelse(Name %in% c('TOKA-022', 'TOKA-068'), 3, 1)) %>% 
  
  addCircleMarkers(fillColor =  ~ pal1(`Total NitrogenA`),
                   popup =  ~ label_Nitrogen,
                   group = 'Total Nitrogen (N)',
                   radius = 3.5,
                   weight = 1,
                   fillOpacity = 1,
                   opacity = 1,
                   color = 'black') %>% 
  
  # potassium: (overlapping circles for two depths)
  addCircleMarkers(fillColor =  ~ pal2(PotassiumB),
                   popup =  ~ label_Potassium,
                   group = 'Potassium (K)',
                   radius = 9,
                   fillOpacity = 1,
                   opacity = 1,
                   color = 'black',
                   weight = ~ifelse(Name %in% c('TOKA-022', 'TOKA-068'), 3, 1)) %>% 
  
  addCircleMarkers(fillColor =  ~ pal2(PotassiumA),
                   popup =  ~ label_Potassium,
                   group = 'Potassium (K)',
                   radius = 3.5,
                   weight = 1,
                   fillOpacity = 1,
                   opacity = 1,
                   color = 'black') %>% 
  
  # sodium: (overlapping circles for two depths)
  addCircleMarkers(fillColor =  ~ pal0(SodiumB),
                   popup =  ~ label_Sodium,
                   group = 'Sodium (Na)',
                   radius = 9,
                   fillOpacity = 1,
                   opacity = 1,
                   color = 'black',
                   weight = ~ifelse(Name %in% c('TOKA-022', 'TOKA-068'), 3, 1)) %>% 
  
  addCircleMarkers(fillColor =  ~ pal0(SodiumA),
                   popup =  ~ label_Sodium,
                   group = 'Sodium (Na)',
                   radius = 3.5,
                   weight = 1,
                   fillOpacity = 1,
                   opacity = 1,
                   color = 'black') %>% 
  
  # magnesium: (overlapping circles for two depths)
  addCircleMarkers(fillColor =  ~ pal3(MagnesiumB),
                   popup =  ~ label_Magnesium,
                   group = 'Magnesium (Mg)',
                   radius = 9,
                   fillOpacity = 1,
                   opacity = 1,
                   color = 'black',
                   weight = ~ifelse(Name %in% c('TOKA-022', 'TOKA-068'), 3, 1)) %>% 
  
  addCircleMarkers(fillColor =  ~ pal3(MagnesiumA),
                   popup =  ~ label_Magnesium,
                   group = 'Magnesium (Mg)',
                   radius = 3.5,
                   weight = 1,
                   fillOpacity = 1,
                   opacity = 1,
                   color = 'black') %>% 
  
  # calcium: (overlapping circles for two depths)
  addCircleMarkers(fillColor =  ~ pal4(CalciumB),
                   popup =  ~ label_Calcium,
                   group = 'Calcium (Ca)',
                   radius = 9,
                   fillOpacity = 1,
                   opacity = 1,
                   color = 'black',
                   weight = ~ifelse(Name %in% c('TOKA-022', 'TOKA-068'), 3, 1)) %>% 
  
  addCircleMarkers(fillColor =  ~ pal4(CalciumA),
                   popup =  ~ label_Calcium,
                   group = 'Calcium (Ca)',
                   radius = 3.5,
                   weight = 1,
                   fillOpacity = 1,
                   opacity = 1,
                   color = 'black') %>% 
  
  # pH: (overlapping circles for two depths)
  addCircleMarkers(fillColor =  ~ pal5(pHB),
                   popup =  ~ label_pH,
                   group = 'pH',
                   radius = 9,
                   fillOpacity = 1,
                   opacity = 1,
                   color = 'black',
                   weight = ~ifelse(Name %in% c('TOKA-022', 'TOKA-068'), 3, 1)) %>% 
  
  addCircleMarkers(fillColor =  ~ pal5(pHA),
                   popup =  ~ label_pH,
                   group = 'pH',
                   radius = 3.5,
                   weight = 1,
                   fillOpacity = 1,
                   opacity = 1,
                   color = 'black') %>% 
  
  # legends (one per metric)
  addLegend(pal = pal1,
            values = c(dat_lab$`Total NitrogenA`, dat_lab$`Total NitrogenB`),
            title = 'Total Nitrogen',
            group = 'Total Nitrogen (N)',
            position = 'topright',
            opacity = 1,
            na.label = 'No data') %>% 
  
  addLegend(pal = pal2,
            values = c(dat_lab$PotassiumA, dat_lab$PotassiumB),
            title = 'Potassium',
            group = 'Potassium (K)',
            position = 'topright',
            opacity = 1,
            na.label = 'No data') %>% 
  
  addLegend(pal = pal0,
            values = c(dat_lab$SodiumA, dat_lab$SodiumB),
            title = 'Sodium',
            group = 'Sodium (Na)',
            position = 'topright',
            opacity = 1,
            na.label = 'No data') %>% 
  
  addLegend(pal = pal3,
            values = c(dat_lab$MagnesiumA, dat_lab$MagnesiumB),
            title = 'Magnesium',
            group = 'Magnesium (Mg)',
            position = 'topright',
            opacity = 1,
            na.label = 'No data') %>% 
  
  addLegend(pal = pal4,
            values = c(dat_lab$CalciumA, dat_lab$CalciumB),
            title = 'Calcium',
            group = 'Calcium (Ca)',
            position = 'topright',
            opacity = 1,
            na.label = 'No data') %>% 
  
  addLegend(pal = pal5,
            values = c(dat_lab$pHA, dat_lab$pHB),
            title = 'pH',
            group = 'pH',
            position = 'topright',
            opacity = 1,
            na.label = 'No data') %>% 
  
  ## toggles
  addLayersControl(position = 'bottomleft',
                   options = layersControlOptions(collapsed = F),
                   overlayGroups = c('Total Nitrogen (N)', 'Potassium (K)', 
                                     'Sodium (Na)', 'Magnesium (Mg)', 
                                     'Calcium (Ca)', 'pH')) %>% 
  
  hideGroup('Potassium (K)') %>%
  hideGroup('Sodium (Na)') %>%
  hideGroup('Magnesium (Mg)') %>%
  hideGroup('Calcium (Ca)') %>%
  hideGroup('pH') %>%
  
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

title <- paste0('TomKat Soil Nutrients 2015')

htmlwidgets::saveWidget(map3,
                        here::here(output3),
                        selfcontained = TRUE,
                        title = title)
