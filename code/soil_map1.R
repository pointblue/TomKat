# README----------------------
# Script to produce soil map 1: current soil map
# - bulk density, carbon at 2 depths, water infiltration, & overall "soil health" score

## packages
library(tidyverse)
library(sf)
library(htmlTable)
library(leaflet)
library(mapview)

## input files
masterdat <- 'data_master/TK_soil_master.csv'

## output files
output1 <- 'soil_map1.html'

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
  select(Name, Year, bulk.dens.gcm3, water.infil, carbonA, carbonB)  %>%
  mutate(water.infil = water.infil/60) #convert to minutes

# percentile scores (based on all years combined to be able to show change in percentiles over years)
f1 <- ecdf(dat$carbonA)
f2 <- ecdf(dat$carbonB)
f3 <- ecdf(max(dat$bulk.dens.gcm3, na.rm = T) - dat$bulk.dens.gcm3) #reverse so lower density = higher score
f4 <- ecdf(max(dat$water.infil, na.rm = T) - dat$water.infil)

dat_perc <- dat %>%
  filter(Year == max(Year)) %>%
  mutate(carbonA_perc = round(f1(carbonA) * 100, digits = 0),
         carbonB_perc = round(f2(carbonB) * 100, digits = 0),
         bulk.dens.gcm3_perc = round(f3(max(bulk.dens.gcm3, na.rm = T) - bulk.dens.gcm3) * 100, digits = 0),
         water.infil_perc = round(f4(max(water.infil, na.rm = T) - water.infil) * 100, digits = 0)) %>%
  gather(bulk.dens.gcm3:water.infil_perc, key = 'var', value = 'Value') %>%
  separate(var, into = c('var', 'type'), sep = '_', fill = 'right') %>%
  mutate(type = case_when(is.na(type) ~ 'Value',
                          TRUE ~ 'Percentile')) %>%
  spread(key = 'type', value = 'Value') 

# calculate overall score 
mean_perc <- dat_perc %>%
  group_by(Name) %>%
  summarize(var = 'mean',
            Percentile = round(mean(Percentile, na.rm = T), digits = 0),
            Value = NA) %>%
  bind_rows(dat_perc) %>%
  mutate(var = factor(var, levels = c('bulk.dens.gcm3', 'water.infil', 'carbonA', 'carbonB', 'mean'))) %>%
  arrange(Name, var)  

# POPUP HTML TABLES-------------
# flag points 22 and 68 as having had compost applied

dat_lab <- dat %>%
  filter(Year == max(Year)) %>%
  full_join(mean_perc %>% filter(var == 'mean') %>% 
              select(Name, Percentile) %>% rename('mean' = 'Percentile')) %>%
  mutate(
    label_bulk.dens.gcm3 = case_when(
      Name %in% c('TOKA-022', 'TOKA-068') ~ 
        map(Name,
            ~ mean_perc %>% filter(Name == .x & var == 'bulk.dens.gcm3') %>%
              select(Value, Percentile) %>% 
              mutate(Value = txtRound(Value, digits = 2, txt.NA = 'NA')) %>%
              htmlTable(header = c('Value', 'Percentile'),
                        align = c('r', 'r'),
                        rnames = 'Bulk Density (g/cm<sup>3</sup>)',
                        caption = paste0('<b>', .x, ' (compost applied)</b>'))),
      TRUE ~ 
        map(Name,
            ~ mean_perc %>% filter(Name == .x & var == 'bulk.dens.gcm3') %>%
              select(Value, Percentile) %>%
              mutate(Value = txtRound(Value, digits = 2, txt.NA = 'NA')) %>%
              htmlTable(header = c('Value', 'Percentile'),
                        align = c('r', 'r'),
                        rnames = 'Bulk Density (g/cm<sup>3</sup>)',
                        caption = paste0('<b>', .x, '</b>')))),
    label_water.infil = case_when(
      Name %in% c('TOKA-022', 'TOKA-068') ~ map(
        Name,
        ~ mean_perc %>% filter(Name == .x & var == 'water.infil') %>%
          select(Value, Percentile) %>%
          mutate(Value = txtRound(Value, digits = 2, txt.NA = 'NA')) %>%
          htmlTable(header = c('Value', 'Percentile'),
                    align = c('r', 'r'),
                    rnames = 'Water Infiltration (min/in)',
                    caption = paste0('<b>', .x, ' (compost applied)</b>'))),
      TRUE ~ map(
        Name,
        ~ mean_perc %>% filter(Name == .x & var == 'water.infil') %>%
          select(Value, Percentile) %>%
          mutate(Value = txtRound(Value, digits = 2, txt.NA = 'NA')) %>%
          htmlTable(header = c('Value', 'Percentile'),
                    align = c('r', 'r'),
                    rnames = 'Water Infiltration (min/in)',
                    caption = paste0('<b>', .x, '</b>')))),
    label_carbon = case_when(
      Name %in% c('TOKA-022', 'TOKA-068') ~ map(
        Name,
        ~ mean_perc %>% 
          filter(Name == .x & var %in% c('carbonA', 'carbonB')) %>%
          select(Value, Percentile) %>%
          mutate(Value = txtRound(Value, digits = 1, txt.NA = 'NA')) %>%
          htmlTable(header = c('Value', 'Percentile'),
                    align = c('r', 'r'),
                    rnames = c('% Carbon (0-10cm)', '% Carbon (10-40cm)'),
                    caption = paste0('<b>', .x, ' (compost applied)</b>'))),
      TRUE ~ map(
        Name,
        ~ mean_perc %>% 
          filter(Name == .x & var %in% c('carbonA', 'carbonB')) %>%
          select(Value, Percentile) %>%
          mutate(Value = txtRound(Value, digits = 1, txt.NA = 'NA')) %>%
          htmlTable(header = c('Value', 'Percentile'),
                    align = c('r', 'r'),
                    rnames = c('% Carbon (0-10cm)', '% Carbon (10-40cm)'),
                    caption = paste0('<b>', .x, '</b>')))),
    label_overall = case_when(
      Name %in% c('TOKA-022', 'TOKA-068') ~ map(
        Name,
        ~ mean_perc %>% filter(Name == .x & var != 'mean') %>% 
          select(Value, Percentile) %>%
          mutate(Value = txtRound(Value, digits = 2, txt.NA = 'NA')) %>%
          htmlTable(header = c('Value', 'Percentile'),
                    align = c('r', 'r'),
                    rnames = c('Bulk Density (g/cm<sup>3</sup>)',
                               'Water Infiltration (min/in)',
                               '% Carbon (0-10cm)',
                               '% Carbon (10-40cm)'),
                    caption = paste0('<b>', .x, ' (compost applied)</b>'),
                    tfoot = paste0('<b>Overall score: ', 
                                   mean_perc %>% 
                                     filter(Name == .x & var=='mean') %>% 
                                     select(Percentile), 
                                   '</b>'))),
      TRUE ~ map(
        Name,
        ~ mean_perc %>% filter(Name == .x & var != 'mean') %>%
          select(Value, Percentile) %>%
          mutate(Value = txtRound(Value, digits = 2, txt.NA = 'NA')) %>%
          htmlTable(header = c('Value', 'Percentile'),
                    align = c('r', 'r'),
                    rnames = c('Bulk Density (g/cm<sup>3</sup>)',
                               'Water Infiltration (min/in)',
                               '% Carbon (0-10cm)',
                               '% Carbon (10-40cm)'),
            caption = paste0('<b>', .x, '</b>'),
            tfoot = paste0('<b>Overall score: ', 
                           mean_perc %>% 
                             filter(Name == .x & var=='mean') %>% 
                             select(Percentile), 
                           '</b>')))))


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
pal0 <- colorBin(
  palette = colorRamp(colors = c('#ffffff', pointblue.palette[4])),
  domain = dat_lab$mean,
  bins = c(0, 20, 40, 60, 80, 100),
  na.color = pointblue.palette[6])

pal1 <- colorBin(
  palette = colorRamp(colors = c('#ffffff', pointblue.palette[3])),
  domain = dat_lab$bulk.dens.gcm3,
  bins = c(0.7, 0.9, 1.1, 1.3, 1.5),
  na.color = pointblue.palette[6])

pal2 <- colorBin(
  palette = colorRamp(colors = c('#ffffff', pointblue.palette[10])),
  domain = dat_lab$water.infil,
  bins = c(0, 1, 5, 10, 20, 60),
  na.color = pointblue.palette[6])

pal3 <- colorBin(
  palette = colorRamp(colors = c('#ffffff', tk.palette[8])),
  domain = c(dat_lab$carbonA, dat_lab$carbonB),
  bins = c(0, 2, 4, 6, 10),
  na.color = tk.palette[4])


# MAP-----------

map1 <- leaflet(shp_pts, height = 500) %>% 
  setView(lng = -122.3598,
          lat = 37.26693,
          zoom = 14) %>% 
  
  # background terrain
  addProviderTiles("Stamen.Terrain",
                   options = providerTileOptions(minzoom = 14, maxzoom = 15)) %>%
  
  # pasture boundaries:
  addPolygons(data = shp_poly, fillColor = pointblue.palette[2], 
              color = 'black', weight = 1, fillOpacity = 0.1) %>% 
  
  # ranch boundary
  addPolygons(data = shp_ranch, color = 'black', fill = F, weight = 3) %>%
  
  # overall score:
  addCircleMarkers(fillColor =  ~ pal0(mean),
                   popup =  ~ label_overall,
                   group = 'Overall score',
                   radius = 9,
                   color = 'black',
                   fillOpacity = 1,
                   opacity = 1,
                   weight = ~ifelse(Name %in% c('TOKA-022', 'TOKA-068'), 3, 1)) %>%
  
  # bulk density:
  addCircleMarkers(fillColor =  ~ pal1(bulk.dens.gcm3),
                   popup =  ~ label_bulk.dens.gcm3,
                   group = 'Bulk density',
                   radius = 9,
                   color = 'black',
                   fillOpacity = 1,
                   opacity = 1,
                   weight = ~ifelse(Name %in% c('TOKA-022', 'TOKA-068'), 3, 1)) %>%
  
  # water infiltration:
  addCircleMarkers(fillColor =  ~ pal2(water.infil),
                   popup =  ~ label_water.infil,
                   group = 'Water infiltration',
                   radius = 9,
                   color = 'black',
                   fillOpacity = 1,
                   opacity = 1,
                   weight = ~ifelse(Name %in% c('TOKA-022', 'TOKA-068'), 3, 1)) %>% 
  
  # carbon: (overlapping circles for two depths)
  addCircleMarkers(fillColor =  ~ pal3(carbonB),
                   popup =  ~ label_carbon,
                   group = '% Carbon',
                   radius = 9,
                   color = 'black',
                   fillOpacity = 1,
                   opacity = 1,
                   weight = ~ifelse(Name %in% c('TOKA-022', 'TOKA-068'), 3, 1)) %>% 
  
  addCircleMarkers(fillColor =  ~ pal3(carbonA),
                   popup =  ~ label_carbon,
                   group = '% Carbon',
                   radius = 3.5,
                   color = 'black',
                   fillOpacity = 1,
                   opacity = 1,
                   weight = 1) %>% 
  
  # legends (one per metric)
  addLegend(pal = pal0,
            values = dat_lab$mean,
            title = 'Overall score',
            group = 'Overall score',
            position = 'topright',
            opacity = 1,
            na.label = 'No data') %>% 
  
  addLegend(pal = pal1,
            values = dat_lab$bulk.dens.gcm3,
            title = 'Bulk density<br>(g/cm<sup>3</sup>)',
            group = 'Bulk density',
            position = 'topright',
            opacity = 1,
            na.label = 'No data') %>% 
  
  addLegend(pal = pal2,
            values = dat_lab$water.infil,
            title = 'Water infiltration<br>(min/in)',
            group = 'Water infiltration',
            position = 'topright',
            opacity = 1,
            na.label = 'No data') %>% 
  
  addLegend(pal = pal3,
            values = c(dat_lab$carbonA, dat_lab$carbonB),
            title = '% Carbon',
            group = '% Carbon',
            position = 'topright',
            opacity = 1,
            na.label = 'No data') %>% 
  
  ## toggles
  addLayersControl(position = 'bottomleft',
                   options = layersControlOptions(collapsed = F),
                   overlayGroups = c('Overall score', 'Bulk density', 
                                     'Water infiltration', '% Carbon')) %>% 
  
  hideGroup('Bulk density') %>%
  hideGroup('Water infiltration') %>%
  hideGroup('% Carbon') %>%
  
  ## logo
  addLogo(img = logo, src = 'remote', url = 'http://www.pointblue.org',
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

title <- paste0('TomKat Soil Map ', max(dat$Year))

htmlwidgets::saveWidget(map1,
                        here::here(output1),
                        selfcontained = TRUE,
                        title = title)
