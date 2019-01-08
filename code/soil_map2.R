# README----------------------
# Script to produce soil map 2: change in soil productivity metrics since 2015
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
output2 <- 'soil_map2.html'

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
    '#666666')

# DATA SET UP-------------

dat <- read_csv(here::here(masterdat)) %>%
  rename(Name = 'Point Name') %>%
  select(Name, Year, bulk.dens.gcm3, water.infil, carbonA, carbonB)  %>%
  mutate(water.infil = water.infil/60) #convert to minutes

# recalculate percentile scores based on all years combined 
# (to be able to show change in percentiles over years)
f1 <- ecdf(dat$carbonA)
f2 <- ecdf(dat$carbonB)
f3 <- ecdf(max(dat$bulk.dens.gcm3, na.rm = T) - dat$bulk.dens.gcm3) #reverse so lower density = higher score
f4 <- ecdf(max(dat$water.infil, na.rm = T) - dat$water.infil)


dat_perc <- dat %>% 
  mutate(carbonA_perc = round(f1(carbonA) * 100, digits = 0),
         carbonB_perc = round(f2(carbonB) * 100, digits = 0),
         bulk.dens.gcm3_perc = round(f3(max(bulk.dens.gcm3, na.rm = T) - bulk.dens.gcm3) * 100, digits = 0),
         water.infil_perc = round(f4(max(water.infil, na.rm = T) - water.infil) * 100, digits = 0)) %>%
  gather(bulk.dens.gcm3:water.infil_perc, key = 'var', value = 'Value') %>%
  separate(var, into = c('var', 'type'), sep = '_', fill = 'right') %>%
  mutate(type = case_when(is.na(type) ~ 'Value',
                          TRUE ~ 'Percentile'),
         Value = case_when(is.nan(Value) ~ NA_real_,
                           TRUE ~ Value)) %>%
  spread(key = 'type', value = 'Value') 

# calculate overall score (missing values = missing overall score)
mean_perc <- dat_perc %>%
  group_by(Name, Year) %>%
  summarize(var = 'mean',
            Percentile = round(mean(Percentile), digits = 0),
            Value = NA) %>%
  ungroup() %>%
  bind_rows(dat_perc) %>%
  mutate(var = factor(var, levels = c('bulk.dens.gcm3', 'water.infil', 'carbonA', 'carbonB', 'mean'))) %>%
  arrange(Name, var)

# NET CHANGE----------------
net_change <- mean_perc %>%
  gather(Percentile:Value, key = 'type', value = 'value') %>%
  mutate(yr = case_when(Year == max(Year) ~ 'current',
                        Year == 2015 ~ 'baseline'),
         Year = NULL) %>%
  spread(key = yr, value = value) %>%
  mutate(net = current - baseline) %>%
  filter((var == 'mean' & type == 'Percentile') | (var != 'mean' & type == 'Value')) %>%
  select(-type) %>%
  gather(baseline:net, key = 'group', value = 'value') %>%
  arrange(Name, var, group) %>%
  mutate(y_round = case_when(var != 'mean' ~ txtRound(value, digits = 2, txt.NA = 'NA'),
                             TRUE ~ txtRound(value, digits = 0, txt.NA = 'NA')),
         y_round = case_when(group == 'net' & value > 0 ~ 
                               paste0('+', y_round),
                             TRUE ~ y_round))


# POPUP HTML TABLES-------------
# flag points 22 and 68 as having had compost applied

dat_lab <- net_change %>%
  filter(group == 'net') %>%
  select(Name, var, value) %>%
  spread(key = var, value = value) %>%
  mutate(
    label_bulk.dens.gcm3 = case_when(
      Name %in% c('TOKA-022', 'TOKA-068') ~ map(
        Name,
        ~ net_change %>% filter(Name == .x & var == 'bulk.dens.gcm3') %>%
          select(y_round) %>%
          htmlTable(header = 'Bulk density<br>g/cm<sup>3</sup>',
                    align = 'r',
                    rnames = c('2015', '2018', 'Difference'),
                    total = TRUE,
                    caption = paste0('<b>', .x, ' (compost applied)</b>'))),
      TRUE ~ map(
        Name,
        ~ net_change %>% filter(Name == .x & var == 'bulk.dens.gcm3') %>%
          select(y_round) %>%
          htmlTable(header = 'Bulk density<br>g/cm<sup>3</sup>',
                    align = 'r',
                    rnames = c('2015', '2018', 'Difference'),
                    total = TRUE,
                    caption = paste0('<b>', .x, '</b>')))),
    label_water.infil = case_when(
      Name %in% c('TOKA-022', 'TOKA-068') ~ map(
        Name,
        ~ net_change %>% filter(Name == .x & var == 'water.infil') %>%
          select(y_round) %>%
          htmlTable(header = 'Water<br>infiltration (min)',
                    align = 'r',
                    rnames = c('2015', '2018', 'Difference'),
                    total = TRUE,
                    caption = paste0('<b>', .x, ' (compost applied)</b>'))),
      TRUE ~ map(
        Name,
        ~ net_change %>% filter(Name == .x & var == 'water.infil') %>%
          select(y_round) %>%
          htmlTable(header = 'Water<br>infiltration (min)',
                    align = 'r',
                    rnames = c('2015', '2018', 'Difference'),
                    total = TRUE,
                    caption = paste0('<b>', .x, '</b>')))),
    label_carbon = case_when(
      Name %in% c('TOKA-022', 'TOKA-068') ~ map(
        Name,
        ~ net_change %>% filter(Name == .x & var %in% c('carbonA', 'carbonB')) %>%
          select(var, group, y_round) %>%
          spread(key = var, value = y_round) %>%
          select(-group) %>%
          htmlTable(header = c('% Carbon<br>0-10 cm', '% Carbon<br>10-40 cm'),
                    align = c('r', 'r'),
                    rnames = c('2015', '2018', 'Difference'),
                    total = TRUE,
                    caption = paste0('<b>', .x, ' (compost applied)</b>'))),
      TRUE ~ map(
        Name,
        ~ net_change %>% filter(Name == .x & var %in% c('carbonA', 'carbonB')) %>%
          select(var, group, y_round) %>%
          spread(key = var, value = y_round) %>%
          select(-group) %>%
          htmlTable(header = c('% Carbon<br>0-10 cm', '% Carbon<br>10-40 cm'),
                    align = c('r', 'r'),
                    rnames = c('2015', '2018', 'Difference'),
                    total = TRUE,
                    caption = paste0('<b>', .x, '</b>')))),
    label_overall = case_when(
      Name %in% c('TOKA-022', 'TOKA-068') ~ map(
        Name,
        ~ net_change %>% filter(Name == .x & var == 'mean') %>%
          select(y_round) %>%
          htmlTable(header = c('Overall score<br>(percentile)'),
                    align = 'r',
                    rnames = c('2015', '2018', 'Difference'),
                    total = TRUE,
                    caption = paste0('<b>', .x, ' (compost applied)</b>'))),
      TRUE ~ map(
        Name,
        ~ net_change %>% filter(Name == .x & var == 'mean') %>%
          select(y_round) %>%
          htmlTable(header = c('Overall score<br>(percentile)'),
                    align = 'r',
                    rnames = c('2015', '2018', 'Difference'),
                    total = TRUE,
                    caption = paste0('<b>', .x, '</b>')))))


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
pal0 <- colorBin(palette = colorRamp(colors = c(pointblue.palette[3], 
                                                '#ffffff', 
                                                pointblue.palette[4])),
                 domain = dat_lab$mean,
                 bins = c(-40, -10, 10, 40),
                 na.color = pointblue.palette[6])

pal1 <- colorBin(palette = colorRamp(colors = c(pointblue.palette[4], 
                                                '#ffffff', 
                                                pointblue.palette[3])),
                 domain = dat_lab$bulk.dens.gcm3,
                 bins = c(-0.25, -0.05, 0.05, 0.25),
                 na.color = pointblue.palette[6])

pal2 <- colorBin(palette = colorRamp(colors = c(pointblue.palette[4], 
                                                '#ffffff', 
                                                pointblue.palette[3])),
                 domain = dat_lab$water.infil,
                 bins = c(-50, -1, 1, 50),
                 na.color = pointblue.palette[6])

pal3 <- colorBin(palette = colorRamp(colors = c(pointblue.palette[3], 
                                                '#ffffff', 
                                                pointblue.palette[4])),
                 domain = c(dat_lab$carbonA, dat_lab$carbonB),
                 bins = c(-5, -1, 1, 5),
                 na.color = pointblue.palette[6])


# MAP-----------

map2 <- leaflet(shp_pts, height = 500) %>% 
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
  addLegend(colors = pal0(c(-20, 0, 20, NA)),
            labels = c('declining', 'little change', 'improving', 'no data'),
            title = 'Direction<br>of change',
            position = 'topright',
            opacity = 1) %>% 
  
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
map2$dependencies <- c(map2$dependencies, 
                       list(
                         htmltools::htmlDependency(
                           name = 'tomkat-leaflet',
                           version = '1.0.0',
                           src = here::here('Rmd'),
                           stylesheet = 'tk_leaflet.css'
                         )
                       ))

title <- paste0('TomKat Soil Changes 2014-', max(dat$Year))

htmlwidgets::saveWidget(map2,
                        here::here(output2),
                        selfcontained = TRUE,
                        title = title)
