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
  mutate(water.infil = water.infil/60) %>% #convert to minutes
  gather(bulk.dens.gcm3:carbonB, key = 'metric', value = 'value')

net_change <- dat %>% 
  mutate(yr = case_when(Year == max(Year) ~ 'current',
                        Year == 2015 ~ 'baseline'),
         Year = NULL) %>%
  spread(key = yr, value = value) %>%
  mutate(net = current - baseline,
         net = case_when(is.nan(net) ~ NA_real_,
                         TRUE ~ net)) %>%
  select(-baseline, -current) %>%
  spread(key = metric, value = net)

net_change_long <- net_change %>%
  gather(bulk.dens.gcm3:water.infil, key = 'metric', value = 'netchange') %>%
  mutate('Net change' = case_when(
    netchange > 0 ~ paste0('+', round(netchange, digits = 2)),
    netchange <= 0 ~ as.character(round(netchange, digits = 2)),
    TRUE ~ as.character(netchange)
  ))

dat_long <- dat %>%
  mutate(value = case_when(is.nan(value) ~ NA_real_,
                           TRUE ~ round(value, digits = 2))) %>%
  spread(key = Year, value = value) %>%
  full_join(net_change_long %>% select(Name, metric, `Net change`),
            by = c('Name', 'metric')) %>%
  gather(`2015`:`Net change`, key = year, value = value) %>%
  arrange(Name, metric, year)


# POPUP HTML TABLES-------------

dat_lab <- net_change %>%
  mutate(
    label_bulk.dens.gcm3 = map(
      Name,
      ~ dat_long %>% filter(Name == .x & metric == 'bulk.dens.gcm3') %>%
        select(year, value) %>%
        mutate(value = txtRound(value, digits = 2, txt.NA = 'NA')) %>%
        htmlTable(
          header = c('Year', 'g/cm<sup>3</sup>'),
          align = c('l', 'r'),
          rnames = FALSE,
          total = TRUE,
          caption = paste0('<b>', .x, ': Bulk density</b>')
        )
    ),
    label_water.infil = map(
      Name,
      ~ dat_long %>% filter(Name == .x & metric == 'water.infil') %>%
        select(year, value) %>%
        mutate(value = txtRound(value, digits = 2, txt.NA = 'NA')) %>%
        htmlTable(
          header = c('Year', 'Minutes'),
          align = c('l', 'r'),
          rnames = FALSE,
          total = TRUE,
          caption = paste0('<b>', .x, ': Water infiltration</b>')
        )
    ),
    label_carbon = map(
      Name,
      ~ dat_long %>% filter(Name == .x & metric %in% c('carbonA', 'carbonB')) %>%
        select(metric, year, value) %>%
        mutate(value = txtRound(value, digits = 1, txt.NA = 'NA')) %>%
        spread(key = metric, value = value) %>%
        htmlTable(
          header = c('Year', '0-10 cm', '10-40 cm'),
          align = c('l', 'r'),
          rnames = FALSE,
          total = TRUE,
          caption = paste0('<b>', .x, ': % Carbon</b>')
        )
    )
  )


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
pal1 <-
  colorBin(
    palette = colorRamp(colors = c(
      pointblue.palette[4], '#ffffff', pointblue.palette[3]
    )),
    domain = net_change$bulk.dens.gcm3,
    bins = c(-0.25, -0.1, -0.05, 0.05, 0.1, 0.25),
    na.color = pointblue.palette[7]
  )

pal2 <-
  colorBin(
    palette = colorRamp(colors = c(
      pointblue.palette[4], '#ffffff', pointblue.palette[3]
    )),
    domain = net_change$water.infil,
    bins = c(-50, -10, -5, -0.25, 0.25, 5, 10, 50),
    na.color = pointblue.palette[7]
  )

pal3 <-
  colorBin(
    palette = colorRamp(colors = c(
      pointblue.palette[3], '#ffffff', pointblue.palette[4]
    )),
    domain = c(net_change$carbonA, net_change$carbonB),
    bins = c(-5, -1, -0.25, 0.25, 1, 5),
    na.color = pointblue.palette[7]
  )


# MAP-----------

map2 <- leaflet(height = 500) %>% setView(lng = -122.3598,
                                  lat = 37.26693,
                                  zoom = 14) %>%
  # background terrain
  addProviderTiles("Stamen.Terrain", 
                   options = providerTileOptions(minzoom = 14, maxzoom = 15)) %>% 
  
  # pasture boundaries:
  addPolygons(
    data = shp_poly,
    color = 'black',
    fillColor = pointblue.palette[2],
    fillOpacity = 0.1,
    weight = 1
  ) %>% 
  
  # ranch boundary
  addPolygons(
    data = shp_ranch,
    color = 'black',
    fill = F,
    weight = 3
  ) %>%
  
  # bulk density:
  addCircleMarkers(
    data = shp_pts,
    radius = ~ifelse(Name %in% c('TOKA-022', 'TOKA-068'), 10, 6),
    weight = 1.5,
    fillOpacity = 1,
    color = 'black',
    fillColor =  ~ pal1(bulk.dens.gcm3),
    popup =  ~ label_bulk.dens.gcm3,
    group = 'Bulk density'
  ) %>%
  
  # water infiltration:
  addCircleMarkers(
    data = shp_pts,
    radius = ~ifelse(Name %in% c('TOKA-022', 'TOKA-068'), 10, 6),
    weight = 1.5,
    fillOpacity = 1,
    color = 'black',
    fillColor =  ~ pal2(water.infil),
    popup =  ~ label_water.infil,
    group = 'Water infiltration'
  ) %>% 
  
  # carbon: (overlapping circles for two depths)
  addCircleMarkers(
    data = shp_pts,
    radius = ~ifelse(Name %in% c('TOKA-022', 'TOKA-068'), 12, 9),
    weight = 1.5,
    fillOpacity = 1,
    color = 'black',
    fillColor =  ~ pal3(carbonB),
    popup =  ~ label_carbon,
    group = '% Carbon'
  ) %>% 
  
  addCircleMarkers(
    data = shp_pts,
    radius = ~ifelse(Name %in% c('TOKA-022', 'TOKA-068'), 7, 4),
    weight = 1.5,
    fillOpacity = 1,
    color = 'black',
    fillColor =  ~ pal3(carbonA),
    popup =  ~ label_carbon,
    group = '% Carbon'
  ) %>% 
  
  # legends (one per metric)
  addLegend(
    position = 'topright',
    title = 'Bulk density<br>(g/cm<sup>3</sup>)',
    opacity = 1,
    pal = pal1,
    values = net_change$bulk.dens.gcm3,
    group = 'Bulk density',
    na.label = 'No data'
  ) %>% 
  
  addLegend(
    position = 'topright',
    title = 'Water infiltration<br>(minutes)',
    opacity = 1,
    # pal = pal2,
    values = net_change$water.infil,
    # labFormat = labelFormat(transform = function(x) {
      # if (x < -10) {x = '< -10'} else if (x > 10) {x = '> +10'} else if (x > 0) {x = paste0('+', x)}}),
    colors = pal2(c(-12, -7, -2, 0, 2, 7, 12)),
    labels = c('< -10', '-10 &ndash; -5', '-5 &ndash; -0.25', '-0.25 &ndash; +0.25', '+0.25 &ndash; +5', '+5 &ndash; 10', '> +10'),
    group = 'Water infiltration',
    na.label = 'No data'
  ) %>% 
  
  addLegend(
    position = 'topright',
    title = '% Carbon',
    opacity = 1,
    pal = pal3,
    values = c(net_change$carbonA, net_change$carbonB),
    # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
    group = '% Carbon',
    na.label = 'No data'
  ) %>% 
  
  ## toggles
  addLayersControl(
    position = 'bottomleft',
    options = layersControlOptions(collapsed = F),
    overlayGroups = c('Bulk density', 'Water infiltration', '% Carbon')
  ) %>% 
  
  hideGroup('Water infiltration') %>%
  hideGroup('% Carbon') %>%
  
  ## logo
  addLogo(
    img = logo,
    src = 'remote',
    url = 'http://www.pointblue.org',
    width = 174,
    height = 90,
    offset.y = -5
  )

title <- paste0('TomKat Soil Changes 2014-', max(dat$Year))

htmlwidgets::saveWidget(map2,
                        here::here(output2),
                        selfcontained = TRUE,
                        title = title)
