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
masterdat <- 'data_master/TK_soil_master.csv'

## output files
output3 <- 'soil_map3.html'

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
    '#8643b7',
    '#d2c921')

# DATA SET UP-------------

dat <- read_csv(here::here(masterdat)) %>%
  rename(Name = 'Point Name') %>%
  select(Name, Year, `Total NitrogenA`:`Total NitrogenB`, PotassiumA:PotassiumB, 
         SodiumA:SodiumB, MagnesiumA:MagnesiumB, CalciumA:CalciumB, pHA:pHB) %>%
  gather(key = var, value = value, `Total NitrogenA`:pHB) %>%
  filter(!is.na(value)) %>%
  filter(Year == max(Year)) %>%
  spread(key = var, value = value)

dat_long <- dat %>%
  gather(key = var, value = value, `Total NitrogenA`:`Total NitrogenB`, 
         PotassiumA:PotassiumB, SodiumA:SodiumB, MagnesiumA:MagnesiumB, 
         CalciumA:CalciumB, pHA:pHB)


# POPUP HTML TABLES-------------

dat_lab <- dat %>%
  mutate(
    label_Nitrogen = map(
      Name,
      ~ dat_long %>% filter(Name == .x & var %in% c('Total NitrogenA', 'Total NitrogenB')) %>%
        select(value) %>%
        mutate(value = txtRound(value, digits = 2, txt.NA = 'NA')) %>%
        htmlTable(
          header = c('Value'),
          align = 'r',
          rnames = c('0-10 cm', '10-40 cm'),
          caption = paste0('<b>', .x, ': Total Nitrogen</b>')
        )
    ),
    label_Potassium = map(
      Name,
      ~ dat_long %>% filter(Name == .x & var %in% c('PotassiumA', 'PotassiumB')) %>%
        select(value) %>%
        mutate(value = txtRound(value, digits = 2, txt.NA = 'NA')) %>%
        htmlTable(
          header = c('Value'),
          align = 'r',
          rnames = c('0-10 cm', '10-40 cm'),
          caption = paste0('<b>', .x, ': Potassium</b>')
        )
    ),
    label_Sodium = map(
      Name,
      ~ dat_long %>% filter(Name == .x & var %in% c('SodiumA', 'SodiumB')) %>%
        select(value) %>%
        mutate(value = txtRound(value, digits = 2, txt.NA = 'NA')) %>%
        htmlTable(
          header = c('Value'),
          align = 'r',
          rnames = c('0-10 cm', '10-40 cm'),
          caption = paste0('<b>', .x, ': Sodium</b>')
        )
    ),
    label_Magnesium = map(
      Name,
      ~ dat_long %>% filter(Name == .x & var %in% c('MagnesiumA', 'MagnesiumB')) %>%
        select(value) %>%
        mutate(value = txtRound(value, digits = 2, txt.NA = 'NA')) %>%
        htmlTable(
          header = c('Value'),
          align = 'r',
          rnames = c('0-10 cm', '10-40 cm'),
          caption = paste0('<b>', .x, ': Magnesium</b>')
        )
    ),
    label_Calcium = map(
      Name,
      ~ dat_long %>% filter(Name == .x & var %in% c('CalciumA', 'CalciumB')) %>%
        select(value) %>%
        mutate(value = txtRound(value, digits = 2, txt.NA = 'NA')) %>%
        htmlTable(
          header = c('Value'),
          align = 'r',
          rnames = c('0-10 cm', '10-40 cm'),
          caption = paste0('<b>', .x, ': Calcium</b>')
        )
    ),
    label_pH = map(
      Name,
      ~ dat_long %>% filter(Name == .x & var %in% c('pHA', 'pHB')) %>%
        select(value) %>%
        mutate(value = txtRound(value, digits = 2, txt.NA = 'NA')) %>%
        htmlTable(
          header = c('Value'),
          align = 'r',
          rnames = c('0-10 cm', '10-40 cm'),
          caption = paste0('<b>', .x, ': pH</b>')
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
pal0 <-
  colorBin(
    palette = colorRamp(colors = c('#ffffff', pointblue.palette[11])),
    domain = c(dat$SodiumA, dat$SodiumB),
    bins = c(0, 0.25, 0.5, 0.75, 1),
    na.color = pointblue.palette[7]
  )

pal1 <-
  colorBin(
    palette = colorRamp(colors = c('#ffffff', pointblue.palette[4])),
    domain = c(dat$`Total NitrogenA`, dat$`Total NitrogenB`),
    bins = c(0, 0.2, 0.3, 0.4, 0.7),
    na.color = pointblue.palette[7]
  )

pal2 <-
  colorBin(
    palette = colorRamp(colors = c('#ffffff', pointblue.palette[8])),
    domain = c(dat$PotassiumA, dat$PotassiumB),
    bins = c(0, 0.5, 1, 1.5, 3),
    na.color = pointblue.palette[7]
  )

pal3 <-
  colorBin(
    palette = colorRamp(colors = c('#ffffff', pointblue.palette[3])),
    domain = c(dat$MagnesiumA, dat$MagnesiumB),
    bins = c(0, 5, 8, 11, 15),
    na.color = pointblue.palette[7]
  )

pal4 <-
  colorBin(
    palette = colorRamp(colors = c('#ffffff', pointblue.palette[10])),
    domain = c(dat$CalciumA, dat$CalciumB),
    bins = c(0, 10, 15, 20, 25),
    na.color = pointblue.palette[7]
  )

pal5 <-
  colorBin(
    palette = colorRamp(colors = c('#ffffff', pointblue.palette[9])),
    domain = c(dat$pHA, dat$pHB),
    bins = c(5, 5.5, 6, 6.5),
    na.color = pointblue.palette[7]
  )


# MAP-----------

map3 <- leaflet(height = 500) %>% setView(lng = -122.3598,
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
  
  # nitrogen: (overlapping circles for two depths)
  addCircleMarkers(
    data = shp_pts,
    radius = 9,
    weight = 1.5,
    fillOpacity = 1,
    color = 'black',
    fillColor =  ~ pal1(`Total NitrogenB`),
    popup =  ~ label_Nitrogen,
    group = 'Total Nitrogen (N)'
  ) %>% 
  
  addCircleMarkers(
    data = shp_pts,
    radius = 4,
    weight = 1.5,
    fillOpacity = 1,
    color = 'black',
    fillColor =  ~ pal1(`Total NitrogenA`),
    popup =  ~ label_Nitrogen,
    group = 'Total Nitrogen (N)'
  ) %>% 
  
  # potassium: (overlapping circles for two depths)
  addCircleMarkers(
    data = shp_pts,
    radius = 9,
    weight = 1.5,
    fillOpacity = 1,
    color = 'black',
    fillColor =  ~ pal2(PotassiumB),
    popup =  ~ label_Potassium,
    group = 'Potassium (K)'
  ) %>% 
  
  addCircleMarkers(
    data = shp_pts,
    radius = 4,
    weight = 1.5,
    fillOpacity = 1,
    color = 'black',
    fillColor =  ~ pal2(PotassiumA),
    popup =  ~ label_Potassium,
    group = 'Potassium (K)'
  ) %>% 
  
  # sodium: (overlapping circles for two depths)
  addCircleMarkers(
    data = shp_pts,
    radius = 9,
    weight = 1.5,
    fillOpacity = 1,
    color = 'black',
    fillColor =  ~ pal0(SodiumB),
    popup =  ~ label_Sodium,
    group = 'Sodium (Na)'
  ) %>% 
  
  addCircleMarkers(
    data = shp_pts,
    radius = 4,
    weight = 1.5,
    fillOpacity = 1,
    color = 'black',
    fillColor =  ~ pal0(SodiumA),
    popup =  ~ label_Sodium,
    group = 'Sodium (Na)'
  ) %>% 
  
  # magnesium: (overlapping circles for two depths)
  addCircleMarkers(
    data = shp_pts,
    radius = 9,
    weight = 1.5,
    fillOpacity = 1,
    color = 'black',
    fillColor =  ~ pal3(MagnesiumB),
    popup =  ~ label_Magnesium,
    group = 'Magnesium (Mg)'
  ) %>% 
  
  addCircleMarkers(
    data = shp_pts,
    radius = 4,
    weight = 1.5,
    fillOpacity = 1,
    color = 'black',
    fillColor =  ~ pal3(MagnesiumA),
    popup =  ~ label_Magnesium,
    group = 'Magnesium (Mg)'
  ) %>% 
  
  # calcium: (overlapping circles for two depths)
  addCircleMarkers(
    data = shp_pts,
    radius = 9,
    weight = 1.5,
    fillOpacity = 1,
    color = 'black',
    fillColor =  ~ pal4(CalciumB),
    popup =  ~ label_Calcium,
    group = 'Calcium (Ca)'
  ) %>% 
  
  addCircleMarkers(
    data = shp_pts,
    radius = 4,
    weight = 1.5,
    fillOpacity = 1,
    color = 'black',
    fillColor =  ~ pal4(CalciumA),
    popup =  ~ label_Calcium,
    group = 'Calcium (Ca)'
  ) %>% 
  
  # pH: (overlapping circles for two depths)
  addCircleMarkers(
    data = shp_pts,
    radius = 9,
    weight = 1.5,
    fillOpacity = 1,
    color = 'black',
    fillColor =  ~ pal5(pHB),
    popup =  ~ label_pH,
    group = 'pH'
  ) %>% 
  
  addCircleMarkers(
    data = shp_pts,
    radius = 4,
    weight = 1.5,
    fillOpacity = 1,
    color = 'black',
    fillColor =  ~ pal5(pHA),
    popup =  ~ label_pH,
    group = 'pH'
  ) %>% 
  
  # legends (one per metric)
  addLegend(
    position = 'topright',
    title = 'Total Nitrogen',
    opacity = 1,
    pal = pal1,
    values = c(dat_lab$`Total NitrogenA`, dat_lab$`Total NitrogenB`),
    group = 'Total Nitrogen (N)',
    na.label = 'No data'
  ) %>% 
  
  addLegend(
    position = 'topright',
    title = 'Potassium',
    opacity = 1,
    pal = pal2,
    values = c(dat_lab$PotassiumA, dat_lab$PotassiumB),
    group = 'Potassium (K)',
    na.label = 'No data'
  ) %>% 
  
  addLegend(
    position = 'topright',
    title = 'Sodium',
    opacity = 1,
    pal = pal0,
    values = c(dat_lab$SodiumA, dat_lab$SodiumB),
    group = 'Sodium (Na)',
    na.label = 'No data'
  ) %>% 
  
  addLegend(
    position = 'topright',
    title = 'Magnesium',
    opacity = 1,
    pal = pal3,
    values = c(dat_lab$MagnesiumA, dat_lab$MagnesiumB),
    group = 'Magnesium (Mg)',
    na.label = 'No data'
  ) %>% 
  
  addLegend(
    position = 'topright',
    title = 'Calcium',
    opacity = 1,
    pal = pal4,
    values = c(dat_lab$CalciumA, dat_lab$CalciumB),
    group = 'Calcium (Ca)',
    na.label = 'No data'
  ) %>% 
  
  addLegend(
    position = 'topright',
    title = 'pH',
    opacity = 1,
    pal = pal5,
    values = c(dat_lab$pHA, dat_lab$pHB),
    group = 'pH',
    na.label = 'No data'
  ) %>% 
  
  ## toggles
  addLayersControl(
    position = 'bottomleft',
    options = layersControlOptions(collapsed = F),
    overlayGroups = c('Total Nitrogen (N)', 'Potassium (K)', 'Sodium (Na)', 
                      'Magnesium (Mg)', 'Calcium (Ca)', 'pH')
  ) %>% 
  
  hideGroup('Potassium (K)') %>%
  hideGroup('Sodium (Na)') %>%
  hideGroup('Magnesium (Mg)') %>%
  hideGroup('Calcium (Ca)') %>%
  hideGroup('pH') %>%
  
  ## logo
  addLogo(
    img = logo,
    src = 'remote',
    url = 'http://www.pointblue.org',
    width = 174,
    height = 90,
    offset.y = -5
  )

title <- paste0('TomKat Soil Nutrients ', max(dat$Year))

htmlwidgets::saveWidget(map3,
                        here::here(output3),
                        selfcontained = TRUE,
                        title = title)
