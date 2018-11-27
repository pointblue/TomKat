# README----------------------
# Script to produce vegetation map 2: change in veg cover since 2012
# From RStudio Viewer: Export as webpage "docs/vegetation_map2.html"

## packages
library(tidyverse)
library(sf)
library(htmlTable)
library(leaflet)
library(mapview)

## input files
masterveg <- 'data_master/TK_veg_master.csv'

## output files
output2 <- 'vegetation_map2.html'

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
dat <- read_csv(here::here(masterveg)) %>%
  filter(vegtype != 'Trees') %>% #inconsistent treatment; not included in map
  mutate(
    cover = round(cover, digits = 1),
    vegtype = as.factor(vegtype),
    Pasture = as.factor(Pasture)
  ) 

net_change <- dat %>%
  filter(Year == 2012 | Year == max(Year)) %>%
  mutate(yr = case_when(Year == max(Year) ~ 'current',
                        Year == 2012 ~ 'baseline'),
         Year = NULL) %>%
  spread(key = yr, value = cover) %>%
  mutate(net = current - baseline) %>%
  select(-baseline, -current) %>%
  spread(key = vegtype, value = net)

net_change_long <- net_change %>%
  gather(AnnualGr:Weeds, key = 'vegtype', value = 'netchange') %>%
  mutate('Net change' = case_when(
    netchange > 0 ~ paste0('+', round(netchange, digits = 0)),
    netchange < 0 ~ as.character(round(netchange, digits = 0))
  ))

dat_long <- dat %>%
  mutate(cover = round(cover, digits = 0)) %>%
  spread(key = Year, value = cover) %>%
  full_join(net_change_long %>% select(Pasture, vegtype, `Net change`),
            by = c('Pasture', 'vegtype')) %>%
  gather(`2011`:`Net change`, key = year, value = value) %>%
  arrange(Pasture, vegtype, year)


# POPUP HTML TABLES-------------

dat_lab <- net_change %>%
  mutate(
    label_PereGr = map(
      net_change$Pasture,
      ~ dat_long %>% filter(Pasture == .x & vegtype == 'PereGr') %>%
        select(year, value) %>%
        htmlTable(
          header = c('Year', '% Cover'),
          align = c('c', 'r'),
          rnames = F,
          total = T,
          caption = paste0('<b>Pasture ', .x, '</b>: Perennial Grasses')
        )
    ),
    label_AnnualGr = map(
      net_change$Pasture,
      ~ dat_long %>% filter(Pasture == .x & vegtype == 'AnnualGr') %>%
        select(year, value) %>%
        htmlTable(
          header = c('Year', '% Cover'),
          align = c('c', 'r'),
          rnames = F,
          total = T,
          caption = paste0('<b>Pasture ', .x, '</b>: Annual Grasses')
        )
    ),
    label_NativeGr = map(
      net_change$Pasture,
      ~ dat_long %>% filter(Pasture == .x & vegtype == 'NativeGr') %>%
        select(year, value) %>%
        htmlTable(
          header = c('Year', '% Cover'),
          align = c('c', 'r'),
          rnames = F,
          total = T,
          caption = paste0('<b>Pasture ', .x, '</b>: Native Grasses')
        )
    ),
    label_Grass = map(
      net_change$Pasture,
      ~ dat_long %>% filter(Pasture == .x & vegtype == 'Grass') %>%
        select(year, value) %>%
        htmlTable(
          header = c('Year', '% Cover'),
          align = c('c', 'r'),
          rnames = F,
          total = T,
          caption = paste0('<b>Pasture ', .x, '</b>: All Grasses')
        )
    ),
    label_Shrubs = map(
      net_change$Pasture,
      ~ dat_long %>% filter(Pasture == .x & vegtype == 'Shrubs') %>%
        select(year, value) %>%
        htmlTable(
          header = c('Year', '% Cover'),
          align = c('c', 'r'),
          rnames = F,
          total = T,
          caption = paste0('<b>Pasture ', .x, '</b>: Shrubs')
        )
    ),
    label_Forbs = map(
      net_change$Pasture,
      ~ dat_long %>% filter(Pasture == .x & vegtype == 'Forbs') %>%
        select(year, value) %>%
        htmlTable(
          header = c('Year', '% Cover'),
          align = c('c', 'r'),
          rnames = F,
          total = T,
          caption = paste0('<b>Pasture ', .x, '</b>: Forbs')
        )
    ),
    label_Weeds = map(
      net_change$Pasture,
      ~ dat_long %>% filter(Pasture == .x & vegtype == 'Weeds') %>%
        select(year, value) %>%
        htmlTable(
          header = c('Year', '% Cover'),
          align = c('c', 'r'),
          rnames = F,
          total = T,
          caption = paste0('<b>Pasture ', .x, '</b>: Weeds')
        )
    ),
    label_BareGround = map(
      net_change$Pasture,
      ~ dat_long %>% filter(Pasture == .x & vegtype == 'BareGround') %>%
        select(year, value) %>%
        htmlTable(
          header = c('Year', '% Cover'),
          align = c('c', 'r'),
          rnames = F,
          total = T,
          caption = paste0('<b>Pasture ', .x, '</b>: Bare Ground')
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
    palette = colorRamp(colors = c(
      pointblue.palette[3], '#ffffff', pointblue.palette[4]
    )),
    domain = c(-100, 100),
    bins = c(-100, -75, -50, -25, -10, 10, 25, 50, 75, 100),
    na.color = pointblue.palette[7]
  )


# MAP----------------------

map2 <- leaflet(height = 500) %>% setView(lng = -122.3598,
                                          lat = 37.26693,
                                          zoom = 14) %>%
  ## background terrain
  addProviderTiles("Stamen.Terrain",
                   options = providerTileOptions(minzoom = 14, maxzoom = 15)) %>%
  
  # ranch boundary
  addPolygons(
    data = shp_ranch,
    color = 'black',
    fill = F,
    weight = 3
  ) %>%
  
  ## perennial grasses
  addPolygons(
    data = shp_poly,
    color = 'black',
    fillOpacity = 1,
    weight = 1.5,
    fillColor = ~ pal(PereGr),
    group = 'Perennial Grasses',
    popup = ~ label_PereGr
  ) %>%
  
  ## native grasses
  addPolygons(
    data = shp_poly,
    color = 'black',
    fillOpacity = 1,
    weight = 1.5,
    fillColor = ~ pal(NativeGr),
    group = 'Native Grasses',
    popup = ~ label_NativeGr
  ) %>%
  
  ## annual grasses
  addPolygons(
    data = shp_poly,
    color = 'black',
    fillOpacity = 1,
    weight = 1.5,
    fillColor = ~ pal(AnnualGr),
    group = 'Annual Grasses',
    popup = ~ label_AnnualGr
  ) %>%
  
  ## all grasses
  addPolygons(
    data = shp_poly,
    color = 'black',
    fillOpacity = 1,
    weight = 1.5,
    fillColor = ~ pal(Grass),
    group = 'All Grasses',
    popup = ~ label_Grass
  ) %>%
  
  ## shrubs
  addPolygons(
    data = shp_poly,
    color = 'black',
    fillOpacity = 1,
    weight = 1.5,
    fillColor = ~ pal(Shrubs),
    group = 'Shrubs',
    popup = ~ label_Shrubs
  ) %>%
  
  ## forbs
  addPolygons(
    data = shp_poly,
    color = 'black',
    fillOpacity = 1,
    weight = 1.5,
    fillColor = ~ pal(Forbs),
    group = 'Forbs',
    popup = ~ label_Forbs
  ) %>%
  
  ## weeds
  addPolygons(
    data = shp_poly,
    color = 'black',
    fillOpacity = 1,
    weight = 1.5,
    fillColor = ~ pal(Weeds),
    group = 'Invasive Weeds',
    popup = ~ label_Weeds
  ) %>%
  
  ## bare ground
  addPolygons(
    data = shp_poly,
    color = 'black',
    fillOpacity = 1,
    weight = 1.5,
    fillColor = ~ pal(BareGround),
    group = 'Bare Ground',
    popup = ~ label_BareGround
  ) %>%
  
  ## legend
  addLegend(
    position = 'topright',
    opacity = 1,
    title = '% Cover',
    pal = pal,
    values = dat %>% filter(Year == max(Year)) %>% select(cover),
    labFormat = labelFormat(suffix = '%'),
    na.label = 'No data'
  ) %>%
  
  ## toggles
  addLayersControl(
    baseGroups = c(
      'Perennial Grasses',
      'Native Grasses',
      'Annual Grasses',
      'All Grasses',
      'Shrubs',
      'Forbs',
      'Invasive Weeds',
      'Bare Ground'
    ),
    options = layersControlOptions(collapsed = F),
    position = 'bottomleft'
  ) %>%
  
  ## logo
  addLogo(
    img = logo,
    src = 'remote',
    url = 'http://www.pointblue.org',
    width = 174,
    height = 90,
    offset.y = -5
  )

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

title <- paste0('TomKat Vegetation Changes 2012-', max(dat$Year))

htmlwidgets::saveWidget(map2,
                        here::here(output2),
                        selfcontained = TRUE,
                        title = title)
