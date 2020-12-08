# README----------------------
# Script to produce vegetation map 2: change in veg cover over time
# New in 2018: average 2012-2014 and compare to recent average (2017-2019)

## packages
library(tidyverse)
library(sf)
library(htmlTable)
library(leaflet)
library(mapview)

## input files
masterveg <- 'data_clean/TK_veg_main.csv'

## output files
output2 <- 'docs/widget/vegetation_map2.html'

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
# calculate "baseline" (2012-14) and "recent" averages for each
#  vegtype in each pasture

## use most recent 3 years of data as "recent"
maxyear = read_csv(here::here(masterveg), col_types = cols()) %>% 
  pull(Year) %>% max()
minyear = maxyear - 3

dat <- read_csv(here::here(masterveg), col_types = cols()) %>%
  filter(vegtype != 'Trees') %>% #inconsistent treatment; not included in map
  mutate(vegtype = as.factor(vegtype),
         Pasture = as.factor(Pasture),
         group = case_when(Year<2015 ~ 'baseline',
                           Year>minyear ~ 'recent')) %>%
  filter(Year != 2011) %>%
  filter(!is.na(group)) %>% 
  group_by(Pasture, group, vegtype) %>%
  summarize(cover = round(mean(cover, na.rm = T), digits = 0)) %>%
  ungroup()

# calculate net change between recent and baseline years
net_change <- dat %>%
  spread(key = group, value = cover) %>%
  mutate(net = recent - baseline,
         prop = case_when(abs(net) >= 10 ~ (net/baseline) * 100,
                          abs(net) >= 5 & vegtype %in% c('PereGr', 'NativeGr') ~ (net/baseline) * 100,
                          TRUE ~ 0),
         prop = case_when(is.infinite(prop) ~ 1000,
                          TRUE ~ prop),
         text = case_when(net > 0 ~ paste0('+', round(net, digits = 0), '%'),
                          net < 0 ~ paste0(round(net, digits = 0), '%'),
                          net == 0 ~ '0%'))
# ,
#          text2 = case_when(prop < -90 ~ paste0('>90% decline'),
#                           prop < -10 ~ paste0('>10% decline'),
#                           prop == 0 ~ paste0('little change'),
#                           prop >= 90 ~ paste0('>90% increase'),
#                           prop > 10 ~ paste0('>10% increase')))

# format to include in pop-up tables
net_change_long <- net_change %>%
  select(-net, -prop) %>%
  gather(baseline:text, key = group, value = cover) %>%
  arrange(Pasture, vegtype, group)


# POPUP HTML TABLES-------------

dat_lab <- net_change %>%
  select(-baseline, -recent, -net, -text) %>%
  spread(key = vegtype, value = prop) %>%
  mutate(
    label_PereGr = map(unique(net_change$Pasture),
                       ~ net_change_long %>% 
                         filter(Pasture == .x & vegtype == 'PereGr') %>%
                         select(cover) %>%
                         htmlTable(header = c('Average<br>% Cover'), 
                                   rnames = c('2012-2014', paste0(minyear, '-', maxyear), 'Difference'),
                                   align = 'r', total = T,
                                   caption = paste0('<b>Pasture ', .x, 
                                                    '</b>: Perennial Grasses'))),
    label_AnnualGr = map(unique(net_change$Pasture),
                         ~ net_change_long %>% 
                           filter(Pasture == .x & vegtype == 'AnnualGr') %>%
                           select(cover) %>%
                           htmlTable(header = c('Average<br>% Cover'), 
                                     rnames = c('2012-2014', paste0(minyear, '-', maxyear), 'Difference'),
                                     align = 'r', total = T,
                                     caption = paste0('<b>Pasture ', .x, 
                                                      '</b>: Annual Grasses'))),
    label_NativeGr = map(unique(net_change$Pasture),
                         ~ net_change_long %>% 
                           filter(Pasture == .x & vegtype == 'NativeGr') %>%
                           select(cover) %>%
                           htmlTable(header = c('Average<br>% Cover'), 
                                     rnames = c('2012-2014', paste0(minyear, '-', maxyear), 'Difference'),
                                     align = 'r', total = T,
                                     caption = paste0('<b>Pasture ', .x, 
                                                      '</b>: Native Grasses'))),
    label_Grass = map(unique(net_change$Pasture),
                      ~ net_change_long %>% 
                        filter(Pasture == .x & vegtype == 'Grass') %>%
                        select(cover) %>%
                        htmlTable(header = c('Average<br>% Cover'), 
                                  rnames = c('2012-2014', paste0(minyear, '-', maxyear), 'Difference'),
                                  align = 'r', total = T,
                                  caption = paste0('<b>Pasture ', .x, 
                                                   '</b>: All Grasses'))),
    label_Shrubs = map(unique(net_change$Pasture),
                       ~ net_change_long %>% 
                         filter(Pasture == .x & vegtype == 'Shrubs') %>%
                         select(cover) %>%
                         htmlTable(header = c('Average<br>% Cover'), 
                                   rnames = c('2012-2014', paste0(minyear, '-', maxyear), 'Difference'),
                                   align = 'r', total = T,
                                   caption = paste0('<b>Pasture ', .x, 
                                                    '</b>: Shrubs'))),
    label_Forbs = map(unique(net_change$Pasture),
                      ~ net_change_long %>% 
                        filter(Pasture == .x & vegtype == 'Forbs') %>%
                        select(cover) %>%
                        htmlTable(header = c('Average<br>% Cover'), 
                                  rnames = c('2012-2014', paste0(minyear, '-', maxyear), 'Difference'),
                                  align = 'r', total = T,
                                  caption = paste0('<b>Pasture ', .x, 
                                                   '</b>: Forbs'))),
    label_Weeds = map(unique(net_change$Pasture),
                      ~ net_change_long %>% 
                        filter(Pasture == .x & vegtype == 'Weeds') %>%
                        select(cover) %>%
                        htmlTable(header = c('Average<br>% Cover'), 
                                  rnames = c('2012-2014', paste0(minyear, '-', maxyear), 'Difference'),
                                  align = 'r', total = T,
                                  caption = paste0('<b>Pasture ', .x, 
                                                   '</b>: Weeds'))),
    label_BareGround = map(unique(net_change$Pasture),
                           ~ net_change_long %>% 
                             filter(Pasture == .x & vegtype == 'BareGround') %>%
                             select(cover) %>%
                             htmlTable(header = c('Average<br>% Cover'), 
                                       rnames = c('2012-2014', paste0(minyear, '-', maxyear), 'Difference'),
                                       align = 'r', total = T,
                                       caption = paste0('<b>Pasture ', .x, 
                                                        '</b>: Bare Ground'))))


# SHAPEFILES SET UP------

shp_poly <- st_read(here::here('GIS'), poly, quiet = TRUE) %>%
  st_transform('+proj=longlat +datum=WGS84') %>%
  full_join(dat_lab, by = 'Pasture')

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
pal <- colorBin(palette = colorRamp(colors = c(pointblue.palette[3], 
                                              '#ffffff', 
                                              pointblue.palette[4])),
                domain = c(-100, 100),
                bins = c(-100, -90, -10, 10, 90, max(net_change$prop)),
                na.color = pointblue.palette[7])

# MAP----------------------

map2 <- leaflet(shp_poly, height = 500) %>% 
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
            title = '% Change relative<br>to 2012-14',
            colors = pal(c(-95, -25, 0, 25, 95)),
            values = net_change %>% pull(prop),
            labels = c('strong decline', 'decline', 
                       'little change', 'increase', 'strong increase'),
            labFormat = labelFormat(suffix = '%'),
            na.label = 'No data',
            opacity = 1) %>%
  
  ## toggles
  addLayersControl(baseGroups = c('Perennial Grasses', 'Native Grasses',
                                  'Annual Grasses', 'All Grasses', 'Shrubs',
                                  'Forbs', 'Invasive Weeds', 'Bare Ground'),
                   options = layersControlOptions(collapsed = F),
                   position = 'bottomleft') %>%
  
  ## logo
  leafem::addLogo(img = logo, src = 'remote', url = 'http://www.pointblue.org',
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

title <- paste0('TomKat Vegetation Changes 2012-', maxyear)

htmlwidgets::saveWidget(map2,
                        here::here(output2),
                        selfcontained = FALSE, libdir = 'lib',
                        title = title)
