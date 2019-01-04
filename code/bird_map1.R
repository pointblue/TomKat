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
masterdat <- 'data_master/TK_bird_density_by_point.csv'

## output files
output1 <- 'bird_map1.html'

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


# DATA SET UP------------------
dat <- read_csv(here::here(masterdat)) %>%
  select(Label, Estimate, species) %>%
  filter(species != 'WCSP') %>% #for now
  mutate(Estimate = Estimate * 2.47105) %>% #convert to birds/acre
  spread(key = species, value = Estimate) %>%
  mutate(total = GRSP + SAVS) %>%
  gather(GRSP:total, key = species, value = abund) %>%
  mutate(fullname = recode(species,
                           GRSP = 'Grasshopper Sparrow',
                           SAVS = 'Savannah Sparrow',
                           total = 'Total'),
         abund_round = txtRound(abund, digits = 1, txt.NA = 'NA'))


# POPUP HTML TABLES-------------

dat_lab <- dat %>%
  select(-fullname, -abund_round) %>%
  spread(key = species, value = abund) %>%
  mutate(label_tot = map(Label, ~ dat %>%
                           filter(Label == .x) %>%
                           select(abund_round) %>%
                           htmlTable(header = c('Density<br>(birds/acre)'),
                                     align = 'r',
                                     rnames = c('Grasshopper Sparrow', 
                                                'Savannah Sparrow', 
                                                'Total'),
                                     total = TRUE,
                                     caption = paste0('<b>', .x, '</b>'))
                         ))

    
# RASTER SET UP--------------------------
# convert point counts to raster grid with density estimates as value
# Note: do not load raster package, because it conflicts with tidyverse

## first, create generic raster that matches the extent and resolution, placing
## the point count grid in center of each cell (200 x 200m)

shp_pts <- st_read(here::here('GIS'), pts, quiet = TRUE) %>%
  right_join(dat_lab, by = c('Name' = 'Label')) #(drop points not surveyed)
# Note: leave in UTM for now, to allow setting raster grid size in meters

r <- raster::raster(ncols = 21,
                    nrows = 13,
                    resolution = 200,
                    xmn = st_bbox(shp_pts)$xmin - 100,
                    xmx = st_bbox(shp_pts)$xmax + 100,
                    ymn = st_bbox(shp_pts)$ymin - 100,
                    ymx = st_bbox(shp_pts)$ymax + 100,
                    crs = st_crs(shp_pts)$proj4string)

## convert both to lat-long
r <- raster::projectRaster(r, crs = '+proj=longlat +datum=WGS84')

shp_pts2 <- shp_pts %>% 
  st_transform('+proj=longlat +datum=WGS84') %>%
  select(Name, GRSP, SAVS, total)
shp_pts2 <- as(shp_pts2, 'Spatial') #convert to SpatialPointsDataFrame for rasterize

## transfer density values from point count coordinates to corresponding 
## raster cell:
grsp_rast <- raster::rasterize(shp_pts2, r, field = shp_pts2$GRSP)
savs_rast <- raster::rasterize(shp_pts2, r, field = shp_pts2$SAVS)
all_rast <- raster::rasterize(shp_pts2, r, field = shp_pts2$total)


# SHAPEFILES SET UP--------------------------

# convert coords to longlat for mapping
shp_pts_map <- shp_pts %>% 
  st_transform('+proj=longlat +datum=WGS84')

## pasture polygons
shp_poly <- st_read(here::here('GIS'), poly, quiet = TRUE) %>%
  st_transform('+proj=longlat +datum=WGS84')

## ranch polygon
shp_ranch <- st_read(here::here('GIS'), ranch, quiet = TRUE) %>%
  st_transform('+proj=longlat +datum=WGS84')



# COLOR PALETTE-----------
# set color scale for densities

boxplot(dat_lab$total, plot = F)$stats
boxplot(dat_lab$total, plot = F)$out %>% max()

pal1 <- colorBin(
  palette = c('#ffffff', pointblue.palette[4]), 
  domain = dat_lab$total, 
  bins = c(0, 0.001, 1, 3, 5, max(dat_lab$total)), 
  na.color = 'transparent')


# MAP-----------

map1 <- leaflet(shp_pts_map, height = 500) %>% 
  setView(lng = -122.3598,
          lat = 37.26693,
          zoom = 14) %>% 
  
  # background terrain
  addProviderTiles("Stamen.Terrain",
                   options = providerTileOptions(minzoom = 14, maxzoom = 15)) %>%
  
  ## add rasters (under polygon boundaries) showing densities by cell:
  addRasterImage(x = all_rast, group = 'Combined', 
                 colors = pal1, opacity = 0.8, project = F) %>%
  addRasterImage(x = grsp_rast, group = 'Grasshopper Sparrow',
                 colors = pal1, opacity = 0.8, project = F) %>%
  addRasterImage(x = savs_rast, group = 'Savannah Sparrow',
                 colors = pal1, opacity = 0.8, project = F) %>% 
  
  # pasture boundaries:
  addPolygons(data = shp_poly, fillColor = pointblue.palette[2], 
              color = 'black', weight = 1, fillOpacity = 0.1) %>% 
  
  # ranch boundary
  addPolygons(data = shp_ranch, fill = FALSE,
              color = 'black', weight = 3) %>%  
  
  # add transparent circle markers for each species to show the popup tables
  # combined: 
  addCircleMarkers(popup = ~ label_tot, group = 'Combined',
                   radius = 11, weight = 1.5, 
                   options = popupOptions(maxWidth = 800),
                   color = 'transparent',
                   fillColor = 'transparent') %>% 
  # GRSP:
  addCircleMarkers(popup = ~ label_tot, group = 'Grasshopper Sparrow',
                   radius = 11, weight = 1.5,
                   options = popupOptions(maxWidth = 800),
                   color = 'transparent',
                   fillColor = 'transparent') %>% 
  
  # SAVS:
  addCircleMarkers(popup = ~ label_tot, group = 'Savannah Sparrow',
                   radius = 11, weight = 1.5,
                   options = popupOptions(maxWidth = 800),
                   color = 'transparent',
                   fillColor = 'transparent') %>% 
  
  # add legend & layer controls:
  addLegend(position = 'topright', 
            colors = pal1(c(0, 0.5, 2, 4, 7.5)),
            labels = c('0', '< 1', '1 - 3', '3 - 5', '> 5'),
            values = NULL,
            opacity = 1,
            title = 'Density<br>(birds/acre)') %>%
  
  addLayersControl(position = 'bottomleft', 
                   options = layersControlOptions(collapsed = F),
                   baseGroups = c('Combined', 
                                  'Grasshopper Sparrow', 
                                  'Savannah Sparrow')) %>%
  
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

title <- paste0('TomKat Bird Map 2018')

htmlwidgets::saveWidget(map1,
                        here::here(output1),
                        selfcontained = TRUE,
                        title = title)
