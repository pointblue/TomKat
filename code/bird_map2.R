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
masterdat <- 'data_master/TK_richness_by_point.csv'

## output files
output2 <- 'bird_map2.html'

## shapefiles
poly <- 'TK_veg_fields'
ranch <- 'TomKat_ranch_boundary'
pts <- 'TOKA_point_count_grid'
pts_hocr <- 'HOCR_point_count_riparian'

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
  select(Point, S.ACE, S.obs, n) %>%
  gather(S.ACE:n, key = variable, value = value) %>%
  mutate(variable = factor(variable, levels = c('S.ACE', 'S.obs', 'n'))) %>%
  arrange(Point, variable) 


# POPUP HTML TABLES-------------
dat_lab <- dat %>%
  filter(variable == 'S.ACE') %>%
  mutate(label_tot = map(Point, ~ dat %>%
                           filter(Point == .x) %>%
                           select(value) %>%
                           htmlTable(header = NA,
                                     align = 'r',
                                     rnames = c('Estimated species', 
                                                'Observed species', 
                                                'Number of surveys'),
                                     total = FALSE,
                                     caption = paste0('<b>', .x, '</b>'))
                         ))

    
# RASTER SET UP--------------------------
# convert TOKA point counts to raster grid with density estimates as value
# Note: do not load raster package, because it conflicts with tidyverse

## first, create generic raster that matches the extent and resolution, placing
## the point count grid in center of each cell (200 x 200m)

shp_pts <- st_read(here::here('GIS'), pts, quiet = TRUE) %>%
  inner_join(dat_lab, by = c('Name' = 'Point')) #(drop points not surveyed)
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
  select(Name, value)
shp_pts2 <- as(shp_pts2, 'Spatial') #convert to SpatialPointsDataFrame for rasterize

## transfer richness values from point count coordinates to corresponding 
## raster cell:
rich_rast <- raster::rasterize(shp_pts2, r, field = shp_pts2$value)


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

shp_pts_hocr <- st_read(here::here('GIS'), pts_hocr, quiet = TRUE) %>%
  st_transform('+proj=longlat +datum=WGS84') %>%
  select(Name) %>%
  left_join(dat_lab, by = c('Name' = 'Point'))


# COLOR PALETTE-----------
# set color scale for densities

boxplot(dat_lab$value, plot = F)$stats
boxplot(dat_lab$value, plot = F)$out %>% max()

pal1 <- colorBin(
  palette = c('#ffffff', pointblue.palette[4]), 
  domain = dat_lab$value, 
  bins = c(0, 15, 25, 35, 45, max(dat_lab$value)), 
  na.color = 'transparent')


# MAP-----------

map2 <- leaflet(shp_pts_map, height = 500) %>% 
  setView(lng = -122.3598,
          lat = 37.26693,
          zoom = 14) %>% 
  
  # background terrain
  addProviderTiles("Stamen.Terrain",
                   options = providerTileOptions(minzoom = 14, maxzoom = 15)) %>%
  
  ## add rasters (below polygon boundaries) showing densities by cell:
  addRasterImage(x = rich_rast, group = 'Combined',
                 colors = pal1, opacity = 0.8, project = F) %>%
  
  # pasture boundaries:
  addPolygons(data = shp_poly, fillColor = pointblue.palette[2], 
              color = 'black', weight = 1, fillOpacity = 0.1) %>% 
  
  # ranch boundary
  addPolygons(data = shp_ranch, fill = FALSE,
              color = 'black', weight = 3) %>%  
  
  # add transparent circle markers for TOKA points to show the popup tables
  addCircleMarkers(popup = ~ label_tot,
                   radius = 11, weight = 1.5, 
                   options = popupOptions(maxWidth = 800),
                   color = 'transparent',
                   fillColor = 'transparent') %>% 
  
  # add circle markers for HOCR points with popup tables
  addCircleMarkers(data = shp_pts_hocr, popup = ~ label_tot,
                   radius = 8, weight = 1.5, 
                   options = popupOptions(maxWidth = 800),
                   color = 'black',
                   fillColor = ~pal1(value), 
                   fillOpacity = 0.8) %>% 

  # add legend & layer controls:
  addLegend(position = 'topright', 
            colors = pal1(c(10, 20, 30, 40, 50)),
            labels = c('0 - 15', '16 - 25', '25 - 35', '35 - 45', '> 45'),
            values = NULL,
            opacity = 1,
            title = 'Estimated<br>species<br>richness') %>%
  
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

title <- paste0('TomKat Bird Richness Map 2018')

htmlwidgets::saveWidget(map2,
                        here::here(output2),
                        selfcontained = TRUE,
                        title = title)
