# README----------------------
# Script to produce soil map 4: bacterial richness map

## packages
library(tidyverse)
library(sf)
library(htmlTable)
library(leaflet)
library(mapview)

## input files
masterdat <- 'data_master/TK_soil_master.csv'
microbephyla <- 'data_raw/Bacterial_Phyla_For_SOTR.xlsx'

## output files
output4 <- 'soil_map4.html'

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

dat <- read_csv(here::here(masterdat), col_types = cols()) %>%
  rename(Name = 'Point Name') %>%
  filter(Year == 2015) %>%
  select(Name, richA, richB) %>%
  filter(!is.na(richA))

# proportions by phylum
pdat <- readxl::read_excel(here::here(microbephyla)) %>%
  filter(Sample_ID != 'avg') %>% #get rid of last line
  gather(-Sample_ID, key = 'phylum', value = 'prop') %>%
  separate(Sample_ID, into = c('site', 'point', 'depth')) %>%
  unite('point', site:point, sep = '-') %>%
  mutate(depth = recode(depth, '10' = 'richA', '40' = 'richB'),
         group = case_when(phylum %in% c('Bacteroidetes', 'Actinobacteria', 'Firmicutes') ~ 'copiotrophs',
                           phylum %in% c('Acidobacteria', 'Verrucomicrobia') ~ 'oligotrophs',
                           TRUE ~ 'other'),
         group = factor(group, levels = c('copiotrophs', 'other', 'oligotrophs'))) %>%
  arrange(point, depth, group, phylum) %>%
  mutate(phylum = factor(phylum, levels = unique(phylum)[c(1:6, 8:11, 7)]))
  

# POPUP PIE CHARTS----------------
## create list of pie charts for microbe popups showing proportion of each
## phylum present at each depth

blank_theme <- theme_minimal() +
  theme(
    axis.title.x = element_blank(), axis.title.y = element_blank(),
    axis.text.x = element_blank(), axis.text.y = element_blank(),
    panel.border = element_blank(), panel.grid = element_blank(),
    axis.ticks = element_blank(), legend.position = 'right',
    legend.key.size = unit(0.75, 'lines'),
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25),'pt'),
    plot.background = element_rect(fill = 'white', color = NA),
    panel.background = element_rect(fill = 'white', color = NA)
  )

pplots <- map(dat$Name, 
              ~ pdat %>% filter(point == .x) %>%
                ggplot(aes(x = depth, y = prop, fill = phylum)) +
                geom_col(color = 'gray80') + 
                coord_polar(start = 0, theta = 'y') + 
                blank_theme + 
                ggtitle(.x) + 
                scale_y_continuous(expand = c(0, 0)) +
                scale_fill_viridis_d())

popplots <- popupGraph(pplots, type = 'png', width = 250, height = 200)


# SHAPEFILES SET UP------
shp_pts <- st_read(here::here('GIS'), pts, quiet = TRUE) %>%
  st_transform('+proj=longlat +datum=WGS84') %>%
  right_join(dat, by = 'Name')

shp_poly <- st_read(here::here('GIS'), poly, quiet = TRUE) %>%
  st_transform('+proj=longlat +datum=WGS84')

shp_ranch <- st_read(here::here('GIS'), ranch, quiet = TRUE) %>%
  st_transform('+proj=longlat +datum=WGS84')


# COLOR PALETTE-----------
## Define color palette for richness:
pal <- colorNumeric(palette = colorRamp(colors = c('#ffffff', pointblue.palette[4])),
                    domain = c(dat$richA, dat$richB))

# MAP-----------

map4 <- leaflet(shp_pts, height = 500) %>% 
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
  
  # bacterial richness: color=richness, composition=popup graphs
  addCircleMarkers(fillColor = ~pal(richB),
                   popup = ~popplots,
                   radius = 9, 
                   color = 'black',
                   weight = 1.5, 
                   fillOpacity = 1) %>%
  
  addCircleMarkers(fillColor = ~pal(richA),
                   popup = ~popplots,
                   radius = 4,
                   color = 'black',
                   weight = 1.5,
                   fillOpacity = 1) %>%
  
  addLegend(title = 'Bacterial<br>richness', 
            pal = pal,
            values = c(dat$richA, dat$richB),
            position = 'topright', 
            opacity = 1) %>%
  
  ## logo
  addLogo(img = logo, src = 'remote', url = 'http://www.pointblue.org',
          width = 174, height = 90, offset.y = -5)

# add CSS
map4$dependencies <- c(map4$dependencies, 
                       list(
                         htmltools::htmlDependency(
                           name = 'tomkat-leaflet',
                           version = '1.0.0',
                           src = here::here('Rmd'),
                           stylesheet = 'tk_leaflet.css'
                         )
                       ))

title <- paste0('TomKat Soil Microbes')

htmlwidgets::saveWidget(map4,
                        here::here(output4),
                        selfcontained = TRUE,
                        title = title)
