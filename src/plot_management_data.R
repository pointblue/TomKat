# README----------------------
# Script to produce management maps 1 and 2


# Create html tables----------------------

create_mgt_html_tables<-function(dat){
  fields <- st_read(here::here('GIS'), 'TK_veg_fields', quiet = TRUE) %>%
    st_set_geometry(NULL)
  
  dat <- dat %>%
    mutate(ADA = as.numeric(ADA),
           Pasture = factor(Pasture, levels = fields$Pasture),
           season = factor(season, levels = c('Growing', 'Dormant', 'Total')),
           ndays_round = txtRound(tgd, digits = 1),
           ADA_round = txtRound(ADA, digits = 1)) %>%
    tidyr::complete(Pasture, season, fill = list(tgd = 0, 
                                                 ADA = 0, 
                                                 ndays_round = '0.0', 
                                                 ADA_round = '0.0')) %>%
    arrange(Pasture, season)
  
  dat_lab <- dat %>%
    select(-ndays_round, -ADA_round) %>%
    gather(tgd:ADA, key = 'var', value = 'value') %>%
    unite('var', var, season) %>%
    spread(key = 'var', value = 'value') %>%
    mutate(label_ada = map(dat %>% filter(season == 'Total') %>% pull(Pasture),
                           ~ dat %>% filter(Pasture == .x) %>% select(ADA_round) %>%
                             htmlTable(header = 'ADA',
                                       align = 'r',
                                       rnames = c('Growing (Feb 2024 - July 2024)',
                                                  'Dormant (July 2023 - Feb 2024)',
                                                  'Total (July 2023 - July 2024)'),
                                       total = T,
                                       caption = paste0('<b>Pasture ', .x, '</b>'))),
           label_days = map(dat %>% filter(season == 'Total') %>% pull(Pasture),
                            ~ dat %>% filter(Pasture == .x) %>% select(ndays_round) %>%
                              htmlTable(header = 'Days',
                                        align = 'r',
                                        rnames = c('Growing (Feb 2024 - July 2024)',
                                                   'Dormant (July 2023 - Feb 2024)',
                                                   'Total (July 2023 - July 2024)'),
                                        total = T,
                                        caption = paste0('<b>Pasture ', .x, '</b>'))))
  return(dat_lab)
}

# MAP 1----------------------
# animal days per acre

mgmt_ADA_plot<-function(dat){
  # SHAPEFILES SET UP------
  shp_poly <- st_read(here::here('GIS'), 'TK_veg_fields', quiet = TRUE) %>%
    st_transform('+proj=longlat +datum=WGS84') %>%
    full_join(dat, by = c('Pasture'))
  shp_ranch <- st_read(here::here('GIS'), 'TomKat_ranch_boundary', quiet = TRUE) %>%
    st_transform('+proj=longlat +datum=WGS84')
  
  # COLOR PALETTE-----------
  logo <- 'https://data.pointblue.org/apps/assets/images/pb-logo-full.png'
  
  pointblue.palette <-
    c('#4495d1',
      '#74b743',
      '#f7941d',
      '#005baa',
      '#bfd730',
      '#a7a9ac',
      '#666666')
  ## Define color palette for ADA range, grouped into bins:
  pal1 <- colorBin(palette = colorRamp(colors = c('#ffffff', pointblue.palette[4])),
                   domain = c(0, max(dat$ADA_Total)),
                   bins = c(0, 0.01, 25, 50, 100, 150),
                   na.color = pointblue.palette[6])
  #Create map
  map1 <- leaflet(shp_poly, height = 500) %>% 
    setView(lng = -122.3598, lat = 37.26693, zoom = 14) %>%
    
    ## background terrain
    addProviderTiles("Esri.WorldStreetMap",
                     options = providerTileOptions(minzoom = 14, maxzoom = 15)) %>%
    
    # ranch boundary
    addPolygons(data = shp_ranch, color = 'black', fill = F, weight = 3) %>%
    
    ## growing
    addPolygons(fillColor = ~ pal1(ADA_Growing),
                group = 'Growing (Feb 2024 - July 2024)',
                popup = ~ label_ada,
                color = 'black', fillOpacity = 1, weight = 1.5) %>%
    
    ## dormant
    addPolygons(fillColor = ~ pal1(ADA_Dormant),
                group = 'Dormant (July 2023 - Feb 2024)',
                popup = ~ label_ada,
                color = 'black', fillOpacity = 1, weight = 1.5) %>%
    
    
    ## total
    addPolygons(fillColor = ~ pal1(ADA_Total),
                group = 'Total (July 2023 - July 2024)',
                popup = ~ label_ada,
                color = 'black', fillOpacity = 1, weight = 1.5) %>%
    
    ## legend
    addLegend(position = 'topright',
              colors = pal1(c(0, 15, 30, 75, 125)),
              labels = c('0', '< 25', '25 - 50', '50 - 100', '> 100'),
              na.label = 'No data',
              opacity = 1, 
              title = 'Animal days<br>per acre') %>%
    
    ## toggles
    addLayersControl(baseGroups = c('Growing (Feb 2024 - July 2024)',
                                    'Dormant (July 2023 - Feb 2024)',
                                    'Total (July 2023 - July 2024)'),
                     options = layersControlOptions(collapsed = F),
                     position = 'bottomleft'
    ) %>%
    
    ## logo
    leafem::addLogo(img = logo, src = 'remote', url = 'https://www.pointblue.org',
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
  return(map1)
}


# MAP 2 --------------------------
# total days
mgmt_grazingdays_plot<-function(dat){
  shp_poly <- st_read(here::here('GIS'), 'TK_veg_fields', quiet = TRUE) %>%
    st_transform('+proj=longlat +datum=WGS84') %>%
    full_join(dat, by = c('Pasture'))
  shp_ranch <- st_read(here::here('GIS'), 'TomKat_ranch_boundary', quiet = TRUE) %>%
    st_transform('+proj=longlat +datum=WGS84')
  logo <- 'https://data.pointblue.org/apps/assets/images/pb-logo-full.png'
  pointblue.palette <-
  c('#4495d1',
    '#74b743',
    '#f7941d',
    '#005baa',
    '#bfd730',
    '#a7a9ac',
    '#666666')
  ## color palette for ndays range
  pal2 <- colorBin(palette = colorRamp(colors = c('#ffffff', pointblue.palette[2])),
                 domain = c(0, max(dat$tgd_Total)),
                 bins = c(0, 0.01, 25, 50, 100, Inf),
                 na.color = pointblue.palette[6])
  map2 <- leaflet(shp_poly, height = 500) %>% 
    setView(lng = -122.3598, lat = 37.26693, zoom = 14) %>%
    
    ## background terrain
    addProviderTiles("Esri.WorldStreetMap",
                     options = providerTileOptions(minzoom = 14, maxzoom = 15)) %>%
    
    # ranch boundary
    addPolygons(data = shp_ranch, color = 'black', fill = F, weight = 3) %>%
    
    ## growing
    addPolygons(fillColor = ~ pal2(tgd_Growing),
                group = 'Growing (Feb 2024 - July 2024)',
                popup = ~ label_days,
                color = 'black', fillOpacity = 1, weight = 1.5) %>%
    
    ## dormant
    addPolygons(fillColor = ~ pal2(tgd_Dormant),
                group = 'Dormant (July 2023 - Feb 2024)',
                popup = ~ label_days,
                color = 'black', fillOpacity = 1, weight = 1.5) %>%
    
    
    ## total
    addPolygons(fillColor = ~ pal2(tgd_Total),
                group = 'Total (July 2023 - July 2024)',
                popup = ~ label_days,
                color = 'black', fillOpacity = 1, weight = 1.5) %>%
    
    ## legend
    addLegend(position = 'topright',
              colors = pal2(c(0, 15, 30, 75, 125)),
              labels = c('0', '< 25', '25 - 50', '50 - 100', '> 100'),
              na.label = 'No data',
              opacity = 1, 
              title = 'Total<br>grazing days') %>%
    
    ## toggles
    addLayersControl(baseGroups = c('Growing (Feb 2024 - July 2024)',
                                    'Dormant (July 2023 - Feb 2024)',
                                    'Total (July 2023 - July 2024)'),
                     options = layersControlOptions(collapsed = F),
                     position = 'bottomleft'
    ) %>%
    
    ## logo
    leafem::addLogo(img = logo, src = 'remote', url = 'https://www.pointblue.org',
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
  return(map2)
}

