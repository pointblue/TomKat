##' @title map_data
##' @description functions for generating leaflet maps for TomKat data, with
##'   supporting function for making pop-up html tables
##'
##' @export
##' @author Kristen Dybala
##'
##'   

make_html_tables = function(dat, table.total = FALSE, table.header = NA,
                            caption = '') {
  dat %>% select(Point) %>% distinct() %>% 
    mutate(
      table_html = purrr::map(
        Point, 
        function(x) {
          sdat = dat %>% filter(Point == x) %>% 
            column_to_rownames('group')
          htmlTable(x = sdat %>% select(-Point),
                    header = table.header, 
                    align = 'r',
                    total = table.total,
                    caption = paste0('<b>', x, '</b>', caption))
        }
      ))
}

map_data = function(dat, maplayers = 'single', htmltab = NULL, 
                    pts_toka, as_raster = FALSE, 
                    bins = NULL, palette = NULL,
                    legend.title = NULL, legend.labels = NULL, 
                    pts_hocr = NULL, fields = NULL, boundary = NULL) {
  
  # turn pc points into raster grid:
  shp_pts_utm <- read_sf(pts_toka) %>% select(Name) %>% 
    filter(Name %in% dat$Point) #(drop points not surveyed)
  
  # Note: leave orig version in UTM, to allow setting raster grid size in meters,
  # but aso create longlat version for mapping, joined to data:
  shp_pts_longlat <- shp_pts_utm %>% 
    st_transform(4326) %>% 
    left_join(dat, by = c('Name' = 'Point'))
  
  # add htmltables (if using)
  if (!is.null(htmltab)) {
    shp_pts_longlat <- shp_pts_longlat %>% 
      inner_join(htmltab, by = c('Name' = 'Point', 'maplayer')) %>% 
      mutate(maplayer = factor(maplayer, levels = maplayers)) %>% 
      split(.$var)
  }

  # repeat for pts_hocr (if using)
  if (!is.null(pts_hocr)) {
    shp_pts_hocr <- read_sf(pts_hocr) %>% 
      st_transform(4326) %>%
      left_join(dat, by = c('Name' = 'Point'))
    
    # add htmltables (if using)
    if (!is.null(htmltab)) {
      shp_pts_hocr <- shp_pts_hocr %>% 
        inner_join(htmltab, by = c('Name' = 'Point'))
    }
  }
  
  
  # start map base:------------------
  m <- leaflet(shp_pts_longlat[[1]], height = 500) %>% 
    setView(lng = -122.3598,
            lat = 37.26693,
            zoom = 14) %>% 
    # background terrain
    addProviderTiles("Esri.WorldStreetMap",
                     options = providerTileOptions(minzoom = 14, maxzoom = 15)) %>% 
    # add logo
    leafem::addLogo(img = logo, src = 'remote', 
                    url = 'https://www.pointblue.org',
                    width = 174, height = 90, offset.y = -5)
  
  
  
  # ADD RASTERS:-----------------------
  # create generic raster template that matches the extent and resolution,
  # placing the point count grid in center of each cell (200 x 200m), then
  # convert to longlat
  if (as_raster) {
    rtemplate <- raster::raster(ncols = 21,
                                nrows = 13,
                                resolution = 200,
                                xmn = st_bbox(shp_pts_utm)$xmin - 100,
                                xmx = st_bbox(shp_pts_utm)$xmax + 100,
                                ymn = st_bbox(shp_pts_utm)$ymin - 100,
                                ymx = st_bbox(shp_pts_utm)$ymax + 100,
                                crs = st_crs(shp_pts_utm)$proj4string) %>% 
      raster::projectRaster(crs = crs(shp_pts_longlat))
    
    # for each level of "group" in data, transfer richness values from point count
    # coordinates to corresponding raster cell:
    rast = shp_pts_longlat %>% select(group, Name, value) %>% split(.$group) %>% 
      purrr::map(function(x) {
        df = as(x, 'Spatial')
        raster::rasterize(x = df, y = rtemplate, field = x$value)
      })
    
    # color palette:
    pal.blues <- leaflet::colorBin(
      palette = c('#ffffff', '#005baa'), #white to point blue palette[4]
      domain = dat$value, 
      bins = bins, #user defined
      na.color = 'transparent')
    
    # add rasters 
    for (i in c(1:length(rast))) {
      m <- m %>% 
        addRasterImage(x = rast[[i]], group = names(rast)[i],
                       colors = pal.blues, opacity = 0.8, project = F)
    }
    
    # if more than one raster, add layers control:
    if (length(rast) > 1) {
      m <- m %>% 
        addLayersControl(position = 'bottomleft', 
                         options = layersControlOptions(collapsed = F),
                         baseGroups = names(rast))
    }
    
    # add Legend
    m <- m %>% 
      addLegend(position = 'topright', 
                colors = pal.blues(bins[-length(bins)] + diff(bins)/2), #midpoints
                labels = legend.labels,
                values = NULL,
                opacity = 1,
                title = legend.title) 
  }

  # ADD OTHER POLYGONS: (if included)----------------
  ## pasture boundaries
  if (!is.null(fields)) {
    shp_poly <- read_sf(fields) %>% st_transform(4326)
    m <- m %>% 
      addPolygons(data = shp_poly, fillColor = '#74b743', #point blue green
                  color = 'black', weight = 1, fillOpacity = 0.1)
  }
  ## ranch boundary
  if (!is.null(boundary)) {
    shp_ranch <- read_sf(boundary) %>% st_transform(4326)
    m <- m %>% 
      addPolygons(data = shp_ranch, fill = FALSE,
                  color = 'black', weight = 3)
  }
  
  # ADD POINTS & POPUP TABLES (if using)---------------
  # ** Note: add AFTER pasture and ranch boundary polygons so they aren't buried
  
  if (!is.null(htmltab) & as_raster) {
    # if using rasters, use transparent circle markers, one for each set of
    # pop-ups to go with each layer in rast
    for (i in c(1:length(rast))) {
      m <- m %>% 
        addCircleMarkers(popup = ~ table_html,
                         group = names(rast),
                         radius = 11, weight = 1.5, 
                         options = popupOptions(maxWidth = 800),
                         color = 'transparent',
                         fillColor = 'transparent')
    }
  } else if (!is.null(htmltab)) {
    for (i in c(1:length(shp_pts_longlat))) {
      p = which(names(palette) == shp_pts_longlat[[i]]$maplayer[1])
      m <- m %>% 
        addCircleMarkers(data = shp_pts_longlat[[i]],
                         popup = ~table_html,
                         group = ~maplayer,
                         radius = ~radius, 
                         color = 'black', opacity = 1,
                         fillColor =  ~palette[[p]](Value), 
                         fillOpacity = 1, 
                         weight = ~weight,
                         options = popupOptions(maxWidth = 800))
    }
  }
    
  # if more than one maplayers, add layers control and legends to match
  if (length(maplayers) > 1) {
    for (i in c(1:length(maplayers))) {
      m <- m %>% 
        addLegend(pal = palettes[[i]],
                  values = dat %>% filter(maplayer == maplayers[[i]]) %>% 
                    pull(Value),
                  title = maplayers[i],
                  group = maplayers[i],
                  position = 'topright',
                  opacity = 1,
                  na.label = 'No data') 
    }
    m <- m %>% 
      addLayersControl(position = 'bottomleft', 
                       options = layersControlOptions(collapsed = F),
                       overlayGroups = maplayers) %>% 
      hideGroup(maplayers[2:length(maplayers)])
  }
  
  # ADD HOCR points (if using)--------------
  if (!is.null(pts_hocr)) {
    if (!is.null(htmltab)) {
      # include popup table
      m <- m %>% 
        addCircleMarkers(data = shp_pts_hocr, popup = ~ table_html,
                         radius = 8, weight = 1.5, 
                         options = popupOptions(maxWidth = 800),
                         color = 'black',
                         fillColor = ~pal.blues(value), 
                         fillOpacity = 0.8) 
    } else {
      m <- m %>% 
        addCircleMarkers(data = shp_pts_hocr, 
                         radius = 8, weight = 1.5, 
                         color = 'black',
                         fillColor = ~pal.blues(value), 
                         fillOpacity = 0.8) 
    }
  }
  
  # MAP FORMAT-------------
  # add CSS
  m$dependencies <- c(m$dependencies,
                      list(
                        htmltools::htmlDependency(
                          name = 'tomkat-leaflet',
                          version = '1.0.0',
                          src = here::here('Rmd'),
                          stylesheet = 'tk_leaflet.css')))
  
  return(m)
}
