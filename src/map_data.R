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

# "dat" should have field "var"
map_data = function(dat, as_raster = FALSE, maplayers = 'single', htmltab = NULL, 
                    pts_toka, fields = NULL, boundary = NULL,
                    palette = NULL, bins = NULL, legend.labels = NULL, 
                    legend.title = NULL, 
                    pts_hocr = NULL) {
  
  # all field data are based on grid of point count stations:
  shp_pts_utm <- read_sf(pts_toka) %>% select(Point = Name)
  
  # convert to longlat version for mapping & join to data to be mapped:
  # (right join to drop any points not in data)
  shp_pts_longlat <- shp_pts_utm %>% st_transform(4326) %>% inner_join(dat)
  
  # join with code for pop-up htmltables (if using)
  if (!is.null(htmltab)) {
    shp_pts_longlat <- shp_pts_longlat %>% left_join(htmltab)
  }
  
  # color palette------
  if (is.null(palette)) {
    palette <- leaflet::colorBin(
      palette = c('#ffffff', '#005baa'), #white to point blue palette[4]
      domain = dat$value,
      bins = bins, #user defined
      na.color = 'transparent')
  }

  # start map base------------------
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
  
  # add rasters----------
  # if using (e.g. for showing bird density data as a raster), add any raster
  # version of data to map first, so it lies under polygons and points

  if (as_raster) {
    # first create generic raster template that matches the extent and
    # resolution of the point count grid, so that each point count station falls
    # in the center of each cell (200 x 200m)
    rtemplate <- raster::raster(ncols = 21,
                                nrows = 13,
                                resolution = 200,
                                xmn = st_bbox(shp_pts_utm)$xmin - 100,
                                xmx = st_bbox(shp_pts_utm)$xmax + 100,
                                ymn = st_bbox(shp_pts_utm)$ymin - 100,
                                ymx = st_bbox(shp_pts_utm)$ymax + 100,
                                crs = st_crs(shp_pts_utm)$proj4string) %>% 
      # convert to long-lat
      raster::projectRaster(crs = st_crs(shp_pts_longlat)$proj4string)
    
    # for each level of "group" in data, transfer richness values from point count
    # coordinates to corresponding raster cell:
    rast = shp_pts_longlat %>% select(group, Point, value) %>% split(.$group) %>% 
      purrr::map(function(x) {
        df = as(x, 'Spatial')
        raster::rasterize(x = df, y = rtemplate, field = x$value)
      })
    
    # add rasters to map
    for (i in c(1:length(rast))) {
      m <- m %>% 
        addRasterImage(x = rast[[i]], group = names(rast)[i],
                       colors = palette, opacity = 0.8, project = F)
    }
    
    # if more than one raster, also add layers control:
    if (length(rast) > 1) {
      m <- m %>% 
        addLayersControl(position = 'bottomleft', 
                         options = layersControlOptions(collapsed = F),
                         baseGroups = names(rast))
    }
    
    # add raster legend
    m <- m %>% 
      addLegend(position = 'topright', 
                colors = palette(bins[-length(bins)] + diff(bins)/2), #midpoints
                labels = legend.labels,
                values = NULL,
                opacity = 1,
                title = legend.title) 
  }

  # add boundaries-----------
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
  
  # add points & popups---------------

  if (!is.null(htmltab) & as_raster) {
    # if using rasters, use transparent circle markers to allow pop-up tables, 
    # one layer for each set of pop-ups to go with any layer in rast
    for (i in c(1:length(rast))) {
      m <- m %>% 
        addCircleMarkers(data = shp_pts_longlat,
                         popup = ~table_html,
                         group = names(rast)[i],
                         # make them large to cover most of the cell:
                         radius = 11, weight = 1.5, 
                         options = popupOptions(maxWidth = 800),
                         color = 'transparent',
                         fillColor = 'transparent')
    }
    
  } else if (!is.null(htmltab)) {
    for (i in c(1:length(shp_pts_longlat))) {
      # match to correct palette for maplayer
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
    
    # regardless of how many maplayers or types of data will be mapped together,
    # specify order of maplayers and split data by var (one set of points per data
    # layer):
    shp_pts_longlat <-  shp_pts_longlat %>% 
      mutate(maplayer = factor(maplayer, levels = maplayers)) %>% 
      split(.$var)
    
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
  
  # add HOCR points--------------
  # if using, repeat process to set up data (but won't ever be shown as raster)
  if (!is.null(pts_hocr)) {
    shp_pts_hocr <- read_sf(pts_hocr) %>% st_transform(4326) %>%
      select(Point = Name) %>% left_join(dat, by = 'Point')
    
    # add htmltables (if using)
    if (!is.null(htmltab)) {
      shp_pts_hocr <- shp_pts_hocr %>% left_join(htmltab, by = 'Point')
      
      m <- m %>% 
        addCircleMarkers(data = shp_pts_hocr, 
                         popup = ~table_html,
                         radius = 8, weight = 1.5, 
                         options = popupOptions(maxWidth = 800),
                         color = 'black',
                         fillColor = ~palette(value), 
                         fillOpacity = 0.8) 
    } else {
      m <- m %>% 
        addCircleMarkers(data = shp_pts_hocr, 
                         radius = 8, weight = 1.5, 
                         color = 'black',
                         fillColor = ~palette(value), 
                         fillOpacity = 0.8) 
    }
  }
  
  
  # add CSS-------------
  m$dependencies <- c(m$dependencies,
                      list(
                        htmltools::htmlDependency(
                          name = 'tomkat-leaflet',
                          version = '1.0.0',
                          src = here::here('Rmd'),
                          stylesheet = 'tk_leaflet.css')))
  
  return(m)
}
