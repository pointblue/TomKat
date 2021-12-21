##' @title map_data
##' @description functions for generating leaflet maps for TomKat data, with
##'   supporting function for making pop-up html tables
##'
##' @export
##' @author Kristen Dybala
##'
##'   

create_html_tables = function(dat, set) {
  if (set == 'birdrich_point') {
    return(
      dat %>% 
        select(Point = id, estimated = boot, observed = Species, n) %>%
        pivot_longer(-Point, names_to = 'rowname', values_to = 'value') %>% 
        mutate(rowname = factor(rowname, levels = c('estimated', 'observed', 'n')),
               rowname = recode(rowname,
                                estimated = 'Estimated species',
                                observed = 'Observed species',
                                n = 'Number of surveys'),
               value_round = txtRound(value, digits = 0, txt.NA = 'NA')) %>% 
        select(Point, rowname, value_round) %>% 
        make_html_tables(table.total = FALSE,
                         table.header = NA))
  }
  if (set == 'soil_productivity') {
    return(
      bind_rows(
        # one set of tables for overall score
        dat %>% 
          select(maplayer, Point, table_rowname, value = value_round, 
                 percentile, table_caption) %>% 
          make_html_tables(table.total = TRUE) %>% 
          mutate(maplayer = 'Overall score'),
        # and one set of tables for each of the individual factors
        dat %>% filter(maplayer != 'Overall score') %>% 
          select(maplayer, Point, table_rowname, value = value_round, 
                 percentile, table_caption) %>% 
          split(.$maplayer, drop = TRUE) %>% 
          purrr::map_df(make_html_tables, table.total = FALSE, .id = 'maplayer') 
          )
      )
  }
  if (set == 'soil_productivity_change') { 
    return(
      bind_rows(
        # one set of tables for carbon (two columns)
        dat %>% filter(maplayer == '% Carbon') %>% 
          select(maplayer, Point, table_rowheader, table_rowname, 
                 var, value = value_round, table_caption) %>% 
          pivot_wider(names_from = var) %>% 
          rename('0-10cm' = carbonA, '10-40cm' = carbonB) %>% 
          make_html_tables(table.total = TRUE) %>% 
          mutate(maplayer = '% Carbon'),
        # and one set of tables for everything else
        dat %>% filter(maplayer != '% Carbon') %>% 
          select(maplayer, Point, table_rowheader, table_header, table_rowname, 
                 value = value_round, table_caption) %>% 
          split(.$maplayer) %>% 
          purrr::map_df(make_html_tables, table.total = TRUE, .id = 'maplayer')
      )
    )
  } 
  if (set == 'soil_nutrients') {
    return(
      dat %>% 
        select(maplayer, Point, table_rowheader, table_header, table_rowname, value_round, table_caption) %>% 
        split(.$maplayer) %>% 
        purrr::map_df(make_html_tables, table.total = FALSE, .id = 'maplayer')
    )
  }
  if (set == 'soil_nutrient_change') {
    return(
      dat %>% 
        select(maplayer, Point, table_rowheader, table_rowname, 
               depth, value = value_round, table_caption) %>% 
        pivot_wider(names_from = depth) %>% 
        rename('0-10cm' = A, '10-40cm' = B) %>%
        split(.$maplayer) %>% 
        purrr::map_df(make_html_tables, table.total = TRUE, .id = 'maplayer')
    )

  }
}

# internal function (iterates over all Points)
make_html_tables = function(dat, table.total = FALSE, table.header = NULL,
                            row.label = NULL, caption = '') {
  dat %>% select(Point) %>% distinct() %>% 
    mutate(
      table_html = purrr::map(
        Point, 
        function(x) {
          sdat = dat %>% filter(Point == x) %>% 
            column_to_rownames('table_rowname')
          htmlTable(x = sdat %>% select(!(c(maplayer, Point, starts_with('table')))),
                    header = sdat$table_header[1], 
                    rowlabel = sdat$table_rowheader[1],
                    align = 'r',
                    total = table.total,
                    caption = paste0('<b>', x, '</b>', sdat$table_caption[1]))
        }
      ))
}

create_pop_plots = function(df) {
  map(unique(df$Point) %>% rlang::set_names(), 
      ~ df %>% filter(Point == .x) %>%
        ggplot(aes(x = Depth, y = prop, fill = phylum)) +
        geom_col(color = 'gray50', size = 0.4) + 
        coord_polar(start = 0, theta = 'y') + 
        blank_theme + 
        ggtitle(.x) + 
        scale_y_continuous(expand = c(0, 0)) +
        scale_fill_viridis_d(option = 'inferno'))
}

blank_theme <- ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(), 
    axis.title.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(), 
    axis.text.y = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(), 
    panel.grid = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(), legend.position = 'right',
    legend.key.size = ggplot2::unit(0.75, 'lines'),
    plot.margin = ggplot2::unit(c(0.25, 0.25, 0.25, 0.25),'pt'),
    plot.background = ggplot2::element_rect(fill = 'white', color = NA),
    panel.background = ggplot2::element_rect(fill = 'white', color = NA)
  )

map_data = function(dat, as_raster = FALSE, maplayers = NULL, htmltab = NULL, 
                    pts_toka, fields = NULL, boundary = NULL,
                    palette = NULL, bins = NULL, legend.values, legend.labels = NULL, 
                    legend.title = NULL, multilegend = FALSE,
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
    
    # for each maplayer in data, transfer richness values from point count
    # coordinates to corresponding raster cell:
    rast = shp_pts_longlat %>% select(maplayer, Point, value) %>% split(.$maplayer) %>% 
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
    # define point layer order by combination of maplayer and descending point
    # radius, then split data by point layer:
    if (length(maplayers) == 1) {
      # default
      shp_pts_longlat <-  shp_pts_longlat %>% arrange(desc(point_radius), Point)
    } else {
      shp_pts_longlat <-  shp_pts_longlat %>% 
        mutate(maplayer = factor(maplayer, levels = maplayers)) %>%
        arrange(maplayer, desc(point_radius), Point)
    }

    shp_pts_longlat <- shp_pts_longlat %>% 
      mutate(pointlayer = factor(pointlayer, levels = unique(shp_pts_longlat$pointlayer))) %>% 
      split(.$pointlayer)
    
    for (i in c(1:length(shp_pts_longlat))) {
      # match to correct palette by maplayer
      p = which(names(palette) == shp_pts_longlat[[i]]$maplayer[1])
      m <- m %>% 
        addCircleMarkers(data = shp_pts_longlat[[i]],
                         popup = ~table_html,
                         group = ~maplayer,
                         radius = ~point_radius, 
                         color = 'black', opacity = 1,
                         fillColor =  ~palette[[p]](value), 
                         fillOpacity = 1, 
                         weight = ~point_weight,
                         options = popupOptions(maxWidth = 800))
    }
  }
    
  # if more than one maplayers, add layers control and legends to match
  if (!is.null(maplayers) & length(maplayers) > 1) {
    
    if (multilegend) {
      # one legend for each maplayer
      for (i in c(1:length(maplayers))) {
        m <- m %>% 
          addLegend(pal = palette[[i]],
                    values = dat %>% filter(maplayer == maplayers[[i]]) %>% 
                      pull(value),
                    title = maplayers[i],
                    group = maplayers[i],
                    position = 'topright',
                    opacity = 1,
                    na.label = 'No data') 
      }
    } else {
      # one legend for all maplayers
      m <- m %>% 
        addLegend(colors = palette[[1]](legend.values),
                  labels = legend.labels,
                  title = legend.title,
                  position = 'topright',
                  opacity = 1,
                  na.label = 'No data') 
    }
    # add legend last (so hideGroup applies to legends as well)
    m <- m %>% 
      addLayersControl(position = 'bottomleft', 
                       options = layersControlOptions(collapsed = F),
                       overlayGroups = maplayers) %>% 
      hideGroup(maplayers[2:length(maplayers)])
  } else if (!is.null(maplayers)){
    # one legend only
    m <- m %>% 
      addLegend(pal = palette[[1]],
                values = dat %>% pull(value),
                bins = bins,
                title = legend.title,
                position = 'topright',
                opacity = 1,
                na.label = 'No data')
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
