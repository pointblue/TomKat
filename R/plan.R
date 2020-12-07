plan = drake_plan(
  # INDEX & README----------
  # "home" page for https://pointblue.github.io/TomKat/
  index_page = render_Rmd(file_in("Rmd/index.Rmd"), 
                          file_out("docs/index.html")),
  # readme for git repo https://github.com/pointblue/TomKat
  readme_page = render_Rmd(file_in("Rmd/README.Rmd"),
                           file_out("README.md")),
  
  # BIRDS-------------
  
  # "grassland" points:
  pcgrid = read_sf(file_in('GIS/TOKA_point_count_grid_grass&hay.shp')),
  
  birddat = compile_bird_data(file_in('data_raw/TOKA_HOCR_PC_2010_2020.csv')) %>%
    # add simplistic habitat classifications:
    mutate(habitat = case_when(substr(Point, 1, 4) == 'HOCR' ~ 'riparian',
                               Point %in% pcgrid$Name ~ 'grassland',
                               TRUE ~ 'other')) %>% 
    write_csv(file_out('data_clean/TOKA_birds_main.csv')),
  # Note: birddat does include all distances <300 and juveniles, but not flyovers
  
  # check that number of species and surveys hasn't decreased: update these
  # numbers with last year's totals!
  birdtest1 = testthat::expect_gte(count_species(birddat), 94),
  birdtest2 = testthat::expect_gte(count_surveys(birddat), 1205),
  
  # * 1: MAP avg density per point------------- 
  # distance sampling to fit detection curves for each focal species (GRSP,
  # SAVS, WCSP) across all TOKA points and all years (not including HOCR), then
  # estimate average densities at each point (lumping all years together)
  
  birddens_point = calculate_focal_density(
    dat = birddat %>% filter(Transect == 'TOKA'),
    strata = 'Point', dist = 'Distance Bin', dist_bin_id = NULL) %>%
    write_csv(file_out('data_clean/TOKA_birds_density_by_point.csv')),
  # -> 171 unique strata/sampling locations, 1-16 visits per sampling location
  
  # add totals and reformat:
  birddens_point_format = birddens_point %>% 
    filter(species != 'WCSP') %>% 
    # add totals
    bind_rows(birddens_point %>% filter(species != 'WCSP') %>% 
                group_by(Label) %>% 
                summarize(species = 'total', 
                          Estimate = sum(Estimate),
                          .groups = 'drop')) %>% 
    select(Point = Label, group = species, value = Estimate) %>%
    #optional: convert to birds per 10 acres (from birds/ha)
    mutate(value = value / 2.47105 * 10,
           group = factor(group, levels = c('GRSP', 'SAVS', 'total')),
           group = recode(group,
                          GRSP = 'Grasshopper Sparrow',
                          SAVS = 'Savannah Sparrow',
                          total = 'Total'),
           table_text = txtRound(value, digits = 1, txt.NA = 'NA')) %>% 
    arrange(Point, group),
  
  birddens_point_tables = birddens_point_format %>% 
    make_html_tables(table.total = TRUE,
                     table.header = NA),
  
  birddens_point_map = birddens_point_format %>% 
    map_data(pts_toka = file_in('GIS/TOKA_point_count_grid.shp'),
             fields = file_in('GIS/TK_veg_fields.shp'),
             boundary = file_in('GIS/TomKat_ranch_boundary.shp'),
             bins = c(0, 0.001, 1, 5, 10, 50),
             legend.labels = c('0', '< 1', '1 - 5', '5 - 10', '> 10'),
             legend.title = 'Density<br>(birds/10 acres)',
             htmltab = birddens_point_tables) %>% 
    save_widget(pathout = file_out('docs/widget/bird_map_density.html'),
                selfcontained = FALSE, libdir = 'lib',
                title = 'TomKat Bird Density Map'),
  
  # * 2: PLOT avg density per year & trend--------------
  # distance sampling to fit detection curves for each focal species (GRSP,
  # SAVS, WCSP) across all "grassland" TOKA points and all years (again not
  # including HOCR), then estimate densities in each year (lumping all points
  # together)

  birddens_trend = calculate_focal_density(
    dat = birddat %>% filter(Transect == 'TOKA' & habitat == 'grassland'),
    strata = 'Transect', year = 'Year', 
    dist = 'Distance Bin', dist_bin_id = NULL) %>%
    write_csv(file_out('data_clean/TOKA_birds_density_by_year.csv')),
  # -> 10 strata (one per year), 105 unique sampling locations, 1-2 visits each
  
  # plot trend
  birddens_trend_plot = birddens_trend %>%
    filter(species != 'WCSP') %>% 
    rename(group = species) %>% 
    separate(Label, c('Transect', 'year')) %>%
    #optional: convert to birds per 10 acres
    mutate_at(vars(Estimate:ucl), function(x) x / 2.47105 * 10) %>% 
    mutate(year = as.numeric(year),
           value = Estimate,
           minus = Estimate-lcl,
           plus = ucl-Estimate,
           label_text = paste0(round(Estimate, digits = 2), ' (',
                               round(lcl, digits = 2), '-',
                               round(ucl, digits = 2), ')'),
           group = recode(group,
                          GRSP = 'Grasshopper Sparrow',
                          SAVS = 'Savannah Sparrow')) %>%
    plotly_trend(colors = pointblue.palette[c(2, 3)],
                 yrange = c(0, 10),
                 ytitle = 'Density (birds/10 acres)') %>%
    save_widget(pathout = file_out('docs/widget/bird_trend_density.html'),
                selfcontained = FALSE, libdir = 'lib',
                title = 'TomKat Bird Density Trends'),
  # -> year x group model has no signiicant trends; but year + group model 
  #   shows overall decline for both groups, with SAVS significantly lower
  
  # * 3: MAP spp richness per point------------
  
  birdrich_point = birddat %>% 
    rename(id = Point) %>% 
    calculate_species_richness() %>% 
    write_csv(file_out('data_clean/TOKA_birds_richness_by_point.csv')),
  
  # pop-up html tables for map:
  birdrich_point_tables = birdrich_point %>% 
    select(Point = id, estimated = boot, observed = Species, n) %>%
    pivot_longer(-Point, names_to = 'group', values_to = 'value') %>% 
    mutate(group = factor(group, levels = c('estimated', 'observed', 'n')),
           group = recode(group,
                          estimated = 'Estimated species',
                          observed = 'Observed species',
                          n = 'Number of surveys'),
           table_text = txtRound(value, digits = 0, txt.NA = 'NA')) %>% 
    make_html_tables(table.total = FALSE,
                     table.header = NA),
  
  # map only estimated richness (but include observed richness and # surveys in
  # pop-up tables)
  birdrich_point_map = birdrich_point %>% 
    select(Point = id, value = boot) %>% 
    mutate(group = 'estimated') %>% #need to specify at least one group
    map_data(pts_toka = file_in('GIS/TOKA_point_count_grid.shp'),
             pts_hocr = file_in('GIS/HOCR_point_count_riparian.shp'),
             bins = c(0, 15, 25, 35, 45, 100),
             legend.labels = c('0 - 15', '16 - 25', '25 - 35', '35 - 45', '> 45'),
             legend.title = 'Estimated<br>species<br>richness',
             htmltab = birdrich_point_tables,
             fields = file_in('GIS/TK_veg_fields.shp'),
             boundary = file_in('GIS/TomKat_ranch_boundary.shp')) %>% 
    save_widget(pathout = file_out('docs/widget/bird_map_richness.html'),
                title = 'TomKat Bird Richness Map',
                selfcontained = FALSE, libdir = 'lib'),
  
  # * 4: PLOT spp richness per year-------
  # compare grassland and riparian points

  birdrich_trend = birddat %>% 
    filter(habitat != 'other') %>% 
    unite('id', habitat, Year, remove = FALSE) %>% 
    unite('Visit', Point, Visit, remove = FALSE) %>% 
    calculate_species_richness() %>% 
    write_csv(file_out('data_clean/TOKA_birds_richness_by_year.csv')),
  
  # plot trend
  birdrich_trend_plot = birdrich_trend %>% 
    separate(id, c('group', 'year')) %>%
    mutate(year = as.numeric(year),
           value = boot,
           minus = boot.se,
           plus = boot.se,
           label_text = paste0(round(value, digits = 0), ' (', 
                               round(boot - boot.se, digits = 0), '-', 
                               round(boot + boot.se, digits = 0), ')')) %>% 
    plotly_trend(colors = pointblue.palette[c(2, 4)],
                 yrange = c(0, 75),
                 ytitle = 'Estimated number of species') %>% 
    save_widget(pathout = file_out('docs/widget/bird_trend_richness.html'),
                selfcontained = FALSE, libdir = 'lib',
                title = 'TomKat Bird Richness Trends'),
  # -> year x group model shows declining trend for grassland birds, and 
  #  different trend (slightly increasing) for riparian birds)

  # * WEBPAGE----------
  # report = knit(knitr_in("report.Rmd"), file_out("report.md"), quiet = TRUE)
  bird_page = render_Rmd(file_in("Rmd/birds.Rmd"), 
                         file_out("docs/birds.html")),
  
  # STREAM---------------
  
  streamdat = compile_stream_data(
    path = file_in('data_raw/Pe01 Honsinger Creek streamflow WY2011 - WY2020.xlsx')) %>% 
    write_csv(file_out('data_clean/TOKA_stream_main.csv')),
  
  # * daily------
  streamdat_daily = calculate_stream_daily(streamdat) %>% 
    write_csv(file_out('data_clean/TOKA_stream_daily_stats.csv')),
  
  streamdat_daily_plot = streamdat_daily %>% 
    select(name, date, value = mean) %>% 
    plot_streamdat(colors = pointblue.palette[c(3,2,4)],
                   ytitle = c('Temperature (C)', 'Flow (cfs)', 'Depth (ft)'),
                   ymax = c(20, 100, 4),
                   ymin = c(0, 0, 0),
                   type = 'scatter', mode = 'lines',
                   selector = TRUE, slider = TRUE) %>%
    save_widget(pathout = file_out('docs/widget/stream_plot_daily.html'),
                selfcontained = FALSE, libdir = 'lib',
                title = 'Honsinger Creek Daily Stats'),

  # * monthly---------
  streamdat_monthly = calculate_stream_monthly(streamdat_daily) %>% 
    write_csv(file_out('data_clean/TOKA_stream_monthly_stats.csv')),
  
  streamdat_monthly_plot = streamdat_monthly %>% 
    select(name, date, value = diff) %>% 
    plot_streamdat(colors = pointblue.palette[c(3,2,4)],
                   ytitle = c('Temperature (C)', 'Flow (cfs)', 'Depth (ft)'),
                   ymax = c(4, 5, 1),
                   ymin = c(-4, -5, -1),
                   type = 'bar', mode = NULL,
                   selector = FALSE, slider = FALSE) %>%
    save_widget(pathout = file_out('docs/widget/stream_plot_monthly.html'),
                selfcontained = FALSE, libdir = 'lib',
                title = 'Honsinger Creek Monthly Stats'),
  
  # * WEBPAGE----------
  # report = knit(knitr_in("report.Rmd"), file_out("report.md"), quiet = TRUE)
  stream_page = render_Rmd(file_in("Rmd/stream.Rmd"), 
                         file_out("docs/stream.html")),
  
  # WEATHER-------------------- 
  # * Daily---------
  # NOTE: orig Weather West file (Oct 17 through Dec 18) has an extra column
  # (Offset) and the way degrees was included in column names was problematic;
  # check subsequent files for consistent headings and order of columns. Also
  # check date format. Orig file uses 4-digit year, latest file (through Sept
  # 2020) uses 2-digit year. Code is currently set up to handle these two files
  # correctly, but may need changes with future iterations.

  # NOTE 2: Weather West data is listed in reverse order, with most recent
  # readings at the top of the file
  
  #extract daily rain totals and max/min temps:
  weatherdat = bind_rows(
    compile_weather_old(path = file_in('data_raw/weather_station/TOKA_Weather_ALL_9.8.10_7.24.17.csv')),
    compile_weather_west(dir = file_in('data_raw/weather_station'))) %>% 
    write_csv(file_out('data_clean/TOKA_weather_main.csv')),
  
  # plot daily weather
  weatherdat_daily_plot = plot_daily_weather(
    weatherdat, colors = pointblue.palette[c(3,2,4)],
    ytitle = c('Temperature (F)', 'Precipitation (in)'), 
    yrange = c(0, 100, 0, 8),
    selector = TRUE, slider = TRUE) %>%
    save_widget(pathout = file_out('docs/widget/weather_plot_daily.html'),
                selfcontained = FALSE, libdir = 'lib',
                title = 'TomKat Daily Weather'),
  
  # * Monthly----------
  # show as differences from 1980-2010 "normals"
  
  weatherdat_monthly = calculate_weather_monthly(weatherdat) %>% 
    write_csv(file_out('data_clean/TOKA_weather_monthly_stats.csv')),
  
  # GET HALF MOON BAY DATA FROM NOAA
  # set up API:
  # --> If not done already, get API key from NOAA (by submitting email address):
  #       https://www.ncdc.noaa.gov/cdo-web/token
  # The system will send an email address with a "token". Save token by:
  #  1. add to .Renviron file in your home directory as: NOAA_KEY = 'token'
  #  2. add to your .Rprofile file as: noaakey = 'token'
  #  3. quickest option (but not permanent) is by running from the command line in
  # R: options(noaakey = 'token')
  # (replace 'token' with the character string NOAA sends you)
  
  # 10 year max per request: (add additional requests as needed)
  hmb_monthly = bind_rows(
    get_ncdc_weather(start.date = '2010-10-01',
                     end.date = '2020-09-15'),
    get_ncdc_weather(start.date = '2020-10-01',
                     end.date = '2020-11-25')),
  
  # estimate TomKat monthly differences from historical "normals" based on 
  #  estimates derived from Half Moon Bay normals and the current monthly
  #  differences between TomKat and Half Moon Bay weather
  weatherdat_monthly_diffnorm = calculate_weather_diffs(
    localdat = weatherdat_monthly, 
    hmbdat = hmb_monthly,
    hmb_historic = file_in('data_clean/HMB_1981-2010_Normals.csv')),
  
  # plot monthly differences from normal
  weatherdat_monthly_plot = plot_monthly_weather(
    weatherdat_monthly_diffnorm, 
    colors = pointblue.palette[c(3, 2, 4)],
    selector = TRUE) %>%
    save_widget(pathout = file_out('docs/widget/weather_plot_monthly.html'),
                selfcontained = FALSE, libdir = 'lib',
                title = 'TomKat Monthly Weather'),

  # * Drought indices------------
  # get data from NOAA (requires API as described above)
  pdsi = get_drought_indices(datname = 'pdsidv'),
  zndx = get_drought_indices(datname = 'zndxdv'),
  
  # plot PDSI
  pdsi_plot = plot_drought_index(
    pdsi, valuebreaks = c(-20, -4, -3, -2, 2, 3, 4, 20),
    colors = c(pointblue.palette[3], '#ffffff', pointblue.palette[4]),
    yrange = c(-10, 8)) %>%
    save_widget(pathout = file_out('docs/widget/weather_plot_pdsi.html'),
                selfcontained = FALSE, libdir = 'lib',
                title = 'Palmer Drought Severity Index'),
  
  zndx_plot = plot_drought_index(
    zndx, valuebreaks = c(-20, -2.75, -2, -1.25, 1, 2.5, 3.5, 20),
    colors = c(pointblue.palette[3], '#ffffff', pointblue.palette[4]),
    yrange = c(-6, 8)) %>%
    save_widget(pathout = file_out('docs/widget/weather_plot_zndx.html'),
                selfcontained = FALSE, libdir = 'lib',
                title = 'Palmer Z Index'),
  
  # * WEBPAGE----------
  weather_page = render_Rmd(file_in("Rmd/weather.Rmd"), 
                            file_out("docs/weather.html"))

)