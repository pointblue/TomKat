# READ ME--------
# Main script for analyzing TomKat Ranch data

# LOAD PACKAGES
source('src/packages.R')

# LOAD FUNCTIONS
source('src/plot_standards.R')
source('src/map_data.R')


# INDEX & README----------
# edit Rmd files as needed, then use these to render corresponding html

# "home" page for https://pointblue.github.io/TomKat/
rmarkdown::render(input = 'Rmd/index.Rmd',
                  output_file = here::here('docs/index.html'))

# readme for git repo https://github.com/pointblue/TomKat
rmarkdown::render(input = 'Rmd/README.Rmd',
                  output_file = here::here('README.md')) # store in top-level directory

# BIRDS-------------
source('src/process_bird_data.R')
source('src/plot_bird_data.R')
source('src/fit_distance_models.R')

## data set up--------
# bird data (update filepath to most recent one):
birddat = compile_bird_data('data_raw/TOKA_HOCR_PC_2010_2025.csv') %>%
  # add simplistic habitat classifications:
  left_join(read_csv('data_clean/sample_point_habitat.csv'), by = 'Point') %>% 
  mutate(habitat = if_else(is.na(habitat), 'other', habitat)) %>% 
  write_csv('data_clean/TOKA_birds_main.csv')
# Note: birddat does include all distances <300 and juveniles, but not flyovers

# check that number of species and surveys hasn't decreased: update these
# numbers with last year's totals!
birdtest1 = testthat::expect_gte(count_species(birddat), 95)
birdtest2 = testthat::expect_gte(count_surveys(birddat), 1289)


## 1. MAP avg density per point------------- 
# distance sampling to fit detection curves for each focal species (GRSP,
# SAVS, WCSP) across all TOKA points and all years (not including HOCR), then
# estimate average densities at each point (lumping all years together)

birddens_point = calculate_focal_density(
  dat = birddat %>% filter(Transect == 'TOKA'),
  strata = 'Point', dist = 'Distance Bin', dist_bin_id = NULL)
write_csv(birddens_point, 'data_clean/TOKA_birds_density_by_point.csv')
# -> 171 unique strata/sampling locations, 1-16 visits per sampling location

# add totals and reformat:
birddens_point_format = format_bird_density(birddens_point)

birddens_point_tables = create_html_tables(
  birddens_point_format, 
  set = 'birddens_point')

birddens_point_map = map_data(
  dat = birddens_point_format,
  as_raster = TRUE,
  pts_toka = 'GIS/TOKA_point_count_grid.shp',
  fields = 'GIS/TK_veg_fields.shp',
  boundary = 'GIS/TomKat_ranch_boundary.shp',
  bins = c(0, 0.001, 1, 5, 10, 50),
  legend.labels = c('0', '< 1', '1 - 5', '5 - 10', '> 10'),
  legend.title = 'Density<br>(birds/10 acres)',
  htmltab = birddens_point_tables
)

save_widget(birddens_point_map,
            pathout = 'docs/sandbox/bird_map_density.html',
            selfcontained = FALSE, libdir = 'lib',
            title = 'TomKat Bird Density Map')

save_widget(birddens_point_map,
            pathout = 'docs/widget/bird_map_density.html',
            selfcontained = FALSE, libdir = 'lib',
            title = 'TomKat Bird Density Map')

## 2. PLOT avg density per year & trend--------------
# distance sampling to fit detection curves for each focal species (GRSP,
# SAVS, WCSP) across all "grassland" TOKA points and all years (again not
# including HOCR), then estimate densities in each year (lumping all points
# together)

birddens_trend = calculate_focal_density(
  dat = birddat %>% filter(Transect == 'TOKA' & habitat == 'grassland'),
  strata = 'Transect', year = 'Year', 
  dist = 'Distance Bin', dist_bin_id = NULL)
write_csv(birddens_trend, 'data_clean/TOKA_birds_density_by_year.csv')
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
               ytitle = 'Density (birds/10 acres)')

save_widget(birddens_trend_plot,
            pathout = 'docs/sandbox/bird_trend_density.html',
            selfcontained = FALSE, libdir = 'lib',
            title = 'TomKat Bird Density Trends')

save_widget(birddens_trend_plot,
            pathout = 'docs/widget/bird_trend_density.html',
            selfcontained = FALSE, libdir = 'lib',
            title = 'TomKat Bird Density Trends')
# -> year x group model has no significant trends; but year + group model 
#   shows overall decline for both groups, with SAVS significantly lower

## 3. MAP spp richness per point------------

birdrich_point = birddat %>% 
  rename(id = Point) %>% 
  calculate_species_richness()
write_csv(birdrich_point, 'data_clean/TOKA_birds_richness_by_point.csv')

birdrich_point_format = format_bird_richness(birdrich_point)

# pop-up html tables for map:
birdrich_point_tables = create_html_tables(
  dat = birdrich_point_format,
  set = 'birdrich_point')

# map only estimated richness (but include observed richness and # surveys in
# pop-up tables)

birdrich_point_map = map_data(
  dat = birdrich_point_format %>% filter(var == 'estimated'),
  as_raster = TRUE,
  pts_toka = 'GIS/TOKA_point_count_grid.shp',
  pts_hocr = 'GIS/HOCR_point_count_riparian.shp',
  bins = c(0, 15, 25, 35, 45, 100),
  legend.labels = c('0 - 15', '16 - 25', '25 - 35', '35 - 45', '> 45'),
  legend.title = 'Estimated<br>species<br>richness',
  htmltab = birdrich_point_tables,
  fields = 'GIS/TK_veg_fields.shp',
  boundary = 'GIS/TomKat_ranch_boundary.shp')

save_widget(birdrich_point_map,
            pathout = 'docs/sandbox/bird_map_richness.html',
            title = 'TomKat Bird Richness Map',
            selfcontained = FALSE, libdir = 'lib')

save_widget(birdrich_point_map,
            pathout = 'docs/widget/bird_map_richness.html',
            title = 'TomKat Bird Richness Map',
            selfcontained = FALSE, libdir = 'lib')

## 4. PLOT spp richness per year-------
# compare grassland and riparian points

birdrich_trend = birddat %>% 
  filter(habitat != 'other') %>% 
  unite('id', habitat, Year, remove = FALSE) %>% 
  unite('Visit', Point, Visit, remove = FALSE) %>% 
  calculate_species_richness()
write_csv(birdrich_trend, 'data_clean/TOKA_birds_richness_by_year.csv')

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
               ytitle = 'Estimated number of species')

save_widget(birdrich_trend_plot,
            pathout = 'docs/sandbox/bird_trend_richness.html',
            selfcontained = FALSE, libdir = 'lib',
            title = 'TomKat Bird Richness Trends')

save_widget(birdrich_trend_plot,
            pathout = 'docs/widget/bird_trend_richness.html',
            selfcontained = FALSE, libdir = 'lib',
            title = 'TomKat Bird Richness Trends')
# -> year x group model shows declining trend for grassland birds, and 
#  different trend (slightly increasing) for riparian birds)

## update webpage----------
# first review and update text in "Rmd/birds.Rmd"

# once all of the plots and maps above are updated, use this to update the
# corresponding bird page
rmarkdown::render(input = 'Rmd/birds.Rmd',
                  output_file = here::here('docs/birds.html'))



# STREAM---------------
source('src/process_stream_data.R')
source('src/plot_stream_data.R')

# create CSV for latest WY of data (Excel file too large to store on Github)
streamdat_raw = readxl::read_excel(
  path = 'data_raw/stream/Pe01 Honsinger Creek streamflow WY2011 - WY2021.xlsx',
  sheet = 2, skip = 4) %>% 
  rename(date.time = 'date/time') %>% 
  mutate(year = format(date.time, '%Y') %>% as.numeric(),
         mo = format(date.time, '%m') %>% as.numeric(),
         WY = if_else(mo <= 9, year, year + 1))

# UPDATE CODE ANNUALLY: filter to latest water year of data, and change file name
streamdat_raw %>% filter(WY == 2021) %>% 
  select('date/time' = date.time, `Water Temp`:`Streamflow, ft3/s`) %>% 
  write_csv('data_raw/stream/WY2021.csv')

# compile all raw data
streamdat = compile_stream_data(dir = 'data_raw/stream')
write_csv(streamdat, 'data_clean/TOKA_stream_main.csv')

## 1. PLOT daily stats------
streamdat_daily = calculate_stream_daily(streamdat)
write_csv(streamdat_daily, 'data_clean/TOKA_stream_daily_stats.csv')

streamdat_daily_plot = streamdat_daily %>% 
  select(name, date, value = mean) %>% 
  plot_streamdat(colors = pointblue.palette[c(3,2,4)],
                 ytitle = c('Temperature (C)', 'Flow (cfs)', 'Depth (ft)'),
                 ymax = c(20, 100, 4),
                 ymin = c(0, 0, 0),
                 type = 'scatter', mode = 'lines',
                 selector = TRUE, slider = TRUE)

save_widget(streamdat_daily_plot, 
            pathout = 'docs/widget/stream_plot_daily.html',
            selfcontained = FALSE, libdir = 'lib',
            title = 'Honsinger Creek Daily Stats')

## 2. PLOT monthly stats---------
streamdat_monthly = calculate_stream_monthly(streamdat_daily) 
write_csv(streamdat_monthly, 'data_clean/TOKA_stream_monthly_stats.csv')

streamdat_monthly_plot = streamdat_monthly %>% 
  select(name, date, value = diff) %>% 
  plot_streamdat(colors = pointblue.palette[c(3,2,4)],
                 ytitle = c('Temperature (C)', 'Flow (cfs)', 'Depth (ft)'),
                 ymax = c(4, 5, 1),
                 ymin = c(-4, -5, -1),
                 type = 'bar', mode = NULL,
                 selector = FALSE, slider = FALSE)

save_widget(streamdat_monthly_plot,
            pathout = 'docs/widget/stream_plot_monthly.html',
            selfcontained = FALSE, libdir = 'lib',
            title = 'Honsinger Creek Monthly Stats')

## update webpage----------
# first review and update text in "Rmd/stream.Rmd"

# once the above plots are updated, use this to update the corresponding webpage
rmarkdown::render(input = 'Rmd/stream.Rmd',
                  output_file = here::here('docs/stream.html'))

# WEATHER-------------------- 
source('src/process_weather_data.R')
source('src/plot_weather_data.R')

## 1. PLOT daily stats---------
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
  compile_weather_old(path = 'data_raw/weather_station/TOKA_Weather_ALL_9.8.10_7.24.17.csv'), 
  compile_weather_west(dir = 'data_raw/weather_station'),
  compile_weather_new(path = './data_raw/weather_station/TOKA_weathernew_9.24.20_12.31.24.csv'))
write_csv(weatherdat, 'data_clean/TOKA_weather_main.csv')

# plot daily weather
weatherdat_daily_plot = plot_daily_weather(
  weatherdat, colors = pointblue.palette[c(3,2,4)],
  ytitle = c('Temperature (F)', 'Precipitation (in)'), 
  yrange = c(0, 100, 0, 8),
  selector = TRUE, slider = TRUE)

save_widget(weatherdat_daily_plot,
            pathout = 'docs/sandbox/weather_plot_daily.html',
            selfcontained = FALSE, libdir = 'lib',
            title = 'TomKat Daily Weather')

save_widget(weatherdat_daily_plot,
            pathout = 'docs/widget/weather_plot_daily.html',
            selfcontained = FALSE, libdir = 'lib',
            title = 'TomKat Daily Weather')

## 2. PLOT monthly stats----------
# show as differences from 1980-2010 "normals"

weatherdat_monthly = calculate_weather_monthly(weatherdat) 
write_csv(weatherdat_monthly, 'data_clean/TOKA_weather_monthly_stats.csv')

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
                   end.date = '2024-12-31'))

# estimate TomKat monthly differences from historical "normals" based on 
#  estimates derived from Half Moon Bay normals and the current monthly
#  differences between TomKat and Half Moon Bay weather
weatherdat_monthly_diffnorm = calculate_weather_diffs(
  localdat = weatherdat_monthly, 
  hmbdat = hmb_monthly,
  hmb_historic = 'data_clean/HMB_1981-2010_Normals.csv')

# plot monthly differences from normal
weatherdat_monthly_plot = plot_monthly_weather(
  weatherdat_monthly_diffnorm, 
  colors = pointblue.palette[c(3, 2, 4)],
  selector = TRUE)

save_widget(weatherdat_monthly_plot,
            pathout = 'docs/sandbox/weather_plot_monthly.html',
            selfcontained = FALSE, libdir = 'lib',
            title = 'TomKat Monthly Weather')

save_widget(weatherdat_monthly_plot,
            pathout = 'docs/widget/weather_plot_monthly.html',
            selfcontained = FALSE, libdir = 'lib',
            title = 'TomKat Monthly Weather')

## 3. PLOT drought indices------------
# get latest data from NOAA (requires API as described above)
pdsi = get_drought_indices(datname = 'pdsidv')
zndx = get_drought_indices(datname = 'zndxdv')

# plot PDSI
pdsi_plot = plot_drought_index(
  pdsi, valuebreaks = c(-20, -4, -3, -2, 2, 3, 4, 20),
  colors = c(pointblue.palette[3], '#ffffff', pointblue.palette[4]),
  yrange = c(-10, 8))

save_widget(pdsi_plot,
            pathout = 'docs/widget/weather_plot_pdsi.html',
            selfcontained = FALSE, libdir = 'lib',
            title = 'Palmer Drought Severity Index')

zndx_plot = plot_drought_index(
  zndx, valuebreaks = c(-20, -2.75, -2, -1.25, 1, 2.5, 3.5, 20),
  colors = c(pointblue.palette[3], '#ffffff', pointblue.palette[4]),
  yrange = c(-6, 8))

save_widget(zndx_plot,
            pathout = 'docs/widget/weather_plot_zndx.html',
            selfcontained = FALSE, libdir = 'lib',
            title = 'Palmer Z Index')

## update webpage----------
# first review and update text in "Rmd/weather.Rmd"

rmarkdown::render(input = 'Rmd/weather.Rmd',
                  output_file = here::here('docs/weather.html'))



# SOIL--------
source('src/process_soil_data.R')

## data set up---------
# Download data from CADC; save as CSV to avoid Excel auto-formatting water
# infiltration times 

# Note: these functions may need updating with additional sample years!

soildat = compile_soil_fielddata('data_raw/soil/TOKA_soildata_CADC_2014to24.csv') %>% 
  calculate_bulk_density() %>% 
  calculate_water_infiltration() %>% 
  summarize_soil_fielddata() %>% 
  left_join(compile_soil_labdata('data_raw/soil/TKR_SoilData_2015to2021_Lab.csv'),
            by = c('Point', 'SampleYear')) %>% 
  left_join(compile_soil_microbedata('data_raw/soil/Bacterial_Richness_For_SOTR.xlsx'),
            by = c('Point', 'SampleYear'))

write_csv(soildat, 'data_clean/TOKA_soil_main.csv')

# list of points sampled in the most recent year:
samplepts = soildat %>% filter(SampleYear == max(SampleYear)) %>% 
  pull(Point) %>% unique()


## 1. MAP soil productivity data-----

# assign percentile values to each metric at each point for all years of data:
soildat_productivity = soildat %>% 
  select(Point, SampleYear, bulk.dens.gcm3, water.infil, carbonA, carbonB) %>% 
  calculate_productivity_metrics() %>% 
  format_soil_productivity_metrics() %>% 
  # additional formatting for specific points/layers
  mutate(point_weight = if_else(Point %in% c('TOKA-022', 'TOKA-068') & 
                                  var != 'carbonA', 3, 1),
         table_caption = if_else(Point %in% c('TOKA-022', 'TOKA-068'), 
                                 ' (compost applied)', ''))

# create pop-up tables of data (most recent year only)
soildat_productivity_tables = create_html_tables(
  soildat_productivity %>% filter(SampleYear == max(soildat$SampleYear)), 
  set = 'soil_productivity')  

# generate color palettes for range of each metric (most recent year only)
soildat_productivity_palettes = create_palettes(
  soildat_productivity %>% filter(SampleYear == max(soildat$SampleYear)),
  set = 'soil_productivity')

# create map
soil_productivity_map = map_data(
  dat = soildat_productivity %>% filter(SampleYear == max(soildat$SampleYear)),
  pts_toka = 'GIS/TOKA_point_count_grid.shp',
  fields = 'GIS/TK_veg_fields.shp',
  boundary = 'GIS/TomKat_ranch_boundary.shp',
  palette = soildat_productivity_palettes,
  maplayers = names(soildat_productivity_palettes),
  multilegend = TRUE,
  htmltab = soildat_productivity_tables
)
# sandbox version for testing:
save_widget(soil_productivity_map,
            pathout = 'docs/sandbox/soil_map_productivity.html',
            selfcontained = FALSE, libdir = 'lib',
            title =  paste0('TomKat Soil Map ', max(soildat$SampleYear)))
# final version
save_widget(soil_productivity_map,
            pathout = 'docs/widget/soil_map_productivity.html',
            selfcontained = FALSE, libdir = 'lib',
            title =  paste0('TomKat Soil Map ', max(soildat$SampleYear)))


## 2. MAP change in soil productivity-----
# building on soildat_productivity_format from previous section

# calculate change in each metric and change in overall percentile score
soildat_productivity_change = calculate_productivity_change(
  df = soildat_productivity,
  current = max(soildat_productivity$SampleYear),
  baseline = 2015,
  difflabel = 'Difference<br>(2024-2015)') %>% 
  mutate(
    table_rowheader = maplayer,
    table_header = case_when(
      maplayer == 'Bulk density' ~ '(g/cm<sup>3</sup>)',
      maplayer == 'Water infiltration' ~ '(minutes)',
      maplayer == 'Overall score' ~ '(percentile)'))


# create pop-up html tables only for points sampled in most recent year
soildat_productivity_change_tables = create_html_tables(
  dat = soildat_productivity_change %>% filter(Point %in% samplepts), 
  set = 'soil_productivity_change')

soildat_productivity_change_palettes = create_palettes(
  soildat_productivity_change %>% filter(Point %in% samplepts), 
  set = 'soil_productivity_change')

soil_productivity_change_map = map_data(
  dat = soildat_productivity_change %>% filter(Point %in% samplepts),
  pts_toka = 'GIS/TOKA_point_count_grid.shp',
  fields = 'GIS/TK_veg_fields.shp',
  boundary = 'GIS/TomKat_ranch_boundary.shp',
  palette = soildat_productivity_change_palettes,
  maplayers = names(soildat_productivity_change_palettes),
  multilegend = FALSE, 
  legend.values = c(20, 0, -20),
  legend.labels = c('Improving', 'Little change', 'Declining'),
  legend.title = 'Direction of change',
  htmltab = soildat_productivity_change_tables
)
# sandbox version for testing
save_widget(soil_productivity_change_map,
            pathout = 'docs/sandbox/soil_map_productivity_change.html',
            selfcontained = FALSE, libdir = 'lib',
            title =  paste0('TomKat Soil Changes 2015-', max(soildat$SampleYear)))
# final version
save_widget(soil_productivity_change_map,
            pathout = 'docs/widget/soil_map_productivity_change.html',
            selfcontained = FALSE, libdir = 'lib',
            title =  paste0('TomKat Soil Changes 2015-', max(soildat$SampleYear)))


## 3. MAP soil nutrient concentrations------
# most recent year of data only (2015)

soildat_nutrients = soildat %>% 
  select(Point, SampleYear, ends_with(c('_A', '_B'))) %>%
  format_soil_nutrients() %>% 
  # additional formatting for specific points:
  mutate(point_weight = if_else(Point %in% c('TOKA-022', 'TOKA-068') & 
                                  depth != 'A', 3, 1),
         table_caption = if_else(Point %in% c('TOKA-022', 'TOKA-068'), 
                                 ' (compost applied)', ''))

# pop-up html tables for each variable and point; most recent year only
soildat_nutrient_tables = create_html_tables(
  soildat_nutrients %>% filter(SampleYear == max(SampleYear)),  
  set = 'soil_nutrients')

soildat_nutrient_palettes = create_palettes(
  soildat_nutrients %>% filter(SampleYear == max(SampleYear)), 
  set = 'soil_nutrients')

# create map:
soil_nutrient_map = map_data(
  dat = soildat_nutrients %>% filter(SampleYear == max(SampleYear)),
  pts_toka = 'GIS/TOKA_point_count_grid.shp',
  fields = 'GIS/TK_veg_fields.shp',
  boundary = 'GIS/TomKat_ranch_boundary.shp',
  maplayers = names(soildat_nutrient_palettes),
  palette = soildat_nutrient_palettes,
  multilegend = TRUE, 
  htmltab = soildat_nutrient_tables
)

# sandbox version for testing
save_widget(
  soil_nutrient_map,
  pathout = 'docs/sandbox/soil_map_nutrients.html',
  selfcontained = FALSE, libdir = 'lib',
  title =  paste0('TomKat Soil Nutrients ', max(soildat_nutrients$SampleYear)))
# final version
save_widget(
  soil_nutrient_map,
  pathout = 'docs/widget/soil_map_nutrients.html',
  selfcontained = FALSE, libdir = 'lib',
  title =  paste0('TomKat Soil Nutrients ', max(soildat_nutrients$SampleYear)))

## 4. MAP soil nutrient change----
soildat_nutrient_change = calculate_productivity_change(
  df = soildat_nutrients %>% filter(SampleYear >= 2018), #exclude 2015 data
  current = max(soildat_nutrients$SampleYear),
  baseline = 2018,
  difflabel = 'Difference') %>% 
  mutate(table_rowheader = if_else(table_header == 'pH', 'pH', 
                                   paste(gsub('<br>', ' ', table_rowheader),
                                         '<br>',
                                         gsub('<br>', ' ', table_header))))

# create pop-up html tables only for points sampled in most recent year
soildat_nutrient_change_tables = create_html_tables(
  dat = soildat_nutrient_change %>% filter(Point %in% samplepts), 
  set = 'soil_nutrient_change')

soildat_nutrient_change_palettes = create_palettes(
  soildat_nutrient_change %>% filter(Point %in% samplepts), 
  set = 'soil_nutrient_change')

soil_nutrient_change_map = map_data(
  dat = soildat_nutrient_change %>% filter(Point %in% samplepts & !is.na(maplayer)),
  pts_toka = 'GIS/TOKA_point_count_grid.shp',
  fields = 'GIS/TK_veg_fields.shp',
  boundary = 'GIS/TomKat_ranch_boundary.shp',
  palette = soildat_nutrient_change_palettes,
  maplayers = names(soildat_nutrient_change_palettes),
  multilegend = FALSE, 
  legend.values = c(0.5, 0, -0.5, NA),
  legend.labels = c('Increasing', 'No change', 'Decreasing', 'Missing data'),
  legend.title = 'Direction of change',
  htmltab = soildat_nutrient_change_tables
)
# sandbox version for testing
save_widget(soil_nutrient_change_map,
            pathout = 'docs/sandbox/soil_map_nutrient_change.html',
            selfcontained = FALSE, libdir = 'lib',
            title =  paste0('TomKat Soil Changes 2018-', max(soildat$SampleYear)))
# final version
save_widget(soil_nutrient_change_map,
            pathout = 'docs/widget/soil_map_nutrient_change.html',
            selfcontained = FALSE, libdir = 'lib',
            title =  paste0('TomKat Soil Changes 2018-', max(soildat$SampleYear)))

## 5. MAP soil microbes----

soildat_microbes = soildat %>% filter(SampleYear == 2015) %>% 
  select(Point, SampleYear, richA, richB) %>%
  filter(!is.na(richA)) %>% 
  pivot_longer(richA:richB, names_to = 'pointlayer') %>% 
  mutate(
    maplayer = 'default',
    # additional formatting for specific points/layers
    point_weight = if_else(Point %in% c('TOKA-022', 'TOKA-068') & 
                             pointlayer != 'richA', 3, 1),
    point_radius = if_else(pointlayer == 'richA', 3.5, 9))

# additional data on proportions by phylum (for pop-up graphs)
soildat_phyla = compile_phyladat('data_raw/soil/Bacterial_Phyla_For_SOTR.xlsx')

# create pop-up figures with phyladat
soildat_phyla_popplots = tibble(
  Point = unique(soildat_phyla$Point),
  table_html = create_pop_plots(soildat_phyla) %>% 
    leafpop::popupGraph(type = 'png', width = 400, height = 267))

# create color palette
soildat_microbe_palette = create_palettes(
  soildat_microbes, 
  set = 'soil_microbes')

# create map:
soil_microbe_map = map_data(
  dat = soildat_microbes,
  pts_toka = 'GIS/TOKA_point_count_grid.shp',
  fields = 'GIS/TK_veg_fields.shp',
  boundary = 'GIS/TomKat_ranch_boundary.shp',
  palette = soildat_microbe_palette, bins = 4,
  maplayers = names(soildat_microbe_palette),
  legend.title = 'Bacterial<br>richness',
  htmltab = soildat_phyla_popplots
)
# sandbox version for testing
save_widget(
  soil_microbe_map,
  pathout = 'docs/sandbox/soil_map_microbes.html',
  selfcontained = FALSE, libdir = 'lib',
  title =  paste0('TomKat Soil Microbes ', max(soildat_microbes$SampleYear)))
# final version
save_widget(
  soil_microbe_map,
  pathout = 'docs/widget/soil_map_microbes.html',
  selfcontained = FALSE, libdir = 'lib',
  title =  paste0('TomKat Soil Microbes ', max(soildat_microbes$SampleYear)))

## update webpage------
# first review and update text in "Rmd/soil.Rmd"
# sandbox version for testing
rmarkdown::render(input = 'Rmd/soil_sandbox.Rmd',
                  output_file = here::here('docs/soil_sandbox.html'))

rmarkdown::render(input = 'Rmd/soil.Rmd',
                  output_file = here::here('docs/soil.html'))


# VEGETATION-------
## data set up
## 1. MAP current veg cover
## 2. MAP vegetation change
## 3. GRAPH ranch-wide veg trends
## 4. GRAPH ranch-wide grass trends
## 5. MAP vegetation species diversity
## update webpage

# MANAGEMENT-------
## data set up
## 1. MAP animal days per acre
## 2. MAP total grazing days
## update webpage