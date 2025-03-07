##' @title process_weather_data
##' @description functions for cleaning, analyzing, and summarizing TomKat
##'   weather data (CSV format); new data files expected to start with
##'   "TOKA_weatherwest_"; old format data expected to be
##'   "TOKA_Weather_ALL_9.8.10_7.24.17"
##' @param dir filepath to directory containing new Weather West station data
##' @param path filepath to old weather station data
##' @param dat tibble or data.frame containing compiled point count data
##'   (expects fields Spp, Point, Visit, Date, Year)
##'
##' @export
##' @author Kristen Dybala
##'
##'   

compile_weather_new = function(path) {
  #Daily weather (tempmax, tempmin, and precip) data downloaded from https://www.westernweathergroup.com.
  # Does not need to be aggregated by day
  read_csv(path,  
           col_names = c('Date', 'DailyMaxTemp', 'DailyMinTemp', 'DailyRain'),skip=1) %>%
    mutate(Date = as.Date(Date, '%m/%d/%y'))
}

compile_weather_west = function(dir) {
  filelist <- list.files(path = dir, 
                         pattern = 'TOKA_weatherwest_.*csv', 
                         full.names = TRUE)
  
  bind_rows(
    read_csv(filelist[1], skip = 1,
             col_names = c('Date', 'Time', 'Offset', 'MinBat', 'DewPt', 
                           'LatestRain', 'DailyRain', 'Pressure', 'RH', 'Temp', 
                           'DailyMaxTemp', 'MaxTempTime', 'DailyMinTemp', 
                           'MinTempTime', 'WindDir', 'WindGust', 
                           'WindGustDir', 'WindSpd'),
             col_types = cols_only(Date = 'c',
                              Time = 'c',
                              DailyMaxTemp = 'd',
                              DailyMinTemp = 'd',
                              DailyRain = 'd')) %>% 
      mutate(Date = as.Date(Date, format = '%m/%d/%Y')),
    read_csv(filelist[2], skip = 1,
             col_names = c('Date', 'Time', 'MinBat', 'DewPt', 
                           'LatestRain', 'DailyRain', 'Pressure', 'RH', 'Temp', 
                           'DailyMaxTemp', 'MaxTempTime', 'DailyMinTemp', 
                           'MinTempTime', 'WindDir', 'WindGust', 
                           'WindGustDir', 'WindSpd'),
             col_types = cols_only(Date = 'c',
                                   Time = 'c',
                                   DailyMaxTemp = 'd',
                                   DailyMinTemp = 'd',
                                   DailyRain = 'd')) %>% 
      mutate(Date = as.Date(Date, format = '%m/%d/%y'))
    ) %>% 
    arrange(Date) %>% 
    filter(Date > '2017-10-25') %>% #drop first day of incomplete readings
    filter(Time == 2300) %>% #last reading of the day = daily rain tot and max/min temp
    select(-Time)
}

compile_weather_old = function(path) {
  # data from old weather station: half-hour data
  #   summarize to extract daily min/max temp and rain totals
  read_csv(path, skip = 4, 
           col_names = c('DateTime', 'V', 'WindDir', 'WindSpd', 'MaxWindSpd',
                         'Temp', 'RH', 'Pressure', 'DailyRain'),
           col_types = cols_only(DateTime = 'c', 
                                 DailyRain = 'd',
                                 Temp = 'd')) %>%
    mutate(DateTime = as.POSIXct(DateTime, format = '%m/%d/%Y %H:%M'),
           Date = as.Date(format(DateTime, '%Y-%m-%d')),
           Time = format(DateTime, '%M')) %>%
    filter(Time == '00') %>%  # drop to one hour time intervals for comparability
    filter(Temp >= 0) %>% # remove few very negative temps (appear to be errors)
    group_by(Date) %>% #daily summary
    summarize(n.temp = sum(!is.na(Temp)),
              DailyMaxTemp = max(Temp, na.rm = T),
              DailyMinTemp = min(Temp, na.rm = T),
              n.rain = sum(!is.na(DailyRain)),
              DailyRain = max(DailyRain, na.rm = T),
              DailyRain = case_when(is.infinite(DailyRain) ~ NA_real_,
                                    TRUE ~ DailyRain),
              .groups = 'drop') %>%
    # if fewer than 20 records per day, consider min/max temp and rain unknown
    mutate(DailyMaxTemp = case_when(n.temp < 20 ~ NA_real_,
                                    TRUE ~ DailyMaxTemp),
           DailyMinTemp = case_when(n.temp < 20 ~ NA_real_,
                                    TRUE ~ DailyMinTemp),
           DailyRain = case_when(n.rain < 20 ~ NA_real_,
                                 TRUE ~ DailyRain)) %>%
    select(-n.temp, -n.rain) %>% 
    filter(Date > '2010-09-21') #no data from day 1
  
}

calculate_weather_monthly = function(dat) {
  # monthly min/max temp averages, and rain totals
  dat %>%
    mutate(yearmo = format(Date, '%Y-%m')) %>%
    group_by(yearmo) %>%
    summarize(n.temp = sum(!is.na(DailyMaxTemp)),
              MonthlyMaxTemp = mean(DailyMaxTemp, na.rm=T),
              MonthlyMinTemp = mean(DailyMinTemp, na.rm=T),
              n.rain = sum(!is.na(DailyRain)),
              MonthlyRain = sum(DailyRain, na.rm = T)) %>%
    # if fewer than 25 days of mostly complete readings, consider monthly temp/rain unknown
    mutate(MonthlyMaxTemp = case_when(n.temp < 25 ~ NA_real_,
                                      TRUE ~ MonthlyMaxTemp),
           MonthlyMinTemp = case_when(n.temp < 25 ~ NA_real_,
                                      TRUE ~ MonthlyMinTemp),
           MonthlyRain = case_when(n.rain < 25 ~ NA_real_,
                                   TRUE ~ MonthlyRain)) %>%
    # turn yearmo into a date for plotting purposes
    mutate(Date = as.Date(paste0(yearmo, '-01'))) %>%
    select(-yearmo, -n.temp, -n.rain)
}

get_ncdc_weather = function(station = 'GHCND:USC00043714',
                            start.date = '2010-10-01',
                            end.date = as.character(Sys.Date())) {
  if (difftime(end.date, start.date)/365 > 10) {
    stop('Max of 10 years can be requested at one time.')
  } else {
    rnoaa::ncdc(stationid = station, 
                datasetid = 'GSOM',
                startdate = start.date, 
                enddate = end.date,
                datatypeid = c('TMAX', 'TAVG', 'TMIN', 'PRCP'), 
                limit = 500)$data
  }
}

get_drought_indices = function(datname) {
  url <- "ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/"
  filelist = RCurl::getURL(url, dirlistonly = TRUE) %>% 
    strsplit("\\r\\n")
  df = tibble(name = filelist[[1]]) %>% 
    filter(grepl(datname, name))
  dat <- try(RCurl::getURL(paste0(url, df$name[1]))) %>% 
    strsplit("   \\n")
  read_table(dat[[1]],
             col_names = c('ID', 'JAN', 'FEB', 'MAR', 'APR', 
                           'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 
                           'OCT', 'NOV', 'DEC')) %>% 
    mutate(index = datname,
           state = substr(ID, 1, 2),
           climdiv = paste0(substr(ID, 3, 4)),
           year = substr(ID, 7, 10)) %>%
    filter(year >= 2010 & state == '04') %>% # California
    select(-state, -ID) %>% 
    pivot_longer(JAN:DEC, names_to = 'month', values_to = 'value') %>%
    mutate(climdiv = recode(climdiv, 
                            `01` = "North Coast Drainage",
                            `02` = "Sacramento Drainage",
                            `03` = "Northeast Interior Basins",
                            `04` = "Central Coast",
                            `05` = "San Joaquin Drainage",
                            `06` = "South Coast Drainage",
                            `07` = "Southeast Desert Basin"),
           date = as.Date(paste0(year, '-', month, '-01'), format = '%Y-%b-%d'),
           value = case_when(value < -99.9 ~ NA_real_, 
                             TRUE ~ value)) %>%
    arrange(date)
  
  ## Climate divisions within California:
  ## 01: North Coast Drainage
  ## 02: Sacramento Drainage
  ## 03: Northeast Interior Basins
  ## 04: Central Coast **TomKat**
  ## 05: San Joaquin Drainage
  ## 06: South Coast Drainage
  ## 07: Southeast Desert Basin
}

calculate_weather_diffs = function(localdat, hmbdat, hmb_historic) {
  hmb <- hmbdat %>%
    mutate(Date = as.Date(date, format = '%Y-%m-%d')) %>% 
    select(Date, datatype, value) %>%
    filter(datatype != 'TAVG') %>% 
    # pivot_wider(names_from = datatype, values_from = value) %>%
    # convert units
    mutate(value = case_when(datatype == 'PRCP' ~ value * 0.0393701, # mm to in
                             datatype == 'TMAX' ~ value * (9/5) + 32, # C to F
                             datatype == 'TMIN' ~ value * (9/5) + 32),
           datatype = recode(datatype, PRCP = 'MonthlyRain',
                             TMIN = 'MonthlyMinTemp',
                             TMAX = 'MonthlyMaxTemp'))
  
  ## calculate monthly correction factors:
  correction <- localdat %>% 
    pivot_longer(-Date, names_to = 'datatype', values_to = 'observed') %>% 
    left_join(hmb, by = c("Date", 'datatype')) %>%
    mutate(diff = observed - value,
           mo = format(Date, '%m')) %>% 
    group_by(mo, datatype) %>% 
    summarize(diff = mean(diff, na.rm = TRUE),
              .groups = 'drop') %>% 
    pivot_wider(names_from = datatype, values_from = diff)
  
  ## estimate TomKat monthly normals
  normals <- full_join(
   # hmb dat
   read.csv(hmb_historic) %>%
     set_names('mo', 'MonthlyRain', 'MonthlyMinTemp', 'TAVG', 'MonthlyMaxTemp') %>% 
     select(-TAVG) %>% 
     pivot_longer(MonthlyRain:MonthlyMaxTemp, values_to = 'hmb'),
   # correction factors
   correction %>% 
     pivot_longer(MonthlyMaxTemp:MonthlyRain, values_to = 'diff') %>% 
     mutate(mo = as.numeric(mo)),
   by = c('name', 'mo')) %>% 
   mutate(est = hmb + diff) 
  
  ## calculate monthly differences from estimated normals
  localdat %>% 
    filter(Date > '2010-09-15') %>% #first row is all NA
    pivot_longer(MonthlyMaxTemp:MonthlyRain, values_to = 'observed') %>% 
    mutate(mo = as.integer(format(Date, '%m'))) %>%
    left_join(normals %>% select(mo, name, normal = est), by = c('mo', 'name')) %>%
    mutate(diff = observed - normal) %>%
    select(Date, mo, name, diff) %>%
    # spread(key = var, value = diff) %>%
    arrange(name, Date)
  
}
