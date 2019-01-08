# READ ME-------------
# Clean and process weather station data

# packages
library(tidyverse)

# input files
rawdat <- 'data_raw/TOKA_weatherwest_10.23.17_12.18.18_utf8.csv'
olddat <- 'data_raw/TOKA_Weather_ALL_9.8.10_7.24.17.csv'
# olddat <- 'data_raw/TK_weather_master_2017.csv' #data from old weather station

# output files
master_daily <- 'data_master/TK_weather_master_daily.csv'
master_monthly <- 'data_master/TK_weather_master_monthly.csv'



# DATA SET UP----------------
# new data from WeatherWest (from October 2017): hourly data
#  extract daily min/max temp and rain totals
dat <- read_csv(here::here(rawdat), col_types = cols()) %>%
  mutate(date = as.Date(Date, format = '%m/%d/%Y')) %>%
  rename(temp.F = `Temp (°F)`,
         temp.max = `Daily Max Temp (°F)`,
         temp.min = `Daily Min Temp (°F)`,
         rain.in = `Daily Rain (In)`) %>%
  filter(Time == 2300) %>% #last reading of the day = daily summary
  select(date, temp.max, temp.min, rain.in) %>% 
  arrange(date) %>%
  filter(date > '2017-10-25') #drop first value (temp.min unusually high)

# data from old weather station: half-hour data
#   summarize to extract daily min/max temp and rain totals
dat2 <- read_csv(here::here(olddat), skip = 2, col_types = cols()) %>%
  filter(WindDir != 'Degree') %>% #drop row of units only
  rename('date.time' = `Date/Time`,
         'temp.F' = AirTemp,
         'rain.in' = DailyRain) %>%
  select(date.time, temp.F, rain.in) %>%
  mutate(date.time = as.POSIXct(date.time, format = '%m/%d/%Y %H:%M'),
         date = as.Date(date.time, '%Y-%m-%d'),
         time = format(date.time, '%M'),
         temp.F = as.numeric(temp.F),
         rain.in = as.numeric(rain.in)) %>%
  filter(time == '00') %>%  # drop to one hour time intervals for comparability
  group_by(date) %>% #daily summary
  summarize(n.temp = sum(!is.na(temp.F)),
            temp.max = max(temp.F, na.rm = T),
            temp.min = min(temp.F, na.rm = T),
            n.rain = sum(!is.na(rain.in)),
            rain.in = max(rain.in, na.rm = T),
            rain.in = case_when(is.infinite(rain.in) ~ NA_real_,
                                TRUE ~ rain.in)) %>%
  # if fewer than 20 records per day, consider min/max temp and rain unknown
  mutate(temp.max = case_when(n.temp < 20 ~ NA_real_,
                              TRUE ~ temp.max),
         temp.min = case_when(n.temp < 20 ~ NA_real_,
                              TRUE ~ temp.min),
         rain.in = case_when(n.rain < 20 ~ NA_real_,
                             TRUE ~ rain.in)) %>%
  select(-n.temp, -n.rain)

sdat <- bind_rows(dat2, dat)

write_csv(sdat, here::here(master_daily))


# MONTHLY WEATHER STATS--------------
# monthly min/max temp averages, and rain totals

mdat <- sdat %>%
  mutate(yearmo = format(date, '%Y-%m')) %>%
  group_by(yearmo) %>%
  summarize(n.temp = sum(!is.na(temp.max)),
            temp.max = mean(temp.max, na.rm=T),
            temp.min = mean(temp.min, na.rm=T),
            n.rain = sum(!is.na(rain.in)),
            rain.in = sum(rain.in, na.rm = T)) %>%
  # if fewer than 25 days of mostly complete readings, consider monthly temp/rain unknown
  mutate(temp.max = case_when(n.temp < 25 ~ NA_real_,
                              TRUE ~ temp.max),
         temp.min = case_when(n.temp < 25 ~ NA_real_,
                              TRUE ~ temp.min),
         rain.in = case_when(n.rain < 25 ~ NA_real_,
                             TRUE ~ rain.in)) %>%
  # turn yearmo into a date
  mutate(date = as.Date(paste0(yearmo, '-15'))) %>%
  select(-yearmo, -n.temp, -n.rain)

write_csv(mdat, here::here(master_monthly))
