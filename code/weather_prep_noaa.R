# READ ME-------------
# Script to acquire & process weather data from NOAA:
#  - recent (daily and monthly) weather data from Half Moon Bay weather station
#  - recent drought indices information

# packages
library(tidyverse)
library(rnoaa)

# input files: 

## previously assembled daily & monthly weather data from HMB (to add to these)
olddaily <- 'data_master/HMB_daily_weather.csv'
oldmonthly <- 'data_master/HMB_monthly_weather.csv'

## latest drought indices: download from ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/
pdsi <- 'data_raw/climdiv-pdsidv-v1.0.0-20191205.txt'
zndx <- 'data_raw/climdiv-zndxdv-v1.0.0-20191205.txt'


# output files:
masterpdsi <- 'data_master/CA_Palmer_Drought_Severity_Index.csv'
masterzndx <- 'data_master/CA_Palmer_Z_Index.csv'

# HALF MOON BAY WEATHER-------

# set up API:
# --> If not done already, get API key from NOAA (by submitting email address):
#       https://www.ncdc.noaa.gov/cdo-web/token
# The system will send an email address with a "token". Save in R by running the
#   following line of code, replacing token with your code:
# options(noaakey = 'token')

## DAILY DATA 
## previously acquired:
dat <- read_csv(here::here(olddaily), col_types = cols())

## add more recent data:
## max 1000 lines of data, and date range must be < 1 year (repeat as needed)

newdat <- rnoaa::ncdc(stationid = 'GHCND:USC00043714', # Half Moon Bay
                      datasetid = 'GHCND', 
                      startdate = as.character(as.Date(max(dat$date)) + 1), 
                      enddate = as.character(min(as.Date(max(dat$date)) + 365, Sys.Date())),
                      datatypeid = c('TMAX', 'TMIN', 'PRCP'), 
                      limit = 1000)$data %>%
  mutate(date = as.POSIXct(date),
         fl_t = as.integer(fl_t))

dat <- bind_rows(dat, newdat)

write_csv(dat, here::here(olddaily)) #CAREFUL: overwriting old data


## MONTHLY DATA:
## previously acquired:
dat2 <- read_csv(here::here(oldmonthly), col_types = cols())

## add more recent data:
## max 1000 lines of data, and date range must be < 1 year (repeat as needed)

newdat2 <- rnoaa::ncdc(stationid = 'GHCND:USC00043714', 
                       datasetid = 'GSOM',
                       startdate = as.character(as.Date(max(dat2$date)) + 1), 
                       enddate = as.character(min(as.Date(max(dat$date)) + 365, Sys.Date())),
                       datatypeid = c('TMAX', 'TAVG', 'TMIN', 'PRCP'), 
                       limit = 1000)$data %>%
  mutate(date = as.POSIXct(date),
         fl_a = as.numeric(fl_a),
         fl_M = as.logical(fl_M),
         fl_Q = as.logical(fl_Q),
         fl_S = as.integer(fl_S))

dat2 <- bind_rows(dat2, newdat2)

write_csv(dat2, here::here(oldmonthly)) #CAREFUL: overwriting old data


# DROUGHT INDICES--------------
# Download latest data from: ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/
# Look for PDSI: "climdiv-pdsidv-v1.0.0-20161104" (or whatever the most recent date is)
#   and Palmer's Z Index: "climdiv-zndxdv-v1.0.0-20161104"
# Right-click on each file and save as .txt
#
# If ftp above not available, try link below figure here: 
#    https://www.ncdc.noaa.gov/temp-and-precip/drought/historical-palmers/
#    (currently: https://www1.ncdc.noaa.gov/pub/data/cirs/climdiv/climdiv-pdsidv-v1.0.0-20180105  )
#
# More info here: http://www.ncdc.noaa.gov/temp-and-precip/drought/historical-palmers/overview

## PALMER'S DROUGHT SEVERITY INDEX (long-term drought index)

pdat <- read_table(here::here(pdsi), 
                  col_names = c('ID', 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
                                'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'),
                  col_types = cols()) %>%
  mutate(state = substr(ID, 1, 2),
         climdiv = paste0('PDSI.', substr(ID, 3, 4)),
         year = substr(ID, 7, 10)) %>%
  filter(year >= 2010 & state == '04') %>% # California
  gather(JAN:DEC, key = 'month', value = 'value') %>%
  mutate(date = as.Date(paste0(year, '-', month, '-15'), format = '%Y-%b-%d')) %>%
  mutate(value = case_when(value < -99.9 ~ NA_real_, 
                           TRUE ~ value)) %>%
  select(-ID, -state) %>%
  spread(key = climdiv, value = value) %>%
  arrange(date)


## Climate divisions within California:
## 01: North Coast Drainage
## 02: Sacramento Drainage
## 03: Northeast Interior Basins
## 04: Central Coast **TomKat**
## 05: San Joaquin Drainage
## 06: South Coast Drainage
## 07: Southeast Desert Basin

write_csv(pdat, here::here(masterpdsi))


## Palmer Z Index (shorter-term moisture anomaly)

zdat <- read_table(here::here(zndx),
                   col_names = c('ID', 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
                                 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'),
                   col_types = cols()) %>%
  mutate(state = substr(ID, 1, 2),
         climdiv = paste0('PZI.', substr(ID, 3, 4)),
         year = substr(ID, 7, 10)) %>%
  filter(year >= 2010 & state == '04') %>% # California
  gather(JAN:DEC, key = 'month', value = 'value') %>%
  mutate(date = as.Date(paste0(year, '-', month, '-15'), format = '%Y-%b-%d')) %>%
  mutate(value = case_when(value < -99.9 ~ NA_real_, 
                           TRUE ~ value)) %>%
  select(-ID, -state) %>%
  spread(key = climdiv, value = value) %>%
  arrange(date)

write_csv(zdat, here::here(masterzndx))
