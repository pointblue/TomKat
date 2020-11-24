# READ ME-------------
# Clean and process pasture map management data
# Goal is to present:
#  -	Grazing intensity ((animal units * days grazed)/acre)
#  -	Total days grazed 

# packages
library(tidyverse)
library(sf)

# input files
rawdat1 <- 'data_raw/Tom Kat Ranch - Pastures 201810292309.xlsx'
rawdat2 <- 'data_raw/Tom Kat Ranch - Pastures 2019growingseason.xlsx'

## shapefiles
poly <- 'TK_veg_fields'

# output files
masterdat <- 'data_master/TK_mgmt_master.csv'
season_stats <- 'data_master/TK_mgmt_stats.csv'

# DATA SET UP----------------

fields1 <- readxl::excel_sheets(here::here(rawdat1)) 
fields2 <- readxl::excel_sheets(here::here(rawdat2)) 
# PastureMap output has a separate sheet for each field, plus a summary report
# --> skip summary report and lengthy header on each page
# --> include sheet name as an ID

fields1[-which(toupper(fields1) %in% toupper(fields2))]
fields2[-which(toupper(fields2) %in% toupper(fields1))]

# compare to pastures used for veg data:
shp_poly <- st_read(here::here('GIS'), poly, quiet = TRUE) 

# read in each excel sheet and rbind:
dat1 <- map_dfr(fields1[2:length(fields1)],
                ~ readxl::read_excel(path = here::here(rawdat1), 
                                     sheet = .x, skip = 14) %>%
                  mutate(field = .x))

dat2 <- map_dfr(fields2[2:length(fields2)],
               ~ readxl::read_excel(path = here::here(rawdat2), 
                                    sheet = .x, skip = 16) %>%
                   mutate(field = .x))

# Note: field (excel sheet name) does not always equal recorded 'Pasture' 
# within each sheet (and a few are blank)
# --> assume these are generally subdivisions within each field

# fields in PastureMap with no correspondence to veg fields: (drop these)
dat1 %>% filter(!(toupper(field) %in% toupper(shp_poly$Pasture))) %>% 
  pull(field) %>% as.factor() %>% summary()
##--> assume 4b is part of 4
dat2 %>% filter(!(toupper(field) %in% toupper(shp_poly$Pasture))) %>% 
  pull(field) %>% as.factor() %>% summary()

# OLDER DATA: (NOTE: NEWER DATA IN A DIFFERENT FORMAT)
# clean up column names, format, and filter:
dat_filter1 <- dat1 %>%
  rename(date.time = `Date and time`, 
         area = Pasture,
         activity = Activity,
         herd = Herd,
         n = `Number of animals`) %>%
  select(field, date.time:n) %>%
  mutate(field = toupper(field),
         field = recode(field, '4B' = '4'),
         # show move outs as a negative number of animals
         dn = case_when(activity == 'Move Out' ~ 0 - n,
                        activity == 'Move In' ~ n)) %>%
  filter(field %in% toupper(shp_poly$Pasture)) %>%
  # drop any activity recorded other than moving animals in/out
  filter(activity %in% c('Move In', 'Move Out')) %>%
  # filter(!herd %in% c('Ranch Horses', 'Retired Horses')) %>% # ?? check with Mel
  mutate(season = case_when(date.time<'2018-07-01' ~ 'growing',
                            TRUE ~ 'dormant'),
         season = factor(season, levels = c('growing', 'dormant')),
         date = as.Date(format(date.time, '%Y-%m-%d')),
         time = format(date.time, '%H:%M'),
         hour = format(date.time, '%H'),
         # aggregate herds
         herd = recode(herd, 
                       Bulls = 'cattle', 
                       Cows = 'cattle', 
                       'D' = 'cattle', 
                       'E' = 'cattle',
                       'Non-Breeding' = 'cattle',
                       IR = 'cattle', 
                       'Ranch Horses' = 'horses', 
                       'Retired Horses' = 'horses')) %>%
  #running total by field, subdivision, and hour
  group_by(herd, season, date, hour, field, area) %>%
  summarize(dn = sum(dn)) %>%
  ungroup() %>%
  arrange(herd, field, date, hour, dn)

# PROOF DATA--------------------
# check running total of dn values by field:
tmp <- dat_filter %>%
  group_by(herd, field) %>%
  mutate(total = cumsum(dn))

# find any remaining instances where: 
# - move out > move in (running total < 0)
tmp %>% filter(total < 0) %>% arrange(date)
# --> assume more animals were moved in between "move in" and "move out"
#  timestamps recorded; to avoid underestimating grazing intensity, assume
#  extra animals moved in at the same time as original move in

# - field not empty after move out:
tmp %>% filter(total > 0 & dn < 0) %>% arrange(date)
# in some cases, remaining animals move out at a later timestamp and there is
#  no error; in other cases, dates appear to be incorrect, with animals
#  moving into another area/subdivision before moving out of the first; in 
#  still others, assume remaining animals did move out at a previous
#  timestamp and weren't recorded; to ensure these animals are counted as 
#  lingering there indefinitely, assume they moved out at the same time as the
#  others

# check for errors in date by following herd sizes around the ranch:
tmp <- tmp %>% arrange(herd, date, hour, dn)


# correct errors:
dat_proof <- dat_filter %>%
  mutate(dn = case_when(field == 19 & 
                          area == 19 &
                          date == '2018-03-13' ~ dn + 6,
                        field == 43 & 
                          area == 'Subdivision 11' & 
                          date == '2018-06-27' ~ dn + 1,
                        field == 41 & 
                          area == 'Subdivision 5' & 
                          date == '2018-08-02' ~ dn + 7,
                        field == 18 &
                          area == 18 &
                          date == '2018-03-13' ~ dn - 1,
                        field == 1 & 
                          area == 'Between Ralphs and Pond' &
                          date == '2018-04-14' ~ dn - 1,
                        field == 52 & 
                          area == 'Subdivision 3' & 
                          date == '2018-06-04' ~ dn - 2,
                        field == '37B' & 
                          area == 'Subdivision 5' &
                          date == '2018-06-27' ~ dn - 9,
                        field == '37B' &
                          area == 'Subdivision 16' &
                          date == '2018-09-25' ~ dn - 4,
                        field == '38A' & 
                          area == 'Bees' &
                          date == '2018-08-09' ~ dn - 1,
                        field == '39B' &
                          area == 'Subdivision 4' &
                          date == '2018-10-03' ~ dn - 1,
                        field == 59 &
                          area == 59 &
                          date == '2018-08-25' ~ dn - 1,
                        field == 60 &
                          area == 60 &
                          date == '2018-09-19' ~ dn - 4,
                        TRUE ~ dn),
         date = case_when(field == 10 & 
                            area == 'Subdivision 1' &
                            date == '2018-04-02' ~ as.Date('2018-04-01'),
                          field == 52 & 
                            area == 'Subdivision 3' &
                            date == '2018-01-19' ~ as.Date('2018-01-18'),
                          field == '38A' &
                            area == 'Finger 2' &
                            date == '2018-08-16' ~ as.Date('2018-08-15'),
                          TRUE ~ date),
         hour = case_when(field == 48 & 
                            area == 'Subdivision 1' &
                            date == '2018-01-27' ~ '07',
                          TRUE ~ hour)) %>%
  group_by(herd, season, date, hour, field, area) %>%
  summarize(dn = sum(dn)) %>%
  ungroup() %>%
  arrange(herd, field, date, hour, dn)

# check that errors have been fixed:
tmp <- dat_proof %>%
  group_by(herd, field) %>%
  mutate(total = cumsum(dn))

tmp %>% filter(total < 0) %>% arrange(date) #none remaining
tmp %>% filter(total > 0 & dn < 0) %>% arrange(date) %>% as.data.frame()
## all remaining are multi-stage withdrawals of animals:
## Dec 2017: fields 5, 8, 10, 11, 15 ok
## Feb 2018: field 32 ok
## Mar 2018: field 30B ok
## Aug 2018: field 38A area Finger 2 ok

# Note: trailing visits at end of this data set are not counted if there is no
# second timestamp, so for example animals checked into a field in October and
# no follow-up movements elsewhere before the end of this data set, are not 
# included in summary stats

write_csv(dat_proof, here::here(masterdat))


# CALCULATIONS--------------
# - cumulative sum of the number of animals in the field at each timestamp
# - calculate time between consecutive timestamps
# - animal days as the previous #animals * time difference

ddat <- dat_proof %>%
  mutate(date.time = as.POSIXct(paste0(date, ' ', hour, ':00'))) %>%
  select(herd, field, area, season, date.time, dn) %>%
  arrange(herd, field, date.time, dn) %>%
  group_by(herd, field) %>%
  mutate(total = cumsum(dn),
         # don't count timediff when there are no animals present
         timediff = case_when(lag(total) == 0 ~ NA_real_,
                              TRUE ~ as.numeric(difftime(date.time, 
                                                         lag(date.time), 
                                                         units = 'days'))),
         animal.days = lag(total) * timediff)


# SUMMARIZE BY SEASON-----------------------
# - total grazing days
# - total animal days

sdat <- ddat %>%
  group_by(season, field) %>%
  summarize(ndays = sum(timediff, na.rm = T),
            animal.days = sum(animal.days, na.rm = T)) %>%
  ungroup() %>%
  gather(ndays:animal.days, key = 'var', value = 'value') %>%
  spread(key = 'season', value = 'value', fill = 0) %>%
  mutate(total = growing + dormant) %>%
  gather(growing:total, key = 'season', value = 'value')


## compare to pasturemap estimates:
mdat <- readxl::read_excel(here::here(rawdat), sheet = fields[1], skip = 17) %>%
  rename(field = 'Pasture name', acres = 'Area (acres)', ADA = 'ADAs this season') %>%
  select(field, acres, ADA) %>%
  mutate(field = toupper(field)) %>%
  filter(field %in% shp_poly$Pasture)

sdat %>%
  filter(season == 'total' & var == 'animal.days') %>%
  left_join(mdat, by = 'field') %>%
  mutate(ADA2 = value / acres) %>%
  # filter(ADA < 25 & ADA2 > 45)
  ggplot(aes(ADA2, ADA)) + geom_point() + geom_abline(slope = 1)
# pretty good correspondence to PastureMap estimates, but there appear to be
# some real underestimates from PastureMap (?)


## add acreage from pasture map and calculate animal days per acre
sdat_final <- sdat %>% 
  spread(key = 'var', value = 'value') %>%
  left_join(shp_poly %>% st_set_geometry(NULL) %>% select(Pasture, area_ha), 
            by = c('field' = 'Pasture')) %>%
  mutate(area_acres = area_ha * 2.47105) %>%
  mutate(ADA = animal.days / area_acres)


write_csv(sdat_final, here::here(season_stats))
