# READ ME-------------
# Clean and process pasture map management data
# Goal is to present:
#  1.	Grazing intensity ((animal units * days grazed)/acre)
#  2.	Number of visits (i.e, number of times grazed)
#  3.	Total days grazed 

# packages
library(tidyverse)
library(sf)

# input files
rawdat <- 'data_raw/Tom Kat Ranch - Pastures 201810292309.xlsx'

## shapefiles
poly <- 'TK_veg_fields'

# output files
masterdat <- 'data_master/TK_mgmt_master.csv'
season_stats <- 'data_master/TK_mgmt_stats.csv'

# DATA SET UP----------------

fields <- readxl::excel_sheets(here::here(rawdat)) 
# PastureMap output has a separate sheet for each field, plus a summary report
# --> skip summary report and lengthy header on each page
# --> include sheet name as an ID

dat <- map_dfr(fields[2:length(fields)],
               ~ readxl::read_excel(path = here::here(rawdat), sheet = .x, skip = 14) %>%
                   mutate(field = .x))

summary(dat$`Date and time`) #Nov 2017 through Oct 2018

# field (excel sheet name) does not always equal recorded 'Pasture' in each 
# sheet (and a few are blank):
dat %>% pull(Pasture) %>% as.factor() %>% summary()
dat %>% select(Pasture, field) %>% table()
# --> assume these are generally subdivisions within each field

# activity recorded includes adding/consuming hay; filter down to Move In/Out only
dat %>% pull(Activity) %>% as.factor() %>% summary()

# compare to pastures used for veg data:
shp_poly <- st_read(here::here('GIS'), poly, quiet = TRUE) 

# fields in PastureMap with no correspondence to veg fields: (drop these)
dat %>% filter(!(toupper(field) %in% shp_poly$Pasture)) %>% 
  pull(field) %>% as.factor() %>% summary()


# DATA CLEAN UP------------------

dat_filter <- dat %>%
  rename(date.time = `Date and time`, 
         area = Pasture,
         activity = Activity,
         herd = Herd,
         n = `Number of animals`,
         weight = `Total weight (lbs)`) %>%
  select(field, date.time:weight) %>%
  mutate(field = toupper(field)) %>%
  filter(field %in% shp_poly$Pasture) %>%
  filter(activity %in% c('Move In', 'Move Out')) %>%
  # filter(!herd %in% c('Ranch Horses', 'Retired Horses')) %>% # ?? check with Mel
  mutate(season = case_when(date.time<'2018-07-01' ~ 'growing',
                            TRUE ~ 'dormant'),
         date = as.Date(format(date.time, '%Y-%m-%d')),
         time = format(date.time, '%H:%M'),
         hour = format(date.time, '%H'),
         # sort move outs before move ins:
         activity = factor(activity, levels = c('Move Out', 'Move In')),
         season = factor(season, levels = c('growing', 'dormant'))) %>%
  arrange(herd, date, time, field, area, activity) 

# spotcheck:
dat_filter[which(duplicated(dat_filter)), c('field', 'area', 'herd', 'date', 'time', 'activity')]
# none

# AGGREGATE FIELD DATA------------
# herds are often moved from one subdivision to another within one field,
# within minutes -- simplify to remove this info, but also check for duplicate 
# entries within minutes

dat_agg <- dat_filter %>%
  # show move outs as negative numbers
  mutate(dn = case_when(activity == 'Move Out' ~ 0 - n,
                        activity == 'Move In' ~ n)) %>%
  # summarize dn by field and hour
  group_by(herd, season, date, hour, field) %>%
  summarize(dn = sum(dn)) %>%
  ungroup()

# spotcheck:
dat_agg %>% filter(dn == 0)
dat_filter %>% filter(herd == 'Bulls' & date == '2018-04-17')

## drop rows with a dn=0 (no net change)
dat_agg <- dat_agg %>% filter(dn != 0)

# PROOF DATA-------------------------
# make sure move outs follow move ins of same # of animals
# review each herd's data to ID patterns and clean up
# Herds: Bulls, Cows, D, E, IR, Non-Breeding, Ranch Horses, Retired Horses

tmp <- dat_agg %>% filter(herd == 'Cows') %>%
  arrange(date, hour, dn)
tmp[1:20,]

# correct errors and re-aggregate data by hour:
dat_proof <- dat_filter %>%
  mutate(date = case_when(herd == 'Bulls' & field == '10' & 
                            date == '2018-04-02' ~ as.Date('2018-04-01'),
                          herd %in% c('Cows','D','E', 'Non-Breeding') & 
                            field == '52' & 
                            date == '2018-01-19' & time == '07:15' ~ 
                            as.Date('2018-01-18'),
                          herd %in% c('Cows', 'Non-Breeding') & field == '10' & 
                            date == '2018-04-02' & time == '08:49' ~ 
                            as.Date('2018-04-01'),
                          herd == 'Cows' & field == '38A' & 
                            date == '2018-08-16' ~ as.Date('2018-08-15'),
                          TRUE ~ date),
         time = case_when(herd %in% c('Cows', 'D', 'E', 'Non-Breeding') & 
                            field == 48 & 
                            date == '2018-01-27' & activity == 'Move Out' ~ 
                            '07:37',
                          TRUE ~ time),
         hour = substr(time, 1, 2),
         # to err on side of overestimating grazing impact, ensure move in 
         # number is as large as move out number (assume additional animals 
         # brought in at same time as initial animals);
         # also: to avoid leaving animals in fields indefinitely, (when they were 
         # probably moved off ranch), ensure move out number matches move in number
         n = case_when(herd == 'Cows' & field == 1 & activity == 'Move Out' &
                         date == '2018-04-14' ~ 43,
                       herd == 'Cows' & field == 18 & activity == 'Move Out' & 
                         date == '2018-03-13' ~ 38,
                       herd == 'Cows' & field == 19 & activity == 'Move In' & 
                         date == '2018-03-13' ~ 43,
                       herd == 'Cows' & field == 52 & activity == 'Move Out' & 
                         date == '2018-06-04' ~ 42,
                       herd == 'Cows' & field == '38A' & activity == 'Move Out' & 
                         date == '2018-08-09' ~ 32,
                       herd == 'Cows' & field == '37B' & activity == 'Move Out' & 
                         date == '2018-09-26' ~ 31,
                       herd == 'D' & field == '37B' & activity == 'Move Out' & 
                         date == '2018-06-27' ~ 15,
                       herd == 'Non-Breeding' & field == 41 & activity == 'Move In' & 
                         date == '2018-08-02' ~ 9,
                       herd == 'Non-Breeding' & field == 43 & activity == 'Move In' & 
                         date == '2018-06-27' & time == '09:27' ~ 2,
                       herd == 'Non-Breeding' & field == 59 & activity == 'Move Out' & 
                         date == '2018-08-25' ~ 17,
                       herd == 'Non-Breeding' & field == 60 & activity == 'Move Out' & 
                         date == '2018-09-19' ~ 16,
                       herd == 'Non-Breeding' & field == '37B' & activity == 'Move Out' & 
                         date == '2018-09-26' ~ 7,
                       herd == 'Non-Breeding' & field == '39B' & activity == 'Move Out' & 
                         date == '2018-10-03' ~ 5,
                       TRUE ~ n))

# check that errors have been corrected:

dat_proof_agg <- dat_proof %>%
  mutate(dn = case_when(activity == 'Move Out' ~ 0 - n,
                        activity == 'Move In' ~ n)) %>%
  group_by(herd, season, date, hour, field) %>%
  summarize(dn = sum(dn)) %>%
  ungroup() %>%
  filter(dn != 0)

tmp <- dat_proof_agg %>% filter(herd == 'Bulls') %>%
  arrange(date, hour, dn)
tmp[1:20,]

write_csv(dat_proof, here::here(masterdat))


# CALCULATIONS--------------
# - aggregate herds within fields
# - calculate time between consecutive timestamps
# - cumulative sum of the number of animals in the field at each timestamp
# - animal days as the previous #animals * time difference

ddat <- dat_proof %>%
  mutate(herd = recode(herd, 
                       Cows = 'cattle', 'D' = 'cattle', 'E' = 'cattle',
                       'Non-Breeding' = 'cattle', Bulls = 'cattle', 
                       IR = 'cattle', 
                       'Ranch Horses' = 'horses', 'Retired Horses' = 'horses')) %>%
  mutate(dn = case_when(activity == 'Move Out' ~ 0 - n,
                        activity == 'Move In' ~ n)) %>%
  group_by(herd, season, date, hour, field, area) %>%
  summarize(dn = sum(dn)) %>%
  ungroup() %>%
  filter(dn != 0) %>% 
  mutate(date.time = as.POSIXct(paste0(date, ' ', hour, ':00'))) %>%
  arrange(herd, field, date.time, dn) %>%
  group_by(herd, field) %>%
  mutate(diff = difftime(date.time, lag(date.time), units = 'days'),
         diff = as.numeric(diff),
         total = cumsum(dn),
         animal.days = lag(total) * diff)

# find any remaining instances where: 
# - move out > move in
ddat %>% filter(total < 0) #none remaining

# - field not empty after move out:
ddat %>% filter(total > 0 & dn < 0) %>% arrange(date)
# in some cases, remaining animals move out at a later timestamp, in others 
# animals are counted as lingering there indefinitely -- check and ensure
# move out #s match move in #s -- assuming other animals moved out and not recorded

## Dec 2017: fields 5, 8, 10, 11, 15 ok
## Feb 2018: field 32 ok
## Mar 2018: field 30B ok
## Apr 2018: field 10 ok
## Aug 2018: field 38A area Finger 2 ok


# Note: trailing visits at end of this data set are not counted if there is no
# second timestamp, so for example animals checked into a field in October and
# no follow-up movements elsewhere before the end of this data set, are not 
# included in summary stats

# SUMMARIZE BY SEASON-----------------------
# - total grazing days
# - total animal days

sdat <- ddat %>%
  group_by(season, field) %>%
  summarize(ndays = sum(diff, na.rm = T),
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
