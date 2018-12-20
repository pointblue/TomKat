# READ ME----
# Clean & process point count data to estimate species richness and focal
# species densities

# packages
library(tidyverse)
library(pbdistance) # devtools::install_github('kdybala/pbdistance')
library(sf)

# input files
rawdat <- 'data_raw/TOKA_HOCR_PC_2010_2018.csv'

grasspc <- 'TOKA_point_count_grid_grass&hay' #surveys points in "grassland" only


# output files
masterdat <- 'data_master/TK_bird_master.csv'
point_density <- 'data_master/TK_bird_density_by_point.csv'
ranch_density <- 'data_master/TK_bird_density_by_year.csv'
point_richness <- 'data_master/TK_richness_by_point.csv'

# DATA SET UP-----------

## TOKA data
dat <- read_csv(here::here(rawdat)) %>% #24752 (older data has more rows?)
  mutate(Date = as.Date(Date, format = '%m/%d/%Y'),
         Year = as.numeric(format(Date, '%Y'))) %>%
  unite('Visit', Year, Visit, sep = '-') %>%
  mutate_at(vars(Project:Visit, 
                 Spp:`Distance Bin ID`,
                 `Breeding Status`,
                 Researcher:`Data Status`), factor) %>%
  mutate_at(vars(Singing), as.logical) %>%
  filter(!(`Distance Bin ID` %in% c('FLO', 'B30'))) %>%
  mutate_at(vars(`Distance Bin`), as.numeric) %>%
  filter(!is.na(`Distance Bin`)) %>%
  # consolidate species codes:
  mutate(Spp = recode(Spp,
                      BSKI = 'WTKI',
                      RSFL = 'NOFL',
                      ORJU = 'DEJU',
                      ECDO = 'EUCD',
                      WIWR = 'PAWR',
                      WESJ = 'CASJ',
                      NWCS = 'WCSP',
                      UNHU = 'XXHU',
                      UNWO = 'XXWO',
                      UNSW = 'XXSW',
                      UNWA = 'XXWA'))

# FYI -- a few points had a 3rd visit in 2011 (?)
# note: ALHU, RUHU, and SPHU

write_csv(dat, here::here(masterdat))


# ANALYSIS 1: AVERAGE DENSITY PER POINT (TOKA only)----
# focal species hotspots/coldspots ranch-wide
# --> include all points in TOKA transect over all years

sdat <- dat %>% 
  filter(Visit != '2011-3' & Transect == 'TOKA') %>% #drop points with an extra third visit
  as.data.frame()

fdat <- format_data_flat(sdat, strata = 'Point', dist = 'Distance Bin', 
                         dist_bin_id = NULL)
# 170 strata = 170 unique sampling locations (points), 1-14 visits per sampling location

# grsp <- fit_distance_models(data = fdat, spec = 'grsp', maxdist = 150,
#                             bins = c(0, 20, 40, 60, 80, 100, 150))
grsp <- fit_distance_models(data = fdat, spec = 'grsp', maxdist = 100,
                            bins = c(0, 30, 50, 70, 80, 90, 100))

# savs <- fit_distance_models(data = fdat, spec = 'savs', maxdist = 150,
#                             bins = c(0, 30, 50, 70, 100, 150))
savs <- fit_distance_models(data = fdat, spec = 'savs', maxdist = 100,
                            bins = c(0, 30, 50, 70, 80, 90, 100))

wcsp <- fit_distance_models(data = fdat, spec = 'wcsp', maxdist = 150, 
                            bins = c(0, 50, 80, 110, 150))

# compile density estimates
speclist <- c('grsp', 'savs', 'wcsp')
density <- do.call(rbind.data.frame, lapply(speclist, function(x) {
  dat = get(tolower(as.character(x)))
  dat = dat$D
  dat$species = toupper(as.character(x))
  return(dat)
}))

density <- density[-grep('Total',density$Label),]

## save the results as a CSV file
write_csv(density, here::here(point_density))


# ANALYSIS 2: AVERAGE RANCH-WIDE DENSITY BY YEAR----
# for creating a figure showing ranch-wide trends in focal species densities
# --> include only "grassland" points

shp <- read_sf(here::here('GIS'), grasspc)

gdat <- dat %>% 
  filter(Visit != '2011-3' & Transect == 'TOKA') %>% #drop points with an extra third visit
  filter(Point %in% shp$Name) %>%
  mutate(Year = as.numeric(format(Date, '%Y'))) %>%
  as.data.frame()

length(unique(gdat$Point)) #104

fdat2 <- format_data_flat(gdat, year = 'Year', dist = 'Distance Bin', 
                          dist_bin_id = NULL) 
# 8 strata (one for each year); 104 unique locations (points); 1-2 visits per point

# grsp2 <- fit_distance_models(data = fdat2, spec = 'grsp', maxdist = 150,
#                              bins = c(0, 20, 40, 60, 80, 100, 150))
grsp2 <- fit_distance_models(data = fdat2, spec = 'grsp', maxdist = 100,
                             bins = c(0, 30, 50, 70, 80, 90, 100))

# savs2 <- fit_distance_models(data = fdat2, spec = 'savs', maxdist = 150, 
#                              bins = c(0, 30, 50, 70, 100, 150))
savs2 <- fit_distance_models(data = fdat2, spec = 'savs', maxdist = 100,
                             bins = c(0, 30, 50, 70, 80, 90, 100))

wcsp2 <- fit_distance_models(data = fdat2, spec = 'wcsp', maxdist = 150, 
                             bins = c(0, 50, 90, 120, 150))

speclist <- c('grsp2', 'savs2', 'wcsp2')
density2 <- do.call(rbind.data.frame, lapply(speclist, function(x) {
  dat = get(tolower(as.character(x)))
  dat = dat$D
  dat$species = toupper(as.character(x))
  dat$species = substr(dat$species, 1, 4)
  return(dat)
}))

density2 <- density2[-grep('Total', density2$Label),]

## save the results as a CSV file:
write_csv(density2, here::here(ranch_density))


# ANALYSIS 3: SPP RICHNESS PER POINT------------
cdat <- dat %>% 
  filter(`Distance Bin` <= 100 & substr(Spp, 1, 2) != 'XX' & Spp != 'SPHU') %>%
  select(Point, Spp, Count) %>%
  group_by(Point, Spp) %>%
  summarize(Count = sum(Count)) %>%
  spread(key = Spp, value = Count, fill = 0) %>%
  as.data.frame()

row.names(cdat) <- cdat$Point
cdat$Point = NULL

richest <- vegan::estimateR(cdat) %>% as.data.frame() %>%
  mutate(method = row.names(.)) %>%
  gather(`HOCR-001`:`TOKA-178`, key = 'Point', value = 'n') %>%
  filter(method %in% c('S.obs', 'S.ACE')) %>%
  spread(key = method, value = n) %>%
  mutate(S.ACE = round(S.ACE, digits = 0),
         prop = S.obs/S.ACE) %>%
  # add number of visits per point
  left_join(dat %>% select(Point, Visit) %>% distinct() %>% count(Point),
            by = 'Point')

write_csv(richest, here::here(point_richness))


