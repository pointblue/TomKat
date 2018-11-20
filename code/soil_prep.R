# READ ME-------------
# Clean and process soil sample data

# packages
library(tidyverse)

# input files
rawdat <- 'data_raw/TOKA_soildata_CADC_2014to18.csv'
labdat <- 'data_raw/TOKA_soildata_Lab_2015.csv'

# output files
masterdat <- 'data_master/TK_soil_master.csv'


# DATA SET UP----------------
# main data set from CADC
dat <- read_csv(here::here(rawdat)) %>%
  mutate(Date = as.Date(Date, format = '%m/%d/%Y'),
         Year = as.factor(format(Date, '%Y')),
         Year = recode(Year, '2014' = '2015'), # treat Dec 2014 as same season
         # missing values: (not processed because of PO oil)
         `Bulk Density Dry Wt` = case_when(`Bulk Density Dry Wt` == 0 ~ NA_real_,
                                           TRUE ~ `Bulk Density Dry Wt`),
         # calculate bulk density
         bulk.density.gcm3 = `Bulk Density Dry Wt` / (`Bulk Density Height` * pi * (`Bulk Density Diameter` /
                                                                                   2) ^ 2 - `Bulk Density Rock Vol`),
         # convert water infiltration time from h:m:s to seconds
         water1 = as.numeric(`Water Infiltration Time 1`),
         water2 = as.numeric(`Water Infiltration Time 2`),
         
         # fix missing values reported as 0 (the only zeroes for carbon)
         `Carbon 0-10 cm` = case_when(`Point Name` == 'TOKA-013' & Year == 2015 ~ NA_real_,
                                      TRUE ~ `Carbon 0-10 cm`),
         `Carbon 10-40 cm` = case_when(`Point Name` == 'TOKA-013' & Year == 2015 ~ NA_real_,
                                       TRUE ~ `Carbon 10-40 cm`))

# summarize values over 5 samples at each point in each sample year
sdat <- dat %>%
  group_by(`Point Name`, Year) %>%
  summarize(bulk.dens.gcm3 = mean(bulk.density.gcm3, na.rm = T),
            bulk.dens.sd = sd(bulk.density.gcm3, na.rm = T),
            water.infil = mean(water1),
            water.infil.sd = sd(water1),
            carbonA = mean(`Carbon 0-10 cm`), #only one bulk sample per point so no sd
            carbonB = mean(`Carbon 10-40 cm`),
            depth = mean(`Max Depth`),
            depth.sd = sd(`Max Depth`))

# additional mineral data from lab
lab <- read_csv(here::here(labdat)) %>%
  separate(PointID, into = c('Point Name', 'depth.group'), 6) %>%
  mutate(`Point Name` = gsub('TK', 'TOKA', `Point Name`),
         CollectDate = as.Date(CollectDate, format = '%d-%b-%y'), 
         Year = as.factor(format(CollectDate, '%Y'))) %>%
  gather(c(`Olsen P`:`Total Nitrogen`), key = 'var', value = 'value') %>%
  mutate(value = case_when(value == '< 2.0' ~ '1',
                           value == '< 0.090' ~ '0.05',
                           value == 'QNS' ~ NA_character_,
                           TRUE ~ value)) %>%
  unite('var', var, depth.group, sep='') %>%
  select(-CollectDate, -County, -Ranch, -Depth) %>%
  spread(key = 'var', value = 'value') %>%
  mutate_at(vars(CalciumA:SodiumB), as.numeric) %>%
  mutate_at(vars(`Total CarbonA`:`Total NitrogenB`), as.numeric)

# master data
mdat <- full_join(sdat, lab, by = c('Point Name', 'Year'))
write_csv(mdat, masterdat)
