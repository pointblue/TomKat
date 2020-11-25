# READ ME-------------
# Clean and process soil sample data

# packages
library(tidyverse)

# input files
rawdat <- 'data_raw/TOKA_soildata_CADC_2014to18.csv'
labdat <- 'data_raw/TOKA_soildata_Lab_2015.csv'
microbedat <- 'data_raw/Bacterial_Richness_For_SOTR.xlsx'

# output files
masterdat <- 'data_clean/TK_soil_main.csv'


# DATA SET UP----------------
# main data set from CADC

dat <- read_csv(here::here(rawdat), col_types = cols()) %>%
  mutate(Date = as.Date(Date, format = '%m/%d/%Y'),
         Year = format(Date, '%Y'),
         Year = as.numeric(recode(Year, '2014' = '2015')), # treat Dec 2014 as same season
         # missing values: (not processed because of PO oil)
         `Bulk Density Dry Wt` = case_when(`Bulk Density Dry Wt` == 0 ~ NA_real_,
                                           TRUE ~ `Bulk Density Dry Wt`),
         # calculate bulk density
         bulk.density.gcm3 = `Bulk Density Dry Wt` / (`Bulk Density Height` * pi * (`Bulk Density Diameter` /
                                                                                   2) ^ 2 - `Bulk Density Rock Vol`),
         # convert water infiltration time from h:m:s to minutes
         water1 = as.numeric(`Water Infiltration Time 1`)/60,
         water2 = as.numeric(`Water Infiltration Time 2`)/60,
         
         # NOTE: two water infiltration tests performed in 2015 (prefer second one); 
         #  but only one performed in 2018 (convert these using an equation)
         # select water infiltration metric based on year:
         water.infil = case_when(Year == 2015 ~ water2,
                                 Year == 2018 ~ exp(0.84 * log(water1) + 1.18)),
         
         # fix missing values reported as 0 (the only zeroes for carbon)
         `Carbon 0-10 cm` = case_when(`Point Name` == 'TOKA-013' & Year == 2015 ~ NA_real_,
                                      TRUE ~ `Carbon 0-10 cm`),
         `Carbon 10-40 cm` = case_when(`Point Name` == 'TOKA-013' & Year == 2015 ~ NA_real_,
                                       TRUE ~ `Carbon 10-40 cm`))



# SUMMARIZE-----------------
# summarize values over 5 samples at each point in each sample year
sdat <- dat %>%
  group_by(`Point Name`, Year) %>%
  summarize(bulk.dens.gcm3 = mean(bulk.density.gcm3, na.rm = T),
            bulk.dens.sd = sd(bulk.density.gcm3, na.rm = T),
            water.infil = mean(water.infil),
            water.infil.sd = sd(water.infil),
            carbonA = mean(`Carbon 0-10 cm`), #only one bulk sample per point so no sd
            carbonB = mean(`Carbon 10-40 cm`),
            depth = mean(`Max Depth`),
            depth.sd = sd(`Max Depth`))


# LAB DATA--------------------
# add additional mineral data from lab
lab <- read_csv(here::here(labdat), col_types = cols()) %>%
  separate(PointID, into = c('Point Name', 'depth.group'), 6) %>%
  mutate(`Point Name` = gsub('TK', 'TOKA', `Point Name`),
         CollectDate = as.Date(CollectDate, format = '%d-%b-%y'), 
         Year = as.numeric(format(CollectDate, '%Y'))) %>%
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


# BACTERIA RICHNESS--------------
bdat <- readxl::read_excel(here::here(microbedat)) %>%
  separate(`sample-id`, into = c('site', 'point', 'depth')) %>%
  unite('point', site:point, sep = '-') %>%
  mutate(depth = recode(depth, '10' = 'richA', '40' = 'richB'),
         Year = 2015) %>%
  spread(key = depth, value = Richness)


# MASTER DATA---------------
mdat <- full_join(sdat, lab, by = c('Point Name', 'Year')) %>%
  full_join(bdat, by = c('Point Name' = 'point', 'Year'))
write_csv(mdat, here::here(masterdat))
