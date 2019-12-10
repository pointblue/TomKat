# READ ME-------------
# Clean and process stream data

# packages
library(tidyverse)

# input files
rawdat <- 'data_raw/Pe01 Honsinger Creek streamflow WY2011 - WY2019.xlsx'

# output files
masterdat <- 'data_master/TK_stream_master.csv'
master_daily <- 'data_master/TK_stream_daily_stats.csv'
master_monthly <- 'data_master/TK_stream_monthly_stats.csv'

# MASTER DATA SET UP----------------

dat <- readxl::read_excel(here::here(rawdat), sheet = 2, skip = 4) %>%
  rename(date.time = 'date/time',
         temp.C = 'Water Temp',
         stage.ft = 'Water Depth, ft',
         flow.cfs = 'Streamflow, ft3/s') %>%
  filter(!is.na(date.time)) %>%
  mutate(date.time = format(date.time, format = '%Y-%m-%d %H:%M'),
         date = as.Date(date.time))
  
write_csv(dat, here::here(masterdat))


# DAILY STATS-----------------------
## daily avg, max, & min temp, flow, and stage data

sdat <- dat %>%
  select(-date.time) %>%
  gather(temp.C:flow.cfs, key = 'var', value = 'value') %>%
  mutate(date = as.character(date)) %>%
  group_by(date, var) %>%
  summarize_all(list(~mean(., na.rm = T), 
                     ~max(., na.rm = T), 
                     ~min(., na.rm = T), 
                     ~sum(!is.na(.)))) %>%
  ungroup() %>%
  ## for days with <72 readings (every 15 mins for 18 hours), consider max/min/avg values unknown
  mutate(max = case_when(is.infinite(max) ~ NA_real_,
                         sum < 72 ~ NA_real_,
                         TRUE ~ max),
         min = case_when(is.infinite(min) ~ NA_real_,
                         sum < 72 ~ NA_real_,
                         TRUE ~ min),
         mean = case_when(sum < 72 ~ NA_real_,
                          TRUE ~ mean)) %>%
  select(-sum) 

sdat_wide <- sdat %>%
  gather(mean:min, key = 'key', value = 'value') %>%
  unite('var', var, key) %>%
  spread(key = var, value = value) %>%
  #drop rows missing all data
  filter(!is.na(flow.cfs_mean) | !is.na(stage.ft_mean) | !is.na(temp.C_mean))

write_csv(sdat_wide, here::here(master_daily))


# MONTHLY STATS--------------

mdat <- sdat %>%
  mutate(date = as.Date(date),
         yearmo = format(date, format = '%Y-%m')) %>%
  select(yearmo, var, mean) %>%
  group_by(yearmo, var) %>%
  summarize(n = sum(!is.na(mean)),
            mean = mean(mean, na.rm = T)) %>%
  ungroup() %>%
  # for months with less than 25 days of stats, consider monthly avg unknown
  mutate(mean = case_when(n < 25 ~ NA_real_,
                          TRUE ~ mean)) %>%
  select(-n) %>%
  mutate(mo = substr(yearmo, 6, 7)) 

## calculate long-term averages by month
longterm <- mdat %>%
  select(-yearmo) %>%
  group_by(mo, var) %>%
  summarize(n = sum(!is.na(mean)),
            mean = mean(mean, na.rm = T)) %>%
  ungroup() %>%
  gather(n:mean, key = 'key', value = 'mean') %>%
  unite('var', key, var, sep = '_') %>%
  spread(key = var, value = mean)
## temp and stage have 7 years of monthly readings; flow has 4-6

## calculate monthly differences from long-term average
mdat_final <- mdat %>% 
  spread(key = var, value = mean) %>%
  left_join(longterm %>% select(-n_flow.cfs, -n_stage.ft, -n_temp.C), by = 'mo') %>%
  mutate(diff_flow.cfs = flow.cfs - mean_flow.cfs,
         diff_stage.ft = stage.ft - mean_stage.ft,
         diff_temp.C = temp.C - mean_temp.C) %>%
  mutate(date = as.Date(paste0(yearmo, '-15'))) %>% #create date field for the 15th
  arrange(date) %>%
  select(date, flow.cfs, stage.ft, temp.C, diff_flow.cfs, diff_stage.ft, diff_temp.C) %>%
  # drop rows missing all values:
  filter(!is.na(flow.cfs) | !is.na(stage.ft) | !is.na(temp.C))
  
write_csv(mdat_final, here::here(master_monthly))

