# READ ME----
# Clean & process pasture vegetation data 

# packages
library(tidyverse)

# input files
rawdatpath <- 'data_raw/veg'
diversitydat <- 'data_raw/veg/TOKA_pvegdiversity_2011to19.csv'

# output files
maindat <- 'data_clean/TK_veg_main.csv'
maindivdat <- 'data_clean/TK_veg_main_div.csv'



## VEGDATA SET UP-----------
# NOTE on native, perennial and annual grasses:
# 	2012-2014: absolute %s
# 	2016 to present: RELATIVE %s (need to be multiplied by Grass to make absolute) 

# read in data, change '<1' to 0.5, and '999' to NA, correct relative values
dat <- map_df(list.files(path = here::here(rawdatpath),
                         pattern = 'TOKA_pveg\\d{4}.csv',
                         full.names = TRUE),
              read_csv,
              col_types = cols(Trees = 'c', 
                               Shrubs = 'c',
                               Misc = 'c',
                               Weeds = 'c',
                               NativeGr = 'c',
                               BareGround = 'c')) %>%
  gather(Trees:BareGround, key = 'key', value = 'value') %>%
  mutate(Pasture = as.factor(toupper(Pasture)),
         key = as.factor(key),
         value = case_when(value == '<1' ~ '0.5',
                           value == '999' ~ NA_character_,
                           TRUE ~ value),
         value = as.numeric(value)) %>%
  spread(key = key, value = value) %>%
  # correct for relative numbers:
  mutate_at(.vars = vars(AnnualGr, NativeGr, PereGr), 
            .funs = ~ case_when(
              Year >= 2016 ~ round(./100 * Grass/100 * 100, digits = 0),
              TRUE ~ .))

## check grass totals: "grasstot" doesn't always match "Grass" as reported
dat %>% 
  mutate(grasstot = AnnualGr + PereGr) %>%
  ggplot(aes(x = Grass, y = grasstot)) + geom_point()
## but remaining diffs are few

## PereGr should always be >> NativeGr?
dat %>%
  ggplot(aes(x = PereGr, y = NativeGr)) + geom_point() +
  geom_abline(intercept = 0, slope = 1)
dat %>% filter(NativeGr > PereGr)

## assume Pasture 45 in 2018: Native & PereGr values are switched?
dat <- dat %>%
  mutate(NativeGr = case_when(Pasture == 45 & Year == 2018 ~ 3,
                              TRUE ~ NativeGr),
         PereGr = case_when(Pasture == 45 & Year == 2018 ~ 38,
                            TRUE ~ PereGr))

## check for total cover:
dat %>% 
  mutate(ground = Shrubs + Forbs + Weeds + Grass + BareGround + Misc) %>%
  ggplot(aes(x = as.factor(Year), y = ground)) + geom_boxplot()
## -->Note variation in veg protocol, with total values >>100% in 2016, 2018-19
## values <100% in 2013 & 2014 are apparently due to "thatch" which was 
##   recorded separately from "Misc" starting in 2013


# save to master data in long format:
dat_long <- dat %>%
  gather(AnnualGr:Weeds, key = 'vegtype', value = 'cover')

write_csv(dat_long, here::here(masterdat))

# DIVERSITY DATA SET UP
ddat <- read_csv(here::here(diversitydat), col_types = cols()) %>%
  mutate(herbdiv = totaldiv - shrubdiv - treediv) %>%
  gather(natgdiv:herbdiv, key = 'group', value = 'species')

write_csv(ddat, here::here(masterdivdat))
