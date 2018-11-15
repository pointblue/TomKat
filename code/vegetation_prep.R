# READ ME----
# Clean & process pasture vegetation data 

# packages
library(tidyverse)

# input files
rawdat <- 'data_raw/TOKA_PVEG_2011_2018.xlsx'

# output files
masterdat <- 'data_master/TK_veg_master.csv'



## DATA SET UP-----------

# read in data, change '<1' to 0.5, and '999' to NA
dat <- readxl::read_excel(here::here(rawdat), 'AllYrs') %>%
  rename(PereGr = 'PerreGr') %>%
  gather(Shrubs:BareGround, key = group, value = value) %>%
  mutate(value = case_when(value == '<1' ~ '0.5',
                           value == '999' ~ NA_character_,
                           TRUE ~ value),
         value = as.numeric(value),
         Year = as.numeric(Year)) %>%
  spread(key = group, value = value)

# ## check grass totals: "grasstot" doesn't always match "Grass" as reported
# dat %>% mutate(grasstot = AnnualGr + PereGr) %>% 
#   ggplot(aes(x = Grass, y = grasstot)) + geom_point()
# 
# dat %>% mutate(grasstot = AnnualGr + PereGr) %>% 
#   filter(grasstot == 100 & Grass<100) %>% select(Year) %>% summary()
# # Note: since 2016, AnnualGr, PereGr, and NativeGr were reported relative to
# #  Grass (total grass cover)

# recalculate in terms of absolute cover:
dat_abs <- dat %>%
  mutate(grasstot = AnnualGr + PereGr,
         AnnualGr = case_when(Year >= 2016 & grasstot == 100 & Grass < 100 ~ AnnualGr/ 100 * Grass,
                              TRUE ~ AnnualGr),
         PereGr = case_when(Year >= 2016 & grasstot == 100 & Grass < 100 ~ PereGr/ 100 * Grass,
                            TRUE ~ PereGr),
         NativeGr = case_when(Year >= 2016 & grasstot == 100 & Grass < 100 ~ AnnualGr/ 100 * Grass,
                              TRUE ~ NativeGr))

# ## a few remaining grasstot do not match, but relatively small differences
# dat_abs %>% mutate(grasstot = AnnualGr + PereGr) %>% 
#   ggplot(aes(x = Grass, y = grasstot)) + geom_point()
# 
# dat_abs %>% mutate(grasstot = AnnualGr + PereGr) %>% filter(Grass - grasstot > 1)

# ## check overall totals for groundcover:
# dat_abs %>% mutate(ground = Shrubs + Forbs + Weeds + Grass + BareGround) %>%
#   select(ground) %>% summary() # 22% to 107%
# 
# dat_abs %>% mutate(ground = Shrubs + Forbs + Weeds + Grass + BareGround) %>%
#   ggplot(aes(x = as.factor(Year), y = ground)) + geom_boxplot()

## -->Note variation in veg protocol, with total values >>100% in 2016 and 2018
## values <100% in 2013 & 2014 are apparently due to "thatch" which was 
##   recorded separately from "Misc" starting in 2013


## check that there is 1 row of data for each pasture by year (and no duplicates):
dat_abs$Pasture = toupper(dat_abs$Pasture) #inconsistency in lower/uppercase pasture names
# table(dat_abs$Pasture, dat_abs$Year)


# save to master data in long format:
dat_long <- dat_abs %>%
  gather(AnnualGr:Weeds, key = 'vegtype', value = 'cover')
write_csv(dat_long, here::here(masterdat))

