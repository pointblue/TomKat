# READ ME-------------
# Calculate differences between TomKat and Half Moon Bay current data to be
# be able to estimate TomKat historical data from Half Moon Bay historical data

# packages
library(tidyverse)

# input files
tk_monthly <- 'data_master/TK_weather_master_monthly.csv'
hmb_monthly <- 'data_master/HMB_monthly_weather.csv'
hmb_historic <- 'data_master/HMB_1981-2010_Normals.csv'

# output files
monthly_diffs <- 'data_master/HMB_monthly_diffs.csv'
tk_normals <- 'data_master/TK_weather_estimated_normals.csv'


# MONTHLY DIFFERENCES, 2010-present-------------
tk <- read_csv(here::here(tk_monthly), col_types = cols())
  
hmb <- read_csv(here::here(hmb_monthly), col_types = cols()) %>%
  mutate(date = as.Date(date, format = '%Y-%m-%d') + 14) %>% # change to 15th of month
  select(date, datatype, value) %>%
  spread(key = datatype, value = value) %>%
  # convert units
  mutate(PRCP = PRCP * 0.0393701, # mm to in
         TAVG = TAVG * (9/5) + 32, # C to F
         TMAX = TMAX * (9/5) + 32,
         TMIN = TMIN * (9/5) + 32)

dat <- left_join(tk, hmb, by = 'date') %>%
  mutate(diff.TMAX = temp.max - TMAX,
         diff.TMIN = temp.min - TMIN,
         diff.PRCP = rain.in - PRCP)
summary(dat)
# diff.TMAX: mean = -1.78 (-6.6 to 5.1); TK max generally cooler
# diff.TMIN: mean = +2.08 (-2.24-7.72); TK mins generally warmer
# diff.PRCP: mean = +0.26 (-3.1 - 5.7); TK precip fairly similar?

# seasonal patterns:
dat %>% 
  mutate(mo = as.numeric(format(date, '%m'))) %>%
  select(diff.TMAX:mo) %>%
  gather(diff.TMAX:diff.PRCP, key = var, value = value) %>%
  ggplot(aes(as.factor(mo), value)) + geom_boxplot() + 
  facet_wrap(~var, scales = 'free_y', ncol = 1) + 
  geom_hline(aes(yintercept=0), color='red')

## max temps: TK cooler than HMB much of year
## min temps: TK warmer than HMB much of year, except Jul-Aug
## precip: TK somewhat wetter, esp. in Dec-Jan

## CONCLUSION: Half Moon Bay & TomKat have similar weather overall. TomKat 
## tends to get more precipitation and to have a narrower range of min/max 
## temperatures, but temperature differences vary seasonally.

## calculate monthly correction factors:
diff <- dat %>%
  mutate(mo = format(date, '%m')) %>%
  group_by(mo) %>%
  summarize(diff.TMAX = median(diff.TMAX, na.rm = T),
            diff.TMIN = median(diff.TMIN, na.rm = T),
            diff.PRCP = median(diff.PRCP, na.rm = T))

write_csv(diff, here::here(monthly_diffs))


# TK WEATHER NORMALS, 1981-2010-----------------
## estimate from current monthly correction factors, applied to Half Moon Bay
## normals, 1981-2010 (downloaded from National Climate Data Center)

hdat <- read.csv(here::here(hmb_historic)) %>%
  rename(month = MONTH,
         PRCP = PRECIP..IN.,
         TMIN = MIN.TMP...F.,
         TAVG = AVG.TMP...F.,
         TMAX = MAX.TMP...F.) %>%
  gather(PRCP:TMAX, key = var, value = hmb)

tk_est <- full_join(hdat,
                    diff %>% 
                      gather(diff.TMAX:diff.PRCP, key = var, value = diff) %>%
                      mutate(var = gsub('diff.', '', var),
                             mo = as.numeric(as.character(mo))), 
                    by = c('var', 'month' = 'mo')) %>%
  filter(var != 'TAVG') %>%
  mutate(tk.estimated = hmb + diff)

ggplot(tk_est, aes(month, hmb)) + geom_line() + 
  geom_line(aes(y = tk.estimated), linetype = 'dashed') + 
  facet_wrap(~var, scales = 'free_y')
## as expected, corrected values for TMIN generally higher, TMAX generally 
##  lower, PRCP similar

tk_est %>% 
  select(month, var, tk.estimated) %>%
  spread(key = var, value = tk.estimated) %>%
  write_csv(here::here(tk_normals))



# ## calculate monthly differences from corrected historic averages
# mdat = read.csv('data/output/TK_weather_monthly_stats.csv')
# mdat$date = as.Date(mdat$date)
# mdat$month = as.numeric(format(mdat$date, '%m'))
# mdat = reshape2::melt(mdat[,c('month','date','max.temp','min.temp','avg.temp','avg.temp2','min.rain')], id.vars=c('month','date'))
# mdat$variable = plyr::revalue(mdat$variable, c('max.temp'='TMAX', 'min.temp'='TMIN', 'avg.temp'='TAVG',
#                                                'avg.temp2'='TAVG2', 'min.rain'='PRCP'))
# dat = merge(mdat, hmb[,c('month','variable','corrected')], by=c('month','variable'), all.x=T)
# dat$diff = dat$value - dat$corrected
# dat = reshape2::dcast(dat, date~variable, value.var='diff')
# write.csv(dat, 'data/output/TK_weather_historicdiff.csv', row.names=F)


