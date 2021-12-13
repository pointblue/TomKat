##' @title process_stream_data
##' @description functions for cleaning, analyzing, and summarizing TomKat
##'   stream flow data
##' @param path filepath to xlsx containing raw stream data
##' @param dat tibble or data.frame containing compiled point count data
##'   (expects fields Spp, Point, Visit, Date, Year)
##'
##' @export
##' @author Kristen Dybala
##'
##'   
compile_stream_data = function(dir) {
  fl = list.files(dir, '.csv', full.names = TRUE)
  purrr::map_df(fl, read_csv, show_col_types = FALSE) %>% 
    rename(date.time = 'date/time',
           temp.C = 'Water Temp',
           stage.ft = 'Water Depth, ft',
           flow.cfs = 'Streamflow, ft3/s') %>%
    filter(!is.na(date.time)) %>%
    mutate(date.time = format(date.time, format = '%Y-%m-%d %H:%M'),
           date = as.Date(date.time))
}

calculate_stream_daily = function(dat) {
  sdat <- dat %>%
    select(-date.time) %>%
    pivot_longer(temp.C:flow.cfs) %>%
    mutate(date = as.character(date)) %>%
    group_by(date, name) %>%
    summarize_all(list(~mean(., na.rm = T), 
                       ~max(., na.rm = T), 
                       ~min(., na.rm = T), 
                       ~sum(!is.na(.)))) %>%
    ungroup() %>%
    ## for days with <72 readings (every 15 mins for 18 hours), consider
    ## max/min/avg values unknown
    mutate(max = case_when(is.infinite(max) ~ NA_real_,
                           sum < 72 ~ NA_real_,
                           TRUE ~ max),
           min = case_when(is.infinite(min) ~ NA_real_,
                           sum < 72 ~ NA_real_,
                           TRUE ~ min),
           mean = case_when(sum < 72 ~ NA_real_,
                            TRUE ~ mean)) %>%
    select(-sum) %>% 
    filter(date != '2011-07-06') %>% #missing data for all 3 vars
    mutate(name = factor(name, 
                         levels = c('temp.C', 'flow.cfs', 'stage.ft')),
           date = as.Date(date)) %>% 
    arrange(name, date)
}

calculate_stream_monthly = function(dat) {
  mdat <- dat %>%
    mutate(yearmo = format(date, format = '%Y-%m')) %>%
    group_by(name, yearmo) %>%
    summarize(n = sum(!is.na(mean)),
              mean = mean(mean, na.rm = T),
              .groups = 'drop') %>%
    # for months with less than 25 days of stats, consider monthly avg unknown
    mutate(mean = case_when(n < 25 ~ NA_real_,
                            TRUE ~ mean)) %>%
    select(-n) %>%
    mutate(mo = substr(yearmo, 6, 7))
  
  ## calculate long-term averages by month
  longterm <- mdat %>% 
    group_by(name, mo) %>%
    summarize(n = sum(!is.na(mean)),
              ltmean = mean(mean, na.rm = T),
              .groups = 'drop')
  ## -->flow has fewer monthly readings than temp and stage
  
  ## calculate monthly differences from long-term average
  mdat %>% 
    left_join(longterm %>% select(-n), by = c('mo', 'name')) %>% 
    mutate(diff = mean - ltmean,
           date = as.Date(paste0(yearmo, '-15'))) %>% #create date field for the 15th
    arrange(name, date) %>% 
    select(name, date, mean, ltmean, diff)  
}