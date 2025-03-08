# READ ME-------------
# Clean and process pasture map management data
# Goal is to present:
#  -	Grazing intensity ((animal units * days grazed)/acre)
#  -	Total days grazed 

compile_mgt_data<-function(dat, season){
  fields <- readxl::excel_sheets(here::here(dat))
  dat1 <- map_dfr(fields[2:length(fields)],
                  ~ readxl::read_excel(path = here::here(dat), 
                                     sheet = .x, skip = 16, col_names=TRUE, col_types = "text") %>%
                    mutate(field = .x)) %>%
    mutate(`Start date` = convertToDateTime(`Start date`),
         `End date` = convertToDateTime(`End date`)) %>%
    rename(Start.Date = `Start date`, 
           End.Date = `End date`,
           Tot_weight = `Total weight (lbs)`) %>%
    filter(Activity == "Grazing Event",
           Tot_weight > 0) %>%
    mutate(days = difftime(End.Date,Start.Date,units="days"),
           season = season,
           Tot_weight = as.numeric(Tot_weight),
           n = Tot_weight/1000) %>%
    select(field, Start.Date, End.Date, Herd, n, days, season)
  return(dat1)
}

ada_calculations<-function(dat){
  shp_poly <- st_read(here::here('GIS'), 'TK_veg_fields', quiet = TRUE) 
  dat1<-dat %>%
    mutate(n=as.numeric(n),
           animal.days = n*days) %>%
    group_by(season, Pasture) %>%
    summarize(AD = sum(animal.days)) %>%
    ungroup() %>%
    left_join(shp_poly %>% st_set_geometry(NULL) %>% select(Pasture, area_ha), 
              by = c('Pasture')) %>%
    mutate(area_acres = area_ha * 2.47105) %>%
    mutate(ADA = AD / area_acres) %>%
    select(season, Pasture, ADA) %>%
    arrange(Pasture, season)
  return(dat1)
}

tgd_calculations<-function(dat){
  tgd <- data.frame(Pasture = rep(unique(dat$Pasture),each=length(unique(dat$season))),
                    season = unique(dat$season),
                    tgd=NA)
  for(i in unique(dat$Pasture)){
    df <- dat[dat$Pasture==i,]
    for(s in unique(df$season)){
      df_s <- df[df$season==s,]
      all_times<-vector()
      for(j in c(1:nrow(df_s))){
        times_in_range<- seq(df_s[j,]$Start.Date,df_s[j,]$End.Date, by="min")
        all_times <- c(all_times, times_in_range)
      }
      tgd[tgd$Pasture==i & tgd$season==s,]$tgd <- length(unique(all_times))/60/24
      }
  }
  return(tgd)
}

mgmt_calculations<-function(dat){
  dat1<- tgd_calculations(dat) %>%
    left_join(ada_calculations(dat),by=c('Pasture','season')) %>%
    mutate_at(c(3:4), ~replace(., is.na(.), 0)) %>%
    pivot_wider(id_cols ='Pasture', names_from = 'season', values_from = c('tgd','ADA')) %>%
    mutate(tgd_Total = tgd_Dormant + tgd_Growing,
           ADA_Total = ADA_Dormant + ADA_Growing) %>%
    pivot_longer(cols = -Pasture, names_to = c('.value','season'),
                 names_sep = '_')
  return(dat1)
}
