library(janitor)

compile_veg_data<-function(dat){
  
  lpi <- read.csv(dat) %>%
    mutate(Date = as.Date(Date, format="%m/%d/%Y"),
           Year = lubridate::year(Date)) %>%
    select(Point.Id, Point.Index, Direction,Year, Canopy1, Canopy2, Top.Layer, starts_with("Lower"), Soil.Surface) 
  
  plants <- read.csv("data_raw/veg/CAPlantsv3.csv") %>% 
    select(-X) %>%
    select(Symbol, FunGrp, Scientific.Name, Common.Name)
  
  lpi_clean <- lpi %>% 
    janitor::remove_empty(which = "rows") %>% 
    pivot_longer(c(Top.Layer, Canopy1, Canopy2, starts_with("Lower"), Soil.Surface)) %>% 
    filter(!is.na(value)  & !value %in% c("L", "S", "AM", "R", "M", "NOPLANT", "EM", 'WL',"",'2LTR')) %>%
    left_join(plants[,c("Symbol","FunGrp")], by = c('value' = 'Symbol')) %>%
    mutate(FunGrp = case_when(value == '2GA' ~ "Annual Grass",
                              value %in% c('2FA','2FORB') ~ 'Annual Forb',
                              value == '2GP' ~ 'Perennial Grass',
                              value %in% c('2LICHN','2PLANT','2GRAM') ~ 'Other',
                              .default = FunGrp)) %>%
    group_by(Point.Id, Year, Point.Index,Direction) %>% 
    distinct(FunGrp)
  
  #Calculate % cover including bareground
  
  bg <- lpi_clean %>%
    group_by(Point.Id, Year) %>% 
    summarise(n=n_distinct(Point.Index,Direction)) %>%
    mutate(cover = (100-n)/100*100,
           FunGrp = "Bare Ground") %>%
    select(Point.Id,Year,FunGrp,cover)
  
  lpi_sum <- lpi_clean %>%
    group_by(Point.Id, Year, Direction) %>% 
    count(FunGrp) %>% 
    mutate(cover = n/100*100) %>%
    select(-n) %>%
    rbind(bg)
  
  #Match points to field polygons and take average of field polygons
  
  shp_poly <- st_read(here::here('GIS'), 'TK_veg_fields', quiet = TRUE) %>%
    st_transform('+proj=longlat +datum=WGS84') 
  
  pasture_labs <- st_read(here::here('GIS'), 'TOKA_point_count_grid', quiet = TRUE) %>%
    st_transform('+proj=longlat +datum=WGS84') %>%
    filter(Name %in% unique(lpi_sum$Point.Id)) %>%
    st_join(shp_poly,join=st_intersects) %>%
    as.data.frame() %>%
    select(Name,Pasture)
  
  pasture_vals <- lpi_sum %>%
    left_join(pasture_labs, by = c('Point.Id' = 'Name')) %>%
    filter(!is.na(Pasture)) %>%
    pivot_wider(names_from = FunGrp, values_from = cover) %>%
    mutate(`All Grasses` = `Annual Grass` + `Perennial Grass`,
           Forbs = `Annual Forb` + `Perennial Forb`) %>%
    pivot_longer(`Annual Forb`:Forbs,names_to='vegtype') %>%
    mutate(value = replace_na(value, 0)) %>%
    group_by(Year, Pasture,vegtype) %>%
    summarise(cover = mean(value)) %>%
    ungroup() %>%
    select(Pasture,Year,vegtype,cover) %>%
    filter(!vegtype %in% c('Annual Forb','Perennial Forb')) %>%
    mutate(vegtype = case_when(vegtype == 'Annual Grass' ~ "AnnualGr",
                               vegtype == 'Perennial Grass' ~ "PereGr",
                               vegtype == 'All Grasses' ~ "Grass",
                               vegtype == 'Bare Ground' ~ "BareGround",
                               .default = vegtype))
  
  return(pasture_vals)
}





  





