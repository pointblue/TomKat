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
                              value %in% c('2FA','2FORB') ~ 'Forb',
                              value == '2GP' ~ 'Perennial Grass',
                              value %in% c('2LICHN','2PLANT','2GRAM') ~ 'Other',
                              FunGrp %in% c('Annual Forb','Perennial Forb') ~ 'Forb',
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
    group_by(Point.Id, Year) %>% 
    count(FunGrp) %>% 
    mutate(cover = n/100*100) %>%
    select(-n) %>%
    rbind(bg) %>%
    pivot_wider(names_from = FunGrp, values_from = cover) %>%
    pivot_longer(`Annual Grass`:`Bare Ground`,names_to='vegtype',values_to = 'cover') %>%
    mutate(cover = replace_na(cover, 0)) %>%
    select(Point.Id,Year,vegtype,cover) %>%
    mutate(vegtype = case_when(vegtype == 'Annual Grass' ~ "AnnualGr",
                               vegtype == 'Perennial Grass' ~ "PereGr",
                               vegtype == 'All Grasses' ~ "Grass",
                               vegtype == 'Bare Ground' ~ "BareGround",
                               .default = vegtype))
  
  return(lpi_sum)
}





  





