##' @title process_bird_data
##' @description functions for cleaning, analyzing, and mapping TomKat bird data
##' @param path filepath to csv containing point count data
##' @param dat tibble or data.frame containing compiled point count data
##'   (expects fields Spp, Point, Visit, Date, Year)
##'
##' @export
##' @author Kristen Dybala
##'
##'   

compile_bird_data = function(path) {
  read_csv(path, col_types = cols()) %>% 
  mutate(Date = as.Date(Date, format = '%m/%d/%Y'),
         Year = as.numeric(format(Date, '%Y'))) %>%
  unite('Visit', Year, Visit, sep = '-', remove = FALSE) %>%
  # mutate_at(vars(Project:Visit, 
  #                Spp:`Distance Bin`,
  #                `Breeding Status`,
  #                Researcher:`Data Status`), factor) %>%
  # mutate_at(vars(Singing), as.logical) %>%
  filter(!(`Distance Bin ID` %in% c('FLO', 'B30'))) %>%
  mutate_at(vars(`Distance Bin`, `Distance Bin ID`), as.numeric) %>%
  filter(!is.na(`Distance Bin`)) %>%
  # consolidate species codes:
  mutate(Spp = recode(Spp,
                      BSKI = 'WTKI',
                      RSFL = 'NOFL',
                      ORJU = 'DEJU',
                      ECDO = 'EUCD',
                      WIWR = 'PAWR',
                      WESJ = 'CASJ',
                      NWCS = 'WCSP',
                      UNHU = 'XXHU',
                      UNWO = 'XXWO',
                      UNSW = 'XXSW',
                      UNWA = 'XXWA'))
}

count_species = function(dat) {
  dat %>%
    ## exclude unidentified spp, SPHU since ALHU&RUHU are both in there
    filter(!(substr(Spp, 1, 2) %in% c('UN', 'XX') | Spp == 'SPHU')) %>%
    pull(Spp) %>%
    unique() %>% 
    length()
}

count_surveys = function(dat) {
  dat %>% 
    unite('id', Point, Visit) %>%
    pull(id) %>%
    unique() %>% 
    length()
}

calculate_focal_density = function(dat, 
                                   strata = 'Transect', 
                                   dist = 'Distance Bin', 
                                   dist_bin_id = NULL, year = NULL) {

  # filter out juveniles & third visits
  dat = dat %>% filter(`Detection Cue` != 'J') %>% 
    filter(Visit != '2011-3') %>% 
    as.data.frame()
  
  fdat = pbdistance::format_data_flat(dat, strata = strata, year = year,
                                      dist = dist, dist_bin_id = dist_bin_id)

  # fit distance functions and estimate densities per sample point for each
  #  focal species: (skip wcsp for now)
  grsp = pbdistance::fit_distance_models(data = fdat, spec = 'grsp', 
                                         maxdist = 100,
                                         bins = c(0, 30, 50, 70, 80, 90, 100))
  savs = pbdistance::fit_distance_models(data = fdat, spec = 'savs', 
                                         maxdist = 100,
                                         bins = c(0, 30, 50, 70, 80, 90, 100))
  # wcsp = pbdistance::fit_distance_models(data = fdat, spec = 'wcsp', 
  #                                        maxdist = 100,
  #                                        bins = c(0, 10, 20, 40, 70, 100))
  
  # compile density estimates
  purrr::map_dfr(list('grsp' = grsp, 'savs' = savs), 
                            ~.x[['D']], .id = 'species') %>%
    mutate(species = toupper(species)) %>% 
    filter(Label != 'Total')
}

calculate_species_richness = function(dat) {
  # expects field 'id'
  cdat = dat %>% 
    filter(`Distance Bin` <= 100 & substr(Spp, 1, 2) != 'XX' & Spp != 'SPHU') %>%
    select(id, Visit, Spp, Count) %>%
    group_by(id, Visit, Spp) %>%
    summarize(Count = sum(Count), .groups = 'drop') %>%
    pivot_wider(names_from = Spp, values_from = Count, values_fill = 0) %>% 
    select(-Visit)
  
  vegan::specpool(cdat %>% select(-id), 
                  pool = cdat$id, 
                  smallsample = TRUE) %>% 
    rownames_to_column('id')
}

