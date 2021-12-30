#' Process soil data
#' @description functions for cleaning, analyzing, and summarizing TomKat soil
#'   data from the field, lab, and microbial analyses
#'
#' @param path file path to raw input data
#' @param df tibble
#'
#' @return
#' @export
#'
#' @examples
compile_soil_fielddata = function(path) {
  read_csv(path, show_col_types = FALSE) %>% 
    rename(Point = 'Point Name') %>% 
    mutate(Date = as.Date(Date, format = '%m/%d/%Y'),
           Year = format(Date, '%Y') %>% as.numeric(),
           Month = format(Date, '%m') %>% as.numeric(),
           SampleYear = if_else(Month < 12, Year, Year + 1), # i.e., Dec 2014 and Jan 2015 as same season
           # missing values: (not processed because of PO oil)
           `Bulk Density Dry Wt` = case_when(`Bulk Density Dry Wt` == 0 ~ NA_real_,
                                             TRUE ~ `Bulk Density Dry Wt`)) %>% 
    # convert water infiltration time from h:m:s to minutes
    mutate(across(c('Water Infiltration Time 1':'Water Infiltration Time 3'),
                  ~as.difftime(., units = 'mins') %>% as.numeric())) %>% 
    # fix missing carbon values reported as 0 (the only zeroes for carbon)
    mutate(
      `Carbon 0-10 cm` = case_when(
        Point == 'TOKA-013' & Year == 2015 ~ NA_real_,
        TRUE ~ `Carbon 0-10 cm`),
      `Carbon 10-40 cm` = case_when(
        Point == 'TOKA-013' & Year == 2015 ~ NA_real_,
        TRUE ~ `Carbon 10-40 cm`)
      )
}

calculate_bulk_density = function(df) {
  df %>% 
    mutate(bulk.density.gcm3 = `Bulk Density Dry Wt` / 
             (`Bulk Density Height` * pi * 
                (`Bulk Density Diameter` / 2) ^ 2 - `Bulk Density Rock Vol`))
}

calculate_water_infiltration = function(df) {
  # select second water infiltration attempt, even though in most years only 
  # one water infiltration time was recorded; if not T2, convert from T1 using
  # an equation: 
  df %>% 
    mutate(
      water.infiltration = if_else(
        !is.na(`Water Infiltration Time 2`), 
        `Water Infiltration Time 2`,
        exp(1.18 + 0.84 * log(`Water Infiltration Time 1`))
      )
    )
}

summarize_soil_fielddata = function(df) {
  df %>%
    group_by(Point, SampleYear) %>%
    summarize(bulk.dens.gcm3 = mean(bulk.density.gcm3, na.rm = T),
              bulk.dens.sd = sd(bulk.density.gcm3, na.rm = T),
              water.infil = mean(water.infiltration),
              water.infil.sd = sd(water.infiltration),
              carbonA = mean(`Carbon 0-10 cm`), #only one bulk sample per point so no sd
              carbonB = mean(`Carbon 10-40 cm`),
              depth = mean(`Max Depth`),
              depth.sd = sd(`Max Depth`),
              .groups = 'drop')
}

compile_soil_labdata = function(path) {
  read_csv(path, col_types = c('ccccccccccccc')) %>% #read everything in as text and convert later
    separate(PointID, into = c('Point', 'depth.group'), 6) %>%
    mutate(Point = gsub('TK', 'TOKA', Point),
           CollectDate = as.Date(CollectDate, format = '%m/%d/%Y'), 
           SampleYear = as.numeric(format(CollectDate, '%Y'))) %>%
    select(Point, SampleYear, depth.group, starts_with('Olsen'):last_col()) %>% 
    pivot_longer(c(starts_with('Olsen'):last_col())) %>%
    mutate(name = gsub(' ', '_', name),
           value = case_when(value == '< 2.0' ~ '1',
                             value == '< 0.090' ~ '0.05',
                             value == 'QNS' ~ NA_character_,
                             TRUE ~ value),
           value = as.numeric(value)) %>%
    unite('name', name, depth.group, sep='_') %>%
    pivot_wider()
}

compile_soil_microbedata = function(path) {
  readxl::read_excel(path) %>%
    separate(`sample-id`, into = c('site', 'point', 'depth')) %>%
    unite('Point', site:point, sep = '-') %>%
    mutate(depth = recode(depth, '10' = 'richA', '40' = 'richB'),
           SampleYear = 2015) %>%
    spread(key = depth, value = Richness)
}

compile_phyladat = function(path) {
  readxl::read_excel(path) %>%
    filter(Sample_ID != 'avg') %>% #get rid of last line
    gather(-Sample_ID, key = 'phylum', value = 'prop') %>%
    separate(Sample_ID, into = c('Site', 'Point', 'Depth')) %>%
    unite('Point', Site:Point, sep = '-') %>%
    mutate(depth = recode(Depth, '10' = 'richA', '40' = 'richB'),
           group = case_when(phylum %in% c('Bacteroidetes', 'Actinobacteria', 'Firmicutes') ~ 'copiotrophs',
                             phylum %in% c('Acidobacteria', 'Verrucomicrobia') ~ 'oligotrophs',
                             TRUE ~ 'other'),
           group = factor(group, levels = c('copiotrophs', 'other', 'oligotrophs'))) %>%
    arrange(Point, Depth, group, phylum) %>%
    mutate(phylum = factor(phylum, levels = unique(phylum)[c(1:6, 8:11, 7)]))
}

calculate_productivity_metrics = function(df) {
  # create cumulative distribution functions for each metric based on all years
  # combined (to be able to show change in percentiles over years)
  f1 <- ecdf(df$carbonA)
  f2 <- ecdf(df$carbonB)
  #reverse so lower density = higher score
  f3 <- ecdf(max(df$bulk.dens.gcm3, na.rm = T) - df$bulk.dens.gcm3) 
  #reverse so shorter infiltration time = higher score
  f4 <- ecdf(max(df$water.infil, na.rm = T) - df$water.infil)
  
  # calculate percentile scores from these functions:
  df = df %>%
    # filter(SampleYear == max(SampleYear)) %>%
    mutate(carbonA_perc = round(f1(carbonA) * 100, digits = 0),
           carbonB_perc = round(f2(carbonB) * 100, digits = 0),
           bulk.dens.gcm3_perc = round(
             f3(max(bulk.dens.gcm3, na.rm = T) - bulk.dens.gcm3) * 100, 
             digits = 0),
           water.infil_perc = round(
             f4(max(water.infil, na.rm = T) - water.infil) * 100, 
             digits = 0)) %>%
    pivot_longer(bulk.dens.gcm3:water.infil_perc) %>% 
    separate(name, into = c('var', 'type'), sep = '_', fill = 'right') %>%
    mutate(type = case_when(is.na(type) ~ 'value',
                            TRUE ~ 'percentile')) %>%
    pivot_wider(names_from = 'type') 
  
  # add mean percentile info across all metrics at a point
  bind_rows(df,
            df %>% group_by(Point, SampleYear) %>%
              summarize(var = 'mean',
                        percentile = mean(percentile, na.rm = T) %>% 
                          round(digits = 0),
                        value = NA,
                        .groups = 'drop')) %>%
    mutate(var = factor(var, 
                        # the order they'll be in the pop up tables:
                        levels = c('bulk.dens.gcm3', 'water.infil', 
                                   'carbonA', 'carbonB', 'mean')),
           depth = if_else(var == 'carbonB', 'B', 'A')) %>%
    arrange(Point, var)
}

format_soil_productivity_metrics = function(df) {
  df %>% 
    mutate(value = if_else(var == 'mean', percentile, value),
           value_round = if_else(var == 'mean', '', txtRound(value, digits = 2)),
           # labels within map layer control
           maplayer = recode(var,
                             bulk.dens.gcm3 = 'Bulk density',
                             water.infil = 'Water infiltration',
                             carbonA = '% Carbon',
                             carbonB = '% Carbon',
                             mean = 'Overall score'),
           # each set of distinct points to be plotted:
           pointlayer = var,
           # default 
           point_weight = 1,
           # smaller point size for surface carbon points
           point_radius = if_else(var == 'carbonA', 3.5, 9),
           # rownames within popup tables
           table_rowname = recode(var,
                                  bulk.dens.gcm3 = 'Bulk density (g/cm<sup>3</sup>)',
                                  water.infil = 'Water Infiltration (min)',
                                  carbonA = '% Carbon (0-10cm)', 
                                  carbonB = '% Carbon (10-40cm)',
                                  mean = 'Overall score'),
           # default:
           table_caption = '',
           legend_title = recode(var,
                                 bulk.dens.gcm3 = 'Bulk density<br>(g/cm<sup>3</sup>)',
                                 water.infil = 'Water<br>Infiltration<br>(min)',
                                 carbonA = '% Carbon', 
                                 carbonB = '% Carbon',
                                 mean = 'Overall score<br>(percentile)')
           )
    
}


calculate_productivity_change = function(df, current, baseline, difflabel = 'Difference') {
  df %>% 
    mutate(yr = case_when(SampleYear == current ~ 'current',
                          SampleYear == baseline ~ 'baseline',
                          TRUE ~ as.character(SampleYear))) %>% 
    select(Point, var, depth, yr, value, maplayer, starts_with('point'), 
           starts_with('table')) %>% 
    pivot_wider(names_from = yr, values_from = value) %>% 
    mutate(diff = current - baseline) %>% 
    select(-table_rowname) %>% # replace old table rowname
    pivot_longer(!(Point:table_caption), names_to = 'table_rowname') %>% 
    # relabel back with their corresponding years (as rownames in popup tables)
    mutate(table_rowname = recode(table_rowname, 
                                  'current' = as.character(current), 
                                  'baseline' = as.character(baseline),
                                  'diff' = difflabel),
           value_round = if_else(
             var == 'mean',
             txtRound(value, digits = 0, txt.NA = 'NA'),
             txtRound(value, digits = 2, txt.NA = 'NA')),
           value_round = case_when(
             grepl('diff', table_rowname, ignore.case = TRUE) & 
               value > 0 ~ paste0('+', value_round),
             TRUE ~ value_round))
}

format_soil_nutrients = function(df) {
  df %>% pivot_longer(!(Point:SampleYear), names_to = 'pointlayer') %>%
    filter(!is.na(value)) %>%
    separate(pointlayer, into = c('var', 'depth'), sep = -1, remove = FALSE) %>% 
    mutate(var = gsub('_$', '', var),
           var = gsub('_', ' ', var),
           value_round = if_else(var == 'pH',
                                 txtRound(value, digits = 1, txt.NA = 'NA'),
                                 txtRound(value, digits = 2, txt.NA = 'NA')),
           # labels within map layer control
           maplayer = case_when(
             grepl('Nitrogen', var) ~ 'Total Nitrogen (N)',
             grepl('Potassium', var) ~ 'Potassium (K)',
             grepl('Sodium', var) ~ 'Sodium (Na)',
             grepl('Magnesium', var) ~ 'Magnesium (Mg)',
             grepl('Calcium', var) ~ 'Calcium (Ca)',
             grepl('pH', var) ~ 'pH',
             grepl('CEC', var) ~ 'Cation Exchange Capacity (CEC)',
             grepl('Olsen', var) ~ 'Extractable Phosphorus (P)'),
           # default
           point_weight = 1,
           # additional point size formatting for surface vs. deeper layers
           point_radius = if_else(depth == 'A', 3.5, 9),
           # pop up table header (units)
           table_header = case_when(
             grepl('Nitrogen', var) ~ '(%)',
             grepl('Potassium|Sodium|Calcium|Magnesium', var) ~ '(cmol (+)/kg)',
             grepl('pH', var) ~ 'pH',
             grepl('CEC', var) ~ '(Sum of Cations<br>me/100g)',
             grepl('Olsen', var) ~ '(ppm)'),
           # rownames within popup tables
           table_rowname = recode(depth,
                                  A = '0-10 cm',
                                  B = '10-40 cm'),
           # label above rownames:
           table_rowheader = case_when(
             grepl('Nitrogen', var) ~ 'Total Nitrogen',
             grepl('Potassium', var) ~ 'Potassium',
             grepl('Sodium', var) ~ 'Sodium',
             grepl('Magnesium', var) ~ 'Magnesium',
             grepl('Calcium', var) ~ 'Calcium',
             grepl('pH', var) ~ '',
             grepl('CEC', var) ~ 'Cation Exchange<br>Capacity',
             grepl('Olsen', var) ~ 'Extractable<br>Phosphorus'),
           # default:
           table_caption = '',
           # legend titles (with line breaks and units?)
           legend_title = if_else(
             maplayer == 'pH',
             table_header,
             paste0(table_rowheader,'<br>', table_header))
           )
}