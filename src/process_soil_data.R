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
        `Point Name` == 'TOKA-013' & Year == 2015 ~ NA_real_,
        TRUE ~ `Carbon 0-10 cm`),
      `Carbon 10-40 cm` = case_when(
        `Point Name` == 'TOKA-013' & Year == 2015 ~ NA_real_,
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
  # select water infiltration metric based on sample year: two water
  # infiltration tests performed in 2015 (prefer second one) but only one
  # performed in 2018 (convert these using an equation)
  df %>% 
    mutate(
      water.infiltration = case_when(
        SampleYear == 2015 ~ `Water Infiltration Time 2`,
        SampleYear == 2018 ~ exp(0.84 * log(`Water Infiltration Time 1`) + 1.18)
      ))
}

summarize_soil_fielddata = function(df) {
  df %>%
    group_by(`Point Name`, SampleYear) %>%
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
  read_csv(path, show_col_types = FALSE) %>%
    separate(PointID, into = c('Point Name', 'depth.group'), 6) %>%
    mutate(`Point Name` = gsub('TK', 'TOKA', `Point Name`),
           CollectDate = as.Date(CollectDate, format = '%d-%b-%y'), 
           SampleYear = as.numeric(format(CollectDate, '%Y'))) %>%
    gather(c(`Olsen P`:`Total Nitrogen`), key = 'var', value = 'value') %>%
    mutate(value = case_when(value == '< 2.0' ~ '1',
                             value == '< 0.090' ~ '0.05',
                             value == 'QNS' ~ NA_character_,
                             TRUE ~ value)) %>%
    unite('var', var, depth.group, sep='') %>%
    select(-CollectDate, -County, -Ranch, -Depth) %>%
    spread(key = 'var', value = 'value') %>%
    mutate_at(vars(CalciumA:SodiumB), as.numeric) %>%
    mutate_at(vars(`Total CarbonA`:`Total NitrogenB`), as.numeric)
}

compile_soil_microbedata = function(path) {
  readxl::read_excel(path) %>%
    separate(`sample-id`, into = c('site', 'point', 'depth')) %>%
    unite('Point Name', site:point, sep = '-') %>%
    mutate(depth = recode(depth, '10' = 'richA', '40' = 'richB'),
           SampleYear = 2015) %>%
    spread(key = depth, value = Richness)
}

calculate_percentiles = function(df) {
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
    filter(SampleYear == max(SampleYear)) %>%
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
    mutate(type = case_when(is.na(type) ~ 'Value',
                            TRUE ~ 'Percentile')) %>%
    pivot_wider(names_from = 'type') 
  
  # add mean percentile info across all metrics at a point
  bind_rows(df,
            df %>% group_by(Point, SampleYear) %>%
              summarize(var = 'mean',
                        Percentile = mean(Percentile, na.rm = T) %>% 
                          round(digits = 0),
                        Value = NA,
                        .groups = 'drop')) %>%
    mutate(var = factor(var, 
                        levels = c('mean', 'bulk.dens.gcm3', 'water.infil', 
                                   'carbonB', 'carbonA'))) %>%
    arrange(Point, var)
}