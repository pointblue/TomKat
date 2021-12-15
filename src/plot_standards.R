##'@title plot_standards
##'@description helper functions for saving widgets, rendering Rmds, and
##'  accessing standard color palettes, fonts, and logos
##'@param widget html widget, such as created by plot_ly or leaflet
##'@param pathin filepath to Rmd used to create html page
##'@param pathout filepath to html file where widget or web page is saved
##'@param title character string giving title of html widget
##'@param selfcontained,libdir passed to \code{\link{htmlwidgets::saveWidget}}
##'
##'@export
##'@author Kristen Dybala
##'
##'  


logo <- 'https://data.pointblue.org/apps/assets/images/pb-logo-full.png'

standardfonts <- list(
  family = 'Lato, sans-serif',
  size = 14
)

save_widget = function(widget, pathout, selfcontained = TRUE, libdir = NULL,
                       title) {
  htmlwidgets::saveWidget(widget,
                          here::here(pathout),
                          selfcontained = selfcontained,
                          libdir = libdir,
                          title = title)
}

tk.palette <- c('#3b4035', '#9c8755', '#61655c', '#d1bc8b', '#40696f', 
                '#2e5150', '#5f5131', '#9e513a')

pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa', '#bfd730',
                       '#a7a9ac', '#666666',
                       #add a few more complementary colors:
                       '#456d28', '#b74374', '#5e2a84', '#d2c921')

create_palettes = function(df, set) {
  if (set == 'soil_productivity') {
    return(
      list(
        'Overall score' = colorBin(
          palette = colorRamp(colors = c('#ffffff', pointblue.palette[4])),
          domain = df %>% filter(maplayer == 'Overall Score') %>% pull(value),
          bins = c(0, 20, 40, 60, 80, 100),
          na.color = pointblue.palette[6]),
        
        'Bulk density' = colorBin(
          palette = colorRamp(colors = c(pointblue.palette[3], '#ffffff')), #reverse
          domain = df %>% filter(maplayer == 'Bulk density') %>% pull(value),
          bins = c(0.7, 0.9, 1.1, 1.3, 1.5),
          na.color = pointblue.palette[6]),
        
        'Water infiltration' = colorBin(
          palette = colorRamp(colors = c(pointblue.palette[10], '#ffffff')), #reverse
          domain = df %>% filter(maplayer == 'Water infiltration') %>% pull(value),
          bins = c(0, 1, 5, 10, 20, 75),
          na.color = pointblue.palette[6]),
        
        '% Carbon' = colorBin(
          palette = colorRamp(colors = c('#ffffff', tk.palette[8])),
          domain = df %>% filter(maplayer == '% Carbon') %>% pull(value),
          bins = c(0, 2, 4, 6, 10),
          na.color = pointblue.palette[6])
      ))
  }
  if (set == 'soil_productivity_change') {
    return(
      list(
        # same scale of declining/little change/improving, but bins are different for
        # each metric
        'Overall score' = colorBin(
          palette = colorRamp(
            colors = c(pointblue.palette[3], '#ffffff', pointblue.palette[4])),
          domain = df %>% 
            filter(maplayer == 'Overall score' & rowname == 'Difference') %>% pull(value),
          bins = c(-40, -10, 10, 40),
          na.color = pointblue.palette[6]),
        
        'Bulk density' = colorBin(
          palette = colorRamp(
            colors = c(pointblue.palette[4], '#ffffff', pointblue.palette[3])), #reverse
          domain = df %>% 
            filter(maplayer == 'Bulk density' & rowname == 'Difference') %>% pull(value),
          bins = c(-0.25, -0.05, 0.05, 0.25),
          na.color = pointblue.palette[6]),
        
        'Water infiltration' = colorBin(
          palette = colorRamp(
            colors = c(pointblue.palette[4], '#ffffff', pointblue.palette[3])), #reverse
          domain = df %>% 
            filter(maplayer == 'Water infiltration' & rowname == 'Difference') %>% pull(value),
          bins = c(-70, -1, 1, 70),
          na.color = pointblue.palette[6]),
        
        '% Carbon' = colorBin(
          palette = colorRamp(
            colors = c(pointblue.palette[3], '#ffffff', pointblue.palette[4])),
          domain = df %>% 
            filter(maplayer == '% Carbon' & rowname == 'Difference') %>% pull(value),
          bins = c(-5, -0.0999, 0.0999, 5),
          na.color = pointblue.palette[6])
      ))
  }
  if (set == 'soil_nutrients') {
    return(
      list('Total Nitrogen (N)' = colorBin(
        palette = colorRamp(colors = c('#ffffff', pointblue.palette[4])),
        domain = df %>% filter(maplayer == 'Total Nitrogen (N)') %>% 
          pull(value),
        bins = c(0, 0.2, 0.3, 0.4, 0.7),
        na.color = pointblue.palette[6]),
        
        'Potassium (K)' = colorBin(
          palette = colorRamp(colors = c('#ffffff', pointblue.palette[3])),
          domain = df %>% filter(maplayer == 'Potassium (K)') %>% 
            pull(value),
          bins = c(0, 0.5, 1, 1.5, 3),
          na.color = pointblue.palette[6]),
        
        'Sodium (Na)' = colorBin(
          palette = colorRamp(colors = c('#ffffff', pointblue.palette[10])),
          domain = df %>% filter(maplayer == 'Sodium (Na)') %>% 
            pull(value),
          bins = c(0, 0.25, 0.5, 0.75, 1),
          na.color = pointblue.palette[6]),
        
        'Magnesium (Mg)' = colorBin(
          palette = colorRamp(colors = c('#ffffff', tk.palette[8])),
          domain = df %>% filter(maplayer == 'Magnesium (Mg)') %>% 
            pull(value),
          bins = c(0, 5, 8, 11, 15),
          na.color = pointblue.palette[6]),
        
        'Calcium (Ca)' = colorBin(
          palette = colorRamp(colors = c('#ffffff', tk.palette[6])),
          domain = df %>% filter(maplayer == 'Calcium (Ca)') %>% 
            pull(value),
          bins = c(0, 10, 15, 20, 25),
          na.color = pointblue.palette[6]),
        
        'pH' = colorBin(
          palette = colorRamp(colors = c('#ffffff', tk.palette[7])),
          domain = df %>% filter(maplayer == 'pH') %>% pull(value),
          bins = c(5, 5.5, 6, 6.5),
          na.color = pointblue.palette[6])
      ))
  }
  if (set == 'soil_microbes') {
    return(
      list(
        'Richness' = colorNumeric(
          palette = colorRamp(colors = c('#ffffff', pointblue.palette[4])),
          domain = df %>% pull(value)))
    )
  }
}
