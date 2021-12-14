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

tk.palette <- c('#3b4035', '#9c8755', '#61655c', '#d1bc8b', '#40696f', 
                '#2e5150', '#5f5131', '#9e513a')

pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa', '#bfd730',
                       '#a7a9ac', '#666666',
                       #add a few more complementary colors:
                       '#456d28', '#b74374', '#5e2a84', '#d2c921')

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

render_Rmd = function(pathin, pathout) {
  rmarkdown::render(pathin, 
                    output_file = here::here(pathout))
}