# README----------------------
# Script to produce vegetation trend graphs (accounting for different sizes of 
# each pasture, total area of pastures surveyed each year, and some missing data)
# 1: ranch-wide trend in % cover of major veg types
# 2: comparison of native, annual, perennial grasses
# Outputs: html for webpages and jpg for powerpoint

## packages
library(tidyverse)
library(sf)
library(plotly)

## input files
poly <- 'TK_veg_fields' ## shapefile

pointblue.palette <-
  c('#4495d1',
    '#74b743',
    '#f7941d',
    '#005baa',
    '#bfd730',
    '#a7a9ac',
    '#666666')

# theme for presentation figs (larger font sizes)
theme_presentation <- theme_classic() + 
  theme(legend.background = element_blank(),
        legend.position = c(0.01, 1), 
        legend.justification = c(0, 1),
        legend.title = element_text(size = 28),
        legend.text = element_text(size = 24), 
        plot.title = element_text(size = 36, face = 'bold', vjust = 1, hjust = 0.5),
        axis.text = element_text(size = 24, color = 'black', face = 'plain'), 
        axis.title = element_text(size = 28, face = 'plain', vjust = 0),
        strip.text = element_text(size = 28, face = 'bold', hjust = 0), 
        strip.background = element_blank())

# formatting sizes for powerpoint template
ppt.width = 10 #inches
ppt.height = 7.5 #inches
ppt.width.wide = 13.33 #inches

# DATA SET UP-------------
shp_poly <- st_read(here::here('GIS'), poly, quiet = TRUE) 

veg_trend_plot<-function(dat){
  dat <- dat %>%
    group_by(Year, vegtype) %>%
    summarize(Cover = mean(cover, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(y_round = paste0(round(Cover, digits = 0), '%'),
           vegtype = factor(vegtype, levels = c('AnnualGr', 
                                                'PereGr','Legumes', 'Forbs', 
                                                'SedgesRushes', 'ShrubsTrees', 
                                                'Other','BareGround'    
           )),
           fullname = recode(vegtype, 
                             AnnualGr = 'Annual Grasses',
                             Legumes = 'Legumes',
                             BareGround = 'Bare Ground',
                             PereGr = 'Perennial Grasses',
                             Forbs = 'Forbs',
                             SedgesRushes = 'Sedges and Rushes',
                             ShrubsTrees = 'Shrubs and Trees',
                             Other = 'Other'))
  
  
  
  
  # PLOT 1-------------
  # Overview: all grasses, shrubs, forbs, bare ground, weeds
  
  plot1 <- plot_ly(x = ~Year) %>%
    add_trace(data = dat %>% filter(vegtype == 'Legumes'), 
              y = ~Cover , 
              type = 'scatter', 
              mode = 'markers+lines',
              line = list(color = pointblue.palette[1]),
              marker = list(color = pointblue.palette[1], 
                            size = 10),
              text = ~y_round,
              hoverinfo = 'x+text', 
              name = 'Legumes') %>%
    add_trace(data = dat %>% filter(vegtype == 'BareGround'), 
              y = ~Cover , 
              type = 'scatter', 
              mode = 'lines+markers',
              line = list(color = 'black'),
              marker = list(color = "black", 
                            size = 10),
              text = ~y_round,
              hoverinfo = 'x+text', 
              name = 'Bare Ground') %>%
    add_trace(data = dat %>% filter(vegtype == 'Forbs'), 
              y = ~Cover , 
              type = 'scatter', 
              mode = 'lines+markers',
              line = list(color = pointblue.palette[3]),
              marker = list(color = pointblue.palette[3], 
                            size = 10),
              text = ~y_round,
              hoverinfo = 'x+text', 
              name = 'Forbs') %>%
    add_trace(data = dat %>% filter(vegtype == 'SedgesRushes'), 
              y = ~Cover , 
              type = 'scatter', 
              mode = 'lines+markers',
              line = list(color = pointblue.palette[4]),
              marker = list(color = pointblue.palette[4], 
                            size = 10),
              text = ~y_round,
              hoverinfo = 'x+text', 
              name = 'Sedges and Rushes') %>%
    add_trace(data = dat %>% filter(vegtype == 'ShrubsTrees'), 
              y = ~Cover , 
              type = 'scatter', 
              mode = 'lines+markers',
              line = list(color = pointblue.palette[6]),
              marker = list(color = pointblue.palette[6], 
                            size = 10),
              text = ~y_round,
              hoverinfo = 'x+text', 
              name = 'Shrubs and Trees') %>%
    add_trace(data = dat %>% filter(vegtype == 'PereGr'), 
              y = ~Cover , 
              type = 'scatter', 
              mode = 'markers+lines',
              line = list(color = pointblue.palette[7]),
              marker = list(color = pointblue.palette[7], 
                            size = 10),
              text = ~y_round,
              hoverinfo = 'x+text', 
              name = 'Perennial Grass') %>%
    add_trace(data = dat %>% filter(vegtype == 'AnnualGr'), 
              y = ~Cover , 
              type = 'scatter', 
              mode = 'markers+lines',
              line = list(color = pointblue.palette[2]),
              marker = list(color = pointblue.palette[2], 
                            size = 10),
              text = ~y_round,
              hoverinfo = 'x+text', 
              name = 'Annual Grass') %>%
    layout(yaxis = list(title = '% Cover',
                        font = list(size = 14),
                        showline = TRUE,
                        ticks = 'outside',
                        tick0 = 0,
                        range = c(0, 100),
                        showgrid = FALSE,
                        automargin = TRUE),
           xaxis = list(title = NA,
                        showline = TRUE,
                        ticks = 'outside',
                        showgrid = FALSE),
           legend = list(x = 0.01, xanchor = 'left', y = 1, yanchor = 'top',
                         bordercolor = ~I('black'), borderwidth = 1,
                         font = list(family = 'sans-serif',
                                     size = 14)),
           hovermode = 'x',
           margin = list(r = 0, b = 10, t = 10)) %>%
    config(displaylogo = FALSE, showTips = FALSE,
           modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d', 
                                         'zoomIn2d', 'zoomOut2d', 
                                         'pan2d', 'toggleSpikelines'))
  return(plot1)
}






