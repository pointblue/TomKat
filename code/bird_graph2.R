# README----------------------
# Script to produce bird graph 1: ranch-wide trend in focal species densities
#  (for all TK grassland survey points combined)

## packages
library(tidyverse)
library(plotly)
library(htmlTable)

## input files
masterdat <- 'data_master/TK_richness_by_year.csv'

## output files
graph2 <- 'bird_graph2.html'

pointblue.palette <-
  c('#4495d1',
    '#74b743',
    '#f7941d',
    '#005baa',
    '#bfd730',
    '#a7a9ac',
    '#666666')

# DATA SET UP------------------

dat <- read_csv(here::here(masterdat)) %>%
  separate(id, c('habitat', 'year')) %>%
  mutate(habitat = as.factor(habitat),
         year = as.numeric(year),
         text = paste0(S.ACE, ' (', min, '-', max, ')')) 


# ESTIMATE TRENDS---------------
# assume relatively even effort, random selection of points per year
# --> Note: this ignores error in richness estimates, so is very rough

mod = glm(S.ACE ~ year * habitat, dat, family = poisson)
mod2 = glm(S.ACE ~ year + habitat, dat, family = poisson)
summary(mod); summary(mod2)
# decline in grassland richness

plot2 <- plot_ly(x = ~year) %>%
  add_trace(data = dat %>% filter(habitat == 'grassland'), 
            y = ~S.ACE, 
            type = 'scatter', 
            mode = 'lines+markers',
            error_y = list(type = 'data',
                           symmetric = FALSE,
                           arrayminus = ~S.ACE - min,
                           array = ~max - S.ACE,
                           color = pointblue.palette[2]),
            line = list(color = pointblue.palette[2]),
            marker = list(color = pointblue.palette[2], 
                          size = 10),
            text = ~text,
            hoverinfo = 'x+text', 
            name = 'grassland') %>%
  add_trace(data = dat %>% filter(habitat == 'riparian'), 
            y = ~S.ACE, 
            type = 'scatter', 
            mode = 'lines+markers',
            error_y = list(type = 'data',
                           symmetric = FALSE,
                           arrayminus = ~S.ACE - min,
                           array = ~max - S.ACE,
                           color = pointblue.palette[4]),
            line = list(color = pointblue.palette[4]),
            marker = list(color = pointblue.palette[4], 
                          size = 10),
            text = ~text,
            hoverinfo = 'x+text', 
            name = 'riparian') %>%
  layout(yaxis = list(title = 'Estimated number of species',
                      font = list(size = 14),
                      showline = TRUE,
                      ticks = 'outside',
                      tick0 = 0,
                      range = c(0, 75),
                      showgrid = FALSE),
         xaxis = list(title = NA,
                      showline = TRUE,
                      ticks = 'outside',
                      showgrid = FALSE),
         legend = list(x = 0.01, xanchor = 'left', y = 1, yanchor = 'top',
                       bordercolor = ~I('black'), borderwidth = 1),
         hovermode = 'x') %>%
  config(collaborate = FALSE, displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d', 
                                       'zoomIn2d', 'zoomOut2d', 
                                       'pan2d', 'toggleSpikelines'))

htmlwidgets::saveWidget(plot2,
                        here::here(graph2),
                        selfcontained = TRUE,
                        title = 'Bird richness trends')
