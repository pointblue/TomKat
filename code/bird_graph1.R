# README----------------------
# Script to produce bird graph 1: ranch-wide trend in focal species densities
#  (for all TK grassland survey points combined)

## packages
library(tidyverse)
library(plotly)
library(htmlTable)

## input files
masterdat <- 'data_master/TK_bird_density_by_year.csv'

## output files
graph1 <- 'bird_graph1.html'

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
  filter(species != 'WCSP') %>% #for now
  separate(Label, c('Project', 'Year')) %>%
  select(species, Year, Estimate, lcl, ucl) %>%
  # mutate_at(vars(Estimate:ucl), funs(. / 2.47105)) %>% #optional: conver to acres
  mutate(species = as.factor(species),
         Year = as.numeric(Year),
         fullname = recode(species,
                           GRSP = 'Grasshopper Sparrow',
                           SAVS = 'Savannah Sparrow'),
         y_round = txtRound(Estimate, digits = 2, txt.NA = 'NA'),
         lcl_round = txtRound(lcl, digits = 2, txt.NA = 'NA'),
         ucl_round = txtRound(ucl, digits = 2, txt.NA = 'NA'),
         text = paste0(y_round, ' (', lcl_round, '-', ucl_round, ')')) 


# ESTIMATE TRENDS---------------
# assume relatively even effort, random selection of points per year
# --> Note: this ignores error in density estimates, so is very rough

mod = glm(Estimate ~ Year * species, dat, family = quasipoisson)
mod2 = glm(Estimate ~ Year + species, dat, family = quasipoisson)
#SAVS declining significantly; weak declining trend for GRSP

plot1 <- plot_ly(x = ~Year) %>%
  add_trace(data = dat %>% filter(species == 'GRSP'), 
            y = ~Estimate, 
            type = 'scatter', 
            mode = 'lines+markers',
            error_y = list(type = 'data',
                           symmetric = FALSE,
                           arrayminus = ~Estimate-lcl,
                           array = ~ucl-Estimate,
                           color = pointblue.palette[2]),
            line = list(color = pointblue.palette[2]),
            marker = list(color = pointblue.palette[2], 
                          size = 10),
            text = ~text,
            hoverinfo = 'x+text', 
            name = 'Grasshopper Sparrow') %>%
  add_trace(data = dat %>% filter(species == 'SAVS'), 
            y = ~Estimate, 
            type = 'scatter', 
            mode = 'lines+markers',
            error_y = list(type = 'data',
                           symmetric = FALSE,
                           arrayminus = ~Estimate-lcl,
                           array = ~ucl-Estimate,
                           color = pointblue.palette[3]),
            line = list(color = pointblue.palette[3]),
            marker = list(color = pointblue.palette[3], 
                          size = 10),
            text = ~text,
            hoverinfo = 'x+text', 
            name = 'Savannah Sparrow') %>%
  layout(yaxis = list(title = 'Density (birds/ha)',
                      font = list(size = 14),
                      showline = TRUE,
                      ticks = 'outside',
                      tick0 = 0,
                      range = c(0,2),
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
                                       'zoomIn2d', 'zoomOut2d', 'autoScale2d',
                                       'pan2d', 'toggleSpikelines'))

htmlwidgets::saveWidget(plot1,
                        here::here(graph1),
                        selfcontained = TRUE,
                        title = 'Bird species trends')
