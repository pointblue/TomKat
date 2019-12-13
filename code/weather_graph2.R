# README----------------------
# Compare monthly weather to historic

## packages
library(tidyverse)
library(plotly)

## input files
masterdat <- 'data_master/TK_weather_master_monthly.csv'
normals <- 'data_master/TK_weather_estimated_normals.csv'

## output files
graph2 <- 'weather_graph2.html'


pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa',
                       '#bfd730', '#a7a9ac', '#666666')

tk.palette <- c('#3b4035', '#9c8755', '#61655c',
                '#d1bc8b', '#40696f', '#2e5150',
                '#5f5131', '#9e513a')

##----Monthly difference from long-term average----
norm <- read_csv(here::here(normals), col_types = cols()) %>%
  rename(high = TMAX,
         low = TMIN,
         rain = PRCP) %>%
  gather(rain:low, key = 'var', value = 'normals')

dat <- read_csv(here::here(masterdat), col_types = cols()) %>%
  rename(high = temp.max,
         low = temp.min,
         rain = rain.in) %>%
  mutate(month = as.integer(format(date, '%m'))) %>%
  gather(high:rain, key = 'var', value = 'observed') %>%
  left_join(norm, by = c('month', 'var')) %>%
  mutate(diff = observed - normals) %>%
  select(date, month, var, diff) %>%
  # spread(key = var, value = diff) %>%
  filter(date > '2010-09-15') %>% #first row is all NA
  arrange(date) %>%
  mutate(value2 = case_when(diff > 0 ~ paste0('+', htmlTable::txtRound(diff, digits = 1)),
                            diff <= 0 ~ htmlTable::txtRound(diff, digits = 1)),
         var = recode(var, high = 'High', low = 'Low', rain = 'Rain'),
         text = paste0('<b>', var, ':</b> ', value2),
         color = case_when(var == 'High' ~ pointblue.palette[3],
                           var == 'Low' ~ pointblue.palette[2],
                           var == 'Rain' ~ pointblue.palette[1]))


# TIME SERIES--------------

p1 <- plot_ly(dat %>% filter(var == 'High'), x = ~date, y = ~diff, type = 'bar',
              color = ~I(color), text = ~text, hoverinfo = 'x+text') %>%
  layout(yaxis = list(title = 'High (F)',
                      font = list(size = 14, family = 'sans-serif'),
                      automargin = TRUE),
         xaxis = list(title = NA,
                      font = list(size = 14, family = 'sans-serif')))

p2 <- plot_ly(dat %>% filter(var == 'Low'), x = ~date, y = ~diff, type = 'bar',
              color = ~I(color), text = ~text, hoverinfo = 'x+text') %>%
  layout(yaxis = list(title = 'Low (F)',
                      font = list(size = 14, family = 'sans-serif'),
                      automargin = TRUE),
         xaxis = list(title = NA,
                      font = list(size = 14, family = 'sans-serif')))

p3 <- plot_ly(dat %>% filter(var == 'Rain'), x = ~date, y = ~diff, type = 'bar',
              color = ~I(color), text = ~text, hoverinfo = 'x+text') %>%
  layout(yaxis = list(title = 'Precipitation (in)',
                      font = list(size = 14, family = 'sans-serif'),
                      automargin = TRUE),
         xaxis = list(title = NA,
                      font = list(size = 14, family = 'sans-serif')))


plot2 <- subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
  layout(showlegend = FALSE,
         hovermode = 'x',
         dragmode = 'pan',
         margin = list(r = 0, b = 50, t = 10),
         xaxis = list(range = c('2010-10-01', '2019-12-31'),
                      rangeselector = list(buttons = list(list(count = 1,
                                                               label = "YTD",
                                                               step = "year",
                                                               stepmode = "todate"),
                                                          list(count = 2,
                                                               label = "2 yrs",
                                                               step = "year",
                                                               stepmode = "backward"),
                                                          list(step = "all")),
                                           font = list(color = 'white'),
                                           bgcolor = tk.palette[2],
                                           activecolor = tk.palette[5])),
         shapes = list(
           list(type = "rect",
                fillcolor = pointblue.palette[6], opacity = 1,
                line = list(color = "transparent"), 
                x0 = "2019-03-01", x1 = "2019-07-01", xref = "x",
                y0 = 0, y1 = 1, yref = "paper"),
           list(type = "rect",
                fillcolor = pointblue.palette[6], line = list(color = "transparent"), opacity = 0.3,
                x0 = "2018-03-01", x1 = "2018-07-01", xref = "x",
                y0 = 0, y1 = 1, yref = "paper"),
           list(type = "rect",
                fillcolor = pointblue.palette[6], line = list(color = "transparent"), opacity = 0.3,
                x0 = "2017-03-01", x1 = "2017-07-01", xref = "x",
                y0 = 0, y1 = 1, yref = "paper"),
           list(type = "rect",
                fillcolor = pointblue.palette[6], line = list(color = "transparent"), opacity = 0.3,
                x0 = "2016-03-01", x1 = "2016-07-01", xref = "x",
                y0 = 0, y1 = 1, yref = "paper"),
           list(type = "rect",
                fillcolor = pointblue.palette[6], line = list(color = "transparent"), opacity = 0.3,
                x0 = "2015-03-01", x1 = "2015-07-01", xref = "x",
                y0 = 0, y1 = 1, yref = "paper"),
           list(type = "rect",
                fillcolor = pointblue.palette[6], line = list(color = "transparent"), opacity = 0.3,
                x0 = "2014-03-01", x1 = "2014-07-01", xref = "x",
                y0 = 0, y1 = 1, yref = "paper"),
           list(type = "rect",
                fillcolor = pointblue.palette[6], line = list(color = "transparent"), opacity = 0.3,
                x0 = "2013-03-01", x1 = "2013-07-01", xref = "x",
                y0 = 0, y1 = 1, yref = "paper"),
           list(type = "rect",
                fillcolor = pointblue.palette[6], line = list(color = "transparent"), opacity = 0.3,
                x0 = "2012-03-01", x1 = "2012-07-01", xref = "x",
                y0 = 0, y1 = 1, yref = "paper"),
           list(type = "rect",
                fillcolor = pointblue.palette[6], line = list(color = "transparent"), opacity = 0.3,
                x0 = "2011-03-01", x1 = "2011-07-01", xref = "x",
                y0 = 0, y1 = 1, yref = "paper"))) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d', 
                                       'zoomIn2d', 'zoomOut2d', 
                                       'pan2d', 'toggleSpikelines'))

htmlwidgets::saveWidget(plot2,
                        here::here(graph2),
                        selfcontained = TRUE,
                        title = 'TomKat Weather Trends')
