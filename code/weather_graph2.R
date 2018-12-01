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


pointblue.palette <-
  c('#4495d1',
    '#74b743',
    '#f7941d',
    '#005baa',
    '#bfd730',
    '#a7a9ac',
    '#666666')


##----Monthly difference from long-term average----
norm <- read_csv(here::here(normals)) %>%
  rename(high = TMAX,
         low = TMIN,
         rain = PRCP) %>%
  gather(rain:low, key = 'var', value = 'normals')

dat <- read_csv(here::here(masterdat)) %>%
  rename(high = temp.max,
         low = temp.min,
         rain = rain.in) %>%
  mutate(month = as.integer(format(date, '%m'))) %>%
  gather(high:rain, key = 'var', value = 'observed') %>%
  left_join(norm, by = c('month', 'var')) %>%
  mutate(diff = observed - normals) %>%
  select(date, month, var, diff) %>%
  # spread(key = var, value = diff) %>%
  filter(date > '2010-09-15' & date < '2018-10-15') %>% #first and last rows all NA
  arrange(date) %>%
  mutate(value2 = case_when(diff > 0 ~ paste0('+', htmlTable::txtRound(diff, digits = 1)),
                            diff <= 0 ~ htmlTable::txtRound(diff, digits = 1)),
         var = recode(var, high = 'High', low = 'Low', rain = 'Rain'),
         text = paste0('<b>', var, ':</b> ', value2),
         color = case_when(var == 'High' ~ pointblue.palette[3],
                           var == 'Low' ~ pointblue.palette[2],
                           var == 'Rain' ~ pointblue.palette[1]))


# TIME SERIES--------------

hl <- list(bgcolor = 'white',
           font = list(family = 'Arial'),
           size = 12,
           color =  pointblue.palette[7])

plot_ly(x = ~date, y = ~diff) %>%
  add_trace(data = dat %>% filter(var == 'High'), color = ~I(color), type = 'bar',
            text = ~text, hoverinfo = 'x+text') %>%
  add_trace(data = dat %>% filter(var == 'Low'), color = ~I(color), type = 'bar',
            text = ~text, hoverinfo = 'x+text') %>%
  add_trace(data = dat %>% filter(var == 'Rain'), color = ~I(color), type = 'bar',
            text = ~text, hoverinfo = 'x+text') %>%
  layout(yaxis = list(title = NA),
         xaxis = list(title = NA),
         showlegend = F,
         hovermode = 'x',
         dragmode = 'pan') %>%
  config(collaborate = FALSE, displaylogo = FALSE)

p1 <- plot_ly(dat %>% filter(var == 'High'), x = ~date, y = ~diff, type = 'bar',
              color = ~I(color), text = ~text, hoverinfo = 'x+text') %>%
  layout(yaxis = list(title = 'High (°F)'),
         xaxis = list(title = NA))

p2 <- plot_ly(dat %>% filter(var == 'Low'), x = ~date, y = ~diff, type = 'bar',
              color = ~I(color), text = ~text, hoverinfo = 'x+text') %>%
  layout(yaxis = list(title = 'Low (°F)'),
         xaxis = list(title = NA))

p3 <- plot_ly(dat %>% filter(var == 'Rain'), x = ~date, y = ~diff, type = 'bar',
              color = ~I(color), text = ~text, hoverinfo = 'x+text') %>%
  layout(yaxis = list(title = 'Precipitation (in)'),
         xaxis = list(title = NA))


plot2 <- subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
  layout(showlegend = FALSE,
         hovermode = 'x',
         dragmode = 'pan',
         xaxis = list(rangeselector = list(buttons = list(list(count = 1,
                                                               label = "YTD",
                                                               step = "year",
                                                               stepmode = "todate"),
                                                          list(count = 1,
                                                               label = "1 yr",
                                                               step = "year",
                                                               stepmode = "backward"),
                                                          list(count = 2,
                                                               label = "2 yrs",
                                                               step = "year",
                                                               stepmode = "backward"),
                                                          list(step = "all")),
                                           font = list(color = 'white'),
                                           bgcolor = pointblue.palette[4],
                                           activecolor = pointblue.palette[1])),
         shapes = list(
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
  config(collaborate = FALSE, displaylogo = FALSE)

htmlwidgets::saveWidget(plot2,
                        here::here(graph2),
                        selfcontained = TRUE,
                        title = 'TomKat Weather Trends')
