# README----------------------
# Produce current weather time series figure

## packages
library(tidyverse)
library(plotly)

## input files
masterdat <- 'data_master/TK_weather_master_daily.csv'

## output files
graph1 <- 'weather_graph1.html'


pointblue.palette <-
  c('#4495d1',
    '#74b743',
    '#f7941d',
    '#005baa',
    '#bfd730',
    '#a7a9ac',
    '#666666')

tk.palette <- c('#3b4035', '#9c8755', '#61655c',
                '#d1bc8b', '#40696f', '#2e5150',
                '#5f5131', '#9e513a')


# DATA SET UP----------------

dat <- read_csv(here::here(masterdat), col_types = cols()) %>%
  rename(high = temp.max,
         low = temp.min,
         rain = rain.in) %>%
  mutate(day = format(date, '%m-%d'),
         year = format(date, '%Y')) %>%
  # fill in missing days
  complete(nesting(day), year) %>%
  mutate(date = as.Date(paste0(year, '-', day))) %>%
  filter(date > '2010-09-08' & date < '2018-10-24') %>%
  gather(high:rain, key = var, value = value) %>%
  # add record high/low values for each month (even those missing observations)
  group_by(var, day) %>%
  mutate(max = max(value, na.rm = T),
         min = min(value, na.rm = T)) %>%
  ungroup() %>%
  select(-day) %>%
  gather(value:min, key = type, value = measurement) %>%
  unite(var, c(var, type)) %>%
  filter(var %in% c('high_max', 'high_value', 'low_min', 'low_value', 'rain_max', 'rain_value')) %>%
  mutate(var = gsub('_max|_min', '_record', var)) %>%
  separate(var, into = c('var', 'type')) %>%
  spread(key = type, value = measurement) %>%
  mutate(text = paste0('<b>', var, ':</b> ', value, ' (record: ', record, ')')) %>%
  arrange(date, var)

# TIME SERIES----------------

plot1 <- plot_ly(x = ~date) %>%
  add_lines(data = dat %>% filter(var == 'high'), y = ~record,
            line = list(color = 'transparent'),
            hoverinfo = 'none') %>%
  add_lines(data = dat %>% filter(var == 'low'), y = ~record, 
            line = list(color = 'transparent'),
            fill = 'tonexty',
            fillcolor = scales::alpha('gray80', alpha = 0.5),
            hoverinfo = 'none') %>%
  add_trace(data = dat %>% filter(var == 'high'), y = ~value, 
            type = 'scatter', mode = 'lines',
            line = list(color = pointblue.palette[3]),
            # marker = list(color = pointblue.palette[3]),
            text = ~text,
            hoverinfo = 'x+text') %>%
  add_trace(data = dat %>% filter(var == 'low'), y = ~value, 
            type = 'scatter', mode = 'lines',
            line = list(color = pointblue.palette[2]),
            # marker = list(color = pointblue.palette[2]),
            text = ~text,
            hoverinfo = 'text') %>%
  add_lines(data = dat %>% filter(var == 'rain'), y = ~record,
            line = list(color = 'transparent'),
            fill = 'tozeroy',
            fillcolor = scales::alpha('gray80', alpha = 0.5),
            hoverinfo = 'none',
            yaxis = 'y2') %>%
  add_trace(data = dat %>% filter(var == 'rain'), y = ~value, 
            type = 'scatter', mode = 'lines',
            line = list(color = pointblue.palette[1]),
            # marker = list(color = pointblue.palette[1]),
            text = ~text,
            hoverinfo = 'text',
            yaxis = 'y2') %>%
  add_annotations(text = 'Precipitation (in)', 
                  textangle = 90,
                  x = 1,
                  y = 0.5,
                  xshift = 50,
                  xref = 'paper',
                  yref = 'paper',
                  yanchor = 'middle',
                  font = list(family = 'sans-serif',
                              size = 14),
                  showarrow = FALSE,
                  visible = TRUE) %>%
  layout(yaxis = list(title = 'Temperature (F)',
                      gridcolor = 'white',
                      font = list(family = 'sans-serif',
                                  size = 14)),
         yaxis2 = list(overlaying = 'y',
                       side = 'right',
                       title = NA,
                       gridcolor = 'white',
                       font = list(family = 'sans-serif',
                                   size = 14)),
         xaxis = list(title = NA, 
                      type = 'date',
                      font = list(family = 'sans-serif',
                                  size = 14),
                      rangeselector = list(buttons = list(list(count = 3,
                                            label = "3 mo",
                                            step = "month",
                                            stepmode = "backward"),
                                       list(count = 6,
                                            label = "6 mo",
                                            step = "month",
                                            stepmode = "backward"),
                                       list(count = 1,
                                            label = "1 yr",
                                            step = "year",
                                            stepmode = "backward"),
                                       list(count = 1,
                                            label = "YTD",
                                            step = "year",
                                            stepmode = "todate"),
                                       list(step = "all")),
                                       font = list(family = 'sans-serif',
                                                   size = 14,
                                                   color = 'white'),
                                       bgcolor = tk.palette[2],
                                       activecolor = tk.palette[5])),
         showlegend = FALSE,
         hovermode = 'x',
         dragmode = 'pan',
         margin = list(r = 80)) %>%
    rangeslider('2018-01-01', '2018-10-24', thickness = 0.05) %>%
    config(collaborate = FALSE, displaylogo = FALSE)


htmlwidgets::saveWidget(plot1,
                        here::here(graph1),
                        selfcontained = TRUE,
                        title = 'TomKat Daily Weather')


## GGPLOTLY VERSION
# dat <- read_csv(here::here(masterdat)) %>%
#   rename(high = temp.max,
#          low = temp.min,
#          rain = rain.in) %>%
#   mutate(day = format(date, '%m-%d')) %>%
#   gather(high:rain, key = var, value = value) %>%
#   # add record high/low values for each date
#   group_by(var, day) %>%
#   mutate(max = max(value, na.rm = T),
#          min = min(value, na.rm = T)) %>%
#   ungroup() 

# p1 <- dat %>%
#   ggplot(aes(x = date, y = value, ymin = min, ymax = max, group = var)) +
#   geom_ribbon(aes(fill = var), alpha = 0.5) +
#   geom_line(aes(col = var)) +
#   # geom_point(aes(col = var)) +
#   scale_fill_manual(values = pointblue.palette[c(3, 2, 1)]) +
#   scale_color_manual(values = pointblue.palette[c(3, 2, 1)]) + 
#   theme_minimal()

## DYGRAPHS VERSION
# datx <- xts(dat %>% select(-date), order.by = dat$date)
# 
# plot1 <- dygraph(datx) %>% 
#   dyAxis('x', drawGrid = F, pixelsPerLabel = 75) %>%
#   dyAxis(
#     'y',
#     drawGrid = F,
#     valueRange = c(0, 95),
#     label = 'Temperature (F)',
#     valueFormatter = htmlwidgets::JS(getZero)
#   ) %>%
#   dyAxis(
#     'y2',
#     drawGrid = F,
#     valueRange = c(0, 9.5),
#     label = 'Precipitation (in)',
#     valueFormatter = htmlwidgets::JS(getZero)
#   ) %>%
#   dyHighlight(
#     highlightCircleSize = 4,
#     highlightSeriesBackgroundAlpha = 1,
#     hideOnMouseOut = F
#   ) %>% 
#   dyEvent('2017-12-31', color = pointblue.palette[7]) %>%
#   dyEvent('2016-12-31', color = pointblue.palette[7]) %>%
#   dyEvent('2015-12-31', color = pointblue.palette[7]) %>%
#   dyEvent('2014-12-31', color = pointblue.palette[7]) %>%
#   dyEvent('2013-12-31', color = pointblue.palette[7]) %>%
#   dyEvent('2012-12-31', color = pointblue.palette[7]) %>%
#   dyEvent('2011-12-31', color = pointblue.palette[7]) %>%
#   dyEvent('2010-12-31', color = pointblue.palette[7]) %>%
#   dySeries(
#     names(datx)[c(3, 1, 2)],
#     label = 'High temp',
#     color = pointblue.palette[3],
#     strokeWidth = 3
#   ) %>%
#   dySeries(
#     names(datx)[c(6, 4, 5)],
#     label = 'Low temp',
#     color = pointblue.palette[2],
#     strokeWidth = 3
#   ) %>%
#   dySeries(
#     names(datx)[c(9, 7, 8)],
#     label = 'Precipitation',
#     color = pointblue.palette[1],
#     strokeWidth = 3,
#     axis = 'y2'
#   ) %>%
#   dyLegend(
#     labelsSeparateLines = T,
#     showZeroValues = T,
#     show = 'follow'
#   ) %>%
#   dyRangeSelector(height = 20,
#                   dateWindow = c(as.Date(max(zoo::index(
#                     datx
#                   )) - 540),
#                   as.Date(max(zoo::index(
#                     datx
#                   )) + 2)))
# 
# htmlwidgets::saveWidget(plot1,
#                         here::here(graph1),
#                         selfcontained = TRUE,
#                         title = 'TomKat Daily Weather')
# 
# 
# 
