# README----------------------
# Produce current weather time series figure

## packages
library(tidyverse)
library(plotly)

## input files
masterdat <- 'data_master/TK_weather_master_daily.csv'

## output files
graph1 <- 'weather_graph1.html'


pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa',
                       '#bfd730', '#a7a9ac', '#666666')

tk.palette <- c('#3b4035', '#9c8755', '#61655c',
                '#d1bc8b', '#40696f', '#2e5150',
                '#5f5131', '#9e513a')

# DATA SET UP----------------

dat <- read_csv(here::here(masterdat), col_types = cols()) %>%
  rename(high = temp.max,
         low = temp.min,
         rain = rain.in) %>%
  mutate(day = format(date, '%m-%d'),
         mo = format(date, '%m'),
         year = format(date, '%Y')) %>%
  # fill in missing days
  complete(nesting(day, mo), year) %>%
  mutate(date = as.Date(paste0(year, '-', day))) %>%
  arrange(date) %>% 
  filter(date > '2010-09-08') %>%
  gather(high:rain, key = var, value = observed) %>%
  # # add record high/low values for each day (even those missing observations)
  # group_by(var, day) %>%
  # mutate(max = max(value, na.rm = T),
  #        min = min(value, na.rm = T)) %>%
  # ungroup() %>%
  # select(-day) %>%
  # gather(value:min, key = type, value = measurement) %>%
  # unite(var, c(var, type)) %>%
  # filter(var %in% c('high_max', 'high_value', 'low_min', 'low_value', 'rain_max', 'rain_value')) %>%
  # mutate(var = gsub('_max|_min', '_record', var)) %>%
  # separate(var, into = c('var', 'type')) %>%
  # spread(key = type, value = measurement) %>%
  mutate(text = paste0('<b>', var, ':</b> ', observed)) %>%
  arrange(date, var)

# calculate monthly extreme values: calculate stats for rain differently?
# (these don't work well for such non-normal distributions, just drop)
stats <- dat %>% 
  group_by(var, mo) %>% 
  mutate(mean = case_when(var == 'rain' ~ NA_real_,
                          TRUE ~ mean(observed, na.rm = T)),
         sd = case_when(var == 'rain' ~ NA_real_,
                        TRUE ~ sd(observed, na.rm = T)),
         high = case_when(var == 'rain' ~ NA_real_,
                          TRUE ~ mean + 2*sd),
         low = case_when(var == 'rain' ~ NA_real_,
                         TRUE ~ mean - 2*sd)) %>% 
  ungroup() 

# TIME SERIES----------------

plot1 <- plot_ly(x = ~date) %>%
  add_lines(data = stats %>% filter(var == 'low'), y = ~high,
            line = list(color = 'transparent'),
            hoverinfo = 'none') %>%
  add_lines(data = stats %>% filter(var == 'low'), y = ~low,
            line = list(color = 'transparent'),
            fill = 'tonexty',
            fillcolor = scales::alpha(pointblue.palette[2], alpha = 0.2),
            hoverinfo = 'none', name = 'spline') %>%
  add_lines(data = stats %>% filter(var == 'high'), y = ~high,
            line = list(color = 'transparent'),
            hoverinfo = 'none') %>%
  add_lines(data = stats %>% filter(var == 'high'), y = ~low,
            line = list(color = 'transparent'),
            fill = 'tonexty',
            fillcolor = scales::alpha(pointblue.palette[3], alpha = 0.2),
            hoverinfo = 'none', name = 'spline') %>%
  add_trace(data = dat %>% filter(var == 'high'), y = ~observed, 
            type = 'scatter', mode = 'lines',
            line = list(color = pointblue.palette[3]),
            # marker = list(color = pointblue.palette[3]),
            text = ~text,
            hoverinfo = 'x+text') %>%
  add_trace(data = dat %>% filter(var == 'low'), y = ~observed, 
            type = 'scatter', mode = 'lines',
            line = list(color = pointblue.palette[2]),
            # marker = list(color = pointblue.palette[2]),
            text = ~text,
            hoverinfo = 'text') %>%
  # add_lines(data = stats %>% filter(var == 'rain'), y = ~high,
  #           line = list(color = 'transparent'),
  #           fill = 'tozeroy',
  #           fillcolor = scales::alpha(pointblue.palette[1], alpha = 0.2),
  #           hoverinfo = 'none', yaxis = 'y2') %>%
  add_trace(data = dat %>% filter(var == 'rain'), y = ~observed, 
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
  layout(yaxis = list(range = c(0, 100),
                      title = 'Temperature (F)',
                      gridcolor = 'white',
                      font = list(family = 'sans-serif',
                                  size = 14),
                      automargin = TRUE),
         yaxis2 = list(overlaying = 'y',
                       side = 'right',
                       range = c(0, 8),
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
         margin = list(r = 50, b = 10, t = 10)) %>%
    rangeslider(max(dat$date)-365, max(dat$date), thickness = 0.05) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d', 
                                       'zoomIn2d', 'zoomOut2d', 
                                       'pan2d', 'toggleSpikelines'))

htmlwidgets::saveWidget(plot1,
                        here::here(graph1),
                        selfcontained = TRUE,
                        title = 'TomKat Daily Weather')
