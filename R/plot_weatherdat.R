##'@title Plot weather data
##'@description function for generating interacting plot_ly figures for TomKat
##'  weather data, including daily temperature and precipitation (line graphs),
##'  monthly differences from normal (bar graphs), and monthly drought indices
##'  (shaded line graphs).
##'@param dat data frame containing stream flow stats. Expects fields "name"
##'  (stat name), "date", and "value", with stats ordered as: temp, flow, depth.
##'@param colors character vector of colors to be used for temp, flow, and
##'  depth, respectively
##'@param ytitle vector of strings to be used as y-axis titles. For
##'  \code{plot_daily_weather}, expects vector of two for left and right y-axes.
##'  For \code{plot_monthly_weather}, expects vector of 3 strings for each of
##'  the subplots for monthly average high temp, low temp, and rain totals.
##'@param yrange vector of minimum and maximum values for y-axes. For
##'  \code{plot_daily_weather}, expects vector of 4 values for the min and max
##'  values of the left and right axes, respectively. For
##'  \code{plot_drought_index}, expects vector of 2 values for the min and max.
##'@param selector logical, whether or not to include selector buttons at top
##'  left for showing plot from last year, last 3 years, or all time; defaults
##'  to FALSE
##'@param slider logical, whether or not to include a range slider at the bottom
##'  of the plot; defaults to FALSE
##'@param valuebreaks vector of values representing the classification breaks
##'  for each drought index
##'@param linecolor,linewidth,linetype vectors of values for each of the lines
##'  representing drought indices for each climate division
##'@param markersymbol,markersize vectors of values for each of the lines
##'  representing drought indices for each climate division
##'
##'@export
##'@author Kristen Dybala
##'
##'  
plot_daily_weather = function(dat, colors, 
                              ytitle = c("Temperature (F)", "Precipitation (in)"),
                              yrange = c(0, 100, 0, 8), 
                              selector = FALSE, slider = FALSE) {
  
  # fill in missing days
  sdat <- dat %>% 
    mutate(day = format(Date, '%m-%d'),
           mo = format(Date, '%m'),
           year = format(Date, '%Y')) %>%
    complete(nesting(day, mo), year) %>%
    mutate(Date = as.Date(paste0(year, '-', day))) %>%
    arrange(Date) %>% 
    filter(Date > '2010-09-08' & Date <= max(dat$Date)) %>%
    pivot_longer(DailyMaxTemp:DailyRain) %>%
    mutate(name = gsub('Daily', '', name),
           name = factor(name, levels = c('MaxTemp', 'MinTemp', 'Rain')),
           text = paste0('<b>', name, ':</b> ', value)) %>% 
    arrange(name, Date) %>% 
    # calculate monthly mean and ~95% range temp
    group_by(name, mo) %>% 
    mutate(group.mean = if_else(name == 'DailyRain', NA_real_, 
                                mean(value, na.rm = T)),
           group.sd = if_else(name == 'DailyRain', NA_real_, sd(value, na.rm = T)),
           high = if_else(name == 'DailyRain', NA_real_, 
                          group.mean + 2 * group.sd),
           low = if_else(name == 'DailyRain', NA_real_,
                         group.mean - 2 * group.sd)) %>% 
    ungroup() %>% 
    split(.$name)
  
  p <- plot_ly(data = sdat[[1]], x = ~Date) %>% 
    add_annotations(text = ytitle[2], 
                    textangle = 90,
                    x = 1,
                    y = 0.5,
                    xshift = 50,
                    xref = 'paper',
                    yref = 'paper',
                    yanchor = 'middle',
                    font = standardfonts,
                    showarrow = FALSE,
                    visible = TRUE) %>%
    layout(yaxis = list(range = yrange[1:2],
                        title = ytitle[1],
                        gridcolor = 'white',
                        font = standardfonts,
                        automargin = TRUE),
           yaxis2 = list(overlaying = 'y',
                         side = 'right',
                         range = yrange[3:4],
                         title = NA,
                         gridcolor = 'white',
                         font = standardfonts),
           xaxis = list(title = NA, 
                        type = 'date',
                        tickfont = standardfonts,
                        gridcolor = 'gray60'),
           showlegend = FALSE,
           hovermode = 'x',
           dragmode = 'pan',
           margin = list(r = 50, b = 10, t = 10)) %>%
    config(displaylogo = FALSE, showTips = FALSE,
           modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d', 
                                         'zoomIn2d', 'zoomOut2d', 
                                         'pan2d', 'toggleSpikelines'))
  
  for (i in c(1:length(sdat))) {
    if (names(sdat)[i] == 'Rain') {
      p <- p %>% 
        add_trace(data = sdat[[i]], y = ~value, 
                  type = 'scatter', mode = 'lines',
                  line = list(color = colors[i]),
                  text = ~text,
                  hoverinfo = 'x+text',
                  yaxis = 'y2')
    } else {
      p <- p %>% 
        add_lines(data = sdat[[i]], y = ~high,
                  line = list(color = 'transparent'),
                  hoverinfo = 'none') %>%
        add_lines(data = sdat[[i]], y = ~low,
                  line = list(color = 'transparent'),
                  fill = 'tonexty',
                  fillcolor = scales::alpha(colors[i], alpha = 0.2),
                  hoverinfo = 'none', name = 'spline') %>%
        add_trace(data = sdat[[i]], y = ~value, 
                  type = 'scatter', mode = 'lines',
                  line = list(color = colors[i]),
                  text = ~text,
                  hoverinfo = 'x+text')
    }
  }

  if (selector) {
    p <- p %>%
      layout(xaxis = list(
        rangeselector = list(
          buttons = list(list(count = 1,
                              label = "1 yr",
                              step = "year",
                              stepmode = "backward"),
                         list(count = 3,
                              label = "3 yrs",
                              step = "year",
                              stepmode = "backward"),
                         list(step = "all")),
          font = c(standardfonts, color = 'white'),
          bgcolor = tk.palette[2],
          activecolor = tk.palette[5])))
  }
  if (slider) {
    p <- p %>% 
      rangeslider(max(dat$Date)-365, max(dat$Date), thickness = 0.05)
  }
  
  return(p)
}

plot_monthly_weather = function(dat, colors,
                                ytitle = c('High (F)', 'Low (F)', 'Rain (in)'),
                                selector = TRUE) {
  sdat <- dat %>% 
    mutate(value2 = if_else(diff > 0,
                            paste0('+', htmlTable::txtRound(diff, digits = 1)),
                            htmlTable::txtRound(diff, digits = 1)),
           name = gsub('Monthly', '', name),
           name = factor(name, levels = c('MaxTemp', 'MinTemp', 'Rain')),
           text = paste0('<b>', name, ':</b> ', value2),
           color = case_when(name == 'MaxTemp' ~ colors[1],
                             name == 'MinTemp' ~ colors[2],
                             name == 'Rain' ~ colors[3])) %>% 
    split(.$name)
  
  mindate <- min(sdat[[1]]$Date)
  if (as.numeric(format(mindate, '%m')) > 7) {
    minyear <- format(mindate, '%Y') %>% as.numeric() + 1
  } else {
    minyear <- format(mindate, '%Y') %>% as.numeric()
  }
  
  maxdate <- max(sdat[[1]]$Date)
  if (as.numeric(format(maxdate, '%m')) <= 3) {
    maxyear <- format(maxdate, '%Y') %>% as.numeric() - 1
  } else {
    maxyear <- format(maxdate, '%Y') %>% as.numeric()
  }
  
  # initiate shaded boxes
  box <- list(type = 'rect', fillcolor = '#666666', opacity = 0.3,
              line = list(color = "transparent"),
              xref = "x", yref = "paper")
  
  p <- purrr::map(c(1:length(sdat)),
                  function(x) {
                    psub <- plot_ly(data = sdat[[x]], x = ~Date+14, y = ~diff,
                                    type = 'bar', color = ~I(color), text = ~text, 
                                    hoverinfo = 'x+text',
                                    hoverlabel = standardfonts) %>%
                      layout(yaxis = list(title = ytitle[x],
                                          font = standardfonts,
                                          automargin = TRUE),
                             xaxis = list(title = NA, type = 'date',
                                          tickfont = standardfonts,
                                          gridcolor = 'gray60'),
                             margin = list(r = 0, b = 0, t = 10))
                    
                    # add shading for growing seasons in each year:
                    boxes <- list()
                    for (i in c(minyear:maxyear)) {
                        box[["x0"]] <- paste0(i, "-03-01")
                        box[["x1"]] <- paste0(i, "-07-01")
                        box[["y0"]] <- 0
                        box[["y1"]] <- 1
                        boxes <- c(boxes, list(box))
                    }
                    
                    psub <- layout(psub, shapes = boxes)
                    return(psub)
                  }) %>%
    subplot(nrows = length(sdat), shareX = TRUE, titleY = TRUE,
            which_layout = 'merge') %>%
    layout(showlegend = FALSE,
           hovermode = 'x',
           dragmode = 'pan',
           margin = list(r = 0, b = 10, t = 10)) %>%
    config(displaylogo = FALSE, showTips = FALSE,
           modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d',
                                         'zoomIn2d', 'zoomOut2d',
                                         'pan2d', 'toggleSpikelines'))
  
  if (selector) {
    p <- p %>%
      layout(xaxis = list(
        rangeselector = list(
          buttons = list(list(count = 1,
                              label = "1 yr",
                              step = "year",
                              stepmode = "backward"),
                         list(count = 3,
                              label = "3 yrs",
                              step = "year",
                              stepmode = "backward"),
                         list(step = "all")),
          font = c(standardfonts, color = 'white'),
          bgcolor = tk.palette[2],
          activecolor = tk.palette[5])))
  }
  return(p)
  
}

plot_drought_index = function(dat, colors, valuebreaks,
                              linecolor = c("#666666", "#666666", "black"),
                              linewidth = c(0.5, 0.5, 1),
                              linetype = c('solid', 'dot', 'solid'),
                              markersymbol = c('square', 'diamond', "circle"),
                              markersize = c(5, 5, 6),
                              yrange = c(-10, 8)) {
  
  pal = colorRampPalette(colors)(7)
  
  sdat <- dat %>%
    filter(date > '2010-07-01') %>% #start with July 2010 as with other weather data
    filter(climdiv %in% c('Central Coast', 
                          'North Coast Drainage', 
                          'South Coast Drainage')) %>% 
    mutate(climdiv = gsub(' Drainage', '', climdiv),
           climdiv = factor(climdiv, 
                            levels = c('North Coast', 
                                       'South Coast',
                                       'Central Coast')),
           # color code each point according to drought index level
           color = cut(value, breaks = valuebreaks,
                       labels = pal),
           text = paste0('<b>', climdiv, ': ', value)) %>% 
    split(.$climdiv)
  
  p <-  plot_ly() %>% 
    layout(shapes = list(hline(valuebreaks[5], color = pal[5]),
                         hline(valuebreaks[4], color = pal[3])),
           yaxis = list(title = NA, showgrid = FALSE, zeroline = FALSE,
                        tickfont = standardfonts,
                        automargin = TRUE, range = yrange),
           xaxis = list(title = NA, type = 'date', tickfont = standardfonts),
           legend = list(x = 1, xanchor = 'right', y = 1, yanchor = 'top',
                         bgcolor = I('white'), 
                         bordercolor = I('black'), borderwidth = 1,
                         font = list(size = 14, family = 'sans-serif')),
           hovermode = 'x',
           dragmode = 'pan',
           margin = list(l = 0, r = 0, b = 50, t = 10)) %>% 
    config(displaylogo = FALSE, showTips = FALSE,
           modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d', 
                                         'zoomIn2d', 'zoomOut2d', 
                                         'pan2d', 'toggleSpikelines'))
  
  for (i in c(1:length(sdat))) {
    p <- p %>% 
      add_trace(data = sdat[[i]], x = ~date, y = ~value, name = names(sdat)[i],
                type = 'scatter', mode = 'lines+markers',
                text = ~text, hoverinfo = "x+text",
                line = list(color = linecolor[i], 
                            width = linewidth[i],
                            dash = linetype[i]),
                marker = list(color = ~I(color), 
                              size = markersize[i], 
                              symbol = markersymbol[i],
                              line = list(color = linecolor[i], 
                                          width = linewidth[i])))
  }
  
  # add drought category labels
  dcat <- tibble(ylimit = valuebreaks[2:7],
                 label = c('Extreme drought',
                           'Severe drought',
                           'Moderate drought',
                           'Moderately moist',
                           'Very moist',
                           'Extremely moist'),
                 color = pal[c(1:3,5:7)])
  
  for (j in c(1:nrow(dcat))) {
    p <- p %>% 
      layout(annotations = list(
        x = 0.01, y = dcat$ylimit[j], text = ~paste(dcat$label[j]),
        xref = 'paper', x = 0.01, xanchor = 'left',
        yref = 'y', yanchor = ifelse(dcat$ylimit[j]>0, 'bottom', 'top'),
        font = list(family = 'sans-serif', size = 12, 
                    color = dcat$color[j]),
        showarrow = FALSE))
  }
  
  return(p)
}

hline <- function(y = 0, color = "blue") {
  list(type = "line", 
       x0 = 0, x1 = 1, xref = "paper",
       y0 = y, y1 = y, line = list(color = color))
}


