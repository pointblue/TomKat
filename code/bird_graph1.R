# README----------------------
# Script to produce bird graph 1: ranch-wide trend in focal species densities
#  (for all TK grassland survey points combined)
# Outputs: html for webpages and jpg for powerpoint

## packages
library(tidyverse)
library(plotly)
library(htmlTable)

## input files
masterdat <- 'data_master/TK_bird_density_by_year.csv'

## output files
graph1 <- 'bird_graph1.html'
ppt1 <- 'figs/bird_graph1_ppt.jpg'

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

# DATA SET UP------------------

dat <- read_csv(here::here(masterdat), col_types = cols()) %>%
  mutate(species = gsub('2', '', species)) %>%
  filter(species != 'WCSP') %>% #for now
  separate(Label, c('Project', 'Year')) %>%
  select(species, Year, Estimate, lcl, ucl) %>%
  mutate_at(vars(Estimate:ucl), funs(. / 2.47105 * 10)) %>% #optional: convert to birds per 10 acres
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

summary(mod); summary(mod2)
#in mod2, SAVS & GRSP declining significantl

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
  layout(yaxis = list(title = 'Density (birds/10 acres)',
                      font = list(size = 14),
                      showline = TRUE,
                      ticks = 'outside',
                      tick0 = 0,
                      range = c(0,10),
                      showgrid = FALSE,
                      automargin = TRUE),
         xaxis = list(title = NA,
                      showline = TRUE,
                      ticks = 'outside',
                      showgrid = FALSE),
         legend = list(x = 0.01, xanchor = 'left', y = 1, yanchor = 'top',
                       bordercolor = ~I('black'), borderwidth = 1),
         hovermode = 'x',
         margin = list(r = 0, b = 10, t = 10)) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d', 
                                       'zoomIn2d', 'zoomOut2d', 
                                       'pan2d', 'toggleSpikelines'))

htmlwidgets::saveWidget(plot1,
                        here::here(graph1),
                        selfcontained = TRUE,
                        title = 'Bird species trends')

# POWERPOINT VERSION ------------------------------------------------------

g1 <- ggplot(dat, aes(Year, color = fullname)) + 
  stat_smooth(aes(y = lcl), method="gam", se = FALSE, 
              formula = y ~ s(x, k = 8),
              method.args = list(family = 'quasipoisson')) +
  stat_smooth(aes(y = ucl), method="gam", se = FALSE, 
              formula = y ~ s(x, k = 8),
              method.args = list(family = 'quasipoisson'))
# build plot object for rendering 
gg1 <- ggplot_build(g1)

# extract data for the loess lines from the 'data' slot
dat2 <- data.frame(x = gg1$data[[1]]$x,
                   smoothlcl = gg1$data[[1]]$y,
                   smoothucl = gg1$data[[2]]$y,
                   fullname = rep(unique(dat$fullname), each = 80)) 

ggplot(dat, aes(Year, Estimate, color = fullname)) + 
  geom_point(size = 3) + 
  # geom_errorbar(aes(ymin = lcl, ymax = ucl)) +
  # geom_ribbon(data = dat2, aes(x, y = NULL, ymin = smoothlcl, ymax = smoothucl,
  #                              fill = fullname, color = NULL), alpha = 0.5,
  #             show.legend = FALSE) +
  geom_smooth(size = 1.5, 
              method="gam", se = FALSE, 
              formula = y ~ s(x, k = 8),
              method.args = list(family = 'quasipoisson')) +
  scale_color_manual(values = pointblue.palette[2:3]) +
  scale_fill_manual(values = pointblue.palette[2:3]) +
  labs(x = NULL, y = "Density (birds / 10 acres)", color = NULL) +
  scale_x_continuous(breaks = seq(2012, 2018, 2), limits = c(2011, 2020.5)) + 
  theme_presentation + theme(legend.position = "none") +
  annotate("text", x = 2019.1, y = 1.9, label = 'Grasshopper\nSparrow', 
           color = pointblue.palette[2], size = 7, hjust = 0) +
  annotate("text", x = 2019.1, y = 0.5, label = 'Savannah\nSparrow',
           color = pointblue.palette[3], size = 7, hjust = 0)

ggsave(ppt1, width = ppt.width, height = ppt.height, units = 'in')

