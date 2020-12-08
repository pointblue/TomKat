# README--------------
# Main script file
# Best practice: restart R after each call (ctrl+shift+F10)


# INDEX & README------------------
# render Rmd to html
rmarkdown::render(here::here("Rmd/index.Rmd"), 
                  output_file = here::here("docs/index.html"))

rmarkdown::render(here::here("Rmd/README.Rmd"),
                  output_file = here::here("README.md"))

# BIRDS----------------
# process & clean data (create main bird file and density estimates)
# Note: this will take a while since it includes distance sampling analysis
source('code/bird_prep.R')

# produce map of ranch-wide focal species average abundance (bird map 1)
source('code/bird_map1.R')

# produce graph of ranch-wide focal species trends (bird graph 1)
source('code/bird_graph1.R')

# produce map of ranch-wide bird species richness (bird map 2)
source('code/bird_map2.R')

# produce graph of ranch-wide trends in bird species richness (bird graph 2)
source('code/bird_graph2.R')

# render Rmd to html
rmarkdown::render(here::here("Rmd/birds.Rmd"), 
                  output_file = here::here("birds.html"))


# VEGETATION-----------
# process & clean data (create main veg file)
source('code/vegetation_prep.R')

# produce current veg map (veg map 1)
source('code/vegetation_map1.R')

# produce net change from 2012 map (veg map 2)
source('code/vegetation_map2.R')

# produce ranch-wide trend graphs
source('code/vegetation_graphs.R')

# produce species diversity map (veg map 3)
source('code/vegetation_map3.R')

# render Rmd to html
rmarkdown::render(here::here("Rmd/vegetation.Rmd"), 
                  output_file = here::here("docs/vegetation.html"))


# SOIL------------------
# process & clean data (create main soil file)
source('code/soil_prep.R')

# produce current soil map (soil map 1)
source('code/soil_map1.R')

# produce net change from 2014 map (soil map 2)
source('code/soil_map2.R')

# produce soil nutrients map (soil map 3)
source('code/soil_map3.R')

# produce soil microbes map (soil map 4?)
source('code/soil_map4.R')

# render Rmd to html
rmarkdown::render(here::here("Rmd/soil.Rmd"), 
                  output_file = here::here("docs/soil.html"))


# WEATHER----------------
# process & clean local weather data (create main weather file)
source('code/weather_prep.R')

# process & clean NOAA weather data (some has to be manually downloaded first)
source('code/weather_prep_noaa.R')

# estimate historic 1981-2010 "Normals" at TomKat from data at Half Moon Bay
source('code/weather_prep_historic.R')

# produce daily weather timeseries (weather graph 1)
source('code/weather_graph1.R')

# produce monthly trend summaries (weather graph 2)
source('code/weather_graph2.R')

# produce drought index graphs (weather graphs 3-4)
source('code/weather_graphs3-4.R')


## render Rmd to html
rmarkdown::render(here::here("Rmd/weather.Rmd"),
                  output_file = here::here("docs/weather.html"))


# WATER-------------------
# process & clean stream data; daily & monthly stats
source('code/stream_prep.R')

# produce daily timeseries (stream graph 1)
source('code/stream_graph1.R')

# produce monthly timeseries (stream graph 2)
source('code/stream_graph2.R')


## render Rmd to html
rmarkdown::render(here::here("Rmd/stream.Rmd"),
                  output_file = here::here("docs/stream.html"))


## Note: temperatures above 23 bad for adult steelhead trout; 
##   optimal for juvenile growth is 15-18
## Other ideas: Include daily precip in height plot; 
##   Plot height/temp vs. days after rain? 
##   Include map showing gauge location & topography/water catchment area?


# MANAGEMENT----------------
# process & clean data from PastureMap
source('code/mgmt_prep.R')

# produce mgmt maps 1 & 2
source('code/mgmt_maps1-2.R')

## render Rmd to html
rmarkdown::render(here::here("Rmd/mgmt.Rmd"),
                  output_file = here::here("docs/mgmt.html"))
