# README--------------
# Master script file


# VEGETATION-----------
# process & clean data (create master veg file)
source('code/vegetation_prep.R')

# produce current veg map (veg map 1) - but manually export map?!
# source('docs/vegetation_map1.R') 

# produce net change from 2012 map (veg map 2)
# source('docs/vegetation_map2.R')

# produce ranch-wide trend graphs
# source('docs/vegetation_graphs.R')

# render Rmd to html
knitr::knit(here::here('vegetation.Rmd'), output = here::here('vegetation.html'))
