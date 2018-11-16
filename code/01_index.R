# README--------------
# Master script file


# VEGETATION-----------
# process & clean data (create master veg file)
source('code/vegetation_prep.R')

# produce current veg map (veg map 1) - but manually export map?!
# source('widgets/vegetation_map1.R') 

# produce net change from 2012 map (veg map 2)
# source('widgets/vegetation_map2.R')

# produce ranch-wide trend graphs
# source('widgets/vegetation_graphs.R')

# render Rmd to html
knitr::knit(here::here('Rmd/vegetation.Rmd'), 
            output = here::here('vegetation.html'))
