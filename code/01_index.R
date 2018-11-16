# README--------------
# Master script file


# VEGETATION-----------
# process & clean data (create master veg file)
source('code/vegetation_prep.R')

# produce current veg map (veg map 1)
source('code/vegetation_map1.R')

# produce net change from 2012 map (veg map 2)
source('code/vegetation_map2.R')

# produce ranch-wide trend graphs
source('code/vegetation_graphs.R')

# render Rmd to html
rmarkdown::render(here::here("Rmd/vegetation.Rmd"), 
                  output_file = here::here("vegetation.html"))

# wishlist: 
# - update htmltable code for veg maps 1 and 2 to use map2 (faster?)
# - increase transparency of polygons so terrain is visible
# - update net change to multi-year trends rather than simple difference