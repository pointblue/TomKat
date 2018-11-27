# README--------------
# Master script file
# Best practice: restart R after each call (ctrl+shift+F10)


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

# wishlist/to do: 
# - update with corrected data from Mel
# - update net change to multi-year trends rather than simple difference

# SOIL------------------
# process & clean data (create master soil file)
source('code/soil_prep.R')

# produce current soil map (soil map 1)
source('code/soil_map1.R')

# produce net change from 2014 map (soil map 2)
source('code/soil_map2.R')

# produce soil nutrients map (soil map 3)
source('code/soil_map3.R')

# produce soil microbes map (soil map 4?)
## -->waiting on data from Chelsea

# render Rmd to html
rmarkdown::render(here::here("Rmd/soil.Rmd"), 
                  output_file = here::here("soil.html"))


# INDEX------------------
# render Rmd to html
rmarkdown::render(here::here("Rmd/index.Rmd"), 
                  output_file = here::here("index.html"))
