# Load all your packages before calling make().

suppressPackageStartupMessages({
  library(drake)
  library(tidyverse)
  library(pbdistance) # devtools::install_github('kdybala/pbdistance')
  library(sf)
  library(plotly)
  library(htmlTable)
  library(leaflet)
  library(mapview)
  library(raster)
  library(rnoaa)
})

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("gather", "tidyr")
conflicted::conflict_prefer("arrange_", "plotly")
conflicted::conflict_prefer("config", "plotly")
conflicted::conflict_prefer("layout", "plotly")

# library(tibble)
# # library(qs) #for quicker reading to/from disk
# # library(fst) #for quicker reading to/from disk

# conflicted::conflict_prefer("extract", "raster")
# conflicted::conflict_prefer("lag", "stats")