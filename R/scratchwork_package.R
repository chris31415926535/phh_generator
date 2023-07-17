library(tidyverse)

future::plan(future::multisession, workers = 20)
future::nbrOfFreeWorkers()

test <- pseudohouseholds::get_phhs_parallel(region = regions, region_idcol = "DBUID", region_popcol = "dbpop2021", roads = ottawa_road_filtered_shp, roads_idcol = "NGD_UID")

ggplot(test) + geom_sf()
