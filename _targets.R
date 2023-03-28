library(targets)
library(sf)
library(dplyr)
library(ggplot2)


list(
  # set up road network
  # https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/rnf-frr/index2021-eng.cfm?year=21
  # https://www150.statcan.gc.ca/n1/en/catalogue/92-500-G2021001
  # https://www12.statcan.gc.ca/census-recensement/2021/geo/ref/domain-domaine/index2021-eng.cfm?lang=e&id=CLASS
  tar_target(statscan_roadnetwork_filepath, "/mnt/c/Users/chris/Documents/large_shapefiles/lrnf000r21a_e/lrnf000r21a_e.shp"),
  tar_target(ottawa_db_shp, sf::st_transform(neighbourhoodstudy::ottawa_dbs_shp2021, crs=32189)),
  tar_target(ottawa_buffer_shp, sf::st_union(ottawa_db_shp) |> sf::st_buffer(100)),
  tar_target(ontario_road_full_shp,
             sf::read_sf(statscan_roadnetwork_filepath) |>
               filter(PRNAME_L =="Ontario" | PRNAME_R == "Ontario") |>
               sf::st_transform(crs=32189) |>
               sf::st_filter(ottawa_buffer_shp)
             ), #
  tar_target(ottawa_road_filtered_shp,
             dplyr::filter(ontario_road_full_shp, CLASS %in% c(20:23))),


  NULL
)
