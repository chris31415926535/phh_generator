library(sf)
library(dplyr)
library(pseudohouseholds)
library(future)


# Generate PHHs for all census divisions in Ontario, Canada
# Using StatsCan 2021 road network

# NOTE! THIS SHOULD NOT BE RUN INTERACTIVELY USING RSTUDIO. IT IS TOO SLOW.
# INSTEAD RUN WITH
# > Rscript phh_ontario.R

# Roads: all roads of class 20, 21, 22, 23 (road, arterial, collector, local)
# AND roads of rank 4 or 5 in class 12, 13? "RANK" IN ('4','5') AND "CLASS" IN ('12', '13')
# AND the trans-canada highway (RANK 1) where the name IS NOT a 400-series highway
# AND the road has a name that is not NULL (so no unnamed logging roads)
# https://www12.statcan.gc.ca/census-recensement/2021/geo/ref/domain-domaine/index2021-eng.cfm?lang=e&id=CLASS
# ("CLASS" IN ('20','21','22','23') OR ("RANK" = '1' AND NAME NOT LIKE '4__%') OR ("RANK" IN ('4','5') AND "CLASS" IN ('12', '13'))) AND NAME IS NOT NULL

province_name <- "Ontario"
province_pruid <- "35"


# set up multi-threading
future::plan(future::multisession, workers = 20)
future::nbrOfFreeWorkers()


message("Loading CD data")
all_cds <- sf::read_sf("~/datascience/data/spatial/lcd_000b21a_e/lcd_000b21a_e.shp") |>
  sf::st_set_geometry(NULL )|>
  dplyr::filter(PRUID == province_pruid)


message("Loading DB data")
all_dbpops <- readr::read_csv("~/datascience/data/spatial/geographic_attribute_file/2021_92-151_X.csv",
                              col_types = readr::cols(.default = "c")) |>
  dplyr::filter(PRNAME_PRNOM == province_name) |>
  dplyr::select(DBUID = DBUID_IDIDU, dbpop = DBPOP2021_IDPOP2021) |>
  dplyr::mutate(dbpop = as.numeric(dbpop))



all_dbs <-  sf::read_sf( "~/datascience/data/spatial/ldb_000b21a_e/ldb_000b21a_e.shp",
                         query = 'SELECT DBUID FROM "ldb_000b21a_e" WHERE "PRUID" = \'35\'') |>
  dplyr::left_join(all_dbpops, by = "DBUID")


message("Loading road data")
ontario_roads <- sf::read_sf("~/datascience/data/spatial/lrnf000r21a_e/lrnf000r21a_e.shp",
                             query = 'SELECT NGD_UID,NAME,RANK,CLASS FROM "lrnf000r21a_e" WHERE
                             ("PRNAME_L" = \'Ontario\' OR "PRNAME_R" = \'Ontario\') AND
                             (
								("CLASS" IN (\'20\',\'21\',\'22\',\'23\')) OR
								("RANK" IN (\'4\',\'5\') AND "CLASS" IN (\'12\', \'13\')) OR
								("RANK" = \'1\' AND NAME NOT LIKE \'4__%\')
							 ) AND NAME IS NOT NULL' )



i <- 1

# set the date stamp once up front, in case running the analysis spans midnight
datestamp <- Sys.Date()
if (!dir.exists(paste0("output/",datestamp))) dir.create(paste0("output/",datestamp))

max_index <- nrow(all_cds)
#max_index <- 5

# Loop through all census divisions and create PHHs
for (i in 1:max_index){
  message(i,"/",max_index)

  this_cd <- all_cds[i,]

  cd_regex <- paste0("^", this_cd$CDUID)

  local_dbs <- dplyr::filter(all_dbs, stringr::str_detect(DBUID, cd_regex))

  local_dbs$dbpop <- dplyr::if_else(is.na(local_dbs$dbpop), 0, local_dbs$dbpop)

  local_dbs_buffer <- sf::st_union(local_dbs) |>
    sf::st_buffer(1)

  local_roads <- sf::st_filter(ontario_roads, local_dbs_buffer)


   # ggplot() + geom_sf(data=local_dbs)

  phhs <- pseudohouseholds::get_phhs_parallel(
    regions = local_dbs,
    region_idcol = "DBUID",
    roads = local_roads,
    region_popcol = "dbpop",
    roads_idcol = "NGD_UID",
    phh_density = 0.005,
    min_phh_pop = 5,
    min_phhs_per_region = 1,
    min_phh_distance = 25,
    road_buffer_m = 5,
    delta_distance_m = 5,
    skip_unpopulated_regions = FALSE
  )


  filename <- paste0(this_cd$CDUID, "-phhs-",Sys.Date(),".shp")

  sf::write_sf(phhs, paste0("output/",datestamp, "/",filename))

}




### COMBINE THEM

#z <- dir("output/bigtest") |>
#  grep(pattern="shp", value=TRUE) |>
#  grep(pattern="-05", value=TRUE)

#z <- dplyr::tibble(filename=z) |>
#  dplyr::mutate(cduid = substr(filename, 1, 4))

#all_cds  |> dplyr::filter(!CDUID %in% z$cduid)
