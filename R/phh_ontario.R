library(sf)
library(tidyverse)
library(leaflet)
library(future)

province_name <- "Ontario"
province_pruid <- "35"

future::plan(future::multisession, workers = 20)

source("~/datascience/R/geospatial/phh_generator/R/testfunction1.R")

all_cds <- sf::read_sf("~/datascience/data/spatial/lcd_000b21a_e/lcd_000b21a_e.shp") |>
  sf::st_set_geometry(NULL )|>
  dplyr::filter(PRUID == province_pruid)



all_dbpops <- readr::read_csv("~/datascience/data/spatial/geographic_attribute_file/2021_92-151_X.csv",
                              col_types = readr::cols(.default = "c")) |>
  dplyr::filter(PRNAME_PRNOM == province_name) |>
  dplyr::select(DBUID = DBUID_IDIDU, dbpop = DBPOP2021_IDPOP2021) |>
  dplyr::mutate(dbpop = as.numeric(dbpop))



all_dbs <-  sf::read_sf( "~/datascience/data/spatial/ldb_000b21a_e/ldb_000b21a_e.shp") |>
  dplyr::filter(PRUID == province_pruid) |>
  dplyr::select(DBUID) |>
  dplyr::left_join(all_dbpops)



#all_roads <- sf::read_sf("~/datascience/data/spatial/lrnf000r21a_e/lrnf000r21a_e.shp")
# ontario_roads <- sf::read_sf("~/datascience/data/spatial/lrnf000r21a_e/lrnf000r21a_e.shp") |> dplyr::filter(PRNAME_L == "Ontario" | PRNAME_R == "Ontario") |> sf::write_sf("~/datascience/data/spatial/lrnf000r21a_e/roads_ontario.shp")
ontario_roads <- sf::read_sf("~/datascience/data/spatial/lrnf000r21a_e/roads_ontario.shp") |>
  dplyr::filter(CLASS %in% c(20:23))

i <- 1
for (i in 42:nrow(all_cds)){
  message(i,"/",nrow(all_cds))

  this_cd <- all_cds[i,]

  cd_regex <- paste0("^", this_cd$CDUID)

  # local_dbs <- sf::read_sf( "~/datascience/data/spatial/ldb_000b21a_e/ldb_000b21a_e.shp") |>
  #   dplyr::select(DBUID) |>
  #   dplyr::left_join(all_dbpops) |>
  local_dbs <- dplyr::filter(all_dbs, stringr::str_detect(DBUID, cd_regex))

  local_dbs$dbpop <- dplyr::if_else(is.na(local_dbs$dbpop), 0, local_dbs$dbpop)

  local_dbs_buffer <- sf::st_union(local_dbs) |>
    sf::st_buffer(1)

  local_roads <- sf::st_filter(ontario_roads, local_dbs_buffer) #|> #sf::read_sf("~/datascience/data/spatial/lrnf000r21a_e/lrnf000r21a_e.shp") |>


   # ggplot() + geom_sf(data=local_dbs)


  regions_for_study <- split(local_dbs, ~ DBUID)


  progressr::with_progress({
    ##https://furrr.futureverse.org/articles/progress.html

    p <- progressr::progressor(length(regions_for_study))

    phh_candidates <- furrr::future_map(regions_for_study, ~{
      p()
      # get_phhs_parallel(db = .x, db_pops = db_pops,
      #                   roads = ottawa_road_filtered_shp, min_phh_pop = 5, min_phhs_per_db=4, road_buffer_m = 5)
      get_phhs_polished(region = .x, region_idcol = "DBUID", region_popcol = "dbpop",
                        roads = local_roads, roads_idcol = "NGD_UID",
                        min_phh_pop = 5, phh_density = 0.005, min_phhs_per_region=4, road_buffer_m = 5)
    } , .options=furrr::furrr_options(seed=NULL)
    ,.progress = TRUE
    )
  })



  phh_valid <- phh_candidates[purrr::map_int(phh_candidates, length) > 0]

  phhs <- dplyr::tibble(x=phh_valid) |>
    tidyr::unnest(cols = c(x)) |>
    sf::st_as_sf()

  filename <- paste0(this_cd$CDUID, "-phhs-",Sys.Date(),".shp")

  sf::write_sf(phhs, paste0("output/bigtest/",filename))

}




### COMBINE THEM

z <- dir("output/bigtest") |>
  grep(pattern="shp", value=TRUE) |>
  grep(pattern="-05", value=TRUE)

z <- dplyr::tibble(filename=z) |>
  dplyr::mutate(cduid = substr(filename, 1, 4))

all_cds  |> dplyr::filter(!CDUID %in% z$cduid)
