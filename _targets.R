library(targets, quietly = TRUE)
library(sf, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(future, quietly = TRUE)

#24 workers, 5.3 minutes
future::plan(future::multisession, workers = 20)

source("R/testfunction1.R")


##22.9 minutes on march 28
## 4.6 minutes for the PHHs on the battlecruiser, march 31 :D

list(
  # set up road network
  # https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/rnf-frr/index2021-eng.cfm?year=21
  # https://www150.statcan.gc.ca/n1/en/catalogue/92-500-G2021001
  # https://www12.statcan.gc.ca/census-recensement/2021/geo/ref/domain-domaine/index2021-eng.cfm?lang=e&id=CLASS
  tar_target(statscan_roadnetwork_filepath, "~/datascience/data/spatial/lrnf000r21a_e/lrnf000r21a_e.shp"),

  tar_target(ottawa_db_shp, sf::st_transform(neighbourhoodstudy::ottawa_dbs_shp2021, crs=32189)),

  tar_target(ottawa_buffer_shp, sf::st_union(ottawa_db_shp) |> sf::st_buffer(100)),

  tar_target(ontario_road_full_shp,
             sf::read_sf(statscan_roadnetwork_filepath) |>
               dplyr::filter(PRNAME_L =="Ontario" | PRNAME_R == "Ontario") |>
               sf::st_transform(crs=32189) |>
               sf::st_filter(ottawa_buffer_shp)
             ), #

  tar_target(ottawa_road_filtered_shp,
             dplyr::filter(ontario_road_full_shp, CLASS %in% c(20:23))),

  tar_target(db_pops, dplyr::mutate(neighbourhoodstudy::ottawa_dbs_pop2021, DBUID = as.character(DBUID))),

  # create our input regions by combining all previous data into one sf
  tar_target(input_regions, {
    dplyr::left_join(ottawa_db_shp, db_pops) |>
      dplyr::select(DBUID, dbpop2021)
  }),

  # shapefile of DBs for study, can trim it here for testing
  tar_target(all_regions, {
    split(input_regions, ~ DBUID)#[0001:2500]#[1:50]
  }),

  tar_target(regions_for_study, all_regions
             ),


  tar_target(phhs, {
    progressr::with_progress({
      ##https://furrr.futureverse.org/articles/progress.html

      p <- progressr::progressor(length(regions_for_study))

      phh_candidates <- furrr::future_map(regions_for_study, ~{
        p()
        # get_phhs_parallel(db = .x, db_pops = db_pops,
        #                   roads = ottawa_road_filtered_shp, min_phh_pop = 5, min_phhs_per_db=4, road_buffer_m = 5)
        get_phhs_polished(region = .x, region_idcol = "DBUID", region_popcol = "dbpop2021",
                          roads = ottawa_road_filtered_shp, roads_idcol = "NAME",
                          min_phh_pop = 5, phh_density = 0.005, min_phhs_per_region=4, road_buffer_m = 5)
      } , .options=furrr::furrr_options(seed=NULL)
      ,.progress = TRUE
      )
    })

    #phh_candidates

    phh_valid <- phh_candidates[purrr::map_int(phh_candidates, length) > 0]


    phhs <- dplyr::tibble(x=phh_valid) |>
      tidyr::unnest(cols = c(x)) |>
      sf::st_as_sf()

    phhs
  }),

  tar_target(save_phhs,
             {
               sf::write_sf(phhs, sprintf("output/phhs-%s.shp", Sys.Date()))
               TRUE
               }),
  #
  # tar_target(testmap, {
  #   ggplot(sf::st_transform(phhs,crs="WGS84")) + geom_sf() + geom_sf(data=sf::st_transform(ottawa_road_filtered_shp, crs="WGS84"))
  #   #+ coord_sf(xlim=c(-75.4,-75.6), ylim=c(45.4,45.5))
  # }),

  NULL
)
