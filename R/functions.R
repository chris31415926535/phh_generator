
# Function called for side effects: create PHHs for each CD using DB populations
# We use a for-loop to loop through all Ontario Census Divisions, find PHHs,
# and then save results to file in a datestamped directory.
generate_phhs <- function (all_cds, all_dbs, db_pops, ontario_roads, province_name) {

  # set the date stamp once up front, in case running the analysis spans midnight
  datestamp <- Sys.Date()
  output_dir <- paste0("output/",province_name, "-", datestamp)
  if (!dir.exists(output_dir)) dir.create(output_dir)

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

    sf::write_sf(phhs, paste0(output_dir, "/",filename))

  }

  # return simple success marker
  return(TRUE)
}
