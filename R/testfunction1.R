

testfunction <- function(db){
  message(db$DBUID)
  phh_householdsize <- 15
  phh_density  <- 0.005

  #db <- test_dbs[i,]
  db_pop <- neighbourhoodstudy::ottawa_dbs_pop2021$dbpop2021[neighbourhoodstudy::ottawa_dbs_pop2021$DBUID == db$DBUID]

  if (db_pop == 0 | length(db_pop) == 0) return(dplyr::tibble());

  num_points <- round(db_pop/householdsize)

  # get the roads that intersect the db plus a buffer?
  roads <- sf::st_filter(test_roads, sf::st_buffer(db, 5))


  # ggplot() + geom_sf(data=db) + geom_sf(data=roads)

  # if it doesn't intersect any roads
  # can test with DBUID 35061853010
  if (nrow(roads) == 0) {
    result <- sf::st_centroid(db)
    result$DBUID <- db$DBUID
    result$pop <- db_pop
    result$offroad <- TRUE
    return(dplyr::tibble())
  }

  ## TODO FIXME: figure out how many phhs to do. should user either density or number
  # maybe depending on size?
  # make points but they're on the streets, cast to points
  phh_onstreet <- sf::st_line_sample(roads, density = phh_density) |>
    sf::st_cast("POINT")

  #phh_onstreet <- sf::st_cast(sf::st_line_sample(roads, n=num_points), "POINT")
  phh_buffer <- sf::st_buffer(phh_onstreet, dist = 5)
  # get db centroid
  db_centroid <- sf::st_centroid(db)

  #ggplot() + geom_sf(data=db) +  geom_sf(data=roads) + geom_sf(data=phh_onstreet) + geom_sf(data=phh_buffer, fill=NA) + geom_sf(data=db_centroid)

  # this gives us a line from each buffer to the centre point. but we want points!
  phh_nearline <- sf::st_nearest_points(db_centroid, phh_buffer)
  # so we extract just the second set of points
  phh_nearpoint <- sf::st_cast(phh_nearline, "POINT")[c(FALSE, TRUE)] |> sf::st_as_sf()

  phh_indb <- sf::st_filter(phh_nearpoint, db)

  #ggplot() + geom_sf(data=db) + geom_sf(data=roads) + geom_sf(data=phh_indb)

  result <- dplyr::as_tibble(phh_indb)

  # se tthe DBUID
  result$DBUID <- db$DBUID

  result$pop <- db_pop/nrow(result)
  #phh_nearpoint|> ggplot() + geom_sf(data=phh_buffer) +geom_sf(aes(colour="red"))

  return(result)
  #results[[i]] <- result
}




get_phhs_parallel <- function(db, db_pops, roads, min_phh_pop = 5, road_buffer_m = 5 ){
  #db_pops <- neighbourhoodstudy::ottawa_dbs_pop2021
  message(db$DBUID)
  #phh_householdsize <- 15
  phh_density  <- 0.005

  #db <- test_dbs[i,]
  db_pop <- db_pops$dbpop2021[db_pops$DBUID == db$DBUID]

  # if no population, return empty result
  if (db_pop == 0) return(dplyr::tibble());

  #num_points <- round(db_pop/householdsize)

  # get the roads that intersect the db plus a buffer?
  roads_touching_db <- sf::st_filter(roads, sf::st_buffer(db, road_buffer_m))


  # ggplot() + geom_sf(data=db) + geom_sf(data=roads_touching_db)

  # if it doesn't intersect any roads
  # can test with DBUID 35061853010
  if (nrow(roads_touching_db) == 0) {
    result <- sf::st_centroid(db)
    result$DBUID <- db$DBUID
    result$pop <- db_pop
    result$offroad <- TRUE
    return(dplyr::tibble())
  }

  ## TODO FIXME: figure out how many phhs to do. should user either density or number
  # maybe depending on size?
  # make points but they're on the streets, cast to points

  phh_onstreet <- sf::st_line_sample(roads_touching_db, density = phh_density) |>
    sf::st_cast("POINT")

  # get phh points slightly off the street based on the on-street points
  # this one is okay but doesn't work on weird-shaped dbs like 35060126020  with holes in them
  phh_indb <-  get_phh_points_easy (db, phh_onstreet, delta_distance = 5)

  # only if we find none, use the more complicated method
  #phh_indb <- get_phh_points_pushpull (db, phh_onstreet, delta_distance = 5) # this one is a bit slower but maybe works better? for now
  #if (nrow(phh_indb) == 0) phh_indb <- get_phh_points_pushpull (db, phh_onstreet, delta_distance = 5) # this one is a bit slower but maybe works better? for now


  #ggplot() + geom_sf(data=db) + geom_sf(data=roads_touching_db) + geom_sf(data=phh_indb)

  ## make sure we get the right number of points here
  # THIS ALGORITHM ISN'T QUITE RIGHT, IT OFTEN GIVES ONE TOO MANY
  if (db_pop/nrow(phh_indb) < min_phh_pop) {
    num_needed <- ceiling(db_pop/min_phh_pop)
    #message("sdaf")
    num_orig <- nrow(phh_indb)
    num_per_rep <- ceiling(num_orig/num_needed)
    num_reps <- ceiling(num_orig/num_per_rep)
    # looks complex but this is to whittle our list down
    # say we have 6 but need 4.
    filter_index <- rep(c(TRUE, rep(FALSE, times= num_per_rep)), times=(num_reps+1))[1:num_orig]
    phh_indb_filtered <- phh_indb[filter_index,]
  } else {
    phh_indb_filtered <- phh_indb
  }

  #ggplot() + geom_sf(data=db) + geom_sf(data=roads_touching_db) + geom_sf(data=phh_indb_filtered)

  result <- dplyr::as_tibble(phh_indb_filtered)

  # se tthe DBUID
  result$DBUID <- db$DBUID

  result$pop <- db_pop/nrow(result)
  #phh_nearpoint|> ggplot() + geom_sf(data=phh_buffer) +geom_sf(aes(colour="red"))

  return(result)
  #results[[i]] <- result
}



get_phh_points_easy <- function (db, phh_onstreet, delta_distance = 5){

  #phh_onstreet <- sf::st_cast(sf::st_line_sample(roads_touching_db, n=num_points), "POINT")

  phh_buffer <- sf::st_buffer(phh_onstreet, dist = 5)
  # get db centroid
  db_centroid <- sf::st_centroid(db)

  #ggplot() + geom_sf(data=db) +  geom_sf(data=roads_touching_db) + geom_sf(data=phh_onstreet) + geom_sf(data=phh_buffer, fill=NA) + geom_sf(data=db_centroid)

  # this gives us a line from each buffer to the centre point. but we want points!
  phh_nearline <- sf::st_nearest_points(db_centroid, phh_buffer)

  # so we extract just the second set of points
  phh_nearpoint <- sf::st_cast(phh_nearline, "POINT")[c(FALSE, TRUE)] |>
    sf::st_as_sf()

  phh_indb <- sf::st_filter(phh_nearpoint, db)

  return(phh_indb)
}


# testing--if we use NAD with metres is there a way to use simple geometry to try
# to get points X metres towards and away from the centroid? then if hte point stowards
# aren't in the db because of weird geometries we can try the pushes away
get_phh_points_pushpull <- function(db, phh_onstreet, delta_distance = 5){
  #
  # phh_onstreet <- sf::st_line_sample(roads_touching_db, density = phh_density) |>
  #   sf::st_cast("POINT")
  #
  # get db centroid
  db_centroid <- sf::st_centroid(db)
  db_centroid_coords <- dplyr::as_tibble(sf::st_coordinates(db_centroid))
  colnames(db_centroid_coords) <- c("DB_X", "DB_Y")


  phh_onstreet_coords <- dplyr::as_tibble(sf::st_coordinates(phh_onstreet))
  colnames(phh_onstreet_coords) <- c("PHH_X", "PHH_Y")

  phh_foranalysis <-   dplyr::bind_cols(phh_onstreet_coords, db_centroid_coords)

# DPLYR
# bench::mark(code = {
#     phh_foranalysis |>
#     dplyr::mutate(deltaY = DB_Y-PHH_Y,
#                   deltaX = DB_X-PHH_X,
#                   magnitude = sqrt((deltaY)^2 + (deltaX)^2),
#                  unit_vecY = deltaY/magnitude,
#                 unit_vecX = deltaX/magnitude
#   ,
#   PHH_X_pull = PHH_X + unit_vecX * delta_distance,
#   PHH_Y_pull = PHH_Y + unit_vecY * delta_distance,
#   PHH_X_push = PHH_X - unit_vecX * delta_distance,
#   PHH_Y_push = PHH_Y - unit_vecY * delta_distance,
#                   )
#
# })

## BASE R is 100x faster than dplyr :(

# bench::mark(code={
phh_foranalysis$deltaY = phh_foranalysis$DB_Y - phh_foranalysis$PHH_Y
phh_foranalysis$deltaX = phh_foranalysis$DB_X - phh_foranalysis$PHH_X
phh_foranalysis$magnitude = sqrt(phh_foranalysis$deltaY^2 + phh_foranalysis$deltaX^2)
phh_foranalysis$unit_vecY = phh_foranalysis$deltaY/phh_foranalysis$magnitude
phh_foranalysis$unit_vecX = phh_foranalysis$deltaX/phh_foranalysis$magnitude
phh_foranalysis$PHH_X_pull = phh_foranalysis$PHH_X + phh_foranalysis$unit_vecX * delta_distance
phh_foranalysis$PHH_Y_pull = phh_foranalysis$PHH_Y + phh_foranalysis$unit_vecY * delta_distance
phh_foranalysis$PHH_X_push = phh_foranalysis$PHH_X - phh_foranalysis$unit_vecX * delta_distance
phh_foranalysis$PHH_Y_push = phh_foranalysis$PHH_Y - phh_foranalysis$unit_vecY * delta_distance
# })

# bench::mark(code = {
phh_push <- sf::st_as_sf(dplyr::select(phh_foranalysis, PHH_X_push, PHH_Y_push), coords = c("PHH_X_push","PHH_Y_push"), crs=32189)
phh_push$id = 1:nrow(phh_push)
phh_pull <- sf::st_as_sf(dplyr::select(phh_foranalysis, PHH_X_pull, PHH_Y_pull), coords = c("PHH_X_pull","PHH_Y_pull"), crs=32189)
phh_pull$id = 1:nrow(phh_pull)
# })

# ggplot() + geom_sf(data=db) +  geom_sf(data=roads_touching_db) + geom_sf(data=phh_push, colour="red")+ geom_sf(data=phh_pull, colour="blue") +  geom_sf(data=db_centroid)
  #phh_onstreet <- sf::st_cast(sf::st_line_sample(roads_touching_db, n=num_points), "POINT")

  # phh_buffer <- sf::st_buffer(phh_onstreet, dist = 5)


  #ggplot() + geom_sf(data=db) +  geom_sf(data=roads_touching_db) + geom_sf(data=phh_onstreet) + geom_sf(data=phh_buffer, fill=NA) + geom_sf(data=db_centroid)

  # this gives us a line from each buffer to the centre point. but we want points!
  # phh_nearline <- sf::st_nearest_points(db_centroid, phh_buffer)
  #
  # # so we extract just the second set of points
  # phh_nearpoint <- sf::st_cast(phh_nearline, "POINT")[c(FALSE, TRUE)] |>
  #   sf::st_as_sf()

  phh_indb <- sf::st_filter(phh_pull, db)

  # if none of the "pull" phhs are in the db, then check the push ones
  if (nrow(phh_indb) == 0) {
    phh_indb <- sf::st_filter(phh_push, db)
  }

  # if none of the push ones are in it either (weird!!) just give the db centroid
  # we manually remove junk from th ecentroid, this won't generalize well FIXME TODO
  # if (nrow(phh_indb) == 0) {
  #   phh_indb <- db_centroid
  #   phh_indb$DGUID <- NULL
  #   phh_indb$DBRPLAMX <- NULL
  #   phh_indb$DBRPLAMY <- NULL
  #   phh_indb$LANDAREA <- NULL
  #   phh_indb$PRUID <- NULL
  # }

  phh_indb$id <- NULL
  phh_indb <- dplyr::rename(phh_indb, x = geometry)

  return (phh_indb)
}
