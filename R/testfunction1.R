

testfunction <- function(db){
  message(db$DBUID)
  phh_householdsize <- 15
  phh_density  <- 0.005

  #db <- test_dbs[i,]
  db_pop <- neighbourhoodstudy::ottawa_dbs_pop2021$dbpop2021[neighbourhoodstudy::ottawa_dbs_pop2021$DBUID == db$DBUID]

  if (db_pop == 0) return(dplyr::tibble());

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




get_phhs_parallel <- function(db, db_pops, roads){
  #db_pops <- neighbourhoodstudy::ottawa_dbs_pop2021
  message(db$DBUID)
  phh_householdsize <- 15
  phh_density  <- 0.005

  #db <- test_dbs[i,]
  db_pop <- db_pops$dbpop2021[db_pops$DBUID == db$DBUID]

  if (db_pop == 0) return(dplyr::tibble());

  num_points <- round(db_pop/householdsize)

  # get the roads that intersect the db plus a buffer?
  roads_touching_db <- sf::st_filter(roads, sf::st_buffer(db, 5))


  # ggplot() + geom_sf(data=db) + geom_sf(data=roads)

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

  #ggplot() + geom_sf(data=db) + geom_sf(data=roads) + geom_sf(data=phh_indb)

  result <- dplyr::as_tibble(phh_indb)

  # se tthe DBUID
  result$DBUID <- db$DBUID

  result$pop <- db_pop/nrow(result)
  #phh_nearpoint|> ggplot() + geom_sf(data=phh_buffer) +geom_sf(aes(colour="red"))

  return(result)
  #results[[i]] <- result
}
