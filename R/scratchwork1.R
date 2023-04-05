powers <- 8
results <- data.frame(iterations = 10^(1:powers), time = rep(NA_real_, powers))
i=8
for (i in 1:powers) {
  start <- tictoc::tic()
  for (j in 1:as.numeric(10^i)) {2+2}
  stop <- tictoc::toc()

  time <- stop$toc - stop$tic

  results$time[[i]] <- time

}

results
plot(results$iterations, results$time)


####################


z <- neighbourhoodstudy::ottawa_dbs_shp2021 |>
  dplyr::bind_cols(
    sf::st_centroid(neighbourhoodstudy::ottawa_dbs_shp2021|>
                     sf::st_make_valid() ) |>
                     sf::st_coordinates() |>
                     tibble::as_tibble())

test_dbs <- z |> dplyr::filter(Y < 45.3, X < -75.9) |>
  sf::st_transform(crs=32189)

test_roads <- ottawa_road_filtered_shp |>
  sf::st_filter(test_dbs)

ggplot() + geom_sf(data = test_dbs) + geom_sf(data=test_roads)

# loop through dbs...

results <- list()
num_dbs <- nrow(test_dbs)
for (i in 1:num_dbs) {
  message(i)
  phh_householdsize <- 15
  phh_density  <- 0.005

  db <- test_dbs[i,]
  db_pop <- neighbourhoodstudy::ottawa_dbs_pop2021$dbpop2021[neighbourhoodstudy::ottawa_dbs_pop2021$DBUID == db$DBUID]

  if (db_pop == 0) next;

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
    next
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

  results[[i]] <- result

}

test_dbs_list <- split(test_dbs, ~ DBUID)
#test <- purrr::map(test_dbs_list[1:5], testfunction)
test <- furrr::future_map(test_dbs_list, testfunction)

phhs <- dplyr::tibble(x=results) |> tidyr::unnest(x) |>
  sf::st_as_sf()


# make some plots

ggplot() + geom_sf(data = test_dbs) + geom_sf(data = phhs)

ggplot() + geom_sf(data=test_roads) +   geom_sf(data = phhs, aes(colour=pop))

ggplot() + geom_sf(data=roads) +   geom_sf(data = phhs, aes(colour=pop))

ggplot(db) + geom_sf()


### test

# missing DBs all have 0 pop?
test_dbs |>
  dplyr::left_join(neighbourhoodstudy::ottawa_dbs_pop2021) |>
  dplyr::filter(!DBUID %in% phhs$DBUID) |>
  dplyr::filter(dbpop2021 > 0)


# test random DBs
# 35061498005  gives points outside of db
db_rnd <- dplyr::filter(test_dbs, DBUID=="35061498005") #35060001002
db_rnd <- dplyr::slice_sample(test_dbs, n=1)
phh_rnd <- dplyr::filter(phhs, DBUID == db_rnd$DBUID)

ggplot() + geom_sf(data=db_rnd) + geom_sf(data=phh_rnd)


which(test_dbs$DBUID=="35061498005")



####### testing functionalized for map

future::plan(future::multisession(), workers=8)
future::nbrOfFreeWorkers()

test_dbs_list <- split(test_dbs, ~ DBUID)

num_to_test <- length(test_dbs_list)
tictoc::tic()
test <- purrr::map(test_dbs_list[1:num_to_test], testfunction)
tictoc::toc()


# with furrr: 30.601 sec elapsed
tictoc::tic()
test_furrr <- furrr::future_map(test_dbs_list[1:num_to_test], testfunction, .options=furrr::furrr_options(seed=NULL))
tictoc::toc()

############################### try for ALLLLLL

ottawa_dbs <-  split(sf::st_transform(neighbourhoodstudy::ottawa_dbs_shp2021, crs=32189), ~ DBUID)

# tictoc::tic()
# progressr::with_progress({
# ##https://furrr.futureverse.org/articles/progress.html
# p <- progressr::progressor(length(ottawa_dbs))
#
# test_furr_all <- furrr::future_map(ottawa_dbs, ~{
#  p()
#   testfunction(.x)
#
# } , .options=furrr::furrr_options(seed=NULL),
# .progress = TRUE)
#
# })
# tictoc::toc()


ottawa_dbs_list <-  split(sf::st_transform(neighbourhoodstudy::ottawa_dbs_shp2021, crs=32189), ~ DBUID)

########## PARALLEL
# 50 dbs, 2 workers: 17s
# 50 dbs, 4 workers: 12s
# 50 dbs, 6 workers: 12s
# 500dbs, 1 workers: 279s
# 500dbs, 4 workers: 95s
# 8559dbs, 4 workers: 1499.5s
future::plan(future::multisession, workers = 4)
future::nbrOfFreeWorkers()
tictoc::tic()
dbs_for_study <- ottawa_dbs#[1:500]
progressr::with_progress({
##https://furrr.futureverse.org/articles/progress.html

p <- progressr::progressor(length(dbs_for_study))

test_furr_all <- furrr::future_map(dbs_for_study, ~{
 p()
  get_phhs_parallel(db = .x, db_pops = neighbourhoodstudy::ottawa_dbs_pop2021,
                    roads = ottawa_road_filtered_shp, min_phh_pop = 5, road_buffer_m = 5)
} , .options=furrr::furrr_options(seed=NULL)
#,.progress = TRUE
)
})
tictoc::toc()

phhs <- dplyr::tibble(x=test_furr_all) |> tidyr::unnest(x) |>
  sf::st_as_sf()

sf::write_sf(test_)

############### NOT PARALLEL
# 500 dbs: 276s if not parallel!!!!
# 50 dbs: 30 seconds
tictoc::tic()
dbs_for_study <- ottawa_dbs[1:50]
progressr::with_progress({
  ##https://furrr.futureverse.org/articles/progress.html
  # 279.045 sec elapsed FOR 500 DBs.... should optimize that
  p <- progressr::progressor(length(dbs_for_study))

  test_purr_all <- purrr::map(dbs_for_study, ~{
    p()
    get_phhs_parallel(db = .x, db_pops = neighbourhoodstudy::ottawa_dbs_pop2021,
                      roads = ottawa_road_filtered_shp, min_phh_pop = 5, road_buffer_m = 5)
  })
})
tictoc::toc()


## EXPLORE RESULTS

phhs <- dplyr::tibble(x=test_furr_all) |> tidyr::unnest(x) |>
  sf::st_as_sf()

ggplot(sf::st_transform(phhs,crs="WGS84")) + geom_sf() + geom_sf(data=sf::st_transform(ottawa_road_filtered_shp, crs="WGS84")) +
  coord_sf(xlim=c(-75.4,-75.6), ylim=c(45.4,45.5))
# Error in (function (.x, .f, ..., .progress = FALSE)  :
#             ℹ In index: 4.
#           ℹ With name: 35060681004.
#
phhs <- dplyr::tibble(x=test_furrr) |> tidyr::unnest(x) |>
  sf::st_as_sf()

sf::write_sf(phhs, sprintf("output/phhs-%s.shp", Sys.Date()))



### any missing dbs???
library(tidyverse)
library(leaflet)
targets::tar_load_everything()

roads <- ottawa_road_filtered_shp; min_phh_pop = 5; road_buffer_m = 5; min_phh_distance = 25

dbs_missing <- dplyr::filter(ottawa_db_shp , ! DBUID %in% unique(phhs$DBUID)) |>
  dplyr::left_join(dplyr::mutate(db_pops, DBUID=as.character(DBUID)), dplyr::join_by("DBUID")) |>
  dplyr::select(DBUID, dbpop2021) |>
  dplyr::arrange(dplyr::desc(dbpop2021))

dbs_missing
sum(dbs_missing$dbpop2021)

ottawa_db_shp |> dplyr::filter(DBUID == dbs_missing$DBUID[[1]]) |> ggplot2::ggplot() + ggplot2::geom_sf()

db <- dbs_for_study[dbs_missing$DBUID[[1]]][[1]]

db <- dbs_for_study["35060002004"][[1]]
db <- dbs_for_study["35061703002"][[1]]

### anny really big dbs????

big_phhs <- phhs |> arrange(desc(pop)) |> slice_head(n=10) |> sf::st_transform(crs="WGS84")
big_dbs <- ottawa_db_shp |> dplyr::filter(DBUID %in% big_phhs$DBUID) |> sf::st_transform(crs="WGS84")

leaflet() |> addTiles() |> addPolygons(data = big_dbs, label = ~ DBUID, popup = ~ DBUID) |> addMarkers(data=big_phhs, label = ~ pop)


### how many phhs per db?

phhs |> sf::st_set_geometry(NULL) |> dplyr::group_by(DBUID) |> summarise(n=n()) |> pull(n) |> hist(breaks = 111) #summarise(median=median(n), mean=mean(n))


### checking dbs too close togehter

db <- dbs_for_study["35061303003"][[1]]

phhs |> dplyr::filter(DBUID == "35061303003") |> ggplot() + geom_sf()

### FOR WEIRD DB GEOMETRIES, LOOK AT MUNSTER!!!


####### TESTING BETTER FUNCTION
library(sf)
library(tidyverse)

targets::tar_load_everything()

region <- dplyr::left_join(dbs_for_study[[1]], dplyr::mutate(neighbourhoodstudy::ottawa_dbs_pop2021, DBUID = as.character(DBUID) )) |>
  dplyr::select(DBUID, dbpop2021, geometry)


roads = ottawa_road_filtered_shp; min_phh_pop = 5; min_phhs_per_region = 4; min_phh_distance = 25; road_buffer_m = 5; region_popcol = "dbpop2021"

sf::st_agr(region) = "constant"
sf::st_agr(roads) = "constant"


test <- get_phhs_polished(region = region, region_idcol = "DBUID", region_popcol = "dbpop2021", roads=roads, roads_idcol = "NAME")

test

ggplot() + geom_sf(data=region) + geom_sf(data=test)














##### point line intersection testing

road <- roads_touching_region[1,] |>
  dplyr::select(OBJECTID, NAME, TYPE)
points <- sf::st_line_sample(road, n=3) |>
  sf::st_cast("POINT") |>
  sf::st_as_sf()

sf::st_agr(road) = "constant"
sf::st_intersection(sf::st_buffer(road, 1), points) |> sf::st_difference()

pointinfo <- sf::st_intersection(road, sf::st_buffer(points,1)) |>
  sf::st_set_geometry(NULL)

dplyr::bind_cols(points, pointinfo)


#### TESTING NEW FNCTION

library(sf)
library(tidyverse)
targets::tar_load_everything()

region <- regions_for_study[[1]]
region_idcol = "DBUID"; region_popcol = "dbpop2021";
roads = ottawa_road_filtered_shp; roads_idcol = "NAME";
min_phh_pop = 5; phh_density = 0.005; min_phhs_per_region=4; road_buffer_m = 5

ggplot() + geom_sf(data=region)

.x = region
get_phhs_polished(region = .x, region_idcol = "DBUID", region_popcol = "dbpop2021",
                  roads = ottawa_road_filtered_shp, roads_idcol = "NAME",
                  min_phh_pop = 5, phh_density = 0.005, min_phhs_per_region=4, road_buffer_m = 5)






### reprexing error

a <- sf::st_as_sf(phh_candidates[[1]][1,]) |> dplyr::mutate(DBUID = as.character(DBUID), pop = as.numeric(pop)) |> dput()
b <- sf::st_as_sf(phh_candidates[[2]][1,])|> dplyr::mutate(DBUID = as.character(DBUID), pop = as.numeric(pop)) |> dput()

dput(a)

a <- dplyr::tibble(NAME = "Green Pine", DBUID = "35060001001", pop=9.4, geometry = )
a |> dplyr::mutate(DBUID = as.character(DBUID), pop = as.numeric(pop)) |> dput()


a


dplyr::bind_rows(a,b)

rbind(a,b)

bench::mark(rbind(a,b))
purrr::reduce(phh_valid, rbind)

bench::mark(purrr::reduce(phh_valid, rbind))


############ RUn funcTION FOR KINGSTON
library(sf)
library(tidyverse)
library(leaflet)
library(future)

future::plan(future::multisession, workers = 20)

cds <- sf::read_sf("~/datascience/data/spatial/lcd_000b21a_e/lcd_000b21a_e.shp")



#allcsds <- sf::read_sf("~/datascience/data/spatial/lcsd000b21a_e/lcsd000b21a_e.shp")
#alldbs <- sf::read_sf( "~/datascience/data/spatial/ldb_000b21a_e/ldb_000b21a_e.shp")
tictoc::tic()
ktowndbs <- sf::read_sf( "~/datascience/data/spatial/ldb_000b21a_e/ldb_000b21a_e.shp") |>
  dplyr::filter(stringr::str_detect(DBUID, "^3510"))
tictoc::toc()

dbpops <- readr::read_csv("~/datascience/data/spatial/geographic_attribute_file/2021_92-151_X.csv") |>
  dplyr::select(DBUID = "DBUID_IDIDU", dbpop = "DBPOP2021_IDPOP2021") |>
  dplyr::filter(stringr::str_detect(DBUID, "^3510"))
dbpops$DBUID <- as.character(dbpops$DBUID)

ktownbuffershp <- sf::st_union(ktowndbs) |> sf::st_buffer(5)
ggplot(ktownbuffershp) + geom_sf()

ktowndbs <- dplyr::left_join(ktowndbs, dbpops)

tictoc::tic()
ktownroads <- sf::read_sf("~/datascience/data/spatial/lrnf000r21a_e/lrnf000r21a_e.shp") |>
  sf::st_filter(ktownbuffershp)
tictoc::toc()

regions_for_study <- split(ktowndbs, ~ DBUID)


progressr::with_progress({
  ##https://furrr.futureverse.org/articles/progress.html

  p <- progressr::progressor(length(regions_for_study))

  phh_candidates <- furrr::future_map(regions_for_study, ~{
    p()
    # get_phhs_parallel(db = .x, db_pops = db_pops,
    #                   roads = ottawa_road_filtered_shp, min_phh_pop = 5, min_phhs_per_db=4, road_buffer_m = 5)
    get_phhs_polished(region = .x, region_idcol = "DBUID", region_popcol = "dbpop",
                      roads = ktownroads, roads_idcol = "NAME",
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


leaflet() |> addTiles() |> addPolygons(data=sf::st_transform(ktowndbs, crs="WGS84"))
#3510010
#ktowncsd <- sf::st_set_geometry(allcsds , NULL) |>
#  dplyr::filter(stringr::str_detect(CSDNAME, "Kingston"))


