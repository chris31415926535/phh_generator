targets::tar_load(input_regions)
targets::tar_load(ottawa_road_filtered_shp)

# works full
get_phhs_region(region = region_shp, region_idcol = "region_id", region_popcol = "population", roads = ottawa_road_filtered_shp, roads_idcol = "road_id")

# works with no population column
get_phhs_region(region = input_regions[1,], region_idcol = "DBUID",  roads = ottawa_road_filtered_shp, roads_idcol = "NGD_UID")

# works with no road id column
get_phhs_region(region = input_regions[1,], region_idcol = "DBUID",  roads = ottawa_road_filtered_shp, roads_idcol = NA)


# should fail: different crs
get_phhs_region(region = input_regions[1,], region_idcol = "DBUID", region_popcol = "dbpop2021", roads = roads_wgs, roads_idcol = "NGD_UID")

# should fail: not sf
get_phhs_region(region = dplyr::tibble(), region_idcol = "DBUID", region_popcol = "dbpop2021", roads = roads_wgs, roads_idcol = "NGD_UID")
get_phhs_region(region = input_regions[1,], region_idcol = "DBUID", region_popcol = "dbpop2021", roads = dplyr::tibble(), roads_idcol = "NGD_UID")

# should fail: no roads or regions
get_phhs_region(region = input_regions[1,], region_idcol = "DBUID", region_popcol = "dbpop2021", roads = ottawa_road_filtered_shp[0,], roads_idcol = "NGD_UID")
get_phhs_region(region = input_regions[0,], region_idcol = "DBUID", region_popcol = "dbpop2021", roads = ottawa_road_filtered_shp[0,], roads_idcol = "NGD_UID")

# should fail: using WGS84
get_phhs_region(region = sf::st_transform(input_regions[1,], crs="WGS84"), region_idcol = "DBUID", region_popcol = "dbpop2021", roads = roads_wgs, roads_idcol = "NGD_UID")

# should fail: bad column ids
get_phhs_region(region = input_regions[1,], region_idcol = "asdf", region_popcol = "dbpop2021", roads = ottawa_road_filtered_shp, roads_idcol = "NGD_UID")
get_phhs_region(region = input_regions[1,], region_idcol = "DBUID", region_popcol = "asdf", roads = ottawa_road_filtered_shp, roads_idcol = "NGD_UID")
get_phhs_region(region = input_regions[1,], region_idcol = "DBUID", region_popcol = "dbpop2021", roads = ottawa_road_filtered_shp, roads_idcol = "asdsf")



# TESTING with synthetic data


coords_shp = matrix(data = c(
  0,0,
  0,1,
  0.5,1.5,
  1,1,
  1,0,
  0,0
), ncol = 2, byrow = TRUE) * 1000

coords_extra <- matrix(
  data = c(
    0,0,
    0.5,0.4,
    1,0
  ), ncol = 2, byrow = TRUE) * 1000

coords_extra2 <- matrix(
  data = c(
    0.5, 0.4,
    0.5, 1.5
  ), ncol = 2, byrow = TRUE) * 1000

region_shp <- sf::st_multipoint(coords_shp) |>
  sf::st_cast("POLYGON")|>
  sf::st_sfc() |>
  sf::st_as_sf(crs = 32189) |>
  dplyr::mutate(region_id = "A")



region_shp  |> ggplot() + geom_sf()


road1_shp <- sf::st_linestring(coords_shp)
road2_shp <- sf::st_linestring(coords_extra)
road3_shp <- sf::st_linestring(coords_extra2)

road_shp <- sf::st_multilinestring(list(road1_shp, road2_shp, road3_shp)) |>
  sf::st_spl
sf::st_sfc() |>
  sf::st_as_sf(crs = 32189) |>
  dplyr::mutate(road_id = 1:dplyr::n())

ggplot(road_shp) + geom_sf()

ggplot() + geom_sf(data = region_shp) + geom_sf(data = road_shp)



phh_test <- get_phhs_region(region = region_shp, region_idcol = "region_id", region_popcol = NA, roads = road_shp,
                              roads_idcol = NA, phh_density = 0.001)

ggplot() + geom_sf(data = region_shp) + geom_sf(data = road_shp) + geom_sf(data = phh_test, colour = "blue")

(sf::st_distance(phh_test))

sf::st_line_sample()
#region = input_regions[1,]; region_idcol = "DBUID"; region_popcol = "dbpop2021"; roads = ottawa_road_filtered_shp; roads_idcol = "NGD_UID";  phh_density = 0.005; min_phh_pop = 5; min_phhs_per_region = 1; min_phh_distance = 25; road_buffer_m = 5; delta_distance_m = 5; skip_unpopulated_regions = TRUE

#roads_idcol = NA
get_phhs (regions = region_shp, region_idcol = "region_id",
          roads = road_shp, region_popcol = "population",
          roads_idcol = "road_id", phh_density = 0.005, min_phh_pop = 5,
          min_phhs_per_region = 1, min_phh_distance = 25, road_buffer_m = 5,
          delta_distance_m = 5, skip_unpopulated_regions = TRUE )
