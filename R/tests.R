targets::tar_load(input_regions)
targets::tar_load(ottawa_road_filtered_shp)


roads_wgs <- sf::st_transform(ottawa_road_filtered_shp, crs = "WGS84")
region = input_regions[1,]; region_idcol = "DBUID"; region_popcol = "dbpop2021"; roads = ottawa_road_filtered_shp; roads_idcol = "NGD_UID";  phh_density = 0.005; min_phh_pop = 5; min_phhs_per_region = 1; min_phh_distance = 25; road_buffer_m = 5; delta_distance_m = 5; skip_unpopulated_regions = TRUE
roads_idcol = NA

# works full
get_phhs_polished(region = input_regions[1,], region_idcol = "DBUID", region_popcol = "dbpop2021", roads = ottawa_road_filtered_shp, roads_idcol = "NGD_UID")

# works with no population column
get_phhs_polished(region = input_regions[1,], region_idcol = "DBUID",  roads = ottawa_road_filtered_shp, roads_idcol = "NGD_UID")

# works with no road id column
get_phhs_polished(region = input_regions[1,], region_idcol = "DBUID",  roads = ottawa_road_filtered_shp, roads_idcol = NA)


# should fail: different crs
get_phhs_polished(region = input_regions[1,], region_idcol = "DBUID", region_popcol = "dbpop2021", roads = roads_wgs, roads_idcol = "NGD_UID")

# should fail: not sf
get_phhs_polished(region = dplyr::tibble(), region_idcol = "DBUID", region_popcol = "dbpop2021", roads = roads_wgs, roads_idcol = "NGD_UID")
get_phhs_polished(region = input_regions[1,], region_idcol = "DBUID", region_popcol = "dbpop2021", roads = dplyr::tibble(), roads_idcol = "NGD_UID")

# should fail: no roads or regions
get_phhs_polished(region = input_regions[1,], region_idcol = "DBUID", region_popcol = "dbpop2021", roads = ottawa_road_filtered_shp[0,], roads_idcol = "NGD_UID")
get_phhs_polished(region = input_regions[0,], region_idcol = "DBUID", region_popcol = "dbpop2021", roads = ottawa_road_filtered_shp[0,], roads_idcol = "NGD_UID")

# should fail: using WGS84
get_phhs_polished(region = sf::st_transform(input_regions[1,], crs="WGS84"), region_idcol = "DBUID", region_popcol = "dbpop2021", roads = roads_wgs, roads_idcol = "NGD_UID")

# should fail: bad column ids
get_phhs_polished(region = input_regions[1,], region_idcol = "asdf", region_popcol = "dbpop2021", roads = ottawa_road_filtered_shp, roads_idcol = "NGD_UID")
get_phhs_polished(region = input_regions[1,], region_idcol = "DBUID", region_popcol = "asdf", roads = ottawa_road_filtered_shp, roads_idcol = "NGD_UID")
get_phhs_polished(region = input_regions[1,], region_idcol = "DBUID", region_popcol = "dbpop2021", roads = ottawa_road_filtered_shp, roads_idcol = "asdsf")



#
