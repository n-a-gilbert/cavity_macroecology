# calculate distance to range edge of each nonzero abundance pixel
library(here)
library(tidyverse)
library(ebirdst)
library(sf)
library(rnaturalearth)
library(terra)
library(nngeo)

# species list
download_these <- readr::read_csv(here::here("data/final_species_list.csv")) |> 
  dplyr::rename(scientific_name = sci,
                species_code = code)

# distance of each cell to nearest coast, created by 2.1_calculate_coast_distance.R
cell_coast_dist <- readr::read_csv(here::here("data/cell_coast_dist.csv"))

# loop through species
df <- list(list())
for( i in 1:nrow(download_these)){
if(download_these$species_code[i] == "nohowl"){ # hawk owl gave issues for some reason? 
  next
}
  # species' abundance raster to get grid cells
  ex_rast <- terra::rast(
    paste( here::here("data/abundance/2023/"), 
           download_these$species_code[i],
           "seasonal",
           paste0(download_these$species_code[i], "_abundance_seasonal_mean_27km_2023.tif"),
           sep = "/"))
  
  # centroids, retain only those within the focal area as indicated in cell coast distance table
  ex_rast_centroids <- terra::xyFromCell( ex_rast[[1]], 1:ncell(ex_rast[[1]])) |> 
    tibble::as_tibble() |> 
    tibble::add_column(n = as.numeric( terra::values(ex_rast[[1]]))) |> 
    dplyr::mutate(cell_id = dplyr::row_number()) |> 
    sf::st_as_sf(
      coords = c("x", "y"),
      crs = terra::crs(ex_rast[[1]])) |> 
    dplyr::filter(cell_id %in% cell_coast_dist$cell_id) # keep only cells in focal area
  
  # load the breeding-season (or year-round for sedentary) range map
  ex_map <- sf::st_read(
    paste( here::here("data/ranges/2023/"), 
           download_these$species_code[i],
           "ranges",
           paste0(download_these$species_code[i], "_range_smooth_27km_2023.gpkg"),
           sep = "/")) |> 
    sf::st_make_valid() |> 
    dplyr::filter(season == "breeding" | season == "resident")
  
  # fill in holes in the map
  ex_map <- nngeo::st_remove_holes(ex_map, max_area = 1e12) |> 
    sf::st_transform(crs = sf::st_crs(ex_rast_centroids))
  
  # figure out which cells are within the range
  in_range_centroids <- sf::st_join(
    ex_rast_centroids, ex_map) |> 
    dplyr::filter(!is.na(species_code)) |> 
    dplyr::select(cell_id, n, geometry)
  
  # get boundary of the range
  range <- ex_map |> 
    sf::st_boundary()
  
  # calculate range-edge distance: the distance from each grid cell centroid to the range edge boundary
  re_dist <- sf::st_distance(
    in_range_centroids, 
    range)
  
  # get the miniminum re distance
  in_range_centroids$range_dist <- apply(re_dist, 1, min)
  
  # each table in the list of tables is a species with the cell coast distances and the range edge distance
  df[[i]] <- cell_coast_dist |> 
    dplyr::right_join(
      in_range_centroids |> 
        sf::st_drop_geometry()) |> 
    tibble::add_column(com = download_these[[i, "com"]],
                       scientific_name = download_these[[i, "scientific_name"]], 
                       species_code = download_these[[i, "species_code"]]) |> 
    dplyr::select(cell_id, com, scientific_name, species_code, n, coast_dist, range_dist) |> 
    dplyr::filter(!is.na(n) & n > 0 ) 
  print(paste0("finished ", i, " of ", nrow(download_these)))
}

# save the file
dplyr::bind_rows(df) |> 
  readr::write_csv(file = here::here("data/cavity_nesters_abundance_dists.csv"))