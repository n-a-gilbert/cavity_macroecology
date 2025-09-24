library(here)
library(tidyverse)
library(ebirdst)
library(sf)
library(rnaturalearth)
library(terra)
library(nngeo)

download_these <- readr::read_csv(here::here("data/cavity_nesters_review.csv")) |> 
  dplyr::filter(com == "Eastern Bluebird") |> 
  dplyr::rename(scientific_name = sci,
                species_code = code)

cell_coast_dist <- readr::read_csv(here::here("data/cell_coast_dist.csv"))

df <- list(list())
for( i in 1:nrow(download_these)){
  
  ex_rast <- terra::rast(
    paste( here::here("data/abundance/2023/"), 
           download_these$species_code[i],
           "seasonal",
           paste0(download_these$species_code[i], "_abundance_seasonal_mean_27km_2023.tif"),
           sep = "/"))
  
  ex_rast_centroids <- terra::xyFromCell( ex_rast[[1]], 1:ncell(ex_rast[[1]])) |> 
    tibble::as_tibble() |> 
    tibble::add_column(n = as.numeric( terra::values(ex_rast[[1]]))) |> 
    dplyr::mutate(cell_id = dplyr::row_number()) |> 
    sf::st_as_sf(
      coords = c("x", "y"),
      crs = terra::crs(ex_rast[[1]])) |> 
    dplyr::filter(cell_id %in% cell_coast_dist$cell_id) # keep only cells in focal area
  
  ####
  ex_map <- sf::st_read(
    paste( here::here("data/ranges/2023/"), 
           download_these$species_code[i],
           "ranges",
           paste0(download_these$species_code[i], "_range_smooth_27km_2023.gpkg"),
           sep = "/")) |> 
    sf::st_make_valid() |> 
    dplyr::filter(season == "breeding")
  
  ex_map <- nngeo::st_remove_holes(ex_map, max_area = 1e12) |> 
    sf::st_transform(crs = sf::st_crs(ex_rast_centroids))
  
  in_range_centroids <- sf::st_join(
    ex_rast_centroids, ex_map) |> 
    dplyr::filter(!is.na(species_code)) |> 
    dplyr::select(cell_id, n, geometry)
  
  range <- ex_map |> 
    sf::st_boundary()
  
  re_dist <- sf::st_distance(
    in_range_centroids, 
    range)
  
  in_range_centroids$range_dist <- apply(re_dist, 1, min)
  
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

dplyr::bind_rows(df) |> 
  dplyr::group_by(com, scientific_name, species_code) |> 
  dplyr::filter(coast_dist > (100 * 1000 )) |> # remove cells within 100 km of coast
  dplyr::mutate( soft_edge = ifelse(range_dist <= quantile(range_dist, 0.1), "edge", "nonedge")) |>
  dplyr::left_join(
    ex_rast_centroids) |> 
  ggplot(aes(geometry = geometry, color = soft_edge)) + 
  geom_sf()

