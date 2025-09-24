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

for(i in 1:length(download_these$scientific_name)){
  ebirdst::ebirdst_download_status( species = download_these$scientific_name[i], 
                                    path = here::here("data/abundance"),
                                    download_abundance = TRUE, 
                                    download_occurrence = FALSE,
                                    download_count = FALSE,
                                    download_ranges = FALSE,
                                    download_regional = FALSE,
                                    download_pis = FALSE,
                                    download_ppms = FALSE,
                                    download_all = FALSE,
                                    pattern = "_27km_")
  print(paste0("Finished ", i, " of ", length(download_these$scientific_name) ))
}

eabl <- terra::rast(
  paste( here::here("data/abundance/2023/"), 
         download_these$species_code[i],
         "seasonal",
         paste0(download_these$species_code[i], "_abundance_seasonal_mean_27km_2023.tif"),
         sep = "/"))

eabl_map <- sf::st_read(
  paste( here::here("data/ranges/2023/"), 
         download_these$species_code[i],
         "ranges",
         paste0(download_these$species_code[i], "_range_smooth_27km_2023.gpkg"),
         sep = "/")) |> 
  sf::st_make_valid() |> 
  dplyr::filter(season == "breeding")

eabl_map <- nngeo::st_remove_holes(eabl_map, max_area = 1e12)

eabl_centroids <- terra::xyFromCell( eabl[[1]], 1:ncell(eabl[[1]])) |> 
  tibble::as_tibble() |> 
  dplyr::mutate(cell_id = dplyr::row_number()) |> 
  sf::st_as_sf(
    coords = c("x", "y"),
    crs = terra::crs(eabl[[1]]))

test <- sf::st_crop( eabl_centroids,
                     sf::st_transform(eabl_map,
                                      terra::crs(eabl[[1]])))

in_range_centroids <- sf::st_join(
  test, 
  sf::st_transform(eabl_map,
                   terra::crs(eabl[[1]]))) |> 
  dplyr::filter(!is.na(species_code)) |> 
  dplyr::select(cell_id, geometry)

land <- rnaturalearth::ne_download(scale = "small",
                                   type = "land", 
                                   category = "physical", returnclass = "sf")

coast <- land |> 
  sf::st_transform(crs = sf::st_crs(in_range_centroids)) |> 
  sf::st_boundary()

range <- eabl_map |> 
  sf::st_transform(crs = sf::st_crs(in_range_centroids)) |> 
  sf::st_boundary()

tmp <- sf::st_distance(
  in_range_centroids, 
  coast)

re_dist <- sf::st_distance(
  in_range_centroids, 
  range)

in_range_centroids$coast_dist <- apply(tmp, 1, min)
in_range_centroids$range_dist <- apply(re_dist, 1, min)

#test$in_range <- as.numeric(cell_in_range)

ggplot() + 
  geom_sf(data = test, aes(color = coast_dist)) +
  geom_sf(data = range, color = "red", fill = NA) +
  scale_color_viridis_c()



in_range_centroids |> 
  filter( coast_dist > 1000*100) |> 
  dplyr::filter(range_dist <= as.numeric( quantile(in_range_centroids$range_dist, 0.15))) |> 
  # filter(in_range == TRUE) |> 
  ggplot() +
  geom_sf( aes(color = range_dist)) +
  geom_sf(data = range, color = "red", fill = NA) +
  scale_color_viridis_c()
