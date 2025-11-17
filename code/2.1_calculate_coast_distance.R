# calculate distance between each grid cell and nearest coastline
library(here)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(terra)

# just grabbing the weird eBird code for a species to create a template raster
download_these <- readr::read_csv(here::here("data/cavity_nesters_review.csv")) |> 
  dplyr::filter(com == "Eastern Bluebird") |> 
  dplyr::rename(scientific_name = sci,
                species_code = code)

# grabbed this code from a loop, so giving it a hard-code i to use :)
# this is an example raster
i <- 1
ex_rast <- terra::rast(
  paste( here::here("data/abundance/2023/"), 
         download_these$species_code[i],
         "seasonal",
         paste0(download_these$species_code[i], "_abundance_seasonal_mean_27km_2023.tif"),
         sep = "/"))

# focal area shapefile
# created this with 1.6_create_focal_area_shapefile.R
focal_area <- sf::st_read( here::here("data/focal_area2.shp")) |> 
  sf::st_transform(crs = terra::crs(ex_rast))

# centroids of the example raster as an sf object
ex_rast_centroids <- terra::xyFromCell( ex_rast[[1]], 1:ncell(ex_rast[[1]])) |> 
  tibble::as_tibble() |> 
  dplyr::mutate(cell_id = dplyr::row_number()) |> 
  sf::st_as_sf(
    coords = c("x", "y"),
    crs = terra::crs(ex_rast[[1]]))

focal_cells <- sf::st_join(
  ex_rast_centroids, 
  focal_area) |> 
  dplyr::filter(!is.na(FID))

# shapefile of land
land <- rnaturalearth::ne_download(scale = "small",
                                   type = "land", 
                                   category = "physical", returnclass = "sf")

# have to get boundary of the land shapefile
coast <- land |> 
  sf::st_transform(crs = sf::st_crs(focal_cells)) |> 
  sf::st_boundary()

tmp <- sf::st_distance(
  focal_cells, 
  coast)

focal_cells$coast_dist <- apply(tmp, 1, min)

cell_coast_dist <- focal_cells |> 
  sf::st_drop_geometry() |> 
  dplyr::select(cell_id, coast_dist)

readr::write_csv(cell_coast_dist, 
                 here::here("data/cell_coast_dist.csv"))