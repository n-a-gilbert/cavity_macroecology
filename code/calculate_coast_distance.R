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

# for(i in 1:length(download_these$scientific_name)){
#   ebirdst::ebirdst_download_status( species = download_these$scientific_name[i], 
#                                     path = here::here("data/abundance"),
#                                     download_abundance = TRUE, 
#                                     download_occurrence = FALSE,
#                                     download_count = FALSE,
#                                     download_ranges = FALSE,
#                                     download_regional = FALSE,
#                                     download_pis = FALSE,
#                                     download_ppms = FALSE,
#                                     download_all = FALSE,
#                                     pattern = "_27km_")
#   print(paste0("Finished ", i, " of ", length(download_these$scientific_name) ))
# }

i <- 1
ex_rast <- terra::rast(
  paste( here::here("data/abundance/2023/"), 
         download_these$species_code[i],
         "seasonal",
         paste0(download_these$species_code[i], "_abundance_seasonal_mean_27km_2023.tif"),
         sep = "/"))

# get polygon for study area: US, Canada, and Mexico, buffered by a little bit
# sf::sf_use_s2(FALSE)
# us <- rnaturalearth::ne_countries(scale = "small", 
#                                   type = "countries") |> 
#   dplyr::filter(admin == "United States of America") |> 
#   sf::st_cast("POLYGON") |> 
#   dplyr::mutate(id = dplyr::row_number()) |> 
#   dplyr::filter(id %in% c(1,9)) |> # keep AK, lower 48; omit HI, territories, etc.
#   sf::st_buffer(dist = 0.25) 
# 
# can <- rnaturalearth::ne_countries(scale = "small", 
#                                    type = "countries") |> 
#   dplyr::filter(admin == "Canada") |> 
#   sf::st_cast("POLYGON") |> 
#   dplyr::mutate(id = dplyr::row_number()) |> 
#   dplyr::select(admin, id, geometry) |> 
#   dplyr::filter(id %in% c(
#     1, # main
#     11, # Newfoundland
#     16, #Moresbey island
#     18, # Vancouver
#     29, #Anticosti Island
#     30 #PEI,
#   )) |> 
#   sf::st_buffer(dist = 0.25) 
# 
# mex <- rnaturalearth::ne_countries(scale = "small", 
#                                    type = "countries") |> 
#   dplyr::filter(admin == "Mexico") |> 
#   sf::st_cast("POLYGON") |> 
#   # dplyr::mutate(id = dplyr::row_number()) |> 
#   # dplyr::filter(id %in% c(1,9)) |>
#   sf::st_buffer(dist = 0.25) 
# 
# focal_area <- dplyr::bind_rows(
#   us, can) |>
#   dplyr::bind_rows(mex) |>
#   dplyr::summarise(geometry = sf::st_union(geometry)) 
# 
# sf::st_write( focal_area, here::here("data/focal_area.shp"))

focal_area <- sf::st_read( here::here("data/focal_area.shp")) |> 
  sf::st_transform(crs = terra::crs(ex_rast))

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

land <- rnaturalearth::ne_download(scale = "small",
                                   type = "land", 
                                   category = "physical", returnclass = "sf")

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
