# plot the maps for Fig 2
# basically, we want to visualize that we're looking at noncoastal range-limit abundance
# and predicting that based on sumemd abundance of heterospecifics within those grid cells
# assemble and annotate these in powerpoint
library(here)
library(sf)
library(terra)
library(tidyverse)
library(nngeo)

# load the eastern bluebird abundance map
eabl <- terra::rast(
  paste( here::here("data/abundance/2023/"), 
         "easblu",
         "seasonal",
         paste0("easblu", "_abundance_seasonal_mean_27km_2023.tif"),
         sep = "/"))

# tree swallow
tres <- terra::rast(
  paste( here::here("data/abundance/2023/"), 
         "treswa",
         "seasonal",
         paste0("treswa", "_abundance_seasonal_mean_27km_2023.tif"),
         sep = "/"))

#white-breasted nuthatch
wbnu <- terra::rast(
  paste( here::here("data/abundance/2023/"), 
         "whbnut",
         "seasonal",
         paste0("whbnut", "_abundance_seasonal_mean_27km_2023.tif"),
         sep = "/"))

# now load the range polygons
eabl_range <- sf::st_read(
  paste( here::here("data/ranges/2023/"), 
         "easblu",
         "ranges",
         paste0("easblu", "_range_smooth_27km_2023.gpkg"),
         sep = "/")) |> 
  dplyr::filter(season == "breeding")

# remove holes for simplicity
eabl_range <- nngeo::st_remove_holes(eabl_range, max_area = 1e12) 

tres_range <- sf::st_read(
  paste( here::here("data/ranges/2023/"), 
         "treswa",
         "ranges",
         paste0("treswa", "_range_smooth_27km_2023.gpkg"),
         sep = "/")) |> 
  dplyr::filter(season == "breeding")

tres_range <- nngeo::st_remove_holes(tres_range, max_area = 1e12)

wbnu_range <- sf::st_read(
  paste( here::here("data/ranges/2023/"), 
         "whbnut",
         "ranges",
         paste0("whbnut", "_range_smooth_27km_2023.gpkg"),
         sep = "/"))

wbnu_range <- nngeo::st_remove_holes(wbnu_range, max_area = 1e12)

# okay, gymnastics to get cell ids/locations
download_these <- readr::read_csv(here::here("data/cavity_nesters_review.csv")) |> 
  dplyr::filter(com == "Eastern Bluebird") |> 
  dplyr::rename(scientific_name = sci,
                species_code = code)
i <- 1
ex_rast <- terra::rast(
  paste( here::here("data/abundance/2023/"), 
         download_these$species_code[i],
         "seasonal",
         paste0(download_these$species_code[i], "_abundance_seasonal_mean_27km_2023.tif"),
         sep = "/"))

ex_rast_centroids <- terra::xyFromCell( ex_rast[[1]], 1:ncell(ex_rast[[1]])) |> 
  tibble::as_tibble() |> 
  dplyr::mutate(cell_id = dplyr::row_number()) |> 
  sf::st_as_sf(
    coords = c("x", "y"),
    crs = terra::crs(ex_rast[[1]]))

# just have the abundance as points
tres_n <- terra::xyFromCell( tres[[1]], 1:ncell(tres[[1]])) |> 
  tibble::as_tibble() |> 
  dplyr::mutate(cell_id = dplyr::row_number()) |> 
  sf::st_as_sf(
    coords = c("x", "y"),
    crs = terra::crs(tres[[1]])) |> 
  tibble::add_column(n = values(tres[[1]])[,1]) |> 
  dplyr::mutate(n = ifelse(is.na(n), 0, n))

wbnu_n <- terra::xyFromCell( wbnu[[1]], 1:ncell(wbnu[[1]])) |> 
  tibble::as_tibble() |> 
  dplyr::mutate(cell_id = dplyr::row_number()) |> 
  sf::st_as_sf(
    coords = c("x", "y"),
    crs = terra::crs(tres[[1]])) |> 
  tibble::add_column(n = values(wbnu[[1]])[,1]) |> 
  dplyr::mutate(n = ifelse(n == 0, NA, n))

focal_area <- sf::st_read( here::here("data/focal_area2.shp")) |> 
  sf::st_transform(crs = terra::crs(ex_rast))

focal_cells <- sf::st_join(
  ex_rast_centroids, 
  focal_area) |> 
  dplyr::filter(!is.na(FID))

# grab the bluebird data
d <- readr::read_csv( here::here("data/cavity_species_with_other_species_abundance.csv"))

edge_eabl <- d |> 
  dplyr::filter(species_code == "easblu") |> 
  dplyr::filter(position == "edge") |> 
  dplyr::select(cell_id, n)

edge_eabl_sf <- focal_cells |> 
  dplyr::filter(cell_id %in% edge_eabl$cell_id) |> 
  dplyr::left_join(edge_eabl) |> 
  sf::st_transform(crs = st_crs(eabl_range))

# for the "right hand" species, grab abundance only for the bluebird's edges
eabl_edge_tres <- tres_n |> 
  dplyr::filter(cell_id %in% edge_eabl_sf$cell_id) |> 
  sf::st_transform(crs = st_crs(eabl_range)) |> 
  dplyr::mutate(n = ifelse(n == 0, NA, n))

eabl_edge_wbnu <- wbnu_n |> 
  dplyr::filter(cell_id %in% edge_eabl_sf$cell_id) |> 
  sf::st_transform(crs = st_crs(eabl_range)) |> 
  dplyr::mutate(n = ifelse(n == 0, NA, n))

# get polygon for study area: US, Canada, and Mexico, buffered by a little bit
sf::sf_use_s2(FALSE)
us <- rnaturalearth::ne_countries(scale = "small",
                                  type = "countries") |>
  dplyr::filter(admin == "United States of America") |>
  sf::st_cast("POLYGON") |>
  dplyr::mutate(id = dplyr::row_number()) |>
  dplyr::filter(id %in% c(1,9)) |> # keep AK, lower 48; omit HI, territories, etc.
  sf::st_buffer(dist = 0.01)

can <- rnaturalearth::ne_countries(scale = "small",
                                   type = "countries") |>
  dplyr::filter(admin == "Canada") |>
  sf::st_cast("POLYGON") |>
  dplyr::mutate(id = dplyr::row_number()) |>
  dplyr::select(admin, id, geometry) |>
  dplyr::filter(id %in% c(
    1, # main
    11, # Newfoundland
    16, #Moresbey island
    18, # Vancouver
    29, #Anticosti Island
    30 #PEI,
  )) |>
  sf::st_buffer(dist = 0.01)

focal_area <- dplyr::bind_rows(
  us, can) |>
  dplyr::summarise(geometry = sf::st_union(geometry)) |> 
  sf::st_transform(crs = st_crs(eabl_range))

eabl_range <- sf::st_intersection(eabl_range, focal_area)
tres_range <- sf::st_intersection(tres_range, focal_area)
wbnu_range <- sf::st_intersection(wbnu_range, focal_area)

ggplot() + 
  geom_sf(data = focal_area, aes(geometry = geometry), fill = "gray90", color = NA) +
  geom_sf(data = eabl_range, aes(geometry = geometry), fill = "gray65", color = NA) +
  geom_sf(data = edge_eabl_sf, aes(geometry =geometry, color = n), shape = 15, size = 0.01) +
  scale_color_viridis_c(option = "G", begin = 0.5, end = 1) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(color = NA, fill = "white"))

ggsave(
  filename = here::here("figures/eabl_map.png"),
  width = 1.5, 
  height = 1, 
  units = "in", 
  dpi = 800
)

ggplot() + 
  geom_sf(data = focal_area, aes(geometry = geometry), fill = "gray90", color = NA) +
  geom_sf(data = tres_range, aes(geometry = geometry), fill = "gray65", color = NA) +
  geom_sf(data = eabl_edge_tres, aes(geometry =geometry, color = n), shape = 15, size = 0.01) +
  scale_color_viridis_c(option = "B", begin = 0.5, end = 1, na.value = "transparent") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(color = NA, fill = "white"))

ggsave(
  filename = here::here("figures/tres_map.png"),
  width = 1.5, 
  height = 1, 
  units = "in", 
  dpi = 800
)

ggplot() + 
  geom_sf(data = focal_area, aes(geometry = geometry), fill = "gray90", color = NA) +
  geom_sf(data = wbnu_range, aes(geometry = geometry), fill = "gray65", color = NA) +
  geom_sf(data = eabl_edge_wbnu, aes(geometry =geometry, color = n), shape = 15, size = 0.01) +
  scale_color_viridis_c(option = "B", begin = 0.5, end = 1, na.value = "transparent") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(color = NA, fill = "white"))

ggsave(
  filename = here::here("figures/wbnu_map.png"),
  width = 1.5, 
  height = 1, 
  units = "in", 
  dpi = 800)