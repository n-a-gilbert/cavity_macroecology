# initially we relied on the Chia et al. nest database
# but wanted to cross-check it and corroborate with the van der Hoek database, 
# since we found some questionable classificaitons in Chia
# This script joins up the datafiles; we had to do a manual review to resolve some name idiosyncracies
library(here)
library(tidyverse)
library(readxl)
library(janitor)
library(terra)
library(sf)

# van der Hoek
fac <- readxl::read_xlsx(
  path = here::here("data/ddi12601-sup-0001-tables1.xlsx"),
  sheet = "Tree-cavity nesters") |> 
  janitor::clean_names()

# the usual rigamorole to get our focal grid cells
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

focal_area <- sf::st_read( here::here("data/focal_area2.shp")) |> 
  sf::st_transform(crs = terra::crs(ex_rast))

focal_cells <- sf::st_join(
  ex_rast_centroids, 
  focal_area) |> 
  dplyr::filter(!is.na(FID))

d <- readr::read_csv( here::here("data/cavity_species_with_other_species_abundance.csv"))

# these next code chunks are what we developed for final formatting to fit models...
# this is to identify what species make it through the final filters
df <- d |> 
  dplyr::rename( n_secondary_1 = n_secondary, 
                 sr_secondary_1 = sr_secondary, 
                 n_primary_1 = n_primary, 
                 sr_primary_1 = sr_primary) |> 
  dplyr::mutate( n_all_1 = n_secondary_1 + n_primary_1, 
                 sr_all_1 = sr_secondary_1 + sr_primary_1,
                 
                 n_all_0.5 = n_secondary_0.5 + n_primary_0.5, 
                 sr_all_0.5 = sr_secondary_0.5 + sr_primary_0.5, 
                 
                 n_all_0.25 = n_secondary_0.25 + n_primary_0.25, 
                 sr_all_0.25 = sr_secondary_0.25 + sr_primary_0.25) |> 
  tidyr::pivot_longer(n_secondary_1:sr_all_0.25, names_to = "type", values_to = "value") |> 
  tidyr::separate(type, into = c("metric", "group", "mass_ratio"), sep = "_") |> 
  dplyr::mutate(is_edge = ifelse(position == "edge", 1, 0),
                mass_ratio = ifelse(mass_ratio == 1, "all", 
                                    ifelse(mass_ratio == 0.5, "50%", "25%")))

final1 <- df |> 
  dplyr::filter( metric == "n") |> 
  dplyr::filter(group == "secondary") |> 
  dplyr::filter(mass_ratio == "all") |> 
  dplyr::mutate(x = as.numeric(scale(log1p(value))),
                is_edge = factor(is_edge)) |> 
  dplyr::rename(edge = is_edge, 
                code = species_code) |> 
  dplyr::filter(edge == 1) |> 
  dplyr::filter(cell_id %in% focal_cells$cell_id) |> 
  dplyr::group_by(com) |> 
  dplyr::mutate( ncell = n()) |> 
  dplyr::filter(ncell > 10) |> 
  dplyr::ungroup()

# first try joining based on scientific name
sci_join <- final1 |> 
  dplyr::select(com, scientific_name, code) |> 
  dplyr::distinct() |> 
  dplyr::left_join(
    fac |> 
      dplyr::select(scientific_name, ob = obligate_or_facultative, type = cavity_nester_type)) |> 
  dplyr::filter(!is.na(ob))

# second try joining based on common name
com_join <- final1 |> 
  dplyr::select(com, scientific_name, code) |> 
  dplyr::distinct() |> 
  dplyr::left_join(
    fac |> 
      dplyr::select(com = name, ob = obligate_or_facultative, type = cavity_nester_type)) |> 
  dplyr::filter(!is.na(ob))


# DANGER ZONE
# there were a few species that didn't join up readily 
# e.g., due to recent splits
# identified those, read them out, and then Hallie did a manual review
# commenting this out so it doesn't get overwritten
# final1 |>
# dplyr::select(com, scientific_name, code) |>
# dplyr::distinct() |>
# dplyr::anti_join(
# dplyr::full_join(com_join, sci_join)) |>
#   readr::write_csv(
#     here::here("data/review_species_van_der_hoek_join.csv")
#   )

# okay, following Hallie's review, we have data for those species that had trouble joining up
rev <- readr::read_csv(here::here("data/review_species_van_der_hoek_join_v2.csv")) |> 
  dplyr::select(com, scientific_name, code, ob = `Obligate or Facultative`, type)

# this gives us VDH data for all species; write out
com_join |> 
  full_join(sci_join) |> 
  full_join(rev) |> 
  readr::write_csv(here::here("data/focal_species_van_der_hoek_classification.csv"))