library(here)
library(tidyverse)
library(terra)
library(sf)
library(readxl)
library(officer)
library(flextable)
library(magrittr)

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

# centroids of the example raster as an sf object
ex_rast_centroids <- terra::xyFromCell( ex_rast[[1]], 1:ncell(ex_rast[[1]])) |> 
  tibble::as_tibble() |> 
  dplyr::mutate(cell_id = dplyr::row_number()) |> 
  sf::st_as_sf(
    coords = c("x", "y"),
    crs = terra::crs(ex_rast[[1]]))

# focal area shapefile
# created this with 1.6_create_focal_area_shapefile.R
focal_area <- sf::st_read( here::here("data/focal_area2.shp")) |> 
  sf::st_transform(crs = terra::crs(ex_rast))

# grid cells that fall within the focal area
focal_cells <- sf::st_join(
  ex_rast_centroids, 
  focal_area) |> 
  dplyr::filter(!is.na(FID))

# species abundances with grid cells, with columns for distance-to-coast and distance-to-range-edge
d <- readr::read_csv( here::here("data/cavity_nesters_abundance_dists.csv")) |> 
  dplyr::filter(cell_id %in% unique(focal_cells$cell_id))

# species list
species <- readr::read_csv(here::here("data/final_species_list.csv")) |> 
  dplyr::rename( scientific_name = sci, species_code = code)

# van der Hoek et al. 2017 table: retain columns for common name, scientific name, obligate/facultative, and type of cavity nester
fac <- readr::read_csv(
  here::here("data/van_der_hoek.csv")) |> 
  janitor::clean_names()|> 
  dplyr::select(com = name, scientific_name, ob = obligate_or_facultative, type = cavity_nester_type)

rev <- readr::read_csv(here::here("data/focal_species_van_der_hoek_classification.csv"))

# okay have to do some joining gymnastics...we already have the VDH dataset for the non-excavators, 
# but also want to pull in the for the excavators. 
new_categories <- species |> 
  dplyr::right_join(
    d |> 
      dplyr::filter(coast_dist > (100 * 1000)) |>  # omit coastal cells
      dplyr::group_by(com) |> 
      # calculate "range position" based on distance to nearest range boundary
      # less than or equal to 10th percentile - edge
      # greater than or equal to 90th percentile - core
      dplyr::mutate( position = ifelse(range_dist <= quantile(range_dist, 0.1), "edge", 
                                       ifelse(range_dist >= quantile(range_dist, 0.9), "core", "junk"))) |> 
      dplyr::filter( position == "edge") |> 
      dplyr::filter(cell_id %in% focal_cells$cell_id) |>
      # calculate how many range-edge cells there are within the focal area
      dplyr::mutate( ncell = n()) |> 
      # retain only species that have at least 10 range-edge cells within the US and Canada
      # this was to get rid of a few species that barely occur (mexican chickadee was one i think) in the area
      dplyr::filter(ncell > 10) |> 
      dplyr::select(com, scientific_name, species_code) |> 
      dplyr::distinct()) |> 
  # this table (rev) we already joined up with the VDH dataset, so we have columns for obligate/type
  dplyr::left_join(rev |> 
                     dplyr::rename(species_code = code)) |> 
  dplyr::filter(is.na(ob)) |> 
  dplyr::select(-ob, -type) |> 
  dplyr::left_join(
    fac |> 
      dplyr::select(-scientific_name)) |>
  # patch in data for a few problem species (recent splits, etc.)
  dplyr::mutate(ob = ifelse(scientific_name == "Picoides dorsalis", "Obligate",
                            ifelse(scientific_name == "Dryobates nuttallii", "Obligate",
                                   ifelse(scientific_name == "Glaucidium brasilianum", "Facultative", ob))),
                type = ifelse(scientific_name == "Picoides dorsalis", "Primary excavator",
                              ifelse(scientific_name == "Dryobates nuttallii", "Primary excavator",
                                     ifelse(scientific_name == "Glaucidium brasilianum", "Non-excavator", type)))) |> 
  dplyr::full_join(
    species |> 
      dplyr::right_join(
        d |> 
          dplyr::select(com, scientific_name, species_code) |> 
          dplyr::distinct()) |> 
      dplyr::left_join(rev |> 
                         dplyr::rename(species_code = code)) |> 
      dplyr::filter(!is.na(ob)) )

nonex <- new_categories |> 
  dplyr::filter(!type == "Primary excavator") |> 
  dplyr::filter(! com %in% c("Sulphur-bellied Flycatcher", "Whiskered Screech-Owl")) |> 
  dplyr::select(order, family, scientific_name, common_name = com, species_code, type_vdh = type) |>
  dplyr::mutate(type = ifelse(type_vdh == "Facultative excavator" | type_vdh == "Non-excavator", 
                              "Non-excavator", NA))

ex <- species |> 
  dplyr::right_join(
    d |> 
      dplyr::filter(coast_dist > (100 * 1000)) |>  # omit coastal cells
      dplyr::group_by(com) |> 
      # calculate "range position" based on distance to nearest range boundary
      # less than or equal to 10th percentile - edge
      # greater than or equal to 90th percentile - core
      # dplyr::mutate( position = ifelse(range_dist <= quantile(range_dist, 0.1), "edge", 
                                       # ifelse(range_dist >= quantile(range_dist, 0.9), "core", "junk"))) |> 
      # dplyr::filter( position == "edge") |> 
      dplyr::filter(cell_id %in% focal_cells$cell_id) |>
      # calculate how many range-edge cells there are within the focal area
      dplyr::mutate( ncell = n()) |> 
      # retain only species that have at least 10 range-edge cells within the US and Canada
      # this was to get rid of a few species that barely occur (mexican chickadee was one i think) in the area
      # dplyr::filter(ncell > 10) |> 
      dplyr::select(com, scientific_name, species_code) |> 
      dplyr::distinct()) |> 
  # this table (rev) we already joined up with the VDH dataset, so we have columns for obligate/type
  dplyr::left_join(rev |> 
                     dplyr::rename(species_code = code)) |> 
  dplyr::filter(is.na(ob)) |> 
  dplyr::select(-ob, -type) |> 
  dplyr::left_join(
    fac |> 
      dplyr::select(-scientific_name)) |>
  # patch in data for a few problem species (recent splits, etc.)
  dplyr::mutate(ob = ifelse(scientific_name == "Picoides dorsalis", "Obligate",
                            ifelse(scientific_name == "Dryobates nuttallii", "Obligate",
                                   ifelse(scientific_name == "Glaucidium brasilianum", "Facultative", ob))),
                type = ifelse(scientific_name == "Picoides dorsalis", "Primary excavator",
                              ifelse(scientific_name == "Dryobates nuttallii", "Primary excavator",
                                     ifelse(scientific_name == "Glaucidium brasilianum", "Non-excavator", type)))) |> 
  dplyr::full_join(
    species |> 
      dplyr::right_join(
        d |> 
          dplyr::select(com, scientific_name, species_code) |> 
          dplyr::distinct()) |> 
      dplyr::left_join(rev |> 
                         dplyr::rename(species_code = code)) |> 
      dplyr::filter(!is.na(ob)) ) |> 
  dplyr::filter(type == "Primary excavator") |> 
  dplyr::select(order, family, scientific_name, common_name = com, species_code, type_vdh = type) |>
  dplyr::mutate(type = ifelse(type_vdh == "Primary excavator", "Excavator", NA))

sp_table <- bind_rows(nonex, ex) |> 
  dplyr::select(-order, -species_code) |> 
  dplyr::rename(scientific = scientific_name, 
                common = common_name)

flextable::set_flextable_defaults(font.size = 10)
ft <- flextable::flextable( data = sp_table, cwidth = 0.7)  

tmp <- tempfile(fileext = ".docx")

officer::read_docx() |> 
  flextable::body_add_flextable(ft) |> 
  print(target = tmp)

utils::browseURL(tmp)
