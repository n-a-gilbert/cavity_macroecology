# This script calculates diversity (abundance and species richness)
# of cavity-nesting heterospecific within range-edge cells for
# focal non-excavator species
library(here)
library(tidyverse)
library(terra)
library(sf)

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

# Okay, my trust in the Chia dataset declined over the course of the project
# it classified some of the bluebirds as primary excavators? sus
# so, pulling in data from van der Hoek et al. 2017
# https://onlinelibrary.wiley.com/doi/full/10.1111/ddi.12601
# this is only for the focal non-excavators!
rev <- readr::read_csv(here::here("data/focal_species_van_der_hoek_classification.csv"))

# species abundances with grid cells, with columns for distance-to-coast and distance-to-range-edge
d <- readr::read_csv( here::here("data/cavity_nesters_abundance_dists.csv")) |> 
  dplyr::filter(cell_id %in% unique(focal_cells$cell_id))

# species list
species <- readr::read_csv(here::here("data/final_species_list.csv")) |> 
  dplyr::rename( scientific_name = sci, species_code = code)

# van der Hoek et al. 2017 table: retain columns for common name, scientific name, obligate/facultative, and type of cavity nester
fac <- readxl::read_xlsx(
  path = here::here("data/ddi12601-sup-0001-tables1.xlsx"),
  sheet = "Tree-cavity nesters") |> 
  janitor::clean_names() |> 
  dplyr::select(com = name, scientific_name, ob = obligate_or_facultative, type = cavity_nester_type)

# okay have to do some joining gymnastics...we already have the VDH dataset for the non-excavators, 
# but also want to pull in the for the excavators. 
new_categories <- species |> 
  dplyr::right_join(
    d |> 
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

# pull the ebird codes for the non-excavators
secondary <- d |> 
  dplyr::group_by(com, scientific_name, species_code) |> 
  dplyr::filter(coast_dist > (100 * 1000 )) |> # remove cells within 100 km of coast
  dplyr::mutate( soft_edge = ifelse(range_dist <= quantile(range_dist, 0.1), "edge", "nonedge")) |> 
  dplyr::filter( soft_edge == "edge") |> 
  dplyr::left_join( new_categories ) |> 
  dplyr::filter( ! type == "Primary excavator") |> 
  dplyr::pull(species_code) |> 
  unique()

# pull in AVONET database to get species masses
avo <- readr::read_csv( here::here("data/avonet.csv")) |> 
  dplyr::select(scientific_name = Species1, mass = Mass)

# dictionary to resolve taxa that have joining problems
# these are mostly due to recent splits
join_problems <- tibble::tibble(
 # ebird common name
   com = c("Northern/Southern House Wren",
          "Flammulated Flycatcher",
          "Arizona Woodpecker", 
          "American Three-toed Woodpecker",
          "Hairy Woodpecker",
          "Lineated Woodpecker", 
          "Pileated Woodpecker",
          "Red-cockaded Woodpecker", 
          "Smoky-brown Woodpecker", 
          "Strickland's Woodpecker",
          "White-headed Woodpecker", 
          "Pacific Parakeet", 
          "Mottled Owl",
          "American Barn Owl",
          "Gartered Trogon"),
   # ebird scientific name
  scientific_name = c("Troglodytes aedon/musculus",
                      "Ramphotrigon flammulatum",
                      "Dryobates arizonae", 
                      "Picoides dorsalis",
                      "Dryobates villosus", 
                      "Dryocopus lineatus", 
                      "Dryocopus pileatus",
                      "Dryobates borealis", 
                      "Dryobates fumigatus", 
                      "Dryobates stricklandi",
                      "Dryobates albolarvatus",
                      "Psittacara strenuus", 
                      "Strix virgata",
                      "Tyto furcata",
                      "Trogon caligatus"),
  # avonet scientific name
  avo_sci = c("Troglodytes aedon",
              "Deltarhynchus flammulatus",
              "Dryobates nuttallii",
              "Picoides tridactylus",
              "Leuconotopicus villosus",
              "Hylatomus lineatus",
              "Hylatomus pileatus",
              "Leuconotopicus borealis",
              "Leuconotopicus fumigatus",
              "Dryobates nuttallii",
              "Leuconotopicus albolarvatus",
              "Psittacara holochlorus",
              "Ciccaba virgata",
              "Tyto alba",
              "Trogon violaceus"))

# table with species and their masses
masses <- d |> 
  dplyr::select(com, scientific_name, species_code) |> 
  dplyr::distinct() |> 
  dplyr::left_join(avo) |> 
  dplyr::filter(!is.na(mass)) |> 
  dplyr::full_join(
    join_problems |> 
      dplyr::rename(ebird_sci = scientific_name, 
                    scientific_name = avo_sci) |> 
      dplyr::left_join(avo) |> 
      dplyr::select(-scientific_name) |> 
      dplyr::rename(scientific_name = ebird_sci)) |> 
  dplyr::left_join(
    species)

# now we loop through and calculate cavity-nester diversity per cell
res <- list(list())
for( i in 1:length(secondary)){ # loop through non-excavator species
  
  # filter to individual non-excavator species 
  focal_df <- d |> 
    dplyr::filter( species_code == secondary[i]) |>
    dplyr::filter(coast_dist > (100 * 1000)) |>  # omit coastal cells
    # calculate "range position" based on distance to nearest range boundary
    # less than or equal to 10th percentile - edge
    # greater than or equal to 90th percentile - core
    dplyr::mutate( position = ifelse(range_dist <= quantile(range_dist, 0.1), "edge", 
                                     ifelse(range_dist >= quantile(range_dist, 0.9), "core", "junk"))) |> 
    dplyr::filter( position == "core" | position == "edge") |>  # retain only core and edge (we didn't end up using core)
    dplyr::left_join(masses) # join with mass data
  
  # filter to ALL OTHER non-excavator species
  n_secondary <- d |> 
    dplyr::filter(cell_id %in% focal_df$cell_id) |> 
    dplyr::filter(!species_code == secondary[i]) |> 
    dplyr::left_join( new_categories ) |>
    dplyr::left_join(
      focal_df |> 
        dplyr::select(cell_id, focal_n = n)) |>      # join with portion of table above
    dplyr::filter(! type == "Primary excavator" ) |>  # omit the excavator species
    dplyr::group_by(cell_id) |> 
    dplyr::summarise(n_secondary = sum(n),       # total abundance of other non-excavators
                     sr_secondary = sum(n > 0))  # species richness of other non-excavators
  
  # same as above, EXCEPT we only focus on similar-sized non-excavators
  n_secondary_0.5 <- d |> 
    dplyr::filter(cell_id %in% focal_df$cell_id) |> 
    dplyr::filter(!species_code == secondary[i]) |> 
    dplyr::left_join( new_categories ) |> 
    dplyr::filter(!type == "Primary excavator" ) |>
    dplyr::left_join( masses ) |> 
    # ratio of mass between non-excavator and focal non-excavator
    #  1 = same mass
    # >1 = other species is larger than the focal non-excavator
    # <1 = other species is smaller than the focal non-excavator
    dplyr::mutate( mass_ratio = mass / unique(focal_df$mass)) |> 
    # retain only species that have a mass ratio between 0.5 and 1.5 
    # similar-sized species
    dplyr::filter(mass_ratio >= 0.5 & mass_ratio <= 1.5) |> 
    dplyr::group_by(cell_id) |> 
    dplyr::summarise(n_secondary_0.5 = sum(n),
                     sr_secondary_0.5 = sum(n > 0))

  # now we do the same thing but with excavators (primary), e.g., woodpeckers
  n_primary <- d |> 
    dplyr::filter(cell_id %in% focal_df$cell_id) |> 
    dplyr::filter(!species_code == secondary[i]) |> 
    dplyr::left_join( new_categories ) |> 
    dplyr::filter( type == "Primary excavator" ) |> 
    dplyr::group_by(cell_id) |> 
    dplyr::summarise(n_primary = sum(n),
                     sr_primary = sum(n > 0))
  
  n_primary_0.5 <- d |> 
    dplyr::filter(cell_id %in% focal_df$cell_id) |> 
    dplyr::filter(!species_code == secondary[i]) |> 
    dplyr::left_join( new_categories ) |> 
    dplyr::filter( type == "Primary excavator" ) |> 
    dplyr::left_join( masses ) |> 
    dplyr::mutate( mass_ratio = mass / unique(focal_df$mass)) |> 
    dplyr::filter(mass_ratio >= 0.5 & mass_ratio <= 1.5) |> 
    dplyr::group_by(cell_id) |> 
    dplyr::summarise(n_primary_0.5 = sum(n),
                     sr_primary_0.5 = sum(n > 0))
  
  # Same but with ALL excavators (previous was strict excavators)
  # this includes species like chickadees, etc., that are facultative excavators
  n_primary2 <- d |> 
    dplyr::filter(cell_id %in% focal_df$cell_id) |> 
    dplyr::filter(!species_code == secondary[i]) |> 
    dplyr::left_join( new_categories ) |> 
    dplyr::filter( !type == "Non-excavator" ) |> 
    dplyr::group_by(cell_id) |> 
    dplyr::summarise(n_primary2 = sum(n),
                     sr_primary2 = sum(n > 0))
  
  n_primary2_0.5 <- d |> 
    dplyr::filter(cell_id %in% focal_df$cell_id) |> 
    dplyr::filter(!species_code == secondary[i]) |> 
    dplyr::left_join( new_categories ) |> 
    dplyr::filter( !type == "Non-excavator" ) |> 
    dplyr::left_join( masses ) |> 
    dplyr::mutate( mass_ratio = mass / unique(focal_df$mass)) |> 
    dplyr::filter(mass_ratio >= 0.5 & mass_ratio <= 1.5) |> 
    dplyr::group_by(cell_id) |> 
    dplyr::summarise(n_primary2_0.5 = sum(n),
                     sr_primary2_0.5 = sum(n > 0))
 
  # stash the results table into a list of dataframes
  res[[i]] <- focal_df |> 
    dplyr::left_join(n_secondary) |> 
    dplyr::left_join(n_secondary_0.5) |> 
    dplyr::left_join(n_primary) |> 
    dplyr::left_join(n_primary_0.5) |> 
    dplyr::left_join(n_primary2) |> 
    dplyr::left_join(n_primary2_0.5) |> 
    dplyr::mutate( across( dplyr::starts_with("n_"), function(x) tidyr::replace_na(x, 0))) |>
    dplyr::mutate( across( dplyr::starts_with("sr_"), function(x) tidyr::replace_na(x, 0)))
}

bind_rows(res) |> 
  readr::write_csv( file = here::here("data/cavity_species_with_other_species_abundance_v02.csv"))