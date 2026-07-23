# This script calculates diversity (abundance and species richness)
# of cavity-nesting heterospecific within range-edge cells for
# focal non-excavator species
library(here)
library(tidyverse)
library(terra)
library(sf)
library(flextable)
library(officer)

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
fac <- readr::read_csv(
  here::here("data/van_der_hoek.csv")) |> 
  janitor::clean_names()|> 
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
  dplyr::mutate( soft_edge = ifelse(range_dist <= quantile(range_dist, 0.05), "edge", "nonedge")) |> 
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
    dplyr::mutate( position = ifelse(range_dist <= quantile(range_dist, 0.05), "edge", 
                                     ifelse(range_dist >= quantile(range_dist, 0.95), "core", "junk"))) |> 
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

d <- bind_rows(res)

# this is a master table that we will filter from for each model
# key columns are: group (primary, strict [obligate] primary, secondary)
# sorry for being inconsistent...primary = excavator, secondary = non-excavator
# metric: sr (species richness), n (abundance)
# mass ratio: all other species, species within 50% body mass. 
# annoyingly, for the strict primary group, mass_ratio = NA for "all other species", just FYI
df <- d |> 
  # renaming...confusing, but the _1 suffix denotes that it's all other species
  dplyr::rename( n_secondary_1 = n_secondary, 
                 sr_secondary_1 = sr_secondary, 
                 n_primary_1 = n_primary, 
                 sr_primary_1 = sr_primary) |> 
  tidyr::pivot_longer(n_secondary_1:sr_primary2_0.5, names_to = "type", values_to = "value") |> 
  tidyr::separate(type, into = c("metric", "group", "mass_ratio"), sep = "_") |> 
  dplyr::mutate(is_edge = ifelse(position == "edge", 1, 0),
                mass_ratio = ifelse(mass_ratio == 1, "all", 
                                    ifelse(mass_ratio == 0.5, "50%", "blah"))) |> 
  dplyr::mutate(group = ifelse(group == "primary2", "general primary", group))

# final formatting for model 1
# here, the predictor variable is abundance of ALL other NON-EXCAVATORS
final1 <- df |> 
  dplyr::filter( metric == "n") |>  # focus on abundance
  dplyr::filter(group == "secondary") |>  # non-excavators
  dplyr::filter(mass_ratio == "all") |>  # all sizes
  dplyr::mutate(x = as.numeric(scale(log1p(value))), # scale the log-transformed summed abundance
                is_edge = factor(is_edge)) |>    # I guess we still have the core cells
  dplyr::rename(edge = is_edge, 
                code = species_code) |> 
  dplyr::filter(edge == 1) |>  # filter only to range-edge
  # retain only focal cells 
  # at one point we included data from mexico, but updated the analysis to retain only US and canada
  dplyr::filter(cell_id %in% focal_cells$cell_id) |>  
  dplyr::group_by(com) |> 
  # calculate how many range-edge cells there are within the focal area
  dplyr::mutate( ncell = n()) |> 
  # retain only species that have at least 10 range-edge cells within the US and Canada
  # this was to get rid of a few species that barely occur (mexican chickadee was one i think) in the area
  dplyr::filter(ncell > 10) |> 
  dplyr::ungroup()

m1 <- glmmTMB::glmmTMB(
  n ~ 1 + x + (1 + x | code),
  family = Gamma(link = "log"),
  data = final1)

# final formatting for model 2
# here, the predictor variable is abundance of SIMILAR-SIZED NON-EXCAVATORS
# otherwise, same process
final2 <- df |> 
  dplyr::filter( metric == "n") |> 
  dplyr::filter(group == "secondary") |> 
  dplyr::filter(mass_ratio == "50%") |> 
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

m2 <- glmmTMB::glmmTMB(
  n ~ 1 + x + (1 + x | code),
  family = Gamma(link = "log"),
  data = final2)

# final formatting for model 3
# here, the predictor variable is abundance of ALL other EXCAVATORS
final3 <- df |> 
  dplyr::filter( metric == "n") |> 
  dplyr::filter(group == "primary") |>  # strict primary = obligate excavator
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

m3 <- glmmTMB::glmmTMB(
  n ~ 1 + x + (1 + x | code),
  family = Gamma(link = "log"),
  data = final3)

# final formatting for model 4
# here, the predictor variable is abundance of SIMLAR-SIZED EXCAVATORS
final4 <- df |> 
  dplyr::filter( metric == "n") |> 
  dplyr::filter(group == "primary") |> 
  dplyr::filter(mass_ratio == "50%") |> 
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

m4 <- glmmTMB::glmmTMB(
  n ~ 1 + x + (1 + x | code),
  family = Gamma(link = "log"),
  data = final4)

modlist <- list(m1, m2, m3, m4)
res <- list(list())
for(i in 1:length(modlist)){
  
  modsum <- summary(modlist[[i]])
  
  res[[i]] <- modsum$coefficients$cond |> 
      as_tibble(rownames = "param") |> 
      dplyr::filter(param == "x") |> 
      janitor::clean_names()
  
}

restab <- bind_rows(res) |> 
  cbind(tibble::tibble(
    group = c( "non-excavator", "non-excavator", "excavator", "excavator"),
    size = c("all", "within 50% mass", "all", "within 50% mass"))) |> 
  dplyr::select(group, size, estimate, se = std_error, pval = pr_z) |> 
  dplyr::mutate(across(estimate:pval, function(x) round(x, 2)))

flextable::set_flextable_defaults(font.size = 10)
ft <- flextable::flextable( data = restab, cwidth = 0.7)  

tmp <- tempfile(fileext = ".docx")

officer::read_docx() |> 
  flextable::body_add_flextable(ft) |> 
  print(target = tmp)

utils::browseURL(tmp)
