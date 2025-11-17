library(here)
library(tidyverse)
library(glmmTMB)
library(effects)
library(MetBrewer)
library(brms)
library(terra)
library(sf)

# same as some other scripts, a bit of code to recover the focal grid cells
# i.e. those within the US and Canada
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

# this table gives us the grid cells we want to focus on
focal_cells <- sf::st_join(
  ex_rast_centroids, 
  focal_area) |> 
  dplyr::filter(!is.na(FID))

# created by 2.3_calculate_diversity_per_cell.R
# this gives us range-edge abundance for non-excavators and
# columns with a bunch of diversity metrics for other cavity-nesters
d <- readr::read_csv( here::here("data/cavity_species_with_other_species_abundance_v02.csv"))

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

# model 1
# here, the predictor variable is abundance of ALL other NON-EXCAVATORS
m1_brm <- brms::brm(
  n ~ 1 + x + (1 + x | code), # random intercept and slope by species
  family = Gamma(link = "log"), 
  data = final1,
  prior = c(prior(normal(0, 2), class = "Intercept"), 
            prior(normal(0, 2), class = "b"), 
            prior(gamma(0.01, 0.01), class = "shape")),
  chains = 3,
  iter = 4000,
  warmup = 2000,
  cores = 3)

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

# model 2
# here, the predictor variable is abundance of SIMILAR-SIZED NON-EXCAVATORS
m2_brm <- brms::brm(
  n ~ 1 + x + (1 + x | code),
  family = Gamma(link = "log"), 
  data = final2,
  prior = c(prior(normal(0, 2), class = "Intercept"), 
            prior(normal(0, 2), class = "b"), 
            prior(gamma(0.01, 0.01), class = "shape")),
  chains = 3,
  iter = 3000,
  warmup = 1500,
  cores = 3)

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

# model 3
# here, the predictor variable is abundance of ALL EXCAVATORS
m3_brm <- brms::brm(
  n ~ 1 + x + (1 + x | code),
  family = Gamma(link = "log"), 
  data = final3,
  prior = c(prior(normal(0, 2), class = "Intercept"), 
            prior(normal(0, 2), class = "b"), 
            prior(gamma(0.01, 0.01), class = "shape")),
  chains = 3,
  iter = 4000,
  warmup = 2000,
  cores = 3)

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

# model 4
# here, the predictor variable is abundance of SIMILAR-SIZED EXCAVATORS
m4_brm <- brms::brm(
  n ~ 1 + x + (1 + x | code),
  family = Gamma(link = "log"), 
  data = final4,
  prior = c(prior(normal(0, 2), class = "Intercept"), 
            prior(normal(0, 2), class = "b"), 
            prior(gamma(0.01, 0.01), class = "shape")),
  chains = 3,
  iter = 4000,
  warmup = 2000,
  cores = 3)

# save model objects and data used to fit them
save(
  m1_brm, 
  m2_brm, 
  m3_brm,
  m4_brm, 
  final1, 
  final2, 
  final3, 
  final4, 
  file = here::here("results/us_canada_edge_results2.RData"))
