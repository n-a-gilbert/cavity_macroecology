# runs after 1.3_make_list_of_cavity_nesters_to_review.R
# we reviewed species for which nesting information was missing, valid some questionable classifications, 
# and flag species that are represented in North American only by small feral populations
library(tidyverse)
library(here)

d <- readr::read_csv( here::here("data/cavity_species_REVIEWED.csv"))

d |> 
  dplyr::filter(!neil_classification == "omit") |> #NG did a first pass-through and flagged species to omit
  # HB did a more detailed review of species marked as uncertain by NG in the first review; retain the NA 
  # (representing species marked as "good" in initial reivew by NG) and HB "good" classifications
  dplyr::filter( is.na(hallie_classification) | hallie_classification == "good") |> 
  # drop species represented only by small feral populations
  dplyr::filter(is.na(feral) | feral == 0) |> 
  dplyr::mutate(primary = ifelse(is.na(primary), 0, primary), 
                secondary = ifelse(is.na(secondary), 1, secondary), 
                tree = ifelse(is.na(tree), 1, tree)) |> 
  dplyr::select(order, family, sci, com, code, primary, secondary, tree) |> 
  dplyr::arrange(order, family, code) |> 
  # final species list for USA, Canada, Mexico
  readr::write_csv( file = here::here("data/final_species_list.csv"))