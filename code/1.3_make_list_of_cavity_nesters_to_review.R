# this runs after 1.2_identify_north_american_cavity_nesters.R
# we did a manual review of eBird S&T species that didn't readily join to the Chia database
# and also to validate some questionable classifications
library(tidyverse)
library(here)

d <- readr::read_csv( here::here("data/north_america_cavity_nesters_to_review.csv"))

chia <- readr::read_csv(here::here("data/chia/NestTrait_v2.csv")) |> 
  janitor::clean_names()

download_these <- readr::read_csv(here::here("data/download_range_maps_for_these_species.csv"))

d |> 
  dplyr::left_join(download_these) |> 
  # omit vultures + ocean birds that nest in burrows
  dplyr::filter(!family %in% c("Cathartidae", "Alcidae", "Oceanitidae", 
                               "Hydrobatidae", "Procellariidae")) |>
  # Nutting's Flycatcher coded as not nesting in trees has to be an error
  dplyr::mutate( tree = ifelse(scientific_name == "Myiarchus nuttingi", 1, tree)) |> 
  dplyr::filter(tree == 1 | is.na(tree)) |> 
  dplyr::arrange(order, family) |> 
  dplyr::select(order, family, 
                sci = scientific_name, 
                com = common_name, 
                code = species_code, 
                primary, 
                secondary, 
                tree) |> 
  readr::write_csv(here::here("data/cavity_nesters_review.csv"))
