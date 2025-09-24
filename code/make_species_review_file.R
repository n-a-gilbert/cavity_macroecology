library(tidyverse)
library(here)

d <- readr::read_csv( here::here("data/north_america_cavity_nesters_to_review.csv"))

chia <- readr::read_csv(here::here("data/chia/NestTrait_v2.csv")) |> 
  janitor::clean_names()

download_these <- readr::read_csv(here::here("data/download_range_maps_for_these_species.csv"))

# these are all feral/exotic parrots
# "Ara ararauna", "Cyanoliseus patagonus", "Ara severus", "Agapornis roseicollis", "Brotogeris chiriri"

d |> 
  dplyr::left_join(download_these) |> 
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
