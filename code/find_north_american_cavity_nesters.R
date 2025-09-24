library(here)
library(tidyverse)
library(sf)
library(rnaturalearth)

sp <- readr::read_csv( here::here("data/download_range_maps_for_these_species.csv")) |> 
  dplyr::arrange(species_code) 

map <- sf::st_read( here::here("data/ranges/2023/acowoo/ranges/acowoo_range_smooth_27km_2023.gpkg"))

countries <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")

target_countries <- countries[countries$admin %in% c("United States of America", "Canada", "Mexico"), ]

study_area <- target_countries |> 
  dplyr::summarise(geometry = sf::st_union(geometry)) |> 
  sf::st_make_valid()

# loop broke bc I guess ebird automatically calls YBSA folder yebsap-example rather than just
# yebsap
res <- list(list())
# for( i in 1:length(sp$species_code)){
for( i in 900:length(sp$species_code)){

  map <- sf::st_read(
    paste( here::here("data/ranges/2023/"), 
           sp$species_code[i],
           "ranges",
           paste0(sp$species_code[i], "_range_smooth_27km_2023.gpkg"),
           sep = "/")) |> 
    sf::st_make_valid()
  
  within <- as.numeric(lengths(sf::st_within(map, study_area)))
  inter <- as.numeric(lengths(sf::st_intersects(map, study_area)))
  
  res[[i]] <- tibble::tibble(
    species_code = sp$species_code[i],
    within = within, 
    inter = inter)
  
  print(paste0("Finished ", i, " of ", length(sp$species_code) ))
  
}

dplyr::bind_rows(res) |> 
  dplyr::group_by(species_code) |> 
  dplyr::summarise( across(c(within, inter), function(x) max(x))) |> 
  dplyr::filter(within == 1 | inter == 1) |>
  dplyr::left_join(sp) |> 
  dplyr::arrange(order) |>
  readr::write_csv( here::here("data/north_america_cavity_nesters_to_review.csv"))