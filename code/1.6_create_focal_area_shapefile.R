# create a little shapefile of USA and Canada, buffered by a little bit, focal study area
library(sf)
library(rnaturalearth)
library(tidyverse)

# little trick to make buffering tolerable 
sf::sf_use_s2(FALSE)

# USA shapefile
us <- rnaturalearth::ne_countries(scale = "small",
                                  type = "countries") |>
  dplyr::filter(admin == "United States of America") |>
  sf::st_cast("POLYGON") |>
  dplyr::mutate(id = dplyr::row_number()) |>
  dplyr::filter(id %in% c(1,9)) |> # keep AK, lower 48; omit HI, territories, etc.
  sf::st_buffer(dist = 0.25) # can have a little buffer, as a treat?

# Canada shapefile
can <- rnaturalearth::ne_countries(scale = "small",
                                   type = "countries") |>
  dplyr::filter(admin == "Canada") |>
  sf::st_cast("POLYGON") |>
  dplyr::mutate(id = dplyr::row_number()) |>
  dplyr::select(admin, id, geometry) |>
  dplyr::filter(id %in% c(
    1,  # main
    11, # Newfoundland
    16, # Moresbey island
    18, # Vancouver
    29, # Anticosti Island
    30  # PEI,
  )) |>
  sf::st_buffer(dist = 0.25)
 
focal_area <- dplyr::bind_rows(us, can) |>
  dplyr::summarise(geometry = sf::st_union(geometry))

sf::st_write( focal_area, here::here("data/focal_area2.shp"))