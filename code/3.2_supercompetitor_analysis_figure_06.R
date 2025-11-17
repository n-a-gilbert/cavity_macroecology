# analysis: effect of supercompetitor abundance on range-limit abundance of similar-sized non-excavators
# also create figure 6
library(here)
library(tidyverse)
library(glmmTMB)
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

d <- readr::read_csv( here::here("data/cavity_nesters_abundance_dists.csv")) |> 
  dplyr::filter(cell_id %in% focal_cells$cell_id) # restrict to US / Canada

species <- readr::read_csv(here::here("data/final_species_list.csv")) |> 
  dplyr::rename( scientific_name = sci, species_code = code)

# list of non-excavators to loop through
secondary <- d |> 
  dplyr::group_by(com, scientific_name, species_code) |> 
  dplyr::filter(coast_dist > (100 * 1000 )) |> # remove cells within 100 km of coast
  dplyr::mutate( soft_edge = ifelse(range_dist <= quantile(range_dist, 0.1), "edge", "nonedge")) |> 
  dplyr::filter( soft_edge == "edge") |> 
  dplyr::left_join( species ) |> 
  dplyr::filter( secondary == 1 ) |> 
  dplyr::mutate(ncell = n()) |> 
  dplyr::filter(ncell > 10) |>  # omit species that barely occur within US (e.g., only in SE AZ)
  dplyr::pull(species_code) |> 
  unique()

# AVONET body masses
avo <- readr::read_csv( here::here("data/avonet.csv")) |> 
  dplyr::select(scientific_name = Species1, mass = Mass)

# dictionary for join issues - mostly due to recent splits
join_problems <- tibble::tibble(
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

# species with masses
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

# loop through the non-excavators
pw_hs <- list(list())
for( i in 1:length(secondary)){
  
  # focal non-excavators
  focal_df <- d |> 
    dplyr::filter( species_code == secondary[i]) |> 
    dplyr::filter(coast_dist > (100 * 1000)) |> 
    dplyr::mutate( position = ifelse(range_dist <= quantile(range_dist, 0.1), "edge", 
                                     ifelse(range_dist >= quantile(range_dist, 0.9), "core", "junk"))) |> 
    dplyr::filter( position == "core" | position == "edge") |> 
    dplyr::left_join(masses)
  
  # join up with house sparrow and starling
  other_df <- d |> 
    dplyr::filter(cell_id %in% focal_df$cell_id) |> 
    dplyr::filter(!species_code == secondary[i]) |> 
    dplyr::filter(species_code %in% c("houspa", "eursta")) |> 
    dplyr::left_join(masses) |> 
    dplyr::select(cell_id, com2 = com, n2 = n, mass) |> 
    dplyr::mutate( mass_ratio =  unique(focal_df$mass) / mass )
  
  # stash each focal non-excavator's table in a list of tables
  pw_hs[[i]] <- focal_df |> 
    dplyr::select(cell_id, position, com1 = com, n1 = n) |> 
    dplyr::left_join(other_df) |> 
    dplyr::group_by(com1, com2, position, mass_ratio) 
}

# pairwise table
# com1 = species name of non-excavator
# com2 = speices name of supercompetitor
pw <- bind_rows(pw_hs) |> 
  dplyr::filter(!is.na(mass_ratio)) |> 
  dplyr::filter(mass_ratio > 0.5 & mass_ratio < 1.5)

ucom1 <- unique(pw$com1)

# loop through each non-excavator and fit GLMM for each
glmm_output <- list(list())
for(i in 1:length(ucom1)){
  
  tmp <- pw |> 
    dplyr::filter(com1 == ucom1[i]) |> 
    dplyr::mutate(x = as.numeric(scale(log1p(n2))))
  
  m <- glmmTMB(
    n1 ~ 1 + x, # abundance of focal non-excavator (response) predicted by abundance of either starling or hosp
    family = Gamma(link = "log"),
    data = tmp)
  
  ms <- summary(m)
  
  # stash results
  glmm_output[[i]] <- ms$coefficients$cond[2,] |> 
    as_tibble(rownames = "what") |> 
    add_column(com1 = unique(tmp$com1),
               com2 = unique(tmp$com2))
  
  print(paste("finished", i, "of", length(ucom1)))
  
}

for_plot <- bind_rows(glmm_output) |> 
  dplyr::filter(grepl("Estimate", what) | grepl("Pr", what) | grepl("Std.", what)) |> 
  tidyr::pivot_wider(names_from = what, values_from = value) |> 
  dplyr::left_join( 
    pw |> 
      dplyr::ungroup() |> 
      dplyr::select(com1, com2, mass_ratio) |> 
      dplyr::distinct()) |> 
  dplyr::mutate(sig = ifelse(`Pr(>|z|)` < 0.05, "significant", "not significant")) |> 
  janitor::clean_names() |> 
  dplyr::mutate(com2 = factor(com2, levels = c("House Sparrow", "European Starling")))

rect_data <- data.frame(com2 = unique(for_plot$com2))

ggplot(for_plot, aes(x = mass_ratio, y = estimate)) +
  facet_wrap(~com2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", alpha = 0.5) +
  geom_rect(
    data = rect_data, 
    inherit.aes = FALSE,
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0), 
    color = NA, 
    fill = MetBrewer::MetPalettes$Isfahan1[[1]][2],
    alpha = 0.3) +
  geom_rect(
    data = rect_data, 
    inherit.aes = FALSE,
    aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf), 
    color = NA, 
    fill = MetBrewer::MetPalettes$Isfahan1[[1]][7],
    alpha = 0.3) +
  geom_point(aes(color = sig), size = 2) +
  # mannually add in some species labels in powerpoint :(
  ggrepel::geom_text_repel(
    data = filter(for_plot, !(com1 %in% c("Violet-green Swallow", "Carolina Wren",
                                          "Red-headed Woodpecker", "Tufted Titmouse",
                                          "Western Bluebird", "Eastern Bluebird"))),
    aes(color = sig,label = com1),
                           size = 2.2) +
  theme_minimal() +
  scale_color_manual(values = c("gray60", "black")) +
  labs(x = "Mass ratio", 
       y = "effect of 'supercompetitor' abundance at range limits") +
  theme(axis.line = element_line(linewidth = 0.2, color = "black"),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_text(color = "black", 
                                  size = 9), 
        axis.text = element_text(color = "black",
                                 size = 8), 
        strip.text = element_text(color = "black", 
                                  size = 9, 
                                  face = "bold"), 
        panel.spacing = unit(1, "lines"),
        # strip.text.x = element_blank(),
        plot.background = element_rect(color = NA, 
                                       fill = "white"))

ggsave(
  filename = here::here("figures/figure_06.png"), 
  width = 5.5, 
  height = 4, 
  units = "in", 
  dpi = 600
)

# get bean-counting stations - how many species show significant positive/negative effects of supercompetitors
bind_rows(glmm_output) |> 
  dplyr::filter(grepl("Estimate", what) | grepl("Pr", what)) |> 
  tidyr::pivot_wider(names_from = what, values_from = value) |> 
  dplyr::left_join( 
    pw |> 
      dplyr::ungroup() |> 
      dplyr::select(com1, com2, mass_ratio) |> 
      dplyr::distinct()) |> 
  dplyr::mutate(sig = ifelse(`Pr(>|z|)` < 0.05, "significant", "not significant"),
                dir = ifelse(Estimate < 0, "neg", "pos")) |> 
  dplyr::group_by(com2, dir, sig) |> 
  dplyr::count() |> 
  dplyr::group_by(com2) |> 
  dplyr::mutate(tot = sum(n)) |> 
  dplyr::mutate(prop = n / tot) |> 
  dplyr::filter(sig == "significant")
