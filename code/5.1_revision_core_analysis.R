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
  dplyr::filter( metric == "n" ) |>  # focus on abundance
  dplyr::filter( group == "secondary" ) |>  # non-excavators
  dplyr::filter( mass_ratio == "all" ) |>  # all sizes
  dplyr::mutate(x = as.numeric(scale(log1p(value))), # scale the log-transformed summed abundance
                is_edge = factor(is_edge) ) |>    # I guess we still have the core cells
  dplyr::rename(edge = is_edge, 
                code = species_code) |> 
  # dplyr::filter(edge == 1) |>  # filter only to range-edge
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
  n ~ 1 + x + edge + x:edge + (1 + x + edge + x:edge | code),
  family = Gamma(link = "log"),
  data = final1)

pdf_m1 <- tidyr::expand_grid(
  edge = unique(final1$edge),
  x = seq(from = min(final1$x),
          to = max(final1$x),
          length.out = 100))

pred_m1 <- predict(m1, pdf_m1, re.form = NA, type = "response", se = TRUE)

final1_sc <- scale(log1p(final1$value))

pdat_m1 <- pdf_m1 |> 
  tibble::add_column(fit = pred_m1$fit,
                     se = pred_m1$se,
                     group = "non-excavator", 
                     size = "all other species") |> 
  dplyr::mutate(het_n = x*attr(final1_sc, "scaled:scale") + attr(final1_sc, "scaled:center"),
                position = ifelse(edge == 1, "edge", "core")) |> 
  dplyr::filter(het_n >= 0) |> 
  dplyr::select(group, size, position, het_n, fit, se)

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
  # dplyr::filter(edge == 1) |> 
  dplyr::filter(cell_id %in% focal_cells$cell_id) |> 
  dplyr::group_by(com) |> 
  dplyr::mutate( ncell = n()) |> 
  dplyr::filter(ncell > 10) |> 
  dplyr::ungroup()

m2 <- glmmTMB::glmmTMB(
  n ~ 1 + x + edge + x:edge + (1 + x + edge + x:edge | code),
  family = Gamma(link = "log"),
  data = final2)

pdf_m2 <- tidyr::expand_grid(
  edge = unique(final2$edge),
  x = seq(from = min(final2$x),
          to = max(final2$x),
          length.out = 100))

pred_m2 <- predict(m2, pdf_m2, re.form = NA, type = "response", se = TRUE)

final2_sc <- scale(log1p(final2$value))

pdat_m2 <- pdf_m2 |> 
  tibble::add_column(fit = pred_m2$fit,
                     se = pred_m2$se,
                     group = "non-excavator", 
                     size = "w/in 50% mass") |> 
  dplyr::mutate(het_n = x*attr(final2_sc, "scaled:scale") + attr(final2_sc, "scaled:center"),
                position = ifelse(edge == 1, "edge", "core")) |> 
  dplyr::filter(het_n >= 0) |> 
  dplyr::select(group, size, position, het_n, fit, se)

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
  # dplyr::filter(edge == 1) |> 
  dplyr::filter(cell_id %in% focal_cells$cell_id) |> 
  dplyr::group_by(com) |> 
  dplyr::mutate( ncell = n()) |> 
  dplyr::filter(ncell > 10) |> 
  dplyr::ungroup()

m3 <- glmmTMB::glmmTMB(
  n ~ 1 + x + edge + x:edge + (1 + x + edge + x:edge | code),
  family = Gamma(link = "log"),
  data = final3)

pdf_m3 <- tidyr::expand_grid(
  edge = unique(final3$edge),
  x = seq(from = min(final3$x),
          to = max(final3$x),
          length.out = 100))

pred_m3 <- predict(m3, pdf_m3, re.form = NA, type = "response", se = TRUE)

final3_sc <- scale(log1p(final3$value))

pdat_m3 <- pdf_m3 |> 
  tibble::add_column(fit = pred_m3$fit,
                     se = pred_m3$se,
                     group = "excavator", 
                     size = "all other species") |> 
  dplyr::mutate(het_n = x*attr(final3_sc, "scaled:scale") + attr(final3_sc, "scaled:center"),
                position = ifelse(edge == 1, "edge", "core")) |> 
  dplyr::filter(het_n >= 0) |> 
  dplyr::select(group, size, position, het_n, fit, se)

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
  # dplyr::filter(edge == 1) |> 
  dplyr::filter(cell_id %in% focal_cells$cell_id) |> 
  dplyr::group_by(com) |> 
  dplyr::mutate( ncell = n()) |> 
  dplyr::filter(ncell > 10) |> 
  dplyr::ungroup()

m4 <- glmmTMB::glmmTMB(
  n ~ 1 + x + edge + x:edge + (1 + x + edge + x:edge | code),
  family = Gamma(link = "log"),
  data = final4)

pdf_m4 <- tidyr::expand_grid(
  edge = unique(final4$edge),
  x = seq(from = min(final4$x),
          to = max(final4$x),
          length.out = 100))

pred_m4 <- predict(m4, pdf_m4, re.form = NA, type = "response", se = TRUE)

final4_sc <- scale(log1p(final4$value))

pdat_m4 <- pdf_m4 |> 
  tibble::add_column(fit = pred_m4$fit,
                     se = pred_m4$se,
                     group = "excavator", 
                     size = "w/in 50% mass") |> 
  dplyr::mutate(het_n = x*attr(final4_sc, "scaled:scale") + attr(final4_sc, "scaled:center"),
                position = ifelse(edge == 1, "edge", "core")) |> 
  dplyr::filter(het_n >= 0) |> 
  dplyr::select(group, size, position, het_n, fit, se)

all <- dplyr::full_join(
  pdat_m1, pdat_m2) |> 
  dplyr::full_join(
    pdat_m3) |> 
  dplyr::full_join(
    pdat_m4) |> 
  dplyr::mutate(group = factor(group, 
                               levels = c("non-excavator", 
                                          "excavator")))

ggplot( data = all,
        aes(x = het_n, 
            y = fit, 
            color = position, 
            fill = position)) +
  facet_grid(size~group) +
  geom_ribbon(aes(ymin = ifelse(fit - 1.96*se > 0, fit - 1.96*se, 0), 
                  ymax = fit + 1.96*se), 
              color = NA, 
              alpha = 0.4) +
  geom_line(linewidth = 1.5) +
  theme_minimal() +
  scale_color_manual(
    values = MetBrewer::MetPalettes$Pillement[[1]][c(1,4)]) +
  scale_fill_manual(
    values = MetBrewer::MetPalettes$Pillement[[1]][c(1,4)]) +
  labs(x = "ln( 1 + abundance of other species)",
       y = "focal non-excavator abundance",
       color = "range position", 
       fill = "range position") +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.2),
    axis.title = element_text(color = "black", size = 9), 
    axis.text = element_text(color = "black", size = 8), 
    strip.text = element_text(color = "black", size = 9, face = "bold"),
    panel.background = element_rect(color = NA, fill = "gray95"),
    plot.background = element_rect(color = NA, fill = "white"),
    legend.position = "bottom",
    legend.title = element_text(color = "black", size = 9),
    legend.text = element_text(color = "black", size = 8))

ggsave(
  filename = here::here("figures/figure_s01.png"), 
  width = 4, 
  height = 3.5, 
  units = "in", 
  dpi = 600)

summary(m1) -> m1s

m1s$coefficients$cond |> 
  as_tibble(rownames = "param") |> 
  janitor::clean_names() |> 
  mutate(across(estimate:pr_z, function(x) round(x, 2))) |>
  mutate(pr_z = ifelse(pr_z == 0, "< 0.01", pr_z)) |>
  dplyr::filter(!param == "(Intercept)")
