library(ebirdst)
library(tidyverse)
library(here)

sp_list <- ebirdst::ebirdst_runs |> 
  tibble::as_tibble()

chia <- readr::read_csv( here::here("data/chia/NestTrait_v2.csv"))

sci_name_join <- chia |> 
  dplyr::select(
    order = Order, 
    family = Family,
    scientific_name = Scientific_name, 
    primary = NestStr_primary_cavity, 
    secondary = NestStr_second_cavity,
    ground = NestSite_ground,
    tree = NestSite_tree, 
    nontree = NestSite_nontree, 
    cliff = NestSite_cliff_bank, 
    underground = NestSite_underground,
    water = NestSite_waterbody, 
    termite = NestSite_termite_ant) |> 
  dplyr::right_join(
    sp_list |> 
      dplyr::select(species_code,
                    scientific_name, 
                    common_name, 
                    is_resident, 
                    resident_quality,
                    breeding_quality)) |> 
  dplyr::filter(!is.na(primary))

got_cavity <- chia |> 
  # recovery a few more joins based on common name
  dplyr::select(
    order = Order, 
    family = Family,
    common_name = Common_name, 
    #scientific_name = Scientific_name, 
    primary = NestStr_primary_cavity, 
    secondary = NestStr_second_cavity,
    ground = NestSite_ground,
    tree = NestSite_tree, 
    nontree = NestSite_nontree, 
    cliff = NestSite_cliff_bank, 
    underground = NestSite_underground,
    water = NestSite_waterbody, 
    termite = NestSite_termite_ant) |> 
  dplyr::right_join(
    sp_list |> 
      dplyr::select(species_code,
                    scientific_name, 
                    common_name, 
                    is_resident, 
                    resident_quality,
                    breeding_quality)) |> 
  dplyr::filter(!is.na(primary)) |> 
  dplyr::filter(! species_code %in% sci_name_join$species_code) |> 
  dplyr::bind_rows(sci_name_join) 

download_these <- got_cavity |> 
  dplyr::select(order, family, species_code, scientific_name, common_name, primary, secondary,
               tree) |> 
  dplyr::filter(primary == 1 | secondary == 1) |> 
  dplyr::full_join(
    sp_list |> 
      dplyr::filter(! scientific_name %in% got_cavity$scientific_name) |> 
      dplyr::select(species_code, scientific_name, common_name))

for(i in 1:length(download_these$scientific_name)){
  ebirdst::ebirdst_download_status( species = download_these$scientific_name[i], 
                                    path = here::here("data/ranges"),
                                    download_abundance = FALSE, 
                                    download_occurrence = FALSE,
                                    download_count = FALSE,
                                    download_ranges = TRUE,
                                    download_regional = FALSE,
                                    download_pis = FALSE,
                                    download_ppms = FALSE,
                                    download_all = FALSE,
                                    pattern = "_27km_")
  print(paste0("Finished ", i, " of ", length(download_these$scientific_name) ))
}

readr::write_csv(download_these, here::here("data/download_range_maps_for_these_species.csv"))
