# download ebirdst abundance maps for the final species list
library(here)
library(tidyverse)
library(ebirdst)

download_these <- readr::read_csv(here::here("data/final_species_list.csv")) |> 
  dplyr::rename(scientific_name = sci)

for(i in 1:length(download_these$scientific_name)){
  ebirdst::ebirdst_download_status( species = download_these$scientific_name[i],
                                    path = here::here("data/abundance"),
                                    download_abundance = TRUE,
                                    download_occurrence = FALSE,
                                    download_count = FALSE,
                                    download_ranges = FALSE,
                                    download_regional = FALSE,
                                    download_pis = FALSE,
                                    download_ppms = FALSE,
                                    download_all = FALSE,
                                    pattern = "_27km_")
  print(paste0("Finished ", i, " of ", length(download_these$scientific_name) ))
}