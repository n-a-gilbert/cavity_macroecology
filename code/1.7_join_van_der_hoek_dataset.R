# initially we relied on the Chia et al. nest database
# but wanted to cross-check it and corroborate with the van der Hoek database, 
# since we found some questionable classificaitons in Chia
# This script joins up the datafiles; we had to do a manual review to resolve some name idiosyncracies
library(here)
library(tidyverse)
library(readxl)
library(janitor)

# van der Hoek
fac <- readxl::read_xlsx(
  path = here::here("data/ddi12601-sup-0001-tables1.xlsx"),
  sheet = "Tree-cavity nesters") |> 
  janitor::clean_names()

# bad coding practice...hard-coding a table that I want to join with VDH
tmp <- tibble::tibble(
  com = c("Barrow's Goldeneye", "Black-bellied Whistling-Duck", "Bufflehead", 
          "Common Goldeneye", "Common Merganser", "Fulvous Whistling-Duck", 
          "Hooded Merganser", "Wood Duck", "American Kestrel", "Purple Martin", 
          "Tree Swallow", "Violet-green Swallow", "Black-capped Chickadee", 
          "Black-crested Titmouse", "Boreal Chickadee", "Bridled Titmouse", 
          "Carolina Chickadee", "Chestnut-backed Chickadee", "Juniper Titmouse", 
          "Mountain Chickadee", "Oak Titmouse", "Tufted Titmouse", "Lucy's Warbler", 
          "Prothonotary Warbler", "Eurasian Tree Sparrow", "House Sparrow", 
          "Brown-headed Nuthatch", "Pygmy Nuthatch", "Red-breasted Nuthatch", 
          "White-breasted Nuthatch", "European Starling", "Bewick's Wren", 
          "Carolina Wren", "Pacific Wren", "Winter Wren", "Northern/Southern House Wren", 
          "Eastern Bluebird", "Mountain Bluebird", "Western Bluebird", 
          "Ash-throated Flycatcher", "Brown-crested Flycatcher", "Dusky-capped Flycatcher", 
          "Great Crested Flycatcher", "Sulphur-bellied Flycatcher", "Lewis's Woodpecker", 
          "Northern Flicker", "Red-headed Woodpecker", "Boreal Owl", "Barred Owl", 
          "Eastern Screech-Owl", "Elf Owl", "Flammulated Owl", "Northern Pygmy-Owl", 
          "Northern Saw-whet Owl", "Spotted Owl", "Western Screech-Owl", 
          "Whiskered Screech-Owl", "American Barn Owl"),
  scientific_name = c("Bucephala islandica", "Dendrocygna autumnalis", "Bucephala albeola", 
                      "Bucephala clangula", "Mergus merganser", "Dendrocygna bicolor", 
                      "Lophodytes cucullatus", "Aix sponsa", "Falco sparverius", "Progne subis", 
                      "Tachycineta bicolor", "Tachycineta thalassina", "Poecile atricapillus", 
                      "Baeolophus atricristatus", "Poecile hudsonicus", "Baeolophus wollweberi", 
                      "Poecile carolinensis", "Poecile rufescens", "Baeolophus ridgwayi", 
                      "Poecile gambeli", "Baeolophus inornatus", "Baeolophus bicolor", 
                      "Leiothlypis luciae", "Protonotaria citrea", "Passer montanus", 
                      "Passer domesticus", "Sitta pusilla", "Sitta pygmaea", "Sitta canadensis", 
                      "Sitta carolinensis", "Sturnus vulgaris", "Thryomanes bewickii", 
                      "Thryothorus ludovicianus", "Troglodytes pacificus", "Troglodytes hiemalis", 
                      "Troglodytes aedon/musculus", "Sialia sialis", "Sialia currucoides", 
                      "Sialia mexicana", "Myiarchus cinerascens", "Myiarchus tyrannulus", 
                      "Myiarchus tuberculifer", "Myiarchus crinitus", "Myiodynastes luteiventris", 
                      "Melanerpes lewis", "Colaptes auratus", "Melanerpes erythrocephalus", 
                      "Aegolius funereus", "Strix varia", "Megascops asio", "Micrathene whitneyi", 
                      "Psiloscops flammeolus", "Glaucidium gnoma", "Aegolius acadicus", 
                      "Strix occidentalis", "Megascops kennicottii", "Megascops trichopsis", 
                      "Tyto furcata"),
  code = c("bargol", "bbwduc", "buffle", "comgol", "commer", "fuwduc", 
           "hoomer", "wooduc", "amekes", "purmar", "treswa", "vigswa", "bkcchi", 
           "blctit4", "borchi2", "britit", "carchi", "chbchi", "juntit1", 
           "mouchi", "oaktit", "tuftit", "lucwar", "prowar", "eutspa", "houspa", 
           "bnhnut", "pygnut", "rebnut", "whbnut", "eursta", "bewwre", "carwre", 
           "pacwre1", "winwre3", "y01309", "easblu", "moublu", "wesblu", 
           "astfly", "bncfly", "ducfly", "grcfly", "subfly", "lewwoo", "norfli", 
           "rehwoo", "borowl", "brdowl", "easowl1", "elfowl", "flaowl", 
           "nopowl", "nswowl", "spoowl", "wesowl1", "whsowl1", "brnowl"))

# first try joining based on scientific name
sci_join <- tmp |> 
  dplyr::left_join(
    fac |> 
      dplyr::select(scientific_name, ob = obligate_or_facultative, type = cavity_nester_type)) |> 
  dplyr::filter(!is.na(ob))

# second try joining based on common name
com_join <- tmp |> 
  dplyr::left_join(
    fac |> 
      dplyr::select(com = name, ob = obligate_or_facultative, type = cavity_nester_type)) |> 
  dplyr::filter(!is.na(ob))

# DANGER ZONE
# there were a few species that didn't join up readily 
# e.g., due to recent splits
# identified those, read them out, and then Hallie did a manual review
# commenting this out so it doesn't get overwritten
# final1 |>
# dplyr::select(com, scientific_name, code) |>
# dplyr::distinct() |>
# dplyr::anti_join(
# dplyr::full_join(com_join, sci_join)) |>
#   readr::write_csv(
#     here::here("data/review_species_van_der_hoek_join.csv")
#   )

# okay, following Hallie's review, we have data for those species that had trouble joining up
rev <- readr::read_csv(here::here("data/review_species_van_der_hoek_join_v2.csv")) |> 
  dplyr::select(com, scientific_name, code, ob = `Obligate or Facultative`, type)

# this gives us VDH data for all species; write out
com_join |> 
  full_join(sci_join) |> 
  full_join(rev) |> 
  readr::write_csv(here::here("data/focal_species_van_der_hoek_classification.csv"))