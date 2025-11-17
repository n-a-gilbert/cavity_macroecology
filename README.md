# Limited evidence that nest-site competition and facilitation drive range limits

### Data/code DOI:
__________________________________________________________________________________________________________________________________________

## Abstract

## Repository Directory

### code 
  * [1.1_download_cavity_nester_range_maps.R](./code/1.1_download_cavity_nester_range_maps.R). This script downloads eBird range maps for cavity-nesting species.
  * [1.2_identify_north_american_cavity_nesters.R](./code/1.2_identify_north_american_cavity_nesters.R). This script identifies which cavity-nesters occur within North America
  * [1.3_make_list_of_cavity_nesters_to_review.R](./code/1.3_make_list_of_cavity_nesters_to_review.R). Script to make a table of species of manually review; there were some species that did not readily join to taxonomy shenanigans
  * [1.4_create_list_of_species_to_download_abundance_maps.R](./code/1.4_create_list_of_species_to_download_abundance_maps.R). Quick script to assemblage the final species list to download abundance maps for
  * [1.5_download_ebird_abundance_maps.R](./code/1.5_download_ebird_abundance_maps.R). Download eBird relative abundance maps for the focal species
  * [1.6_create_focal_area_shapefile](./code/1.6_create_focal_area_shapefile.R). Creates and saves a shapefile for the US and Canada joined.
  * [1.7_join_van_der_hoek_dataset.R](./code/1.7_join_van_der_hoek_dataset.R). Script to join with van der Hoek (2017) database.
  * [2.1_calculate_coast_distance.R](./code/2.1_calculate_coast_distance/R). Calculate distance between each grid cell and the nearest coastline
  * [2.2_calculate_range_edge_distance.R](./code/2.2_calculate_range_edge_distance.R). Calculate distance between each grid cell and nearest range edge for each species
  * [2.3_calculate_diversity_per_cell.R](./code/2.3_calculate_diversity_per_cell.R). Calculate summed abundance of cavity-nesting heterospecifics within range-edge grid cells for every species
  * [3.1_fit_cross_species_models.R](./code/3.1_fit_cross_species_models.R). Fit brms models relating range-edge abundance to abundance of heterospecifics of different categories
  * [3.2_supercompetitor_analysis_figure_06.R](./code/3.2_supercompetitor_analysis_figure_06.R). Do "supercompetitor" analysis and create Figure 6
  * [4.1_create_figure_01b.R](./code/4.1_create_figure_01b.R). Create conceptual graph for Fig. 1b
  * [4.2_create_maps_figure_02.R](./code/4.2_create_maps_figure_02.R). Create species maps for Fig. 2
  * [4.3_create_figures_03_04_05.R](./code/4.3_create_figures_03_04_05.R). Create the other figures 

### data
** NOTE ** eBird range maps and abundance maps are not included in this repository due to file size limitations. Upon running [1.1_download_cavity_nester_range_maps.R](./code/1.1_download_cavity_nester_range_maps.R) and [1.5_download_ebird_abundance_maps.R](./code/1.5_download_ebird_abundance_maps.R), you will have subfolders named `abundance` and `ranges` within the data folder.
  * [chia](./data/chia). Folder with tables from [Chia et al. 2023](https://www.nature.com/articles/s41597-023-02837-1). See that publication for further details
  * [avonet.csv](./data/avonet.csv). AvoNET database; see [Tobias et al. 2022](https://onlinelibrary.wiley.com/doi/10.1111/ele.13898) for detail  
  * [cavity_nesters_abundance_dists.csv](./data/cavity_nesters_abundance_dists.csv). Table with cavity-nester abundance and columns for distance to coast and range edge.
    | column | meaning |
    |--------|---------|
    | cell_id | unique ID for grid cell (27 x 27 km ) |
    | com | common name per eBird |
    | scientific_name | scientific name per eBird |
    | species_code | 6-letter ebird code |
    | n | relative abundance |
    | coast_dist | distance in meters from the grid cell's centroid to the nearest coastline |
    | range_dist | distance in meters from the grid cell's centroid to the nearest range edge |

   

  * 

    
### figures
  * one
  * two
  * three

### results
