# Limited evidence that nest-site competition and facilitation drive range limits

### Data/code DOI:
__________________________________________________________________________________________________________________________________________

## Abstract
**Aim.** Biotic interactions form a pillar of most niche concepts but are frequently overlooked as range-limiting factors. At local scales, bird species that nest in tree cavities but cannot create cavities themselves—“non-excavator” species—experience competition with other non-excavators but facilitation by excavator species such as woodpeckers. Our objective was to evaluate whether nest-site competition and facilitation mediate the range limits of non-excavator cavity-nesting birds.

**Location.** United States and Canada.  

**Time Period.** Contemporary. 

**Group.** Cavity-nesting birds (58 non-excavator species). 

**Methods.** Using eBird relative abundance maps, we modeled abundance of non-excavator species within their non-coastal range limits as a function of either (1) summed abundances of all other non-excavators, (2) summed abundances of non-excavators within 50% body mass of the focal species, (3) summed abundances of all excavators, (4) summed abundances of excavators within 50% body mass of the focal species, or (5) abundances of either House Sparrows (_Passer domesticus_) or European Starlings (_Sturnus vulgaris_), two invasive ‘supercompetitors’.    

**Results.** At a cross-species level, the effects of heterospecific non-excavator and excavator abundance were not significant. At a species level, only 3 species (5% of the total) showed strong (≥95% confidence) competitive effects of non-excavators and strong facilitative effects of excavators. However, non-invasive ‘supercompetitors’ were associated with low range-limit abundance of non-excavators; for example, House Sparrows showed negative effects on range-limit abundance for nine out of the seventeen (53%) non-excavators of similar size.

**Main Conclusions.** Our results are consistent with the ‘Eltonian Noise Hypothesis’, which suggests that biotic interactions get ‘washed out’ at broadening spatial scales such that only abiotic variables correlate with species distributions at broad scales. Among-species variation in habitat selection (e.g., preferences for cavities at different heights) and nesting phenology may contribute to the limited evidence we found of range limits being formed by nest-site competition or facilitation. 

 $~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~$ <img src="https://github.com/n-a-gilbert/cavity_macroecology/blob/main/figures/figure_01.png" width="600" />
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
  * [cavity_nesters_review.csv](./data/cavity_nesters_review.csv). Table we generated to review the Chia et al classifications
    | column | meaning |
    |--------|---------|
    | order | species order |
    | family | species family |
    | sci | scientific name |
    | com | common name |
    | code | 6-letter code |
    | primary | binary indicator of whether or not species is a primary (excavator) cavity-nesting species |
    | secondary | binary indicator of whether or not species is a secondary (non-excavator) cavity-nesting species |
    | tree | binary indicator of whether or not species nests in trees |
  * [cavity_species_REVIEWED.csv](./data/cavity_species_REVIEWED.csv). Final table (same as above, but with our annotations added)
    | column | meaning |
    |--------|---------|
    | order | species order |
    | family | species family |
    | sci | scientific name |
    | com | common name |
    | code | 6-letter code |
    | primary | binary indicator of whether or not species is a primary (excavator) cavity-nesting species |
    | secondary | binary indicator of whether or not species is a secondary (non-excavator) cavity-nesting species |
    | tree | binary indicator of whether or not species nests in trees |
    | feral | binary indicator of whether species occurs only as small feral populations in North America; this was done for parrots only |
    | neil_classification | First author (Neil) did a initial screening and classified species as "good", "omit" (not cavity nesters), or "idk" (species he was unsure of) |
    | hallie_classification | Last author (Hallie) did a subsequent screening of species marked "idk" in the previous step and classified species as "good" (cavity nesters) or "omit" (not cavity nesters) |
    | notes | Notes taken during manual review |
  * [cavity_species_with_other_species_abundance_v02.csv](./data/cavity_species_with_other_species_abundance_v02.csv). Table with cavity-nester abundance and columns for distance to coast and range edge, and abundance of heterospecifics
    | column | meaning |
    |--------|---------|
    | cell_id | unique ID for grid cell (27 x 27 km ) |
    | com | common name per eBird |
    | scientific_name | scientific name per eBird |
    | species_code | 6-letter ebird code |
    | n | relative abundance |
    | coast_dist | distance in meters from the grid cell's centroid to the nearest coastline |
    | range_dist | distance in meters from the grid cell's centroid to the nearest range edge |
    | position | Position within range (edge or core) |
    | mass | species' mass from avonet |
    | order | species order |
    | family | species family |
    | primary | binary indicator of whether or not species is a primary (excavator) cavity-nesting species |
    | secondary | binary indicator of whether or not species is a secondary (non-excavator) cavity-nesting species |
    | tree | binary indicator of whether or not species nests in trees |
    | n_secondary | summed abundance of other non-excavators within the grid cell |
    | sr_secondary | species richness of other non-excavators within the grid cell |
    | n_secondary_0.5 | summed abundance of other non-excavators - similar sized only - within the grid cell |
    | sr_secondary_0.5 | species richness of other non-excavators - similar sized only -  within the grid cell |
    | n_primary | summed abundance of obligate excavators within the grid cell |
    | sr_primary | species richness of obligate excavators within the grid cell |
    | n_primary_0.5 | summed abundance of obligate excavators - similar size only - within the grid cell |
    | sr_primary_0.5 | species richness of obligate excavators - similar size only - within the grid cell |
    | n_primary2 | summed abundance of excavators (obligate and facultative) within the grid cell |
    | sr_primary2 | species richness of excavators (obligate and facultative) within the grid cell |
    | n_primary2_0.5 | summed abundance of excavators (obligate and facultative) - similar size  within the grid cell |
    | sr_primary2_0.5 | species richness of excavators (obligate and facultative) within the grid cell |
  * [cell_coast_dist.csv](./data/cell_coast_dist.csv). Distance from each grid cell centroid to the nearest coastline
    | column | meaning |
    |--------|---------|
    | cell_id | grid cell identifier |
    | coast_dist | distance to nearest coastline (meters) |
  * [ddi12601-sup-0001-tables1.xlsx](./data/ddi12601-sup-0001-tables1.xlsx). Data from [van der Hoek et al. 2017](https://onlinelibrary.wiley.com/doi/full/10.1111/ddi.12601). See that paper for further details.
  * [download_range_maps_for_these_species.csv](./data/download_range_maps_for_these_species.csv). Species to download range maps for
    | column | meaning |
    |--------|---------|
    | order | species order |
    | family | species family |
    | species_code | 6-letter ebird code |
    | scientific_name | scientific name per eBird |
    | common_name | common name per eBird |
    | primary | binary indicator of whether or not species is a primary (excavator) cavity-nesting species |
    | secondary | binary indicator of whether or not species is a secondary (non-excavator) cavity-nesting species |
    | tree | binary indicator of whether or not species nests in trees |
  * [final_species_list.csv](./data/final_species_list.csv). Species list after some filtering/review, etc.
    | column | meaning |
    |--------|---------|
    | order | species order |
    | family | species family |
    | sci | scientific name per eBird |
    | com | common name per eBird |
    | code | 6-letter ebird code |
    | primary | binary indicator of whether or not species is a primary (excavator) cavity-nesting species |
    | secondary | binary indicator of whether or not species is a secondary (non-excavator) cavity-nesting species |
    | tree | binary indicator of whether or not species nests in trees |
  * [focal_area2.shp](./data/focal_area2.shp). Focal area (mainland US and Canada); this polygon was created in [1.6_create_focal_area_shapefile](./code/1.6_create_focal_area_shapefile.R). The other extensions (`.dbf`, etc) are in this folder but not described here.
  * [focal_species_van_der_hoek_classification.csv](./data/focal_species_van_der_hoek_classification.csv). Non-excavators from analysis with van der Hoek classifications.
    | column | meaning |
    |--------|---------|
    | com | common name per eBird |
    | sci | scientific name per eBird |
    | code | 6-letter ebird code |
    | ob | describes species as either an "obligate" or "facultative" cavity-nester |
    | type | describes species as either "excavator" or "non-excavator" |
  * [north_america_cavity_nesters_to_review.csv](./data/north_america_cavity_nesters_to_review.csv). Cavity-nesting species occuring within North America to review manually for accuracy
    | column | meaning |
    |--------|---------|
    | species_code | 6-letter ebird code |
    | within | indicates whether species breeding range is entirely within USA, Canada, and Mexico |
    | inter | indicates whether species breeding range intersects with USA, Canada, and Mexico |
    | com | common name per eBird |
    | sci | scientific name per eBird |
    | order | species order |
    | family | species family |
    | scientific_name | scientific name per eBird |
    | common_name | common name per eBird |
    | primary | binary indicator of whether or not species is a primary (excavator) cavity-nesting species |
    | secondary | binary indicator of whether or not species is a secondary (non-excavator) cavity-nesting species |
  * [review_species_van_der_hoek_join.csv](./data/review_species_van_der_hoek_join.csv). Small table of species to manually review due to taxonomy idiosyncracies
    | column | meaning |
    |--------|---------|
    | com | common name per eBird |
    | scientific_name | scientific name per eBird |
    | code | 6-letter ebird code |
  * [review_species_van_der_hoek_join_v2.csv](./data/review_species_van_der_hoek_join_v2.csv) Same table as above, but post-review
       | column | meaning |
    |--------|---------|
    | com | common name per eBird |
    | scientific_name | scientific name per eBird |
    | code | 6-letter ebird code |
    | Obligate or Facultative | Indicates whether species is an obligate or facultative cavity nester |
    | type | Non-excavator or excavator |

### figures
  * [figure_01.png](./figures/figure_01.png) Figure 1
  * [figure_01.pptx](./figures/figure_01.pptx) Figure 1 (PowerPoint format for annotation)
  * [figure_01b.png](./figures/figure_01b.png) Figure 1b
  * [figure_02.png](./figures/figure_02.png) Figure 2
  * [figure_02.pptx](./figures/figure_02.pptx) Figure 2 (PowerPoint format for collation/annotation)
  * [figure_03.png](./figures/figure_03.png) Figure 3
  * [figure_04.png](./figures/figure_04.png) Figure 4
  * [figure_04.pptx](./figures/figure_04.pptx) Figure 4 (PowerPoint format for annotation)
  * [figure_05.png](./figures/figure_05.png) Figure 5
  * [figure_06.png](./figures/figure_06.png) Figure 6
  * [figure_06.pptx](./figures/figure_06.pptx) Figure 6 (PowerPoint format for annotation)

### results
  * [us_canada_edge_results2.RData](./results/us_canada_edge_results2.RData). Model results generated from [3.1_fit_cross_species_models.R](./code/3.1_fit_cross_species_models.R)  
