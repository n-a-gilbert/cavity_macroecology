# Limited evidence that nest-site competition and facilitation drive range limits

### Data/code DOI:
__________________________________________________________________________________________________________________________________________

## Abstract
**Aim.** Biotic interactions form a pillar of most niche concepts but are frequently overlooked as range-limiting factors. At local scales, bird species that nest in tree cavities but cannot create cavities themselves—“non-excavator” species—experience competition with other non-excavators but facilitation by excavator species such as woodpeckers. Our objective was to evaluate whether nest-site competition and facilitation mediate the range limits of non-excavator cavity-nesting birds.

**Location.** United States and Canada.  

**Time Period.** Contemporary. 

**Group.** Cavity-nesting birds (58 non-excavator species). 

**Methods.** Using eBird relative abundance maps, we modeled abundance of non-excavator species within their non-coastal range limits as a function of either (1) summed abundances of all other non-excavators, (2) summed abundances of non-excavators within 50% body mass of the focal species, (3) summed abundances of all excavators, (4) summed abundances of excavators within 50% body mass of the focal species, or (5) abundances of either House Sparrows (_Passer domesticus_) or European Starlings (_Sturnus vulgaris_), two invasive ‘supercompetitors’.    

**Results.** At a cross-species level, the effects of heterospecific non-excavator and excavator abundance were not significant. At a species level, only 3 species (5% of the total) showed strong (≥95% confidence) competitive effects of non-excavators and strong facilitative effects of excavators. However, invasive ‘supercompetitors’ were associated with low range-limit abundance of non-excavators; for example, House Sparrows showed negative effects on range-limit abundance for nine out of the seventeen (53%) non-excavators of similar size.

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
  * [5.1_revision_core_analysis.R](./code/5.1_revision_core_analysis.R) Revision analysis: comparing effects of heterospecific abundance within range edges and range cores
  * [5.2_revision_edge_5th_percentile.R](./code/5.2_revision_edge_5th_percentile.R) Revision sensitivity analysis: range edges defined based on 5th percentiles of distance-to-range-edge
  * [5.3_revision_edge_20th_percentile.R](./code/5.3_revision_edge_20th_percentile.R) Revision sensitivity analysis: range edges defined based on 20th percentiles of distance-to-range-edge
  * [5.4_revision_coast_50km.R](./code/5.4_revision_coast_50km.R) Revision sensitivity analysis: defining non-coastal cells as >50 km from coast (instead of 100 km)
  * [5.5_revision_supercompetitor_habitat.R](./code/5.5_revision_supercompetitor_habitat.R) Revision analysis: accounting for anthropogenic habitat in supercompetitor models
  * [5.6_revision_body_mass_25percent.R](./code/5.6_revision_body_mass_25percent.R) Revision sensitivity analysis: evaluating effects of heterospecifics within 25% of body mass

### data
** NOTE ** eBird range maps and abundance maps are not included in this repository due to file size limitations. Upon running [1.1_download_cavity_nester_range_maps.R](./code/1.1_download_cavity_nester_range_maps.R) and [1.5_download_ebird_abundance_maps.R](./code/1.5_download_ebird_abundance_maps.R), you will have subfolders named `abundance` and `ranges` within the data folder.
  * [chia](./data/chia). Folder with tables from [Chia et al. 2023](https://www.nature.com/articles/s41597-023-02837-1). See that publication for further details
     * [NestTrait_v2.csv](./data/chia/NestTrait_v2.csv) Nest trait data from Chia et al. 2023
     * [NestTrait_v2_metadata.csv](./data/chia/NestTrait_v2_metadata.csv) Column definitions for [NestTrait_v2.csv](./data/chia/NestTrait_v2.csv)
     * [NestTrait_v2_ref.csv](./data/chia/NestTrait_v2_ref.csv) References used to create [NestTrait_v2.csv](./data/chia/NestTrait_v2.csv)  
  * [GHM_27km_WGS84.tif](.data/GHM_27km_WGS84.tif) Human modification raster aggregated to 27 km resolution using Google Earth Engine (GEE). See [GEE Data Catalogue](https://developers.google.com/earth-engine/datasets/catalog/CSP_HM_GlobalHumanModification) for more information on this layer.
  * [avonet.csv](./data/avonet.csv). AvoNET database; see [Tobias et al. 2022](https://onlinelibrary.wiley.com/doi/10.1111/ele.13898) for detail. This CSV is the 'AVONET1_BirdLife' sheet of the 'AVONET Supplementary dataset 1.xlsx' file available for download on [Figshare](https://figshare.com/s/b990722d72a26b5bfead?file=34480856). Column definitions are provided in the 'Metadata' sheet of the 'AVONET Supplementary dataset 1.xlsx' file.    
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
    | primary | binary indicator of whether a species is primary excavator (1) or not (0) cavity-nesting species |
    | secondary | binary indicator of whether or not species is a secondary non-excavator (1) cavity-nesting species or not (0) |
    | tree | binary indicator of whether a species nests in trees (1) or not (0) |
  * [cavity_species_REVIEWED.csv](./data/cavity_species_REVIEWED.csv). Final table (same as above, but with our annotations added)
    | column | meaning |
    |--------|---------|
    | order | species order |
    | family | species family |
    | sci | scientific name |
    | com | common name |
    | code | 6-letter code |
    | primary | binary indicator of whether a species is primary excavator (1) or not (0) cavity-nesting species |
    | secondary | binary indicator of whether or not species is a secondary non-excavator (1) cavity-nesting species or not (0) |
    | tree | binary indicator of whether a species nests in trees (1) or not (0) |
    | feral | binary indicator of whether the species occurs only as small feral populations in North America (1) or not (0); this was done for parrots only |
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
    | primary | binary indicator of whether a species is primary excavator (1) or not (0) cavity-nesting species |
    | secondary | binary indicator of whether or not species is a secondary non-excavator (1) cavity-nesting species or not (0) |
    | tree | binary indicator of whether a species nests in trees (1) or not (0) |
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
  * [download_range_maps_for_these_species.csv](./data/download_range_maps_for_these_species.csv). Species to download range maps for
    | column | meaning |
    |--------|---------|
    | order | species order |
    | family | species family |
    | species_code | 6-letter ebird code |
    | scientific_name | scientific name per eBird |
    | common_name | common name per eBird |
    | primary | binary indicator of whether a species is primary excavator (1) or not (0) cavity-nesting species |
    | secondary | binary indicator of whether or not species is a secondary non-excavator (1) cavity-nesting species or not (0) |
    | tree | binary indicator of whether a species nests in trees (1) or not (0) |
  * [final_species_list.csv](./data/final_species_list.csv). Species list after some filtering/review, etc.
    | column | meaning |
    |--------|---------|
    | order | species order |
    | family | species family |
    | sci | scientific name per eBird |
    | com | common name per eBird |
    | code | 6-letter ebird code |
    | primary | binary indicator of whether a species is primary excavator (1) or not (0) cavity-nesting species |
    | secondary | binary indicator of whether or not species is a secondary non-excavator (1) cavity-nesting species or not (0) |
    | tree | binary indicator of whether a species nests in trees (1) or not (0) |
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
    | primary | binary indicator of whether a species is primary excavator (1) or not (0) cavity-nesting species |
    | secondary | binary indicator of whether or not species is a secondary non-excavator (1) cavity-nesting species or not (0) |
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
  * [van_der_hoek.csv](./data/van_der_hoek.csv). Data from [van der Hoek et al. 2017](https://onlinelibrary.wiley.com/doi/full/10.1111/ddi.12601). See that paper for further details; this CSV is the 'Tree-cavity nesters' sheet from the ddi12601-sup-0001-TableS1.xlsx table linked under 'Supporting Information'. The 'Meta-data' sheet from this same table provides column definitions.

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
  * [us_canada_edge_results2.RData](./results/us_canada_edge_results2.RData). Model results generated from [3.1_fit_cross_species_models.R](./code/3.1_fit_cross_species_models.R). This is an RData object that contains the following:
    * com_join: a table with the following columns:
       | column | meaning |
       |--------|---------|
       | com | common name |
       | scientific_name | scientific name |
       | code | six-letter eBird code for the species |
       | ob | classifies species as Facultative or Obligate cavity nester |
       | type | classifies species as non-excavator or facultative excavator |
    * fac: a table with the following columns:
       | column | meaning |
       |--------|---------|
       | com | common name |
       | scientific_name | scientific name |
       | ob | classifies species as Facultative or Obligate cavity nester |
       | type | classifies species as non-excavator or facultative excavator |
    * final1: final table used to fit model where the predictor variable is abundance of all other non-excavators
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
      | primary | binary indicator of whether a species is primary excavator (1) or not (0) cavity-nesting species |
      | secondary | binary indicator of whether or not species is a secondary non-excavator (1) cavity-nesting species or not (0) |
      | tree | binary indicator of whether a species nests in trees (1) or not (0) |
      | metric | what biodiversity metric - all rows are abundance 'n' |
      | group | which guild (primary or secondary) cavity nester |
      | mass_ratio | Indicates which-sized species are used in to calculate aggregate abundance of heterospecifics - "all" in this case |
      | value | Summed abundance of heterospecific cavity-nesters within the cell |
      | edge | indicates if the cell is an edge cell (1) or not (0) - should be prefiltered to be all 1's |
      | x | scaled, log1p'd version of the value column |
      | ncell | number of cells with data for the focal non-excavator |
    * final2: table used to fit model where the predictor variable is abundance of similar-sized non-excavators. Column definitions the same as final1 
    * final3: table used to fit model where the predictor variable is abundance of all excavators. Column definitions the same as final1 
    * final4: table used to fit model where the predictor variable is abundance of similar-sized excavators. Column definitions the same as final1 
    * m1_brm: brms model object for model where the predictor variable is abundance of all other non-excavators
    * m2_brm: brms model object for model where the predictor variable is abundance of similar-sized non-excavators
    * m3_brm: brms model object for model where the predictor variable is abundance of all excavators
    * m4_brm: brms model object for model where the predictor variable is abundance of similar-sized excavators
    * rev: a table with the following columns:
       | column | meaning |
       |--------|---------|
       | com | common name |
       | scientific_name | scientific name |
       | code | six-letter eBird code for the species |
       | ob | classifies species as Facultative or Obligate cavity nester |
       | type | classifies species as non-excavator or facultative excavator |
    * sci_join: a table with the following columns:
       | column | meaning |
       |--------|---------|
       | com | common name |
       | scientific_name | scientific name |
       | code | six-letter eBird code for the species |
       | ob | classifies species as Facultative or Obligate cavity nester |
       | type | classifies species as non-excavator or facultative excavator |
    * tmp: a table with the following columns:
       | column | meaning |
       |--------|---------|
       | com | common name |
       | scientific_name | scientific name |
       | code | six-letter eBird code for the species |

### cavity_macroecology.Rproj 
 R Project for organizing/accessing data and code in RStudio IDE

