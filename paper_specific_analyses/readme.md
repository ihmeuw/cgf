# PAPER SPECIFIC ANALYSES

### Overview of each script

##### figure_1_niger_example.R

This script reads in GBD results for Niger that are used in figure 1 and in the assessments of how stunting has changed in Niger over time. To construct those curves it sources the same script about distribution families which is located in the GBD modeling 2 folder about ensemble distributions. 

##### figure_2_global_maps.R

This script pulls in GBD results of stunting, wasting, and underweight prevalences and produces the map grid seen in figure 2.

##### figure_3_change_since_1990.R

This script reads in GBD results for 1990-2020 and assesses relative changes in CGF prevalences since 1990, highlighted in figure 3. It also identifies countries that have experienced the fastest relative improvements in severe and extreme CGF as compared to overall CGF.

##### figure_4_5_with_supplements_UHC_Analysis.R

This script reads in GBD results for CGF prevalences from 1990-2020 as well as UHC estimates from 1990-2020. It then runs MR-BRT models to generate expected values of CGF prevalence for a range of UHC values. The fit of these models are shown in an age-sex specific fashion in the supplementary materials. Results are shown by region for overall CGF in Figure 4, while severe and extreme CGF results can be found in the supplementary materials. Results are also displayed for individual countries. Expected relationships are scaled across severities of CGF and shown in figure 5.

##### figure 6_UHC_disruptions.R

This script reads in GBD results for CGF prevalences from 1990-2020 as well as UHC estimates from 1990-2020. It then dichotomizes those country-years based on whether UHC improved or worsened that year, and asseses changes in CGF prevalences, as shown in figure 6.

##### data_landscape_region_disaggregation_maps.R

This script maps the number of input data sources and also creates maps that reflect which locations are included in which regions/super regions in GBD.

##### stgpr_ensemble_supplementary_launch.R

This script launches stgpr_ensemble_supplementary_parallel.R, allowing for each of HAZ, WAZ, and WHZ portions of the appendix to complete in parallel.

##### stgpr_ensemble_supplementary_parallel.R

This script reads in results for ST-GPR models as well as the final curve estimates. It then plots those for HAZ, WAZ, and WHZ for each location alongside with the input data sources that went into the models for that location.

##### stgpr_standardized_betas.R

This script pulls in the results of the stage 1 prior models and plots the frequency with which different covariates were selected for inclusion into the stage 1 ensemble prior. It also plots the strength of those covariates in models where they were chosen

##### table_of_results.R

This script pulls in the GBD results for 1990, 2000, 2010, and 2020, and formats those into a table for each location for HAZ, WAZ, and WHZ.

##### mrbrt_knot_placement.R

This script pulls in model output from MR-BRT models used in the UHC analysis and creates tables that denote the knot placement for each age-sex specific model, along with the weight each model was given in the ensemble of models.

##### UHC_SDI_Analysis.R

This script is similiar to figure_4_5_with_supplements_UHC_Analysis.R, but in this script the MR-BRT models include SDI as a covariate. Predictions are made for SDI values of 20, 40, 60, and 80.

##### scaled_UHC_SDI.R

This script reads in the results from UHC_SDI_Analysis.R and scales the expected prevalences of CGF based on UHC for four levels of SDI. Appendix J uses a similar approach to that used in the creation of figure 5 with regards to scaling the splines.




