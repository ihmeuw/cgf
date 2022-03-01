
### Project: ST-GPR
### Purpose: Launch custom first stage prior testing and assemble for upload to ST-GPR
### Overview: This script launches the function launch_prior() that in turn runs two functions: test_prior() and assemble_prior(). 
###           Together they allow the creation of an ensemble linear prior for STGPR.
### test_prior()
###   This function runs all combinations of provided covariates on a provided dataset
###   It ranks each sub-model by out-of-sample predictive validity and marks models containing insignificant betas and/or betas that represent the 'wrong' relationship with your outcome
###   Each added covariate adds a significant number of models to test. Only include covariates that may have a reasonable relationship with your outcome
###   The output is a list that includes a dataframe with a row for each model, the fit betas/SE, and whether or not that model violated significance or prior signs
###
### assemble_prior()
###   This function creates out-of-sample predictive validity-weighted predictions of your outcome using the results from the test_prior() function
###   This can be included as a 'cv_custom_prior' in an STGPR data upload
###   This function can also plot a time-series of predictions for submodels, top weighted model, and the ensemble model
###########################################################################################################

######################################
############### SET-UP ###############
######################################

rm(list=ls())
Sys.umask(mode = 002)
os <- .Platform$OS.type
if (os=="windows") {
  j <- "filepath"
  h <- "filepath"
  k <- "filepath"
} else {
  lib_path <- "filepath"
  username <- Sys.info()[["user"]]
  j <- "filepath"
  h <- paste0("filepath",username)
  k <- "filepath"
}
date<-gsub("-", "_", Sys.Date())

# Load Packages and Functions
pacman::p_load(data.table, ini)
source("filepath")

######################################
############## SET ARGS ##############
######################################

## Move to parallel script
## Getting normal QSub arguments
args <- commandArgs(trailingOnly = TRUE)
param_map_filepath <- args[1]

## Retrieving array task_id
task_id <- as.integer(Sys.getenv("SGE_TASK_ID"))
param_map <- fread(param_map_filepath)

## Required
me <- param_map[task_id, me_name]    # ST-GPR me name
decomp_step <- param_map[task_id, step]    # Decomp step (for pulling covariates)
crosswalk_version_id <- param_map[task_id, gbd2020_xw] # Crosswalk version id (for input pulling data)
group <- param_map[task_id, group]
custom_covs <- param_map[task_id, custom_covs]
custom_covs = TRUE



path_to_data <- "filepath"           # Path to data (custom data csv if not using a crosswalk version)

if(me %like% "_mean"){
  data_transform <- "log"       # Transform function for your data (usually "logit" for proportions, "log" for continuous variables)
} else{
  data_transform <- "logit"       # Transform function for your data (usually "logit" for proportions, "log" for continuous variables)
}

message(me, crosswalk_version_id)

## Functions
test_mods <- TRUE               # Whether to run the test_prior() function.  Not necessary if you've saved a previous run and just want to run assemble_prior()
average <- TRUE                 # Whether to run the assemble_prior() function. Not necessary if you just want to run test_prior()
plot_aves <- FALSE               # Whether to plot diagnostics for model averages
age_trend <- FALSE              # Whether to plot diagnostics for model averages by age instead of year
plot_betas <- FALSE              # Whether to plot diagnostics for ensemble betas

## Model Covariates

stg1_cov_table <- fread(paste0("filepath"))
stg1_cov_table <- stg1_cov_table[get(group) == 1]

cov_ids <- stg1_cov_table$covariate_id  # List of covariate IDs to test for predictive validity in ensemble
prior_sign <- stg1_cov_table$prior_sign

if(custom_covs == TRUE){
  custom_covs <- list(c("uhc_goalkeepers", "filepath"))             # List of character vectors containing 1) the custom covariate_name_short and 2) the filepath where the custom covariate is found
  prior_sign <- c(prior_sign, -1)
}

print(cov_ids)
print(prior_sign)


#custom_covs <- TRUE 
polynoms <- NULL                # String of polynomial transformations for covariates using covariate_name_short (e.g. c("sdi^2"))
ban_pairs <- NULL               # List of covariate IDs you wish to ban from occurring together in a sub-model (list of covariate IDs)
fixed_covs <- NULL              # Any covariates you want included in every model (i.e. age fixed effects)
random_effects <- c("(1|super_region_name/region_name/location_name)")  # Any random effects you want included in every model

## Model Settings
modtype <- "lmer"               # Linear model type (lmer if using random effects, lm otherwise)
rank_method <- "oos.rmse"       # OOS error method by which to rank sub-models. Options are Out-of-Sample RMSE ("oos.rmse") or Akaike Information Criterion ("aic")
n_mods <- 50                    # Number of top models to average over. More models is more computationally intensive, and there is a diminishing return
forms_per_job <- 15             # Number of model forms to test for each parallelized cluster job
pred_ages <- c(2,3,388, 389,238, 34)
by_sex <- TRUE                  # Whether your model is sex-specific
kos <- 5                       # Number of knock-outs for each sub-model
ko_prop <- 0.15                 # Proportion of data you want held out for each ko

## Cluster
proj <- "proj_nch"         # Cluster project for launching jobs
m_mem_free <- 4                 # Gigabytes of memory per job launched (depends on forms_per_job)

######################################
############### LAUNCH ###############
######################################

launch_prior(me=me, decomp_step=decomp_step, crosswalk_version_id=crosswalk_version_id, test_mods=test_mods,
             average=average, n_mods=n_mods, plot_aves=plot_aves, plot_betas=plot_betas, age_trend=age_trend,
             cov_ids=cov_ids, prior_sign=prior_sign, ban_pairs=ban_pairs, polynoms=polynoms, modtype=modtype,
             rank_method=rank_method, forms_per_job=forms_per_job, drop_nids=FALSE, fixed_covs=fixed_covs,
             custom_covs=custom_covs, pred_ages = pred_ages, random_effects=random_effects, username=username, by_sex=by_sex)



# END












