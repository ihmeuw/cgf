# CUSTOM ST-GPR STAGE 1 PRIORS

### Overview of each script

##### launch_priors.R

This script launches priors.parallel.R which runs in parallel so that stage 1 priors for multiple datasets can be calculated synchronously.

##### priors.parallel.R

This script takes inputs and specifies other settings before ultimately running the launch_prior function which is sourced from run_priors.R.

##### run_priors.R.

This script sources other scripts in this repo, but is the main function that actually generates and evaluates the options for stage 1 models.

##### make_KOs.R 

This script sets functions that will "knock out" or temporarily erase some of the data, so that some data is used as a training set and the predictiveness can be assessed from this "knocked out" data.

##### bind_covariates.R

This script formats all input covariates so that they are ready to be incorporated into polynomials that will be used as stage 1 priors.

##### assemble_priors.R

This script takes all the covariates entered into the model and creates a list of all possible model combinations using those covariates.

##### test_priors.R

This script evaluates all models that do not violate other specificied requirements to see how well it predicts the "knocked out" data.

##### helper_functions.R

This script includes functions that are used throughout the process, including functions that flag models which violate specificied requirements and functions that format inputs and outputs.

##### db_tools.R

This script includes functions that are used to interact with the GBD databases.










