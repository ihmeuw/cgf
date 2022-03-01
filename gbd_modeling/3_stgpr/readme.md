# ST-GPR (Spatiotemporal Gaussian Process Regression)

### Overview of each script

##### Custom Model Priors Folder

This folder contains the scripts used to launch the custom stage 1 prior used in ST-GPR. These are launched prior to ST-GPR and incorporated to the model by being used as the priors for later components of the ST-GPR pipeline.

##### launch_alldata_stgpr.R

This script launches stgpr models in parallel such that HAZ, WHZ, and WAZ along with each severity can be run simultaneously.

##### parallel_stgpr.R

This script runs ST-GPR models. It incorporates the custom stage 1 prior and also sources scripts central to the ST-GPR process.


*For more information about ST-GPR, refer to the [GBD 2019 Risk Factor Capstone](https://doi.org/10.1016/S0140-6736(20)30752-2). Details can be found in Appendix 1, section 3.3.3.*

