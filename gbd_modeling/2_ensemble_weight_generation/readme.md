# ENSEMBLE WEIGHT GENERATION

### Overview of each script

##### launch_weight_optimization.R

This script launches weight_optimization_parallel.R where each individual task corresponds to one initial condition of weights.

##### weight_optimization_parallel.R

This script takes an initial set of distribution weights, asseses their performance compared to the microdata specified, and then stores those weights along with the goodness of fit statistic for each set of weights.

##### weight_optimization_helper_functions.R

This script contains functions that are used in the optimization function contained in weight_optimization_parallel.R, as well as other processing functions used to correctly format output.

##### distribution_families.R

This script paramaterizes distribution families used in the ensemble distribution according to the 'methods of moments' approach.

##### process_weight_output.R

This script reads in all output sets of ensemble distribution weights along with goodness of fit statistics, identifies the set of weights that performed best, and saves that set of weights.