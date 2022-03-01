# VARIANCE OPTIMIZATION

### Overview of each script

##### launch_variance_optimization.R

This script launches variance_optimization_parallel.R so that each of HAZ, WHZ, and WAZ are run synchronously for each age/sex/year/location.

##### variance_optimization_parallel.R

This script takes in the ST-GPR output as well as the ensemble weights and runs an optimization function that calculates the best variance to align that curve shape with those ST-GPR results. It then integrates the area under that curve to arrive at final estimates of CGF prevalences.