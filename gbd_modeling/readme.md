# GLOBAL BURDEN OF DISEASE (GBD) MODELING

#### 1. Data Processing

Includes the scripts used in reading in data, storing it in databases, and age-sex splitting data that does not align with a GBD age group. 

#### 2. Ensemble Weight Generation

Includes the scripts used in fitting an ensemble distribution that is weighted to best reflect the shape of HAZ, WHZ, and WAZ curves.

#### 3. Spatiotemporal Gaussian Process Regression (ST-GPR)

Includes the scripts used in running ST-GPR models to get estimates of stunting, wasting, and underweight prevalence for all years, sexes, locations, and age groups.

#### 4. Variance Optimization

Includes the scripts used in aligning the characteristic shape of HAZ, WHZ, and WHZ curves with the regression output from ST-GPR by identifying an optimal spread of the curve.