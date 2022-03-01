# DATA PROCESSING

### Overview of each script:

##### readin_format_upload_data.R
This script reads in spreadsheets that include extracted data from tabulation and microdata sources. It makes minor adjustments that may need to be made such as assigning year_ids to surveys that span multiple years (such that a survey occuring from 2010-2012 would be assigned a year_id of 2011) for modeling purposes. It may also adjust for spelling mistakes in the initial extraction sheet. After minor (mostly formatting) adjustments are made, the data is uploaded to databases. From here, age-sex specific data that matches perfectly with a GBD age group is pulled from that database and stored separately, so that age-sex specific models can be launched and inform the age-sex splitting adjustments to data points that may report data in an age-sex aggregated fashion. Of note, this script also has pieces that exit to adjust GBD2019 data, as new age groups were added for GBD 2020 along with a few new subnational locations.

##### launch_age_sex_specific_stgpr.R
This script uses the datasets that *only* include age-sex specific data that perfectly match up with a GBD age group and launches ST-GPR models. It calls scripts that perform the ST-GPR model, which are found in folder number 3 in the above directory and are not duplicated here.

##### launch_age_sex_split.R
This script launches parallel_age_sex_split.R, allowing for each dataset to be split synchronously.

##### parallel_age_sex_split.R
This script performs the age_sex_splitting process, calling in age-sex specific model results from the ST-GPR models run above to inform the disaggregation of non-age-sex specific datapoints.

##### age_sex_split_helper_functions.R
This script is sourced in parallel_age_sex_split.R and includes functions that assist in the age-sex splitting process.