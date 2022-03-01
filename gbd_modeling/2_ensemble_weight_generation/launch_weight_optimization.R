
Sys.umask(mode = 002)

os <- .Platform$OS.type

library(data.table)
library(magrittr)
library(ggplot2)
library(dplyr)
library(stats)





# --------------------------------------------------------------------------------------------------------------------
#Set up parameters in param_map

data_filepath = "filepath"



me_type = "HAZ"
num_of_initial_conditions = 100
max.time = 22 # in hours
maxeval = 5000
optim_type = "sbplx" # either sbplx, nmkb, or nlminb
delete_iteration_csvs = TRUE
run_m1 = FALSE
run_m2 = TRUE


#### What are you actually optimizing? ####
#format: vector of format c("tails_#", "thresholds_#")

option = c("thresholds_5")


# METHODS:
# 1) You are looking at the largest difference between the model and the data at any point along the distribution 
# 2) You are looking at the largest difference between the model and the da ta at any point along the left side of the distribution 
# 3) You are looking for the biggest difference at one of the three tails/thresholds, whichever one of the three that has the biggest difference is chosen
# 4) You are adding a weighted sum of the differences between data and model at the three thresholds/tails
# 5) You are adding a weighted sum of the SQUARED differences between data and model at the three thresholds/tails


# Enter the actual threshold
low_threshold <<- -3
medium_threshold <<- -2
high_threshold <<- -1

# Enter the actual tails
low_tail <<- .03
middle_tail <<- .13
high_tail <<- .25

# if method is 4 or 5, enter weights for the weighted sum
# low_weight refers to the furthest negative threshold/smallest proportion
low_weight <<- (.3)
medium_weight <<- (.3)
high_weight <<- (.3)

launch.date <- "even.weights" #really can be anything you want to use to uniquely denote this launch
# --------------------------------------------------------------------------------------------------------------------



# ----- Make Param Map

metadata <- expand.grid(data_filepath = data_filepath, 
                        me_type = me_type,
                        optim_type = optim_type,
                        initial_condition = 1:num_of_initial_conditions,
                        maxeval = maxeval,
                        delete_iteration_csvs = delete_iteration_csvs,
                        option = option, 
                        low_threshold = low_threshold,
                        medium_threshold = medium_threshold,
                        high_threshold = high_threshold,
                        low_tail = low_tail,
                        middle_tail = middle_tail,
                        high_tail = high_tail,
                        low_weight = low_weight,
                        medium_weight = medium_weight,
                        high_weight = high_weight,
                        launch.date = launch.date,
                        max.time = max.time)

if(run_m1 == FALSE){
  
  param_map_m1 <- cbind(metadata, strategy = "m1")
  
  nid_loc_yr_i <- unique(readRDS(data_filepath)$nid_loc_yr_index)
  m1_params <- data.table(strategy = "m1", nid_loc_yr_i = nid_loc_yr_i)
  param_map_m1 <- merge(param_map_m1, m1_params, allow.cartesian = T)
  
  if (length(option) == 1) {
    param_map_m1_fp = paste0("filepath")
  } 
  
  if (length(option) != 1) {
    param_map_m1_fp = file.path("filepath")
  }
  write.csv(param_map_m1, param_map_m1_fp)
  
  ## QSUB Command
  job_name <- paste0(me_type, "_m1_", optim_type)
  thread_flag <- "-l fthread=1"
  mem_flag <- "-l m_mem_free=1G"
  runtime_flag <- "-l h_rt=2:00:00"
  jdrive_flag <- "-l archive"
  queue_flag <- "-q all.q"
  throttle_flag <- "-tc 800"
  n_jobs <- paste0("1:", nrow(param_map_m1))
  #n_jobs <- "1:1"
  prev_job <- "nojobholds"
  next_script <- paste0("filepath")
  error_filepath <- paste0("filepath")
  output_filepath <- paste0("filepath")
  project_flag<- "-P proj_htwt"
  
  # add jdrive_flag if needed
  qsub_command <- paste( "qsub", thread_flag, "-N", job_name, project_flag, mem_flag, runtime_flag, throttle_flag, queue_flag, "-t", n_jobs, "-e", error_filepath, "-o", output_filepath, "-hold_jid", prev_job,  "/ihme/singularity-images/rstudio/shells/execRscript.sh -i /ihme/singularity-images/rstudio/ihme_rstudio_3601.img -s", next_script, param_map_m1_fp )
  
  system(qsub_command)
  
  
  
}

if(run_m2 == TRUE){
  
  
  param_map_m2 <- cbind(metadata, strategy = "m2")
  
  if (length(option) == 1) {
    param_map_m2_fp = paste0("filepath")  
  }
  
  if (length(option) > 1) {
    param_map_m2_fp = file.path("filepath")
  }
  
  write.csv(param_map_m2, param_map_m2_fp )
  
  
  ## QSUB Command
  job_name <- paste0(me_type, "_m2_", optim_type)
  thread_flag <- "-l fthread=3"
  mem_flag <- "-l m_mem_free=4G"
  runtime_flag <- "-l h_rt=48:00:00"
  jdrive_flag <- "-l archive"
  queue_flag <- "-q long.q"
  throttle_flag <- "-tc 800"
  n_jobs <- paste0("1:", nrow(param_map_m2))
  #n_jobs <- "1:1"
  prev_job <- "nojobholds"
  next_script <- paste0("filepath")
  error_filepath <- paste0("filepath")
  output_filepath <- paste0("filepath")
  project_flag<- "-P proj_htwt"
  
  # add jdrive_flag if needed
  qsub_command <- paste( "qsub", thread_flag, "-N", job_name, project_flag, mem_flag, runtime_flag, throttle_flag, queue_flag, "-t", n_jobs, "-e", error_filepath, "-o", output_filepath, "-hold_jid", prev_job,  "/ihme/singularity-images/rstudio/shells/execRscript.sh -i /ihme/singularity-images/rstudio/ihme_rstudio_3601.img -s", next_script, param_map_m2_fp )
  
  system(qsub_command)
  
  
}






