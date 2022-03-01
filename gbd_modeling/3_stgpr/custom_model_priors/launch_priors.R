Sys.umask(mode = 002)

os <- .Platform$OS.type
my_libs <- file.path("filepath")

library(data.table)
library(magrittr)
library(ggplot2)

step = "iterative"
group = "group4"

## First create param_map
param_map <- fread("filepath")
param_map <- param_map[!is.na(stgpr_me_id) & !me_name %like% 'agesex']
param_map[, step := step]

param_map <- merge(param_map, data.table(step = step, group = group, custom_covs = TRUE), by = "step")

param_map <- param_map[me_name %notlike% "agesex"]

param_map_filepath <- file.path("filepath")
write.csv(param_map, param_map_filepath, row.names = F, na = "")



## QSUB Command
job_name <- "CGFensstg1"
thread_flag <- "-l fthread=5"
mem_flag <- "-l m_mem_free=5G"
runtime_flag <- "-l h_rt=6:00:00"
jdrive_flag <- "-l archive"
queue_flag <- "-q all.q"
n_jobs <- paste0("1:", nrow(param_map))
prev_job <- "nojobholds"
next_script <- file.path("filepath")
error_filepath <- paste0("filepath")
output_filepath <- paste0("filepath")
project_flag<- "-P proj_neonatal"

# add jdrive_flag if needed
qsub_command <- paste( "qsub", thread_flag, "-N", job_name, project_flag, mem_flag, runtime_flag, jdrive_flag, queue_flag, "-t", n_jobs, "-e", error_filepath, "-o", output_filepath, "-hold_jid", prev_job,  "/ihme/singularity-images/rstudio/shells/execRscript.sh -i /ihme/singularity-images/rstudio/ihme_rstudio_3601.img -s", next_script, param_map_filepath )

system(qsub_command)




