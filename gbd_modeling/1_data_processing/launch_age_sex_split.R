
library(data.table)



param <- fread("filepath")


bundle.ids <- unique(param[me_name %like% "agesex"]$stgpr_bundle_id)


param <- data.table(stgpr_bundle_id = bundle.ids)




param_map_filepath <- "filepath"

write.csv(param, param_map_filepath, row.names = F)

## QSUB Command
job_name <- "agesexsplit"
thread_flag <- "-l fthread=1"
mem_flag <- "-l m_mem_free=3G"
runtime_flag <- "-l h_rt=48:00:00"
jdrive_flag <- "-l archive"
queue_flag <- "-q long.q"
throttle_flag <- "-tc 1050"
n_jobs <- paste0("1:", nrow(param))
#n_jobs <- "1:1"
prev_job <- "nojobholds"
next_script <- "filepath"
error_filepath <- paste0("filepath")
output_filepath <- paste0("filepath")
project_flag<- "-P proj_nch"
# add jdrive_flag if needed
qsub_command <- paste( "qsub", thread_flag, "-N", job_name, project_flag, mem_flag, runtime_flag, throttle_flag, queue_flag, "-t", n_jobs, "-e", error_filepath, "-o", output_filepath, "-hold_jid", prev_job,  "/ihme/singularity-images/rstudio/shells/execRscript.sh -i /ihme/singularity-images/rstudio/ihme_rstudio_3601.img -s", next_script, param_map_filepath )
system(qsub_command)



















