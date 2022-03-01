
library(data.table)

param <- data.table(cgf.type.name = c("Stunting",
                                      "Wasting",
                                      "Underweight"),
                    cgf.type.letters = c("HAZ", 
                                         "WHZ", 
                                         "WAZ"),
                    mod.id = c(163046,
                               163037,
                               163022),
                    sev.id = c(163049,
                               163025,
                               163040),
                    mean.id = c(163043,
                                163034,
                                163031),
                    mod.cross = c(31229,
                                  31250,
                                  31241),
                    sev.cross = c(31232,
                                  31253,
                                  31259),
                    mean.cross = c(31235,
                                   31256,
                                   31244))











param_map_filepath <- "filepath"

write.csv(param, param_map_filepath, row.names = F)

## QSUB Command
job_name <- "appendix_B"
thread_flag <- "-l fthread=2"
mem_flag <- "-l m_mem_free=15G"
runtime_flag <- "-l h_rt=48:00:00"
jdrive_flag <- "-l archive"
queue_flag <- "-q long.q"
throttle_flag <- "-tc 600"
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


