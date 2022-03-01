

Sys.umask(mode = 002)

os <- .Platform$OS.type
my_libs <- file.path("filepath")

library(data.table)
library(magrittr)
library(ggplot2)

# ----- Set directories & locals

step = "iterative"

repo = file.path("filepath")
ids_filepath = "filepath"
out_dir = file.path("filepath")
monitor_dir = file.path(out_dir, "filepath")

model.index.id = 63
model.note = "Age-Sex Specific Run Used to Split Non-Age-Sex Specific Data"
data.note = "Age-Sex Specific Run Used to Split Non-Age-Sex Specific Data"
update_latest_runids = 1



# ----- Launch STGPR models
'%notlike%' <- Negate('%like%')
param_map <- fread(ids_filepath)
param_map[, model_index_id := model.index.id]
param_map[, model_note := model.note]
param_map[, data_note := data.note]
param_map[, repo := repo]
param_map[, out_dir := out_dir]
param_map[, ids_filepath := ids_filepath]
param_map[, compare.run_id := 0] 

param_map <- param_map[me_name %in% c("stunting_1_agesex", "stunting_2_agesex", "stunting_3_agesex", "stunting_mean_agesex", 
                                      "underweight_1_agesex", "underweight_2_agesex", "underweight_3_agesex", "underweight_mean_agesex",
                                      "wasting_1_agesex", "wasting_2_agesex", "wasting_3_agesex", "wasting_mean_agesex")]

param_map_filepath <- file.path(out_dir, "filepath")
write.csv(param_map, param_map_filepath, row.names = F, na = "")

## QSUB Command
job_name <- "cgf_stgpr"
thread_flag <- "-l fthread=1"
mem_flag <- "-l m_mem_free=3G"
runtime_flag <- "-l h_rt=8:00:00"
queue_flag <- "-q all.q"
archive_flag <- "-l archive"
n_jobs <- paste0("1:", nrow(param_map))
#n_jobs = "1"
prev_job <- "nojobholds"
next_script <- file.path("filepath")
error_filepath <- paste0("filepath")
output_filepath <- paste0("filepath")

dir.create(error_filepath, recursive = T)
dir.create(output_filepath)

project_flag<- paste0("-P ", "proj_nch")

qsub_command <- paste( "qsub", thread_flag, "-N", job_name, project_flag, mem_flag, runtime_flag, queue_flag, archive_flag, "-t", n_jobs, "-e", error_filepath, "-o", output_filepath, "-hold_jid", prev_job,  "/ihme/singularity-images/rstudio/shells/execRscript.sh -s", next_script, param_map_filepath )

system(qsub_command)




