
rm(list=ls())

os <- .Platform$OS.type
my_libs <- "filepath"

library(data.table)
library(openxlsx)

central_root <- 'filepath'
setwd(central_root)

source('filepath')


args <- commandArgs(trailingOnly = TRUE)
param_map_filepath <- args[1]

## Retrieving array task_id
task_id <- as.integer(Sys.getenv("SGE_TASK_ID"))
param_map <- fread(param_map_filepath)

me.name <- param_map[task_id, me_name]
model.index.id <- param_map[task_id, model_index_id]
repo <- param_map[task_id, repo]
out_dir <- param_map[task_id, out_dir]
ids_filepath <- param_map[task_id, ids_filepath]
compare.run_id <- param_map[task_id, compare.run_id]

monitor_dir = file.path(out_dir, "filepath")
config.path = file.path(out_dir, paste0("config/", me.name, ".csv"))
log.file.path = file.path(out_dir, paste0("logs/", me.name, "_log_file.csv"))

ids <- fread(ids_filepath)

# Registration
run_id <- register_stgpr_model(path_to_config = config.path, 
                               model_index_id = model.index.id)


if(is.na(run_id) == F){
  
  write.csv(data.table(run_id = run_id, model_index_id = model.index.id), paste0(monitor_dir, me.name, "_runid_registered_", model.index.id, "_", run_id, ".csv"), row.names = F, na ="")
  
  log_file <- fread(log.file.path)
  
  config_file <- fread(config.path)
  config_file <- config_file[model_index_id == model.index.id, ]
  
  new_log_row <- data.table(model_index_id = model.index.id, 
                            run_id = run_id, 
                            date = date())
  
  new_log_row <- merge(new_log_row, config_file, by = intersect(names(new_log_row), names(config_file)))
  
  log_file <- rbindlist(list(log_file, new_log_row), use.names = T, fill = T)
  write.csv(log_file, log.file.path, row.names = F, na = "")
  
  
  
  stgpr_sendoff(run_id, "proj_neonatal", nparallel = 100)
  
  if(as.character(compare.run_id) == "default"){
    compare.run_id <- ids[me_name == me.name, gbd_2019_decomp4_run_id]  
  }
  
  # Graphical yaxis settings
  min_yaxis <- config_file[, min_yaxis]
  max_yaxis <- config_file[, max_yaxis]
  
  # set job hold for plotting stgpr completion 
  thread_flag <- "-l fthread=1"
  mem_flag <- "-l m_mem_free=5G"
  runtime_flag <- "-l h_rt=12:00:00"
  jdrive_flag <- "-l archive"
  queue_flag <- "-q all.q"
  prev_job <- paste0("MASTER_", run_id)
  error_filepath <- paste0("filepath")
  output_filepath <- paste0("filepath")
  project_flag<- paste0("-P ", "proj_neonatal")
  
  
} else{
  write.csv(data.table(), paste0(monitor_dir, me.name, "_runid_registration_failed_", model.index.id, "_", run_id, ".csv"), row.names = F, na ="")
}

Sys.sleep(20)

# Hold job open until STGPR models complete
while(length(system(paste0("qstat | grep MASTER_", substr(run_id, 1, 3)), intern = T)) > 0){  Sys.sleep(5)  }

Sys.sleep(200)

while(length(system(paste0("qstat | grep MASTER_", substr(run_id, 1, 3)), intern = T)) > 0){  Sys.sleep(5)  }

# Check successful model completion 
if(file.exists(paste0("filepath"))){
  write.csv(data.table(me = me.name, run_id = run_id), paste0(monitor_dir, me.name, "_stgpr_model_completed_", model.index.id, "_", run_id, ".csv"), row.names = F, na ="")
} else{
  
  write.csv(data.table(), paste0(monitor_dir, me.name, "_stgpr_model_failed_", model.index.id, "_", run_id, ".csv"), row.names = F, na ="")
  stgpr_sendoff(run_id, "proj_neonatal", nparallel = 100)
  write.csv(data.table(), paste0(monitor_dir, me.name, "_stgpr_model_resubmitted_", model.index.id, "_", run_id, ".csv"), row.names = F, na ="")
  
  # Second Try
  while(length(system(paste0("qstat | grep MASTER_", run_id), intern = T)) > 0){  Sys.sleep(5)  }
  if(file.exists(paste0("filepath"))){
    write.csv(data.table(me = me.name, run_id = run_id), paste0(monitor_dir, me.name, "_stgpr_model_completed_", model.index.id, "_", run_id, ".csv"), row.names = F, na ="")
  } else{
    write.csv(data.table(), paste0(monitor_dir, me.name, "_resubmitted_stgpr_model_failed_again_", model.index.id, "_", run_id, ".csv"), row.names = F, na ="")
  }
  
}


