
library(data.table)

rm(list = ls())

invisible(sapply(list.files("filepath", full.names = T), source))

'%ni%' <- Negate('%in%')

os <- .Platform$OS.type

if (os=="windows") {
  jpath<- "filepath"
  my_libs <- NULL
} else {
  jpath<- "filepath"
}

invisible(sapply(list.files("filepath", full.names = T), source))

location_ids = get_demographics(gbd_team = "epi", gbd_round_id = 7)$location_id




loc.met <- get_location_metadata(location_set_id = 35, decomp_step = 'iterative', gbd_round_id = 7)






  param_map <- get_demographics_template(gbd_team = "epi", gbd_round_id = 7)
  param_map <- param_map[age_group_id %in% c(2, 3, 388, 389, 238, 34)]
  param_map <- param_map[year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)]
  param_map <- param_map[sex_id %in% c(1, 2)]
  
  param_map <- expand.grid(age_group_id = c(2, 3, 388, 389, 238, 34),
                           year_id = c(1990, 1995, 2000, 2005, 2010, 2015, 2020),
                           location_id = location_ids,
                           sex_id = c(1,2))
  param_map = data.table(param_map)
  
  


param_map[, gbd_round_id := 7]
param_map[, thresh_save_dir := "filepath"]
param_map[, paf_save_dir := "filepath"]
param_map[, launch.date := "even_run"]
weight_version_vector <- c("thresholds_5_sbplx")








param_map <- fread("filepath")



for(v in c("HAZ", "WAZ", "WHZ")){
  
  param_map[, var := v]
  param_map_filepath <- paste0("filepath")
  write.csv(param_map, param_map_filepath, row.names = F, na = "")
  
  ## QSUB Command
  job_name <- paste0(v, "_ens")
  thread_flag <- "-l fthread=1"
  mem_flag <- "-l m_mem_free=2G"
  runtime_flag <- "-l h_rt=3:00:00"
  queue_flag <- "-q all.q"
  throttle_flag <- "-tc 4000"
  n_jobs <- paste0("1:", nrow(param_map))
  prev_job <- "nojobholds"
  next_script <- "filepath"
  error_filepath <- paste0("filepath")
  output_filepath <- paste0("filepath")
  project_flag<- paste0("-P ", "proj_nch")
  
  qsub_command <- paste( "qsub", thread_flag, "-N", job_name, project_flag, mem_flag, runtime_flag, throttle_flag, queue_flag, "-t", n_jobs, "-e", error_filepath, "-o", output_filepath, "-hold_jid", prev_job,  "/ihme/singularity-images/rstudio/shells/execRscript.sh -s", next_script, param_map_filepath )
  
  system(qsub_command)
  
}




