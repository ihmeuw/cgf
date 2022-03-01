

# load functions and libraries

invisible(sapply(list.files("filepath", full.names = T), source))

library(data.table)
library(ggplot2)
library(dplyr)
library(scales)
library('cowplot', lib.loc = "filepath")
library(gridExtra)
library("ggridges", lib.loc = "filepath")
"%ni%" <- Negate("%in%")

# loading populations

populations <- get_population(gbd_round_id = 7, decomp_step = 'iterative', location_id = 'all', location_set_id = 35, 
                              year_id = c(1990:2020), age_group_id = c(1, 2, 3, 388, 389, 238, 34), sex_id = c(1, 2, 3))
populations$run_id <- NULL


# setting id vars to use in melts for this section

id.vars <- c("metric_id", "age_group_id", "location_id", "measure_id", "modelable_entity_id", "sex_id", "year_id", "model_version_id")



################################################
# 2020 Prevalences and Counts
################################################



cgf.id.names <- data.table(idval = c(10556, 8949, 26941, 10558, 8945, 26943, 10560, 2540, 26942),
                           name = c("Overall Stunting", "Severe Stunting", "Extreme Stunting",
                                    "Overall Wasting", "Severe Wasting", "Extreme Wasting",
                                    "Overall Underweight", "Severe Underweight", "Extreme Underweight"))

# Get global prevalence and count (with uncertainty) for all CGF types and severities in 2020

for (meid in unique(cgf.id.names$idval)) {
  
  
  
  global.prevalence <- get_draws("modelable_entity_id", meid, year_id=c(2020), 
                                 source="epi", gbd_round_id=7, decomp_step="iterative", 
                                 age_group_id = 1, sex_id = c(3), location_id = 1)
  
  global.prevalence <- melt(global.prevalence, id.vars = id.vars)
  global.prevalence <- merge(global.prevalence, populations)
  
  global.prevalence[, cgf.count := value * population]
  
  global.count.mean <- mean(global.prevalence$cgf.count)
  global.count.lower <- quantile(global.prevalence$cgf.count, probs = .025)
  global.count.upper <- quantile(global.prevalence$cgf.count, probs = .975)
  
  global.prev.mean <- mean(global.prevalence$value)
  global.prev.lower <- quantile(global.prevalence$value, probs = .025)
  global.prev.upper <- quantile(global.prevalence$value, probs = .975)
  
  
  name <- cgf.id.names[idval == meid]$name
  
  if(global.count.mean > 100000000){
    mean.magnitude <- "Million"
    mean.divisor <- 1000000
    signif.count <- 4
  } else if(global.count.mean > 1000000){
    mean.magnitude <- "Million"
    mean.divisor <- 1000000
    signif.count <- 3
  } else if(global.count.mean < 1000000){
    mean.magnitude <- "Thousand"
    mean.divisor <- 1000
    signif.count <- 3
  }
  
  if(global.count.lower > 100000000){
    lower.magnitude <- "Million"
    lower.divisor <- 1000000
    signif.count <- 4
  } else if(global.count.lower > 1000000){
    lower.magnitude <- "Million"
    lower.divisor <- 1000000
    signif.count <- 3
  } else if(global.count.lower < 1000000){
    lower.magnitude <- "Thousand"
    lower.divisor <- 1000
    signif.count <- 3
  }
  
  if(global.count.upper > 100000000){
    upper.magnitude <- "Million"
    upper.divisor <- 1000000
    signif.count <- 4
  } else if(global.count.upper > 1000000){
    upper.magnitude <- "Million"
    upper.divisor <- 1000000
    signif.count <- 3
  } else if(global.count.upper < 1000000){
    upper.magnitude <- "Thousand"
    upper.divisor <- 1000
    signif.count <- 3
  }
  
  print(paste0("Global ", name, " Prevalence in 2020: ", signif(global.prev.mean*100, 3), "% (", signif(global.prev.lower*100, 3), "% - ", signif(global.prev.upper*100, 3), "%)."))
  print(paste0("Global ", name, " Count in 2020: ", signif(global.count.mean/mean.divisor, signif.count), " ", mean.magnitude, " (", 
               signif(global.count.lower/lower.divisor, signif.count), " ", lower.magnitude, " - ", signif(global.count.upper/upper.divisor,signif.count), " ", upper.magnitude, ")."))
  
  
  
}




