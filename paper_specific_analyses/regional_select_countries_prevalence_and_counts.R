
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








################################################################################################################
# 2020 Prevalences of CGF 
################################################################################################################


# Stunting: Highlighting Global, South Asia, sub-Sahran Africa, and Southeast Asia
################################################################################################################

stunting.locs <- data.table(location_id = c(1, 158, 166, 9),
                            location_name = c("Global", "South Asia", "Sub-Saharan Africa", "Southeast Asia"))

exposure.cgf.stunting <- get_draws("modelable_entity_id", 10556, year_id=c(2020), 
                                   source="epi", gbd_round_id=7, decomp_step="iterative", 
                                   age_group_id = 1, sex_id = c(3), location_id = stunting.locs$location_id)


exposure.cgf.stunting <- melt(exposure.cgf.stunting, id.vars = id.vars)
exposure.cgf.stunting <- merge(exposure.cgf.stunting, populations)




for (loc.id in unique(stunting.locs$location_id)) {
  
  stunting.2020.mean <- mean(exposure.cgf.stunting[location_id == loc.id]$value)
  stunting.2020.lower <- quantile(exposure.cgf.stunting[location_id == loc.id]$value, probs = .025)
  stunting.2020.upper <- quantile(exposure.cgf.stunting[location_id == loc.id]$value, probs = .975)
  
  print(paste0(stunting.locs[location_id == loc.id]$location_name, " stunting prevalence in 2020: ", signif(stunting.2020.mean*100,3), "% (",
               signif(stunting.2020.lower*100, 3), "% - ", signif(stunting.2020.upper*100, 3), "%)."))
  
}









# Wasting: Highlighting Global, South Asia, Southeast Asia, Sahel countries South Sudan and Chad
################################################################################################################

wasting.locs <- data.table(location_id = c(1, 158, 9, 435, 204, 163),
                           location_name = c("Global", "South Asia", "Southeast Asia", "South Sudan", "Chad", "India"))

exposure.cgf.wasting <- get_draws("modelable_entity_id", 10558, year_id=c(2020), 
                                  source="epi", gbd_round_id=7, decomp_step="iterative", 
                                  age_group_id = 1, sex_id = c(3), location_id = wasting.locs$location_id)


exposure.cgf.wasting <- melt(exposure.cgf.wasting, id.vars = id.vars)
exposure.cgf.wasting <- merge(exposure.cgf.wasting, populations)




for (loc.id in unique(wasting.locs[location_id != 163]$location_id)) {
  
  wasting.2020.mean <- mean(exposure.cgf.wasting[location_id == loc.id]$value)
  wasting.2020.lower <- quantile(exposure.cgf.wasting[location_id == loc.id]$value, probs = .025)
  wasting.2020.upper <- quantile(exposure.cgf.wasting[location_id == loc.id]$value, probs = .975)
  
  print(paste0(wasting.locs[location_id == loc.id]$location_name, " wasting prevalence in 2020: ", signif(wasting.2020.mean*100,3), "% (",
               signif(wasting.2020.lower*100, 3), "% - ", signif(wasting.2020.upper*100, 3), "%)."))
  
}


exposure.cgf.wasting[, wasting.count := value * population]
india.wasting.2020.mean <- mean(exposure.cgf.wasting[location_id == 163]$wasting.count)
india.wasting.2020.lower <- quantile(exposure.cgf.wasting[location_id == 163]$wasting.count, probs = .025)
india.wasting.2020.upper <- quantile(exposure.cgf.wasting[location_id == 163]$wasting.count, probs = .975)

print(paste0("In 2020 in India, ", signif(india.wasting.2020.mean/1000000, 3), " Million (", signif(india.wasting.2020.lower/1000000, 3), " Million - ",
             signif(india.wasting.2020.upper/1000000, 3), " Million) children experienced wasting."))













# Underweight: Highlighting Global, South Asia, Southeast Asia, and sub-Saharal Africa
################################################################################################################

underweight.locs <- data.table(location_id = c(1, 158, 9, 166),
                               location_name = c("Global", "South Asia", "Southeast Asia", "Sub-Saharan Africa"))



exposure.cgf.underweight <- get_draws("modelable_entity_id", 10560, year_id=c(2020), 
                                      source="epi", gbd_round_id=7, decomp_step="iterative", 
                                      age_group_id = 1, sex_id = c(3), location_id = c(underweight.locs$location_id))

exposure.cgf.underweight <- melt(exposure.cgf.underweight, id.vars = id.vars)
exposure.cgf.underweight <- merge(exposure.cgf.underweight, populations)





for (loc.id in unique(underweight.locs$location_id)) {
  
  underweight.2020.mean <- mean(exposure.cgf.underweight[location_id == loc.id]$value)
  underweight.2020.lower <- quantile(exposure.cgf.underweight[location_id == loc.id]$value, probs = .025)
  underweight.2020.upper <- quantile(exposure.cgf.underweight[location_id == loc.id]$value, probs = .975)
  
  print(paste0(underweight.locs[location_id == loc.id]$location_name, " underweight prevalence in 2020: ", signif(underweight.2020.mean*100,3), "% (",
               signif(underweight.2020.lower*100, 3), "% - ", signif(underweight.2020.upper*100, 3), "%)."))
  
}








