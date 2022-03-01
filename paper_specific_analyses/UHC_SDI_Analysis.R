# CGF Paper Epidemiological Transisition Analysis with SDI as a covariate



#------------------SET-UP--------------------------------------------------

# runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "filepath"
  h_root <- "~/"
  central_lib <- "filepath"
} else {
  j_root <- "filepath"
  h_root <- "filepath"
  central_lib <- "filepath"
}


custom.col.sr <- c("Central Europe, Eastern Europe, and Central Asia" = "#771155",
                   "High-income" = "#117777",
                   "Latin America and Caribbean" = "#771122",
                   "North Africa and Middle East" = "#777711", 
                   "South Asia" = "#000080",
                   "Southeast Asia, East Asia, and Oceania" = "#114477", 
                   "Sub-Saharan Africa" = "#774411")


custom.col.r <- c("Central Asia" = "#771155",
                  "Central Europe" = "#AA4488",
                  "Eastern Europe" = "#CC99BB",
                  "Australasia" = "#117777",
                  "High-income Asia Pacific" = "#44AAAA",
                  "High-income North America" = "#77CCCC",
                  "Southern Latin America" = "#117744",
                  "Western Europe" = "#44AA77",
                  "Andean Latin America" = "#771122", # red
                  "Caribbean" = "#993344",
                  "Central Latin America" = "#BB5566",
                  "Tropical Latin America" = "#DD7788",
                  "North Africa and Middle East" = "#777711", 
                  "South Asia" = "#000080", # 
                  "East Asia" = "#114477", 
                  "Oceania" = "#4477AA",
                  "Southeast Asia" = "#77AADD",
                  "Central Sub-Saharan Africa" = "#774411", #orange
                  "Eastern Sub-Saharan Africa" = "#996633",
                  "Southern Sub-Saharan Africa" = "#BB8855",
                  "Western Sub-Saharan Africa" = "#DDAA77")


# load packages, install if missing

library('reticulate', lib.loc = "filepath")
library(scales)
library(zoo)
library("ggnewscale", lib.loc = "filepath")
library(plyr)
library(dplyr)
library("cowplot", lib.loc = "filepath")
library(gridExtra)


packages <- c("data.table","magrittr","ggplot2")

for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  library(p, character.only = T)
}


library(mrbrt001, lib.loc = "filepath")

# Directories -------------------------------------------------------------

invisible(sapply(list.files("filepath", full.names = T), source))

age_weights <- get_age_weights(gbd_round_id = 7)
hierarchy <- get_location_metadata(location_set_id = 35, release_id = 9)

logit <- function(x){log(x/(1-x))}
invlogit <- function(x){exp(x)/(1+exp(x))}


prepare_df_format <- function(df){
  
  
  
  df[age_group_id == 2, age_group_name := "Early Neonatal"]
  df[age_group_id == 3, age_group_name := "Late Neonatal"]
  df[age_group_id == 388, age_group_name := "1-5 Months"]
  df[age_group_id == 389, age_group_name := "6-11 Months"]
  df[age_group_id == 238, age_group_name := "12-23 Months"]
  df[age_group_id == 34, age_group_name := "2-4 Years"]
  
  loc.meta <- standard.locs[, c("location_id", "location_name", "location_type")]
  
  df <- merge(df, loc.meta, by = "location_id")
  
  df[measure_id == 5, measure_name := "Prevalence"]
  df[metric_id == 3, metric_name := "Rate"]
  df[sex_id == 1, sex := "Male"]
  df[sex_id == 2, sex := "Female"]
  df[, lyas := paste0(location_id, "_", year_id, "_", age_group_id, "_", sex_id)]
  df[, val := mean(value), by = lyas]
  df[, lower:= quantile(value, probs = .025), by = lyas]
  df[, upper:= quantile(value, probs = .975), by = lyas]
  df <- df[variable == "draw_0"]
  df$variable <- NULL
  df$value <- NULL
  
  return(df)
  
  
  
}






# Read in  draws and calculate means --------------------------------
id.vars <- c("metric_id", "age_group_id", "location_id", "measure_id", "modelable_entity_id", "sex_id", "year_id", "model_version_id")
standard.locs <- get_location_metadata(location_set_id = 101, release_id = 9)
standard.locations <- unique(standard.locs$location_id)
all.locs <- get_location_metadata(release_id = 9, location_set_id = 35)
countries <- all.locs[level == 3]



###################################################################################################
###################################################################################################
####################################### OVERALL STUNTING ##########################################
###################################################################################################
###################################################################################################

out_dir <- file.path("filepath")


#reading in the dataset, writing out as csv so it's more easily accessible if needed later
mod_stunting_df <- get_draws("modelable_entity_id", 10556, year_id=c(1990:2020),
                             source="epi", gbd_round_id=7, decomp_step="iterative",
                             age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = standard.locations)

mod_stunting_df <- melt(mod_stunting_df, id.vars = id.vars)
mod_stunting_df <- prepare_df_format(df = mod_stunting_df)
write.csv(mod_stunting_df, "filepath", row.names = F)
mod_stunting_df <- fread("filepath")
mod_stunting_df[, metric_name := "Rate"]


# merge on uhc
uhc <- get_covariate_estimates(1097, gbd_round_id = 7, decomp_step = "iterative")

sdi <- get_covariate_estimates(881, gbd_round_id = 7, decomp_step = "iterative")

mod_stunting_df <- merge(mod_stunting_df, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))

mod_stunting_df <- merge(mod_stunting_df, sdi[,.(location_id, year_id, sdi = mean_value)], all.x=T, by = c("location_id", "year_id"))


mod_stunting_df <- merge(mod_stunting_df, hierarchy[, .(location_id, super_region_name)], all.x=T, by = "location_id")

mod_stunting_df$age_group_name <- factor(mod_stunting_df$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))

# run age-, sex-, measure-specific splines

out_dt <- data.table()

pdf(file.path(out_dir, "stunting_overall_spline_plots_sdi.pdf"), height = 7.5, width = 14)

for(mid in unique(mod_stunting_df$measure_id)){
  for(sid in unique(mod_stunting_df$sex_id)){
    for(agid in unique(mod_stunting_df$age_group_id)){
      
      data_dt <- mod_stunting_df[age_group_id == agid & sex_id == sid & measure_id == mid]
      
      
      # use offset before logit
      data_dt[val<1e-7, val := 1e-7]
      data_dt[val>(1-1e-7), val := (1-1e-7)]
      data_dt[, logit_val := logit(val)]
      
      # make SD standard across all locations
      data_dt[, se := 1]              
      
      n_knots=6 
      start=min(uhc$mean_value) 
      prior.start <- max(data_dt[val > 1e-7]$uhc)
      end = max(uhc$mean_value)
      
      
      
      min_dist=0.1
      
      knots_samples <- utils$sample_knots(
        num_intervals = 5L, # n_knots-1,
        knot_bounds = matrix(rep(c(start, end), each=n_knots-2), ncol=2),  # num_intervals-1 knot_bounds specified
        # num rows=num knots n_knots-2=num interior knots and repeat by num of iterior knot times; same for end
        # rbind(start,end repeated for x times internal knots)
        interval_sizes = rbind(c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.)),  # OR like: matrix(rep(c(min_dist, 1.), each=n_knots-1), ncol=2),  # num_intervals = interval_sizes specified
        # ^controls intervals between knots , interval length
        # all proportional to domain; maximum distance of knots is 1 follow same set-up logic as b
        num_samples = 20L # change to 20L once done testing
      )
      ensemble_cov_model1 <- LinearCovModel(
        alt_cov = "uhc",
        use_spline = TRUE,
        spline_knots = array(c(0,0.2, .4, .6, .8, 1)),  # number of specified splines needs to match n_knots; e.g. array(seq(0, 1, by = 0.25)); specifications OVERWRITTEN by ensemble splines specified when using MRBeRT!
        spline_degree = 2L, #3L
        spline_knots_type = 'domain',
        # prior_spline_monotonicity = "increasing",
        prior_spline_derval_gaussian = array(c(1e-7, 1e-7)),
        prior_spline_derval_gaussian_domain = array(c(prior.start, end)),
        #prior_spline_funval_uniform = array(c(1e-6, 1e-6)),
        #prior_spline_funval_uniform_domain = array(c(prior.start, end)),
        use_re = FALSE,
        use_spline_intercept = FALSE,
        spline_r_linear = TRUE,
        spline_l_linear = TRUE,
        prior_spline_monotonicity = "decreasing"
        ## HERE is where can add more priors and other features to splines!
      )
      
      
      
      dat_1 <- MRData()
      dat_1$load_df(
        data = data_dt,  col_obs = "logit_val", col_obs_se = "se",
        col_covs = list("uhc", "sdi"), col_study_id = "super_region_name"
      )
      
      
      
      
      model <- MRBeRT(data = dat_1,  # e for ensemble splines
                      ensemble_cov_model = ensemble_cov_model1,
                      ensemble_knots = knots_samples,
                      cov_models = list(LinearCovModel("intercept", use_re=FALSE),
                                        LinearCovModel("sdi")), inlier_pct = .99)
      
      model$fit_model(inner_max_iter = 30L) 
      
      
      
      
      
      pred_dt <- rbind(data_dt, data.table(uhc = c(0,1)), fill = T)
      
      pred_dt <- pred_dt[order(uhc)]
      
      pred_dt <- rbind(data.table(pred_dt, sdi=0.2),
                       data.table(pred_dt, sdi=0.4),
                       data.table(pred_dt, sdi=0.6),
                       data.table(pred_dt, sdi=0.8),
                       data.table(pred_dt, sdi=1))
      
      dat_pred <- MRData()
      
      dat_pred$load_df(
        data = pred_dt,
        col_covs=list("uhc", "sdi")
      )
      
      #pred_dt$expected <- model$predict(dat_pred) %>% invlogit
      pred_dt$expected <- invlogit(model$predict(dat_pred))
      
      
      out_dt <- rbind(out_dt, pred_dt[!is.na(location_id)])
      
      print(paste0("Done with sex_id ", sid, " age_group_id ", agid))
      
      
      knot.placement <- data.table(model$ensemble_knots, weights = model$weights)
      setnames(knot.placement, old = c("V1", "V2", "V3", "V4", "V5", "V6"), new = c("knot1", "knot2", "knot3", "knot4", "knot5", "knot6"))
      knot.placement[, knot1 := start]
      knot.placement[, knot6 := end]
      write.csv(knot.placement, paste0(out_dir, "knot_placement_", sid, "_", agid, "_sdi.csv"), row.names = F)
    }
    
    out_dt$age_group_name <- factor(out_dt$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))
    
    mod_stunting_plot_df <- copy(mod_stunting_df)
    
    mod_stunting_plot_df[val<1e-7, val := 1e-7]
    mod_stunting_plot_df[val>(1-1e-7), val := (1-1e-7)]
    mod_stunting_plot_df[, logit_val := logit(val)]
    
    
    gg <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=logit_val, x=uhc*100))+
      geom_point(data = mod_stunting_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = logit(expected), group = sdi))+
      theme_bw()+
      scale_color_manual(values = custom.col.sr)+
      labs(title = paste0("Overall Stunting (HAZ < -2 SD) ", unique(data_dt$measure_name), " among ", 
                          unique(data_dt$sex), "s, 1990-2020"),
           y = paste(unique(data_dt$measure_name), "(logit)"), 
           x = "Universal Health Coverage Index",
           color = "Super Region")+
      facet_wrap(~age_group_name, scales = 'free')+
      theme(legend.position="bottom", 
            legend.title.align = .5,
            legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90")) +
      guides(color = guide_legend(override.aes = list(alpha = .5), title.position = "top")) +
      #scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    
    gg2 <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=val, x=uhc*100))+
      geom_point(data = mod_stunting_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = expected, group = sdi))+
      theme_bw()+
      scale_color_manual(values = custom.col.sr)+
      labs(title = paste0("Overall Stunting (HAZ < -2 SD) ", unique(data_dt$measure_name), " among ", 
                          unique(data_dt$sex), "s, 1990-2020"),
           y = paste(unique(data_dt$measure_name)), 
           x = "Socio-demographic Index",
           color = "Super Region")+
      facet_wrap(~age_group_name, scales = 'free')+
      theme(legend.position="bottom", 
            legend.title.align = .5,
            legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90")) +
      guides(color = guide_legend(override.aes = list(alpha = .5), title.position = "top")) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    
    
    print(gg)
    print(gg2)
  }
}

dev.off()


write.csv(out_dt, file.path(out_dir, "observed_and_expected_sdi.csv"), row.names = F)

out_dt <- fread(file.path(out_dir, "observed_and_expected_sdi.csv"))



out_dt <- out_dt[, .(location_id, age_group_id, year_id, sex_id, measure_id, metric_id, expected, observed = val, measure_name, metric_name, sdi)]


# Create aggregates -------------------------------------------------------

# merge on populations

population <- get_population(gbd_round_id = 7, decomp_step = "iterative",
                             location_id = unique(out_dt$location_id), 
                             age_group_id = unique(out_dt$age_group_id),
                             sex_id = unique(out_dt$sex_id),
                             year_id = unique(out_dt$year_id))

population[, run_id := NULL]

out_dt <- merge(out_dt, population, by = c("location_id", "age_group_id", "year_id", "sex_id"), all.x=T)

# merge on age weights
out_dt <- merge(out_dt, age_weights, by = "age_group_id", all.x=T)

# Aggreagate up location hierarchy

for(loc in hierarchy[most_detailed == 0, location_id]){
  
  if(loc == 1){
    agg_dt <- copy(out_dt)
  }else{
    child_locs <- hierarchy[most_detailed == 1 & (parent_id == loc | grepl(paste0(",", loc, ","), path_to_top_parent)), location_id]
    
    agg_dt <- out_dt[location_id %in% child_locs] 
  }
  
  agg_dt <- agg_dt[, .(location_id = loc, expected = weighted.mean(expected, w = population), observed = weighted.mean(observed, w = population),
                       population = sum(population)),
                   by = c("age_group_id", "year_id", "sex_id", "measure_id", "metric_id", "measure_name", "metric_name", "age_group_weight_value", "sdi")]
  
  out_dt <- rbind(out_dt, agg_dt, use.names = T)
  
}


# Aggregate to desired age groups

ages <- list("Under 1" = c(2, 3, 388, 389),
             "Under 5" = c(2, 3, 388, 389, 238, 34),
             "15 - 49" = 8:14,
             "50 - 69" = 15:18,
             "70+" = c(19,20, 20:32, 235), 
             "All Ages" = unique(out_dt$age_group_id),
             "Age-standardized" = unique(out_dt$age_group_id))

sum_dt <- data.table()

for(i in 1:length(ages)){
  
  agg_dt <- out_dt[age_group_id %in% ages[[i]]]
  
  if(names(ages)[i]=="Age-standardized"){
    agg_dt[, weight := age_group_weight_value]
  }else{
    agg_dt[, weight := population]
  }
  
  agg_dt <- agg_dt[, .(expected = weighted.mean(expected, w = weight), observed = weighted.mean(observed, w = weight), age_group_name = names(ages)[i]),
                   by = c("location_id", "year_id", "measure_id", "metric_id", "measure_name", "metric_name", "sdi")]
  
  sum_dt <- rbind(sum_dt, agg_dt, use.names = T)
  
}


# merge on location names
sum_dt <- merge(sum_dt, hierarchy[, .(location_id, location_name, ihme_loc_id, level, super_region_name, region_name)], by = "location_id", all.x=T)

sum_dt[, ratio := observed/expected]

sum_dt <- merge(sum_dt, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))


write.csv(sum_dt, file.path(out_dir, "summary_results_sdi.csv"), row.names = F)



###################################################################################################
###################################################################################################
####################################### SEVERE STUNTING ###########################################
###################################################################################################
###################################################################################################


out_dir <- file.path("filepath")



#reading in the dataset, writing out as csv so it's more easily accessible if needed later

sev_stunting_df <- get_draws("modelable_entity_id", 8949, year_id=c(1990:2020),
                             source="epi", gbd_round_id=7, decomp_step="iterative",
                             age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = standard.locations)
# 
sev_stunting_df <- melt(sev_stunting_df, id.vars = id.vars)
sev_stunting_df <- prepare_df_format(df = sev_stunting_df)
write.csv(sev_stunting_df, "filepath", row.names = F)
sev_stunting_df <- fread("filepath")



# merge on uhc
uhc <- get_covariate_estimates(1097, gbd_round_id = 7, decomp_step = "iterative")

sdi <- get_covariate_estimates(881, gbd_round_id = 7, decomp_step = "iterative")



sev_stunting_df <- merge(sev_stunting_df, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))

sev_stunting_df <- merge(sev_stunting_df, sdi[,.(location_id, year_id, sdi = mean_value)], all.x=T, by = c("location_id", "year_id"))


sev_stunting_df <- merge(sev_stunting_df, hierarchy[, .(location_id, super_region_name)], all.x=T, by = "location_id")

sev_stunting_df$age_group_name <- factor(sev_stunting_df$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))

# run age-, sex-, measure-specific splines

out_dt <- data.table()

pdf(file.path(out_dir, "stunting_severe_spline_plots_sdi.pdf"), height = 7.5, width = 14)

for(mid in unique(sev_stunting_df$measure_id)){
  for(sid in unique(sev_stunting_df$sex_id)){
    for(agid in unique(sev_stunting_df$age_group_id)){
      
      data_dt <- sev_stunting_df[age_group_id == agid & sex_id == sid & measure_id == mid]
      
      
      # use offset before logit
      data_dt[val<1e-7, val := 1e-7]
      data_dt[val>(1-1e-7), val := (1-1e-7)]
      data_dt[, logit_val := logit(val)]
      
      # make SD standard across all locations
      data_dt[, se := 1]
      
      
      
      n_knots=6 
      start=min(uhc$mean_value) 
      prior.start <- max(data_dt[val > 1e-7]$uhc)
      end = max(uhc$mean_value)
      
      
      
      min_dist=0.1
      
      knots_samples <- utils$sample_knots(
        num_intervals = 5L, # n_knots-1,
        knot_bounds = matrix(rep(c(start, end), each=n_knots-2), ncol=2),  # num_intervals-1 knot_bounds specified
        # num rows=num knots n_knots-2=num interior knots and repeat by num of iterior knot times; same for end
        # rbind(start,end repeated for x times internal knots)
        interval_sizes = rbind(c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.)),  # OR like: matrix(rep(c(min_dist, 1.), each=n_knots-1), ncol=2),  # num_intervals = interval_sizes specified
        # ^controls intervals between knots , interval length
        # all proportional to domain; maximum distance of knots is 1 follow same set-up logic as b
        num_samples = 20L # change to 20L once done testing
      )
      ensemble_cov_model1 <- LinearCovModel(
        alt_cov = "uhc",
        use_spline = TRUE,
        spline_knots = array(c(0,0.2, .4, .6, .8, 1)),  # number of specified splines needs to match n_knots; e.g. array(seq(0, 1, by = 0.25)); specifications OVERWRITTEN by ensemble splines specified when using MRBeRT!
        spline_degree = 2L, #3L
        spline_knots_type = 'domain',
        # prior_spline_monotonicity = "increasing",
        prior_spline_derval_gaussian = array(c(1e-7, 1e-7)),
        prior_spline_derval_gaussian_domain = array(c(prior.start, end)),
        #prior_spline_funval_uniform = array(c(1e-6, 1e-6)),
        #prior_spline_funval_uniform_domain = array(c(prior.start, end)),
        use_re = FALSE,
        use_spline_intercept = FALSE,
        spline_r_linear = TRUE,
        spline_l_linear = TRUE,
        prior_spline_monotonicity = "decreasing"
        ## HERE is where can add more priors and other features to splines!
      )
      
      
      
      dat_1 <- MRData()
      dat_1$load_df(
        data = data_dt,  col_obs = "logit_val", col_obs_se = "se",
        col_covs = list("uhc", "sdi"), col_study_id = "super_region_name"
      )
      
      
      
      
      model <- MRBeRT(data = dat_1,  # e for ensemble splines
                      ensemble_cov_model = ensemble_cov_model1,
                      ensemble_knots = knots_samples,
                      cov_models = list(LinearCovModel("intercept", use_re=FALSE),
                                        LinearCovModel("sdi")), inlier_pct = .99)
      
      model$fit_model(inner_max_iter = 30L) 
      
      
      
      
      
      pred_dt <- rbind(data_dt, data.table(uhc = c(0,1)), fill = T)
      
      pred_dt <- pred_dt[order(uhc)]
      
      pred_dt <- rbind(data.table(pred_dt, sdi=0.2),
                       data.table(pred_dt, sdi=0.4),
                       data.table(pred_dt, sdi=0.6),
                       data.table(pred_dt, sdi=0.8),
                       data.table(pred_dt, sdi=1))
      
      dat_pred <- MRData()
      
      dat_pred$load_df(
        data = pred_dt,
        col_covs=list("uhc", "sdi")
      )
      
      #pred_dt$expected <- model$predict(dat_pred) %>% invlogit
      pred_dt$expected <- invlogit(model$predict(dat_pred))
      
      
      out_dt <- rbind(out_dt, pred_dt[!is.na(location_id)])
      
      print(paste0("Done with sex_id ", sid, " age_group_id ", agid))
      
      
      knot.placement <- data.table(model$ensemble_knots, weights = model$weights)
      setnames(knot.placement, old = c("V1", "V2", "V3", "V4", "V5", "V6"), new = c("knot1", "knot2", "knot3", "knot4", "knot5", "knot6"))
      knot.placement[, knot1 := start]
      knot.placement[, knot6 := end]
      write.csv(knot.placement, paste0(out_dir, "knot_placement_", sid, "_", agid, "_sdi.csv"), row.names = F)
    }
    
    out_dt$age_group_name <- factor(out_dt$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))
    
    sev_stunting_plot_df <- copy(sev_stunting_df)
    
    sev_stunting_plot_df[val<1e-7, val := 1e-7]
    sev_stunting_plot_df[val>(1-1e-7), val := (1-1e-7)]
    sev_stunting_plot_df[, logit_val := logit(val)]
    
    
    gg <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=logit_val, x=uhc* 100))+
      geom_point(data = sev_stunting_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = logit(expected), group = sdi))+
      theme_bw()+
      scale_color_manual(values = custom.col.sr)+
      labs(title = paste0("Severe Stunting (HAZ < -3 SD) ", unique(data_dt$measure_name), " among ", 
                          unique(data_dt$sex), "s, 1990-2020"),
           y = paste(unique(data_dt$measure_name), "(logit)"), 
           x = "Universal Health Coverage Index",
           color = "Super Region")+
      facet_wrap(~age_group_name, scales = 'free')+
      theme(legend.position="bottom", 
            legend.title.align = .5,
            legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90")) +
      guides(color = guide_legend(override.aes = list(alpha = .5), title.position = "top")) +
      #scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    
    gg2 <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=val, x=uhc* 100))+
      geom_point(data = sev_stunting_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = expected, group = sdi))+
      theme_bw()+
      scale_color_manual(values = custom.col.sr)+
      labs(title = paste0("Severe Stunting (HAZ < -3 SD) ", unique(data_dt$measure_name), " among ", 
                          unique(data_dt$sex), "s, 1990-2020"),
           y = paste(unique(data_dt$measure_name)), 
           x = "Universal Health Coverage Index",
           color = "Super Region")+
      facet_wrap(~age_group_name, scales = 'free')+
      theme(legend.position="bottom", 
            legend.title.align = .5,
            legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90")) +
      guides(color = guide_legend(override.aes = list(alpha = .5), title.position = "top")) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    
    
    print(gg)
    print(gg2)
  }
}

dev.off()


write.csv(out_dt, file.path(out_dir, "observed_and_expected_sdi.csv"), row.names = F)

out_dt <- fread(file.path(out_dir, "observed_and_expected_sdi.csv"))


out_dt <- out_dt[, .(location_id, age_group_id, year_id, sex_id, measure_id, metric_id, expected, observed = val, measure_name, metric_name, sdi)]


# Create aggregates -------------------------------------------------------

# merge on populations

population <- get_population(gbd_round_id = 7, decomp_step = "iterative",
                             location_id = unique(out_dt$location_id), 
                             age_group_id = unique(out_dt$age_group_id),
                             sex_id = unique(out_dt$sex_id),
                             year_id = unique(out_dt$year_id))

population[, run_id := NULL]

out_dt <- merge(out_dt, population, by = c("location_id", "age_group_id", "year_id", "sex_id"), all.x=T)

# merge on age weights
out_dt <- merge(out_dt, age_weights, by = "age_group_id", all.x=T)

# Aggreagate up location hierarchy

for(loc in hierarchy[most_detailed == 0, location_id]){
  
  if(loc == 1){
    agg_dt <- copy(out_dt)
  }else{
    child_locs <- hierarchy[most_detailed == 1 & (parent_id == loc | grepl(paste0(",", loc, ","), path_to_top_parent)), location_id]
    
    agg_dt <- out_dt[location_id %in% child_locs] 
  }
  
  agg_dt <- agg_dt[, .(location_id = loc, expected = weighted.mean(expected, w = population), observed = weighted.mean(observed, w = population),
                       population = sum(population)),
                   by = c("age_group_id", "year_id", "sex_id", "measure_id", "metric_id", "measure_name", "metric_name", "age_group_weight_value", "sdi")]
  
  out_dt <- rbind(out_dt, agg_dt, use.names = T)
  
}


# Aggregate to desired age groups

ages <- list("Under 1" = c(2, 3, 388, 389),
             "Under 5" = c(2, 3, 388, 389, 238, 34),
             "15 - 49" = 8:14,
             "50 - 69" = 15:18,
             "70+" = c(19,20, 20:32, 235), 
             "All Ages" = unique(out_dt$age_group_id),
             "Age-standardized" = unique(out_dt$age_group_id))

sum_dt <- data.table()

for(i in 1:length(ages)){
  
  agg_dt <- out_dt[age_group_id %in% ages[[i]]]
  
  if(names(ages)[i]=="Age-standardized"){
    agg_dt[, weight := age_group_weight_value]
  }else{
    agg_dt[, weight := population]
  }
  
  agg_dt <- agg_dt[, .(expected = weighted.mean(expected, w = weight), observed = weighted.mean(observed, w = weight), age_group_name = names(ages)[i]),
                   by = c("location_id", "year_id", "measure_id", "metric_id", "measure_name", "metric_name", "sdi")]
  
  sum_dt <- rbind(sum_dt, agg_dt, use.names = T)
  
}


# merge on location names
sum_dt <- merge(sum_dt, hierarchy[, .(location_id, location_name, ihme_loc_id, level, super_region_name, region_name)], by = "location_id", all.x=T)

sum_dt[, ratio := observed/expected]

sum_dt <- merge(sum_dt, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))


write.csv(sum_dt, file.path(out_dir, "summary_results_sdi.csv"), row.names = F)





###################################################################################################
###################################################################################################
####################################### EXTREME STUNTING ##########################################
###################################################################################################
###################################################################################################


out_dir <- file.path("filepath")



#reading in the dataset, writing out as csv so it's more easily accessible if needed later

ex_stunting_df <- get_draws("modelable_entity_id", 26941, year_id=c(1990:2020),
                            source="epi", gbd_round_id=7, decomp_step="iterative",
                            age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = standard.locations)

ex_stunting_df <- melt(ex_stunting_df, id.vars = id.vars)
ex_stunting_df <- prepare_df_format(df = ex_stunting_df)
write.csv(ex_stunting_df, "filepath", row.names = F)
ex_stunting_df <- fread("filepath")



# merge on uhc
uhc <- get_covariate_estimates(1097, gbd_round_id = 7, decomp_step = "iterative")

sdi <- get_covariate_estimates(881, gbd_round_id = 7, decomp_step = "iterative")



ex_stunting_df <- merge(ex_stunting_df, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))

ex_stunting_df <- merge(ex_stunting_df, sdi[,.(location_id, year_id, sdi = mean_value)], all.x=T, by = c("location_id", "year_id"))


ex_stunting_df <- merge(ex_stunting_df, hierarchy[, .(location_id, super_region_name)], all.x=T, by = "location_id")

ex_stunting_df$age_group_name <- factor(ex_stunting_df$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))

# run age-, sex-, measure-specific splines

out_dt <- data.table()

pdf(file.path(out_dir, "stunting_extreme_spline_plots_sdi.pdf"), height = 7.5, width = 14)

for(mid in unique(ex_stunting_df$measure_id)){
  for(sid in unique(ex_stunting_df$sex_id)){
    for(agid in unique(ex_stunting_df$age_group_id)){
      
      data_dt <- ex_stunting_df[age_group_id == agid & sex_id == sid & measure_id == mid]
      
      
      
      # use offset before logit
      data_dt[val<1e-7, val := 1e-7]
      data_dt[val>(1-1e-7), val := (1-1e-7)]
      data_dt[, logit_val := logit(val)]
      
      
      # make SD standard across all locations
      data_dt[, se := 1]
      
      n_knots=6 
      start=min(uhc$mean_value) 
      prior.start <- max(data_dt[val > 1e-7]$uhc)
      end = max(uhc$mean_value)
      
      
      
      min_dist=0.1
      
      knots_samples <- utils$sample_knots(
        num_intervals = 5L, # n_knots-1,
        knot_bounds = matrix(rep(c(start, end), each=n_knots-2), ncol=2),  # num_intervals-1 knot_bounds specified
        # num rows=num knots n_knots-2=num interior knots and repeat by num of iterior knot times; same for end
        # rbind(start,end repeated for x times internal knots)
        interval_sizes = rbind(c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.)),  # OR like: matrix(rep(c(min_dist, 1.), each=n_knots-1), ncol=2),  # num_intervals = interval_sizes specified
        # ^controls intervals between knots , interval length
        # all proportional to domain; maximum distance of knots is 1 follow same set-up logic as b
        num_samples = 20L # change to 20L once done testing
      )
      ensemble_cov_model1 <- LinearCovModel(
        alt_cov = "uhc",
        use_spline = TRUE,
        spline_knots = array(c(0,0.2, .4, .6, .8, 1)),  # number of specified splines needs to match n_knots; e.g. array(seq(0, 1, by = 0.25)); specifications OVERWRITTEN by ensemble splines specified when using MRBeRT!
        spline_degree = 2L, #3L
        spline_knots_type = 'domain',
        # prior_spline_monotonicity = "increasing",
        prior_spline_derval_gaussian = array(c(1e-6, 1e-6)),
        prior_spline_derval_gaussian_domain = array(c(prior.start, end)),
        #prior_spline_funval_uniform = array(c(1e-6, 1e-6)),
        #prior_spline_funval_uniform_domain = array(c(prior.start, end)),
        use_re = FALSE,
        use_spline_intercept = FALSE,
        spline_r_linear = TRUE,
        spline_l_linear = TRUE,
        prior_spline_monotonicity = "decreasing"
        ## HERE is where can add more priors and other features to splines!
      )
      
      
      
      dat_1 <- MRData()
      dat_1$load_df(
        data = data_dt,  col_obs = "logit_val", col_obs_se = "se",
        col_covs = list("uhc"), col_study_id = "super_region_name"
      )
      
      
      
      
      model <- MRBeRT(data = dat_1,  # e for ensemble splines
                      ensemble_cov_model = ensemble_cov_model1,
                      ensemble_knots = knots_samples,
                      cov_models = list(LinearCovModel("intercept", use_re=FALSE),
                                        LinearCovModel("sdi")), inlier_pct = .99)
      
      model$fit_model(inner_max_iter = 30L) 
      
      
      
      
      
      pred_dt <- rbind(data_dt, data.table(uhc = c(0,1)), fill = T)
      
      pred_dt <- pred_dt[order(uhc)]
      
      dat_pred <- MRData()
      
      dat_pred$load_df(
        data = pred_dt,
        col_covs=list("uhc", "sdi")
      )
      
      #pred_dt$expected <- model$predict(dat_pred) %>% invlogit
      pred_dt$expected <- invlogit(model$predict(dat_pred))
      
      
      out_dt <- rbind(out_dt, pred_dt[!is.na(location_id)])
      
      print(paste0("Done with sex_id ", sid, " age_group_id ", agid))
      
      
      knot.placement <- data.table(model$ensemble_knots, weights = model$weights)
      setnames(knot.placement, old = c("V1", "V2", "V3", "V4", "V5", "V6"), new = c("knot1", "knot2", "knot3", "knot4", "knot5", "knot6"))
      knot.placement[, knot1 := start]
      knot.placement[, knot6 := end]
      write.csv(knot.placement, paste0(out_dir, "knot_placement_", sid, "_", agid, "_sdi.csv"), row.names = F)
    }
    
    out_dt$age_group_name <- factor(out_dt$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))
    
    ex_stunting_plot_df <- copy(ex_stunting_df)
    
    ex_stunting_plot_df[val<1e-7, val := 1e-7]
    ex_stunting_plot_df[val>(1-1e-7), val := (1-1e-7)]
    ex_stunting_plot_df[, logit_val := logit(val)]
    
    
    gg <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=logit_val, x=uhc*100))+
      geom_point(data = ex_stunting_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = logit(expected), group = sdi))+
      theme_bw()+
      scale_color_manual(values = custom.col.sr)+
      labs(title = paste0("Extreme Stunting (HAZ < -4 SD) ", unique(data_dt$measure_name), " among ", 
                          unique(data_dt$sex), "s, 1990-2020"),
           y = paste(unique(data_dt$measure_name), "(logit)"), 
           x = "Universal Health Coverage Index",
           color = "Super Region")+
      facet_wrap(~age_group_name, scales = 'free')+
      theme(legend.position="bottom", 
            legend.title.align = .5,
            legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90")) +
      guides(color = guide_legend(override.aes = list(alpha = .5), title.position = "top")) +
      #scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    
    gg2 <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=val, x=uhc*100))+
      geom_point(data = ex_stunting_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = expected, group = sdi))+
      theme_bw()+
      scale_color_manual(values = custom.col.sr)+
      labs(title = paste0("Extreme Stunting (HAZ < -4 SD) ", unique(data_dt$measure_name), " among ", 
                          unique(data_dt$sex), "s, 1990-2020"),
           y = paste(unique(data_dt$measure_name)), 
           x = "Universal Health Coverage Index",
           color = "Super Region")+
      facet_wrap(~age_group_name, scales = 'free')+
      theme(legend.position="bottom", 
            legend.title.align = .5,
            legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90")) +
      guides(color = guide_legend(override.aes = list(alpha = .5), title.position = "top")) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    
    
    print(gg)
    print(gg2)
  }
}

dev.off()



write.csv(out_dt, file.path(out_dir, "observed_and_expected_sdi.csv"), row.names = F)

out_dt <- fread(file.path(out_dir, "observed_and_expected_sdi.csv"))


out_dt <- out_dt[, .(location_id, age_group_id, year_id, sex_id, measure_id, metric_id, expected, observed = val, measure_name, metric_name, sdi)]


# Create aggregates -------------------------------------------------------

# merge on populations

population <- get_population(gbd_round_id = 7, decomp_step = "iterative",
                             location_id = unique(out_dt$location_id), 
                             age_group_id = unique(out_dt$age_group_id),
                             sex_id = unique(out_dt$sex_id),
                             year_id = unique(out_dt$year_id))

population[, run_id := NULL]

out_dt <- merge(out_dt, population, by = c("location_id", "age_group_id", "year_id", "sex_id"), all.x=T)

# merge on age weights
out_dt <- merge(out_dt, age_weights, by = "age_group_id", all.x=T)

# Aggregate up location hierarchy

for(loc in hierarchy[most_detailed == 0, location_id]){
  
  if(loc == 1){
    agg_dt <- copy(out_dt)
  }else{
    child_locs <- hierarchy[most_detailed == 1 & (parent_id == loc | grepl(paste0(",", loc, ","), path_to_top_parent)), location_id]
    
    agg_dt <- out_dt[location_id %in% child_locs] 
  }
  
  agg_dt <- agg_dt[, .(location_id = loc, expected = weighted.mean(expected, w = population), observed = weighted.mean(observed, w = population),
                       population = sum(population)),
                   by = c("age_group_id", "year_id", "sex_id", "measure_id", "metric_id", "measure_name", "metric_name", "age_group_weight_value", "sdi")]
  
  out_dt <- rbind(out_dt, agg_dt, use.names = T)
  
}


# Aggregate to desired age groups

ages <- list("Under 1" = c(2, 3, 388, 389),
             "Under 5" = c(2, 3, 388, 389, 238, 34),
             "15 - 49" = 8:14,
             "50 - 69" = 15:18,
             "70+" = c(19,20, 20:32, 235), 
             "All Ages" = unique(out_dt$age_group_id),
             "Age-standardized" = unique(out_dt$age_group_id))

sum_dt <- data.table()

for(i in 1:length(ages)){
  
  agg_dt <- out_dt[age_group_id %in% ages[[i]]]
  
  if(names(ages)[i]=="Age-standardized"){
    agg_dt[, weight := age_group_weight_value]
  }else{
    agg_dt[, weight := population]
  }
  
  agg_dt <- agg_dt[, .(expected = weighted.mean(expected, w = weight), observed = weighted.mean(observed, w = weight), age_group_name = names(ages)[i]),
                   by = c("location_id", "year_id", "measure_id", "metric_id", "measure_name", "metric_name", "sdi")]
  
  sum_dt <- rbind(sum_dt, agg_dt, use.names = T)
  
}


# merge on location names
sum_dt <- merge(sum_dt, hierarchy[, .(location_id, location_name, ihme_loc_id, level, super_region_name, region_name)], by = "location_id", all.x=T)

sum_dt[, ratio := observed/expected]

sum_dt <- merge(sum_dt, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))


write.csv(sum_dt, file.path(out_dir, "summary_results_sdi.csv"), row.names = F)












###################################################################################################
###################################################################################################
####################################### OVERALL WASTING ###########################################
###################################################################################################
###################################################################################################

out_dir <- file.path("filepath")


#reading in the dataset, writing out as csv so it's more easily accessible if needed later
mod_wasting_df <- get_draws("modelable_entity_id", 10558, year_id=c(1990:2020),
                            source="epi", gbd_round_id=7, decomp_step="iterative",
                            age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = standard.locations)
# 
mod_wasting_df <- melt(mod_wasting_df, id.vars = id.vars)
mod_wasting_df <- prepare_df_format(df = mod_wasting_df)
write.csv(mod_wasting_df, "filepath", row.names = F)
mod_wasting_df <- fread("filepath")



# merge on uhc
uhc <- get_covariate_estimates(1097, gbd_round_id = 7, decomp_step = "iterative")

sdi <- get_covariate_estimates(881, gbd_round_id = 7, decomp_step = "iterative")


mod_wasting_df <- merge(mod_wasting_df, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))

mod_wasting_df <- merge(mod_wasting_df, sdi[,.(location_id, year_id, sdi = mean_value)], all.x=T, by = c("location_id", "year_id"))


mod_wasting_df <- merge(mod_wasting_df, hierarchy[, .(location_id, super_region_name)], all.x=T, by = "location_id")

mod_wasting_df$age_group_name <- factor(mod_wasting_df$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))

# run age-, sex-, measure-specific splines

out_dt <- data.table()

pdf(file.path(out_dir, "wasting_overall_spline_plots_sdi.pdf"), height = 7.5, width = 14)

for(mid in unique(mod_wasting_df$measure_id)){
  for(sid in unique(mod_wasting_df$sex_id)){
    for(agid in unique(mod_wasting_df$age_group_id)){
      
      data_dt <- mod_wasting_df[age_group_id == agid & sex_id == sid & measure_id == mid]
      
      
      
      # use offset before logit
      data_dt[val<1e-7, val := 1e-7]
      data_dt[val>(1-1e-7), val := (1-1e-7)]
      data_dt[, logit_val := logit(val)]
      
      # make SD standard across all locations
      data_dt[, se := 1]
      
      n_knots=6 
      start=min(uhc$mean_value) 
      prior.start <- max(data_dt[val > 1e-7]$uhc)
      end = max(uhc$mean_value)
      
      
      
      min_dist=0.1
      
      knots_samples <- utils$sample_knots(
        num_intervals = 5L, # n_knots-1,
        knot_bounds = matrix(rep(c(start, end), each=n_knots-2), ncol=2),  # num_intervals-1 knot_bounds specified
        # num rows=num knots n_knots-2=num interior knots and repeat by num of iterior knot times; same for end
        # rbind(start,end repeated for x times internal knots)
        interval_sizes = rbind(c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.)),  # OR like: matrix(rep(c(min_dist, 1.), each=n_knots-1), ncol=2),  # num_intervals = interval_sizes specified
        # ^controls intervals between knots , interval length
        # all proportional to domain; maximum distance of knots is 1 follow same set-up logic as b
        num_samples = 20L # change to 20L once done testing
      )
      ensemble_cov_model1 <- LinearCovModel(
        alt_cov = "uhc",
        use_spline = TRUE,
        spline_knots = array(c(0,0.2, .4, .6, .8, 1)),  # number of specified splines needs to match n_knots; e.g. array(seq(0, 1, by = 0.25)); specifications OVERWRITTEN by ensemble splines specified when using MRBeRT!
        spline_degree = 2L, #3L
        spline_knots_type = 'domain',
        # prior_spline_monotonicity = "increasing",
        prior_spline_derval_gaussian = array(c(1e-7, 1e-7)),
        prior_spline_derval_gaussian_domain = array(c(prior.start, end)),
        #prior_spline_funval_uniform = array(c(1e-6, 1e-6)),
        #prior_spline_funval_uniform_domain = array(c(prior.start, end)),
        use_re = FALSE,
        use_spline_intercept = FALSE,
        spline_r_linear = TRUE,
        spline_l_linear = TRUE,
        prior_spline_monotonicity = "decreasing"
        ## HERE is where can add more priors and other features to splines!
      )
      
      
      
      dat_1 <- MRData()
      dat_1$load_df(
        data = data_dt,  col_obs = "logit_val", col_obs_se = "se",
        col_covs = list("uhc"), col_study_id = "super_region_name"
      )
      
      
      
      
      model <- MRBeRT(data = dat_1,  # e for ensemble splines
                      ensemble_cov_model = ensemble_cov_model1,
                      ensemble_knots = knots_samples,
                      cov_models = list(LinearCovModel("intercept", use_re=FALSE),
                                        LinearCovModel("sdi")), inlier_pct = .99)
      
      model$fit_model(inner_max_iter = 30L) 
      
      
      
      
      
      pred_dt <- rbind(data_dt, data.table(uhc = c(0,1)), fill = T)
      
      pred_dt <- pred_dt[order(uhc)]
      
      pred_dt <- rbind(data.table(pred_dt, sdi=0.2),
                       data.table(pred_dt, sdi=0.4),
                       data.table(pred_dt, sdi=0.6),
                       data.table(pred_dt, sdi=0.8),
                       data.table(pred_dt, sdi=1))
      
      dat_pred <- MRData()
      
      dat_pred$load_df(
        data = pred_dt,
        col_covs=list("uhc")
      )
      
      #pred_dt$expected <- model$predict(dat_pred) %>% invlogit
      pred_dt$expected <- invlogit(model$predict(dat_pred))
      
      
      out_dt <- rbind(out_dt, pred_dt[!is.na(location_id)])
      
      print(paste("Done with", mid, sid, agid))
      
      
      knot.placement <- data.table(model$ensemble_knots, weights = model$weights)
      setnames(knot.placement, old = c("V1", "V2", "V3", "V4", "V5", "V6"), new = c("knot1", "knot2", "knot3", "knot4", "knot5", "knot6"))
      knot.placement[, knot1 := start]
      knot.placement[, knot6 := end]
      write.csv(knot.placement, paste0(out_dir, "knot_placement_", sid, "_", agid, "_sdi.csv"), row.names = F)
    }
    
    out_dt$age_group_name <- factor(out_dt$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))
    
    mod_wasting_plot_df <- copy(mod_wasting_df)
    mod_wasting_plot_df[val<1e-7, val := 1e-7]
    mod_wasting_plot_df[val>(1-1e-7), val := (1-1e-7)]
    mod_wasting_plot_df[, logit_val := logit(val)]
    
    gg <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=logit_val, x=uhc * 100))+
      geom_point(data = mod_wasting_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name, group = sdi), alpha = 0.3)+
      geom_line(aes(y = logit(expected)))+
      theme_bw()+
      scale_color_manual(values = custom.col.sr)+
      labs(title = paste0("Overall Wasting (WHZ < -2 SD) ", unique(data_dt$measure_name), " among ", 
                          unique(data_dt$sex), "s, 1990-2020"),
           y = paste(unique(data_dt$measure_name), "(logit)"), 
           x = "Universal Health Coverage Index",
           color = "Super Region")+
      facet_wrap(~age_group_name, scales = 'free')+
      theme(legend.position="bottom", 
            legend.title.align = .5,
            legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90")) +
      guides(color = guide_legend(override.aes = list(alpha = .5), title.position = "top")) +
      #scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    
    gg2 <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=val, x=uhc * 100))+
      geom_point(data = mod_wasting_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = expected, group = sdi))+
      theme_bw()+
      scale_color_manual(values = custom.col.sr)+
      labs(title = paste0("Overall Wasting (WHZ < -2 SD) ", unique(data_dt$measure_name), " among ", 
                          unique(data_dt$sex), "s, 1990-2020"),
           y = paste(unique(data_dt$measure_name)), 
           x = "Universal Health Coverage Index",
           color = "Super Region")+
      facet_wrap(~age_group_name, scales = 'free')+
      theme(legend.position="bottom", 
            legend.title.align = .5,
            legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90")) +
      guides(color = guide_legend(override.aes = list(alpha = .5), title.position = "top")) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    
    
    print(gg)
    print(gg2)
  }
}

dev.off()

write.csv(out_dt, file.path(out_dir, "observed_and_expected_sdi.csv"), row.names = F)

out_dt <- fread(file.path(out_dir, "observed_and_expected_sdi.csv"))



out_dt <- out_dt[, .(location_id, age_group_id, year_id, sex_id, measure_id, metric_id, expected, observed = val, measure_name, metric_name, sdi)]


# Create aggregates -------------------------------------------------------

# merge on populations

population <- get_population(gbd_round_id = 7, decomp_step = "iterative",
                             location_id = unique(out_dt$location_id), 
                             age_group_id = unique(out_dt$age_group_id),
                             sex_id = unique(out_dt$sex_id),
                             year_id = unique(out_dt$year_id))

population[, run_id := NULL]

out_dt <- merge(out_dt, population, by = c("location_id", "age_group_id", "year_id", "sex_id"), all.x=T)

# merge on age weights
out_dt <- merge(out_dt, age_weights, by = "age_group_id", all.x=T)

# Aggreagate up location hierarchy

for(loc in hierarchy[most_detailed == 0, location_id]){
  
  if(loc == 1){
    agg_dt <- copy(out_dt)
  }else{
    child_locs <- hierarchy[most_detailed == 1 & (parent_id == loc | grepl(paste0(",", loc, ","), path_to_top_parent)), location_id]
    
    agg_dt <- out_dt[location_id %in% child_locs] 
  }
  
  agg_dt <- agg_dt[, .(location_id = loc, expected = weighted.mean(expected, w = population), observed = weighted.mean(observed, w = population),
                       population = sum(population)),
                   by = c("age_group_id", "year_id", "sex_id", "measure_id", "metric_id", "measure_name", "metric_name", "age_group_weight_value", "sdi")]
  
  out_dt <- rbind(out_dt, agg_dt, use.names = T)
  
}


# Aggregate to desired age groups

ages <- list("Under 1" = c(2, 3, 388, 389),
             "Under 5" = c(2, 3, 388, 389, 238, 34),
             "15 - 49" = 8:14,
             "50 - 69" = 15:18,
             "70+" = c(19,20, 20:32, 235), 
             "All Ages" = unique(out_dt$age_group_id),
             "Age-standardized" = unique(out_dt$age_group_id))

sum_dt <- data.table()

for(i in 1:length(ages)){
  
  agg_dt <- out_dt[age_group_id %in% ages[[i]]]
  
  if(names(ages)[i]=="Age-standardized"){
    agg_dt[, weight := age_group_weight_value]
  }else{
    agg_dt[, weight := population]
  }
  
  agg_dt <- agg_dt[, .(expected = weighted.mean(expected, w = weight), observed = weighted.mean(observed, w = weight), age_group_name = names(ages)[i]),
                   by = c("location_id", "year_id", "measure_id", "metric_id", "measure_name", "metric_name", "sdi")]
  
  sum_dt <- rbind(sum_dt, agg_dt, use.names = T)
  
}


# merge on location names
sum_dt <- merge(sum_dt, hierarchy[, .(location_id, location_name, ihme_loc_id, level, super_region_name, region_name)], by = "location_id", all.x=T)

sum_dt[, ratio := observed/expected]

sum_dt <- merge(sum_dt, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))


write.csv(sum_dt, file.path(out_dir, "summary_results_sdi.csv"), row.names = F)






###################################################################################################
###################################################################################################
####################################### SEVERE WASTING ############################################
###################################################################################################
###################################################################################################


out_dir <- file.path("filepath")



#reading in the dataset, writing out as csv so it's more easily accessible if needed later

sev_wasting_df <- get_draws("modelable_entity_id", 8945, year_id=c(1990:2020),
                            source="epi", gbd_round_id=7, decomp_step="iterative",
                            age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = standard.locations)
# 
sev_wasting_df <- melt(sev_wasting_df, id.vars = id.vars)
sev_wasting_df <- prepare_df_format(df = sev_wasting_df)
write.csv(sev_wasting_df, "filepath", row.names = F)
sev_wasting_df <- fread("filepath")



# merge on uhc
uhc <- get_covariate_estimates(1097, gbd_round_id = 7, decomp_step = "iterative")

sdi <- get_covariate_estimates(881, gbd_round_id = 7, decomp_step = "iterative")


sev_wasting_df <- merge(sev_wasting_df, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))

sev_wasting_df <- merge(sev_wasting_df, sdi[,.(location_id, year_id, sdi = mean_value)], all.x=T, by = c("location_id", "year_id"))


sev_wasting_df <- merge(sev_wasting_df, hierarchy[, .(location_id, super_region_name)], all.x=T, by = "location_id")

sev_wasting_df$age_group_name <- factor(sev_wasting_df$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))

# run age-, sex-, measure-specific splines

out_dt <- data.table()

pdf(file.path(out_dir, "wasting_severe_spline_plots_sdi.pdf"), height = 7.5, width = 14)

for(mid in unique(sev_wasting_df$measure_id)){
  for(sid in unique(sev_wasting_df$sex_id)){
    for(agid in unique(sev_wasting_df$age_group_id)){
      
      data_dt <- sev_wasting_df[age_group_id == agid & sex_id == sid & measure_id == mid]
      
      
      # use offset before logit
      data_dt[val<1e-7, val := 1e-7]
      data_dt[val>(1-1e-7), val := (1-1e-7)]
      data_dt[, logit_val := logit(val)]
      
      # make SD standard across all locations
      data_dt[, se := 1]
      
      
      n_knots=6 
      start=min(uhc$mean_value) 
      prior.start <- max(data_dt[val > 1e-7]$uhc)
      end = max(uhc$mean_value)
      
      
      
      min_dist=0.1
      
      knots_samples <- utils$sample_knots(
        num_intervals = 5L, # n_knots-1,
        knot_bounds = matrix(rep(c(start, end), each=n_knots-2), ncol=2),  # num_intervals-1 knot_bounds specified
        # num rows=num knots n_knots-2=num interior knots and repeat by num of iterior knot times; same for end
        # rbind(start,end repeated for x times internal knots)
        interval_sizes = rbind(c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.)),  # OR like: matrix(rep(c(min_dist, 1.), each=n_knots-1), ncol=2),  # num_intervals = interval_sizes specified
        # ^controls intervals between knots , interval length
        # all proportional to domain; maximum distance of knots is 1 follow same set-up logic as b
        num_samples = 20L # change to 20L once done testing
      )
      ensemble_cov_model1 <- LinearCovModel(
        alt_cov = "uhc",
        use_spline = TRUE,
        spline_knots = array(c(0,0.2, .4, .6, .8, 1)),  # number of specified splines needs to match n_knots; e.g. array(seq(0, 1, by = 0.25)); specifications OVERWRITTEN by ensemble splines specified when using MRBeRT!
        spline_degree = 2L, #3L
        spline_knots_type = 'domain',
        # prior_spline_monotonicity = "increasing",
        prior_spline_derval_gaussian = array(c(1e-7, 1e-7)),
        prior_spline_derval_gaussian_domain = array(c(prior.start, end)),
        #prior_spline_funval_uniform = array(c(1e-6, 1e-6)),
        #prior_spline_funval_uniform_domain = array(c(prior.start, end)),
        use_re = FALSE,
        use_spline_intercept = FALSE,
        spline_r_linear = TRUE,
        spline_l_linear = TRUE,
        prior_spline_monotonicity = "decreasing"
        ## HERE is where can add more priors and other features to splines!
      )
      
      
      
      dat_1 <- MRData()
      dat_1$load_df(
        data = data_dt,  col_obs = "logit_val", col_obs_se = "se",
        col_covs = list("uhc", "sdi"), col_study_id = "super_region_name"
      )
      
      
      
      
      model <- MRBeRT(data = dat_1,  # e for ensemble splines
                      ensemble_cov_model = ensemble_cov_model1,
                      ensemble_knots = knots_samples,
                      cov_models = list(LinearCovModel("intercept", use_re=FALSE),
                                        LinearCovModel("sdi")), inlier_pct = .99)
      
      model$fit_model(inner_max_iter = 30L) 
      
      
      
      
      
      pred_dt <- rbind(data_dt, data.table(uhc = c(0,1)), fill = T)
      
      pred_dt <- pred_dt[order(uhc)]
      
      pred_dt <- rbind(data.table(pred_dt, sdi=0.2),
                       data.table(pred_dt, sdi=0.4),
                       data.table(pred_dt, sdi=0.6),
                       data.table(pred_dt, sdi=0.8),
                       data.table(pred_dt, sdi=1))
      
      dat_pred <- MRData()
      
      dat_pred$load_df(
        data = pred_dt,
        col_covs=list("uhc", "sdi")
      )
      
      #pred_dt$expected <- model$predict(dat_pred) %>% invlogit
      pred_dt$expected <- invlogit(model$predict(dat_pred))
      
      
      out_dt <- rbind(out_dt, pred_dt[!is.na(location_id)])
      
      print(paste("Done with", mid, sid, agid))
      
      
      knot.placement <- data.table(model$ensemble_knots, weights = model$weights)
      setnames(knot.placement, old = c("V1", "V2", "V3", "V4", "V5", "V6"), new = c("knot1", "knot2", "knot3", "knot4", "knot5", "knot6"))
      knot.placement[, knot1 := start]
      knot.placement[, knot6 := end]
      write.csv(knot.placement, paste0(out_dir, "knot_placement_", sid, "_", agid, "_sdi.csv"), row.names = F)
    }
    
    out_dt$age_group_name <- factor(out_dt$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))
    
    sev_wasting_plot_df <- copy(sev_wasting_df)
    
    sev_wasting_plot_df[val<1e-7, val := 1e-7]
    sev_wasting_plot_df[val>(1-1e-7), val := (1-1e-7)]
    sev_wasting_plot_df[, logit_val := logit(val)]
    
    
    gg <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=logit_val, x=uhc*100))+
      geom_point(data = sev_wasting_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = logit(expected), group = sdi))+
      theme_bw()+
      scale_color_manual(values = custom.col.sr)+
      labs(title = paste0("Severe Wasting (WHZ < -3 SD) ", unique(data_dt$measure_name), " among ", 
                          unique(data_dt$sex), "s, 1990-2020"),
           y = paste(unique(data_dt$measure_name), "(logit)"), 
           x = "Universal Health Coverage Index",
           color = "Super Region")+
      facet_wrap(~age_group_name, scales = 'free')+
      theme(legend.position="bottom", 
            legend.title.align = .5,
            legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90")) +
      guides(color = guide_legend(override.aes = list(alpha = .5), title.position = "top")) +
      #scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    
    gg2 <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=val, x=uhc*100))+
      geom_point(data = sev_wasting_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = expected, group = sdi))+
      theme_bw()+
      scale_color_manual(values = custom.col.sr)+
      labs(title = paste0("Severe Wasting (WHZ < -3 SD) ", unique(data_dt$measure_name), " among ", 
                          unique(data_dt$sex), "s, 1990-2020"),
           y = paste(unique(data_dt$measure_name)), 
           x = "Universal Health Coverage Index",
           color = "Super Region")+
      facet_wrap(~age_group_name, scales = 'free')+
      theme(legend.position="bottom", 
            legend.title.align = .5,
            legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90")) +
      guides(color = guide_legend(override.aes = list(alpha = .5), title.position = "top")) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    
    
    print(gg)
    print(gg2)
  }
}

dev.off()



write.csv(out_dt, file.path(out_dir, "observed_and_expected_sdi.csv"), row.names = F)

out_dt <- fread(file.path(out_dir, "observed_and_expected_sdi.csv"))


out_dt <- out_dt[, .(location_id, age_group_id, year_id, sex_id, measure_id, metric_id, expected, observed = val, measure_name, metric_name, sdi)]


# Create aggregates -------------------------------------------------------

# merge on populations

population <- get_population(gbd_round_id = 7, decomp_step = "iterative",
                             location_id = unique(out_dt$location_id), 
                             age_group_id = unique(out_dt$age_group_id),
                             sex_id = unique(out_dt$sex_id),
                             year_id = unique(out_dt$year_id))

population[, run_id := NULL]

out_dt <- merge(out_dt, population, by = c("location_id", "age_group_id", "year_id", "sex_id"), all.x=T)

# merge on age weights
out_dt <- merge(out_dt, age_weights, by = "age_group_id", all.x=T)

# Aggreagate up location hierarchy

for(loc in hierarchy[most_detailed == 0, location_id]){
  
  if(loc == 1){
    agg_dt <- copy(out_dt)
  }else{
    child_locs <- hierarchy[most_detailed == 1 & (parent_id == loc | grepl(paste0(",", loc, ","), path_to_top_parent)), location_id]
    
    agg_dt <- out_dt[location_id %in% child_locs] 
  }
  
  agg_dt <- agg_dt[, .(location_id = loc, expected = weighted.mean(expected, w = population), observed = weighted.mean(observed, w = population),
                       population = sum(population)),
                   by = c("age_group_id", "year_id", "sex_id", "measure_id", "metric_id", "measure_name", "metric_name", "age_group_weight_value", "sdi")]
  
  out_dt <- rbind(out_dt, agg_dt, use.names = T)
  
}


# Aggregate to desired age groups

ages <- list("Under 1" = c(2, 3, 388, 389),
             "Under 5" = c(2, 3, 388, 389, 238, 34),
             "15 - 49" = 8:14,
             "50 - 69" = 15:18,
             "70+" = c(19,20, 20:32, 235), 
             "All Ages" = unique(out_dt$age_group_id),
             "Age-standardized" = unique(out_dt$age_group_id))

sum_dt <- data.table()

for(i in 1:length(ages)){
  
  agg_dt <- out_dt[age_group_id %in% ages[[i]]]
  
  if(names(ages)[i]=="Age-standardized"){
    agg_dt[, weight := age_group_weight_value]
  }else{
    agg_dt[, weight := population]
  }
  
  agg_dt <- agg_dt[, .(expected = weighted.mean(expected, w = weight), observed = weighted.mean(observed, w = weight), age_group_name = names(ages)[i]),
                   by = c("location_id", "year_id", "measure_id", "metric_id", "measure_name", "metric_name", "sdi")]
  
  sum_dt <- rbind(sum_dt, agg_dt, use.names = T)
  
}


# merge on location names
sum_dt <- merge(sum_dt, hierarchy[, .(location_id, location_name, ihme_loc_id, level, super_region_name, region_name)], by = "location_id", all.x=T)

sum_dt[, ratio := observed/expected]

sum_dt <- merge(sum_dt, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))


write.csv(sum_dt, file.path(out_dir, "summary_results_sdi.csv"), row.names = F)








###################################################################################################
###################################################################################################
####################################### EXTREME WASTING ###########################################
###################################################################################################
###################################################################################################


out_dir <- file.path("filepath")



#reading in the dataset, writing out as csv so it's more easily accessible if needed later

ex_wasting_df <- get_draws("modelable_entity_id", 26943, year_id=c(1990:2020),
                           source="epi", gbd_round_id=7, decomp_step="iterative",
                           age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = standard.locations)
# 
ex_wasting_df <- melt(ex_wasting_df, id.vars = id.vars)
ex_wasting_df <- prepare_df_format(df = ex_wasting_df)
write.csv(ex_wasting_df, "filepath", row.names = F)
ex_wasting_df <- fread("filepath")




# merge on uhc
uhc <- get_covariate_estimates(1097, gbd_round_id = 7, decomp_step = "iterative")

sdi <- get_covariate_estimates(881, gbd_round_id = 7, decomp_step = "iterative")


ex_wasting_df <- merge(ex_wasting_df, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))

ex_wasting_df <- merge(ex_wasting_df, sdi[,.(location_id, year_id, sdi = mean_value)], all.x=T, by = c("location_id", "year_id"))


ex_wasting_df <- merge(ex_wasting_df, hierarchy[, .(location_id, super_region_name)], all.x=T, by = "location_id")

ex_wasting_df$age_group_name <- factor(ex_wasting_df$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))

# run age-, sex-, measure-specific splines

out_dt <- data.table()

pdf(file.path(out_dir, "wasting_extreme_spline_plots_sdi.pdf"), height = 7.5, width = 14)

for(mid in unique(ex_wasting_df$measure_id)){
  for(sid in unique(ex_wasting_df$sex_id)){
    for(agid in unique(ex_wasting_df$age_group_id)){
      
      
      data_dt <- ex_wasting_df[age_group_id == agid & sex_id == sid & measure_id == mid]
      
      # use offset before logit
      data_dt[val<1e-7, val := 1e-7]
      data_dt[val>(1-1e-7), val := (1-1e-7)]
      data_dt[, logit_val := logit(val)]
      
      # make SD standard across all locations
      data_dt[, se := 1]
      
      n_knots=6 
      start=min(uhc$mean_value) 
      prior.start <- max(data_dt[val > 1e-7]$uhc)
      end = max(uhc$mean_value)
      
      
      
      min_dist=0.1
      
      knots_samples <- utils$sample_knots(
        num_intervals = 5L, # n_knots-1,
        knot_bounds = matrix(rep(c(start, end), each=n_knots-2), ncol=2),  # num_intervals-1 knot_bounds specified
        # num rows=num knots n_knots-2=num interior knots and repeat by num of iterior knot times; same for end
        # rbind(start,end repeated for x times internal knots)
        interval_sizes = rbind(c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.)),  # OR like: matrix(rep(c(min_dist, 1.), each=n_knots-1), ncol=2),  # num_intervals = interval_sizes specified
        # ^controls intervals between knots , interval length
        # all proportional to domain; maximum distance of knots is 1 follow same set-up logic as b
        num_samples = 20L # change to 20L once done testing
      )
      ensemble_cov_model1 <- LinearCovModel(
        alt_cov = "uhc",
        use_spline = TRUE,
        spline_knots = array(c(0,0.2, .4, .6, .8, 1)),  # number of specified splines needs to match n_knots; e.g. array(seq(0, 1, by = 0.25)); specifications OVERWRITTEN by ensemble splines specified when using MRBeRT!
        spline_degree = 2L, #3L
        spline_knots_type = 'domain',
        # prior_spline_monotonicity = "increasing",
        prior_spline_derval_gaussian = array(c(1e-7, 1e-7)),
        prior_spline_derval_gaussian_domain = array(c(prior.start, end)),
        #prior_spline_funval_uniform = array(c(1e-6, 1e-6)),
        #prior_spline_funval_uniform_domain = array(c(prior.start, end)),
        use_re = FALSE,
        use_spline_intercept = FALSE,
        spline_r_linear = TRUE,
        spline_l_linear = TRUE,
        prior_spline_monotonicity = "decreasing"
        ## HERE is where can add more priors and other features to splines!
      )
      
      
      
      dat_1 <- MRData()
      dat_1$load_df(
        data = data_dt,  col_obs = "logit_val", col_obs_se = "se",
        col_covs = list("uhc", "sdi"), col_study_id = "super_region_name"
      )
      
      
      
      
      model <- MRBeRT(data = dat_1,  # e for ensemble splines
                      ensemble_cov_model = ensemble_cov_model1,
                      ensemble_knots = knots_samples,
                      cov_models = list(LinearCovModel("intercept", use_re=FALSE),
                                        LinearCovModel("sdi")), inlier_pct = .99)
      
      model$fit_model(inner_max_iter = 30L) 
      
      
      
      
      
      pred_dt <- rbind(data_dt, data.table(uhc = c(0,1)), fill = T)
      
      pred_dt <- pred_dt[order(uhc)]
      
      pred_dt <- rbind(data.table(pred_dt, sdi=0.2),
                       data.table(pred_dt, sdi=0.4),
                       data.table(pred_dt, sdi=0.6),
                       data.table(pred_dt, sdi=0.8),
                       data.table(pred_dt, sdi=1))
      
      dat_pred <- MRData()
      
      dat_pred$load_df(
        data = pred_dt,
        col_covs=list("uhc", "sdi")
      )
      
      #pred_dt$expected <- model$predict(dat_pred) %>% invlogit
      pred_dt$expected <- invlogit(model$predict(dat_pred))
      
      
      out_dt <- rbind(out_dt, pred_dt[!is.na(location_id)])
      
      print(paste0("Done with sex_id ", sid, " age_group_id ", agid))
      
      
      knot.placement <- data.table(model$ensemble_knots, weights = model$weights)
      setnames(knot.placement, old = c("V1", "V2", "V3", "V4", "V5", "V6"), new = c("knot1", "knot2", "knot3", "knot4", "knot5", "knot6"))
      knot.placement[, knot1 := start]
      knot.placement[, knot6 := end]
      write.csv(knot.placement, paste0(out_dir, "knot_placement_", sid, "_", agid, "_sdi.csv"), row.names = F)
      
    }
    
    
    
    out_dt$age_group_name <- factor(out_dt$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))
    
    ex_wasting_plot_df <- copy(ex_wasting_df)
    
    ex_wasting_plot_df[val<1e-7, val := 1e-7]
    ex_wasting_plot_df[val>(1-1e-7), val := (1-1e-7)]
    ex_wasting_plot_df[, logit_val := logit(val)]
    
    
    gg <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=logit_val, x=uhc*100))+
      geom_point(data = ex_wasting_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = logit(expected), group = sdi))+
      theme_bw()+
      scale_color_manual(values = custom.col.sr)+
      labs(title = paste0("Extreme Wasting (WHZ < -4 SD) ", unique(data_dt$measure_name), " among ", 
                          unique(data_dt$sex), "s, 1990-2020"),
           y = paste(unique(data_dt$measure_name), "(logit)"), 
           x = "Universal Health Coverage Index",
           color = "Super Region")+
      facet_wrap(~age_group_name, scales = 'free')+
      theme(legend.position="bottom", 
            legend.title.align = .5,
            legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90")) +
      guides(color = guide_legend(override.aes = list(alpha = .5), title.position = "top")) +
      #scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    
    gg2 <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=val, x=uhc*100))+
      geom_point(data = ex_wasting_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = expected, group = sdi))+
      theme_bw()+
      scale_color_manual(values = custom.col.sr)+
      labs(title = paste0("Extreme Wasting (WHZ < -4 SD) ", unique(data_dt$measure_name), " among ", 
                          unique(data_dt$sex), "s, 1990-2020"),
           y = paste(unique(data_dt$measure_name)), 
           x = "Universal Health Coverage Index",
           color = "Super Region")+
      facet_wrap(~age_group_name, scales = 'free')+
      theme(legend.position="bottom", 
            legend.title.align = .5,
            legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90")) +
      guides(color = guide_legend(override.aes = list(alpha = .5), title.position = "top")) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    
    
    print(gg)
    print(gg2)
    
    
  }
}

dev.off()



write.csv(out_dt, file.path(out_dir, "observed_and_expected_sdi.csv"), row.names = F)

out_dt <- fread(file.path(out_dir, "observed_and_expected_sdi.csv"))


out_dt <- out_dt[, .(location_id, age_group_id, year_id, sex_id, measure_id, metric_id, expected, observed = val, measure_name, metric_name, sdi)]


# Create aggregates -------------------------------------------------------

# merge on populations

population <- get_population(gbd_round_id = 7, decomp_step = "iterative",
                             location_id = unique(out_dt$location_id), 
                             age_group_id = unique(out_dt$age_group_id),
                             sex_id = unique(out_dt$sex_id),
                             year_id = unique(out_dt$year_id))

population[, run_id := NULL]

out_dt <- merge(out_dt, population, by = c("location_id", "age_group_id", "year_id", "sex_id"), all.x=T)

# merge on age weights
out_dt <- merge(out_dt, age_weights, by = "age_group_id", all.x=T)

# Aggreagate up location hierarchy

for(loc in hierarchy[most_detailed == 0, location_id]){
  
  if(loc == 1){
    agg_dt <- copy(out_dt)
  }else{
    child_locs <- hierarchy[most_detailed == 1 & (parent_id == loc | grepl(paste0(",", loc, ","), path_to_top_parent)), location_id]
    
    agg_dt <- out_dt[location_id %in% child_locs] 
  }
  
  agg_dt <- agg_dt[, .(location_id = loc, expected = weighted.mean(expected, w = population), observed = weighted.mean(observed, w = population),
                       population = sum(population)),
                   by = c("age_group_id", "year_id", "sex_id", "measure_id", "metric_id", "measure_name", "metric_name", "age_group_weight_value", "sdi")]
  
  out_dt <- rbind(out_dt, agg_dt, use.names = T)
  
}


# Aggregate to desired age groups

ages <- list("Under 1" = c(2, 3, 388, 389),
             "Under 5" = c(2, 3, 388, 389, 238, 34),
             "15 - 49" = 8:14,
             "50 - 69" = 15:18,
             "70+" = c(19,20, 20:32, 235), 
             "All Ages" = unique(out_dt$age_group_id),
             "Age-standardized" = unique(out_dt$age_group_id))

sum_dt <- data.table()

for(i in 1:length(ages)){
  
  agg_dt <- out_dt[age_group_id %in% ages[[i]]]
  
  if(names(ages)[i]=="Age-standardized"){
    agg_dt[, weight := age_group_weight_value]
  }else{
    agg_dt[, weight := population]
  }
  
  agg_dt <- agg_dt[, .(expected = weighted.mean(expected, w = weight), observed = weighted.mean(observed, w = weight), age_group_name = names(ages)[i]),
                   by = c("location_id", "year_id", "measure_id", "metric_id", "measure_name", "metric_name", "sdi")]
  
  sum_dt <- rbind(sum_dt, agg_dt, use.names = T)
  
}


# merge on location names
sum_dt <- merge(sum_dt, hierarchy[, .(location_id, location_name, ihme_loc_id, level, super_region_name, region_name)], by = "location_id", all.x=T)

sum_dt[, ratio := observed/expected]

sum_dt <- merge(sum_dt, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))


write.csv(sum_dt, file.path(out_dir, "summary_results_sdi.csv"), row.names = F)













###################################################################################################
###################################################################################################
################################### OVERALL UNDERWEIGHT ###########################################
###################################################################################################
###################################################################################################

out_dir <- file.path("filepath")


#reading in the dataset, writing out as csv so it's more easily accessible if needed later
mod_underweight_df <- get_draws("modelable_entity_id", 10560, year_id=c(1990:2020),
                                source="epi", gbd_round_id=7, decomp_step="iterative",
                                age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = standard.locations)
# 
mod_underweight_df <- melt(mod_underweight_df, id.vars = id.vars)
mod_underweight_df <- prepare_df_format(df = mod_underweight_df)
write.csv(mod_underweight_df, "filepath", row.names = F)
mod_underweight_df <- fread("filepath")



# merge on uhc
uhc <- get_covariate_estimates(1097, gbd_round_id = 7, decomp_step = "iterative")

sdi <- get_covariate_estimates(881, gbd_round_id = 7, decomp_step = "iterative")


mod_underweight_df <- merge(mod_underweight_df, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))

mod_underweight_df <- merge(mod_underweight_df, sdi[,.(location_id, year_id, sdi = mean_value)], all.x=T, by = c("location_id", "year_id"))


mod_underweight_df <- merge(mod_underweight_df, hierarchy[, .(location_id, super_region_name)], all.x=T, by = "location_id")

mod_underweight_df$age_group_name <- factor(mod_underweight_df$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))

# run age-, sex-, measure-specific splines

out_dt <- data.table()

pdf(file.path(out_dir, "underweight_overall_spline_plots_sdi.pdf"), height = 7.5, width = 14)

for(mid in unique(mod_underweight_df$measure_id)){
  for(sid in unique(mod_underweight_df$sex_id)){
    for(agid in unique(mod_underweight_df$age_group_id)){
      
      data_dt <- mod_underweight_df[age_group_id == agid & sex_id == sid & measure_id == mid]
      
      
      # use offset before logit
      data_dt[val<1e-7, val := 1e-7]
      data_dt[val>(1-1e-7), val := (1-1e-7)]
      data_dt[, logit_val := logit(val)]
      
      
      # make SD standard across all locations
      data_dt[, se := 1]
      
      n_knots=6 
      start=min(uhc$mean_value) 
      prior.start <- max(data_dt[val > 1e-7]$uhc)
      end = max(uhc$mean_value)
      
      
      
      min_dist=0.1
      
      knots_samples <- utils$sample_knots(
        num_intervals = 5L, # n_knots-1,
        knot_bounds = matrix(rep(c(start, end), each=n_knots-2), ncol=2),  # num_intervals-1 knot_bounds specified
        # num rows=num knots n_knots-2=num interior knots and repeat by num of iterior knot times; same for end
        # rbind(start,end repeated for x times internal knots)
        interval_sizes = rbind(c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.)),  # OR like: matrix(rep(c(min_dist, 1.), each=n_knots-1), ncol=2),  # num_intervals = interval_sizes specified
        # ^controls intervals between knots , interval length
        # all proportional to domain; maximum distance of knots is 1 follow same set-up logic as b
        num_samples = 20L # change to 20L once done testing
      )
      ensemble_cov_model1 <- LinearCovModel(
        alt_cov = "uhc",
        use_spline = TRUE,
        spline_knots = array(c(0,0.2, .4, .6, .8, 1)),  # number of specified splines needs to match n_knots; e.g. array(seq(0, 1, by = 0.25)); specifications OVERWRITTEN by ensemble splines specified when using MRBeRT!
        spline_degree = 2L, #3L
        spline_knots_type = 'domain',
        # prior_spline_monotonicity = "increasing",
        prior_spline_derval_gaussian = array(c(1e-7, 1e-7)),
        prior_spline_derval_gaussian_domain = array(c(prior.start, end)),
        #prior_spline_funval_uniform = array(c(1e-6, 1e-6)),
        #prior_spline_funval_uniform_domain = array(c(prior.start, end)),
        use_re = FALSE,
        use_spline_intercept = FALSE,
        spline_r_linear = TRUE,
        spline_l_linear = TRUE,
        prior_spline_monotonicity = "decreasing"
        ## HERE is where can add more priors and other features to splines!
      )
      
      
      
      dat_1 <- MRData()
      dat_1$load_df(
        data = data_dt,  col_obs = "logit_val", col_obs_se = "se",
        col_covs = list("uhc", "sdi"), col_study_id = "super_region_name"
      )
      
      
      
      
      model <- MRBeRT(data = dat_1,  # e for ensemble splines
                      ensemble_cov_model = ensemble_cov_model1,
                      ensemble_knots = knots_samples,
                      cov_models = list(LinearCovModel("intercept", use_re=FALSE),
                                        LinearCovModel("sdi")), inlier_pct = .99)
      
      model$fit_model(inner_max_iter = 30L) 
      
      
      
      
      
      pred_dt <- rbind(data_dt, data.table(uhc = c(0,1)), fill = T)
      
      pred_dt <- pred_dt[order(uhc)]
      
      pred_dt <- rbind(data.table(pred_dt, sdi=0.2),
                       data.table(pred_dt, sdi=0.4),
                       data.table(pred_dt, sdi=0.6),
                       data.table(pred_dt, sdi=0.8),
                       data.table(pred_dt, sdi=1))
      
      dat_pred <- MRData()
      
      dat_pred$load_df(
        data = pred_dt,
        col_covs=list("uhc", "sdi")
      )
      
      #pred_dt$expected <- model$predict(dat_pred) %>% invlogit
      pred_dt$expected <- invlogit(model$predict(dat_pred))
      
      
      out_dt <- rbind(out_dt, pred_dt[!is.na(location_id)])
      
      print(paste0("Done with sex_id ", sid, " age_group_id ", agid))
      
      
      knot.placement <- data.table(model$ensemble_knots, weights = model$weights)
      setnames(knot.placement, old = c("V1", "V2", "V3", "V4", "V5", "V6"), new = c("knot1", "knot2", "knot3", "knot4", "knot5", "knot6"))
      knot.placement[, knot1 := start]
      knot.placement[, knot6 := end]
      write.csv(knot.placement, paste0(out_dir, "knot_placement_", sid, "_", agid, "_sdi.csv"), row.names = F)
    }
    
    out_dt$age_group_name <- factor(out_dt$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))
    
    mod_underweight_plot_df <- copy(mod_underweight_df)
    
    mod_underweight_plot_df[val<1e-7, val := 1e-7]
    mod_underweight_plot_df[val>(1-1e-7), val := (1-1e-7)]
    mod_underweight_plot_df[, logit_val := logit(val)]
    
    
    gg <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=logit_val, x=uhc*100))+
      geom_point(data = mod_underweight_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = logit(expected), group = sdi))+
      theme_bw()+
      scale_color_manual(values = custom.col.sr)+
      labs(title = paste0("Overall Underweight (WAZ < -2 SD) ", unique(data_dt$measure_name), " among ", 
                          unique(data_dt$sex), "s, 1990-2020"),
           y = paste(unique(data_dt$measure_name), "(logit)"), 
           x = "Universal Health Coverage Index",
           color = "Super Region")+
      facet_wrap(~age_group_name, scales = 'free')+
      theme(legend.position="bottom", 
            legend.title.align = .5,
            legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90")) +
      guides(color = guide_legend(override.aes = list(alpha = .5), title.position = "top")) +
      #scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    
    gg2 <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=val, x=uhc*100))+
      geom_point(data = mod_underweight_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = expected, group = sdi))+
      theme_bw()+
      scale_color_manual(values = custom.col.sr)+
      labs(title = paste0("Overall Underweight (WAZ < -2 SD) ", unique(data_dt$measure_name), " among ", 
                          unique(data_dt$sex), "s, 1990-2020"),
           y = paste(unique(data_dt$measure_name)), 
           x = "Universal Health Coverage Index",
           color = "Super Region")+
      facet_wrap(~age_group_name, scales = 'free')+
      theme(legend.position="bottom", 
            legend.title.align = .5,
            legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90")) +
      guides(color = guide_legend(override.aes = list(alpha = .5), title.position = "top")) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    
    
    print(gg)
    print(gg2)
  }
}

dev.off()


write.csv(out_dt, file.path(out_dir, "observed_and_expected_sdi.csv"), row.names = F)

out_dt <- fread(file.path(out_dir, "observed_and_expected_sdi.csv"))



out_dt <- out_dt[, .(location_id, age_group_id, year_id, sex_id, measure_id, metric_id, expected, observed = val, measure_name, metric_name, sdi)]


# Create aggregates -------------------------------------------------------

# merge on populations

population <- get_population(gbd_round_id = 7, decomp_step = "iterative",
                             location_id = unique(out_dt$location_id), 
                             age_group_id = unique(out_dt$age_group_id),
                             sex_id = unique(out_dt$sex_id),
                             year_id = unique(out_dt$year_id))

population[, run_id := NULL]

out_dt <- merge(out_dt, population, by = c("location_id", "age_group_id", "year_id", "sex_id"), all.x=T)

# merge on age weights
out_dt <- merge(out_dt, age_weights, by = "age_group_id", all.x=T)

# Aggreagate up location hierarchy

for(loc in hierarchy[most_detailed == 0, location_id]){
  
  if(loc == 1){
    agg_dt <- copy(out_dt)
  }else{
    child_locs <- hierarchy[most_detailed == 1 & (parent_id == loc | grepl(paste0(",", loc, ","), path_to_top_parent)), location_id]
    
    agg_dt <- out_dt[location_id %in% child_locs] 
  }
  
  agg_dt <- agg_dt[, .(location_id = loc, expected = weighted.mean(expected, w = population), observed = weighted.mean(observed, w = population),
                       population = sum(population)),
                   by = c("age_group_id", "year_id", "sex_id", "measure_id", "metric_id", "measure_name", "metric_name", "age_group_weight_value", "sdi")]
  
  out_dt <- rbind(out_dt, agg_dt, use.names = T)
  
}


# Aggregate to desired age groups

ages <- list("Under 1" = c(2, 3, 388, 389),
             "Under 5" = c(2, 3, 388, 389, 238, 34),
             "15 - 49" = 8:14,
             "50 - 69" = 15:18,
             "70+" = c(19,20, 20:32, 235), 
             "All Ages" = unique(out_dt$age_group_id),
             "Age-standardized" = unique(out_dt$age_group_id))

sum_dt <- data.table()

for(i in 1:length(ages)){
  
  agg_dt <- out_dt[age_group_id %in% ages[[i]]]
  
  if(names(ages)[i]=="Age-standardized"){
    agg_dt[, weight := age_group_weight_value]
  }else{
    agg_dt[, weight := population]
  }
  
  agg_dt <- agg_dt[, .(expected = weighted.mean(expected, w = weight), observed = weighted.mean(observed, w = weight), age_group_name = names(ages)[i]),
                   by = c("location_id", "year_id", "measure_id", "metric_id", "measure_name", "metric_name", "sdi")]
  
  sum_dt <- rbind(sum_dt, agg_dt, use.names = T)
  
}


# merge on location names
sum_dt <- merge(sum_dt, hierarchy[, .(location_id, location_name, ihme_loc_id, level, super_region_name, region_name)], by = "location_id", all.x=T)

sum_dt[, ratio := observed/expected]

sum_dt <- merge(sum_dt, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))


write.csv(sum_dt, file.path(out_dir, "summary_results_sdi.csv"), row.names = F)









###################################################################################################
###################################################################################################
################################### SEVERE UNDERWEIGHT ############################################
###################################################################################################
###################################################################################################


out_dir <- file.path("filepath")



#reading in the dataset, writing out as csv so it's more easily accessible if needed later

sev_underweight_df <- get_draws("modelable_entity_id", 2540, year_id=c(1990:2020),
                                source="epi", gbd_round_id=7, decomp_step="iterative",
                                age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = standard.locations)
# 
sev_underweight_df <- melt(sev_underweight_df, id.vars = id.vars)
sev_underweight_df <- prepare_df_format(df = sev_underweight_df)
write.csv(sev_underweight_df, "filepath", row.names = F)
sev_underweight_df <- fread("filepath")



# merge on uhc
uhc <- get_covariate_estimates(1097, gbd_round_id = 7, decomp_step = "iterative")

sdi <- get_covariate_estimates(881, gbd_round_id = 7, decomp_step = "iterative")


sev_underweight_df <- merge(sev_underweight_df, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))

sev_underweight_df <- merge(sev_underweight_df, sdi[,.(location_id, year_id, sdi = mean_value)], all.x=T, by = c("location_id", "year_id"))

sev_underweight_df <- merge(sev_underweight_df, hierarchy[, .(location_id, super_region_name)], all.x=T, by = "location_id")

sev_underweight_df$age_group_name <- factor(sev_underweight_df$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))

# run age-, sex-, measure-specific splines

out_dt <- data.table()

pdf(file.path(out_dir, "underweight_severe_spline_plots_sdi.pdf"), height = 7.5, width = 14)

for(mid in unique(sev_underweight_df$measure_id)){
  for(sid in unique(sev_underweight_df$sex_id)){
    for(agid in unique(sev_underweight_df$age_group_id)){
      
      data_dt <- sev_underweight_df[age_group_id == agid & sex_id == sid & measure_id == mid]
      
      
      # use offset before logit
      data_dt[val<1e-7, val := 1e-7]
      data_dt[val>(1-1e-7), val := (1-1e-7)]
      data_dt[, logit_val := logit(val)]
      
      # make SD standard across all locations
      data_dt[, se := 1]
      
      n_knots=6 
      start=min(uhc$mean_value) 
      prior.start <- max(data_dt[val > 1e-7]$uhc)
      end = max(uhc$mean_value)
      
      
      
      min_dist=0.1
      
      knots_samples <- utils$sample_knots(
        num_intervals = 5L, # n_knots-1,
        knot_bounds = matrix(rep(c(start, end), each=n_knots-2), ncol=2),  # num_intervals-1 knot_bounds specified
        # num rows=num knots n_knots-2=num interior knots and repeat by num of iterior knot times; same for end
        # rbind(start,end repeated for x times internal knots)
        interval_sizes = rbind(c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.)),  # OR like: matrix(rep(c(min_dist, 1.), each=n_knots-1), ncol=2),  # num_intervals = interval_sizes specified
        # ^controls intervals between knots , interval length
        # all proportional to domain; maximum distance of knots is 1 follow same set-up logic as b
        num_samples = 20L # change to 20L once done testing
      )
      ensemble_cov_model1 <- LinearCovModel(
        alt_cov = "uhc",
        use_spline = TRUE,
        spline_knots = array(c(0,0.2, .4, .6, .8, 1)),  # number of specified splines needs to match n_knots; e.g. array(seq(0, 1, by = 0.25)); specifications OVERWRITTEN by ensemble splines specified when using MRBeRT!
        spline_degree = 2L, #3L
        spline_knots_type = 'domain',
        # prior_spline_monotonicity = "increasing",
        prior_spline_derval_gaussian = array(c(1e-7, 1e-7)),
        prior_spline_derval_gaussian_domain = array(c(prior.start, end)),
        #prior_spline_funval_uniform = array(c(1e-6, 1e-6)),
        #prior_spline_funval_uniform_domain = array(c(prior.start, end)),
        use_re = FALSE,
        use_spline_intercept = FALSE,
        spline_r_linear = TRUE,
        spline_l_linear = TRUE,
        prior_spline_monotonicity = "decreasing"
        ## HERE is where can add more priors and other features to splines!
      )
      
      
      
      dat_1 <- MRData()
      dat_1$load_df(
        data = data_dt,  col_obs = "logit_val", col_obs_se = "se",
        col_covs = list("uhc", "sdi"), col_study_id = "super_region_name"
      )
      
      
      
      
      model <- MRBeRT(data = dat_1,  # e for ensemble splines
                      ensemble_cov_model = ensemble_cov_model1,
                      ensemble_knots = knots_samples,
                      cov_models = list(LinearCovModel("intercept", use_re=FALSE),
                                        LinearCovModel("sdi")), inlier_pct = .99)
      
      model$fit_model(inner_max_iter = 30L) 
      
      
      
      
      
      pred_dt <- rbind(data_dt, data.table(uhc = c(0,1)), fill = T)
      
      pred_dt <- pred_dt[order(uhc)]
      
      pred_dt <- rbind(data.table(pred_dt, sdi=0.2),
                       data.table(pred_dt, sdi=0.4),
                       data.table(pred_dt, sdi=0.6),
                       data.table(pred_dt, sdi=0.8),
                       data.table(pred_dt, sdi=1))
      
      dat_pred <- MRData()
      
      dat_pred$load_df(
        data = pred_dt,
        col_covs=list("uhc", "sdi")
      )
      
      #pred_dt$expected <- model$predict(dat_pred) %>% invlogit
      pred_dt$expected <- invlogit(model$predict(dat_pred))
      
      
      out_dt <- rbind(out_dt, pred_dt[!is.na(location_id)])
      
      print(paste0("Done with sex_id ", sid, " age_group_id ", agid))
      
      
      knot.placement <- data.table(model$ensemble_knots, weights = model$weights)
      setnames(knot.placement, old = c("V1", "V2", "V3", "V4", "V5", "V6"), new = c("knot1", "knot2", "knot3", "knot4", "knot5", "knot6"))
      knot.placement[, knot1 := start]
      knot.placement[, knot6 := end]
      write.csv(knot.placement, paste0(out_dir, "knot_placement_", sid, "_", agid, "_sdi.csv"), row.names = F)
      
    }
    
    out_dt$age_group_name <- factor(out_dt$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))
    
    
    
    sev_underweight_plot_df <- copy(sev_underweight_df)
    
    sev_underweight_plot_df[val<1e-7, val := 1e-7]
    sev_underweight_plot_df[val>(1-1e-7), val := (1-1e-7)]
    sev_underweight_plot_df[, logit_val := logit(val)]
    
    
    
    
    gg <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=logit_val, x=uhc *100))+
      geom_point(data = sev_underweight_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = logit(expected), group = sdi))+
      theme_bw()+
      scale_color_manual(values = custom.col.sr)+
      labs(title = paste0("Severe Underweight (WAZ < -3 SD) ", unique(data_dt$measure_name), " among ", 
                          unique(data_dt$sex), "s, 1990-2020"),
           y = paste(unique(data_dt$measure_name), "(logit)"), 
           x = "Universal Health Coverage Index",
           color = "Super Region")+
      facet_wrap(~age_group_name, scales = 'free')+
      theme(legend.position="bottom", 
            legend.title.align = .5,
            legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90")) +
      guides(color = guide_legend(override.aes = list(alpha = .5), title.position = "top")) +
      #scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    
    gg2 <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=val, x=uhc * 100))+
      geom_point(data = sev_underweight_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = expected, group = sdi))+
      theme_bw()+
      scale_color_manual(values = custom.col.sr)+
      labs(title = paste0("Severe Underweight (WAZ < -3 SD) ", unique(data_dt$measure_name), " among ", 
                          unique(data_dt$sex), "s, 1990-2020"),
           y = paste(unique(data_dt$measure_name)), 
           x = "Universal Health Coverage Index",
           color = "Super Region")+
      facet_wrap(~age_group_name, scales = 'free')+
      theme(legend.position="bottom", 
            legend.title.align = .5,
            legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90")) +
      guides(color = guide_legend(override.aes = list(alpha = .5), title.position = "top")) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    
    
    print(gg)
    print(gg2)
  }
}

dev.off()


write.csv(out_dt, file.path(out_dir, "observed_and_expected_sdi.csv"), row.names = F)

out_dt <- fread(file.path(out_dir, "observed_and_expected_sdi.csv"))


out_dt <- out_dt[, .(location_id, age_group_id, year_id, sex_id, measure_id, metric_id, expected, observed = val, measure_name, metric_name, sdi)]


# Create aggregates -------------------------------------------------------

# merge on populations

population <- get_population(gbd_round_id = 7, decomp_step = "iterative",
                             location_id = unique(out_dt$location_id), 
                             age_group_id = unique(out_dt$age_group_id),
                             sex_id = unique(out_dt$sex_id),
                             year_id = unique(out_dt$year_id))

population[, run_id := NULL]

out_dt <- merge(out_dt, population, by = c("location_id", "age_group_id", "year_id", "sex_id"), all.x=T)

# merge on age weights
out_dt <- merge(out_dt, age_weights, by = "age_group_id", all.x=T)

# Aggreagate up location hierarchy

for(loc in hierarchy[most_detailed == 0, location_id]){
  
  if(loc == 1){
    agg_dt <- copy(out_dt)
  }else{
    child_locs <- hierarchy[most_detailed == 1 & (parent_id == loc | grepl(paste0(",", loc, ","), path_to_top_parent)), location_id]
    
    agg_dt <- out_dt[location_id %in% child_locs] 
  }
  
  agg_dt <- agg_dt[, .(location_id = loc, expected = weighted.mean(expected, w = population), observed = weighted.mean(observed, w = population),
                       population = sum(population)),
                   by = c("age_group_id", "year_id", "sex_id", "measure_id", "metric_id", "measure_name", "metric_name", "age_group_weight_value", "sdi")]
  
  out_dt <- rbind(out_dt, agg_dt, use.names = T)
  
}


# Aggregate to desired age groups

ages <- list("Under 1" = c(2, 3, 388, 389),
             "Under 5" = c(2, 3, 388, 389, 238, 34),
             "15 - 49" = 8:14,
             "50 - 69" = 15:18,
             "70+" = c(19,20, 20:32, 235), 
             "All Ages" = unique(out_dt$age_group_id),
             "Age-standardized" = unique(out_dt$age_group_id))

sum_dt <- data.table()

for(i in 1:length(ages)){
  
  agg_dt <- out_dt[age_group_id %in% ages[[i]]]
  
  if(names(ages)[i]=="Age-standardized"){
    agg_dt[, weight := age_group_weight_value]
  }else{
    agg_dt[, weight := population]
  }
  
  agg_dt <- agg_dt[, .(expected = weighted.mean(expected, w = weight), observed = weighted.mean(observed, w = weight), age_group_name = names(ages)[i]),
                   by = c("location_id", "year_id", "measure_id", "metric_id", "measure_name", "metric_name", "sdi")]
  
  sum_dt <- rbind(sum_dt, agg_dt, use.names = T)
  
}


# merge on location names
sum_dt <- merge(sum_dt, hierarchy[, .(location_id, location_name, ihme_loc_id, level, super_region_name, region_name)], by = "location_id", all.x=T)

sum_dt[, ratio := observed/expected]

sum_dt <- merge(sum_dt, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))


write.csv(sum_dt, file.path(out_dir, "summary_results_sdi.csv"), row.names = F)








###################################################################################################
###################################################################################################
################################### EXTREME UNDERWEIGHT ###########################################
###################################################################################################
###################################################################################################


out_dir <- file.path("filepath")



#reading in the dataset, writing out as csv so it's more easily accessible if needed later

ex_underweight_df <- get_draws("modelable_entity_id", 26942, year_id=c(1990:2020),
                               source="epi", gbd_round_id=7, decomp_step="iterative",
                               age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = standard.locations)
# 
ex_underweight_df <- melt(ex_underweight_df, id.vars = id.vars)
ex_underweight_df <- prepare_df_format(df = ex_underweight_df)
write.csv(ex_underweight_df, "filepath", row.names = F)
ex_underweight_df <- fread("filepath")



# merge on uhc
uhc <- get_covariate_estimates(1097, gbd_round_id = 7, decomp_step = "iterative")

sdi <- get_covariate_estimates(881, gbd_round_id = 7, decomp_step = "iterative")


ex_underweight_df <- merge(ex_underweight_df, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))


ex_underweight_df <- merge(ex_underweight_df, sdi[,.(location_id, year_id, sdi = mean_value)], all.x=T, by = c("location_id", "year_id"))

ex_underweight_df <- merge(ex_underweight_df, hierarchy[, .(location_id, super_region_name)], all.x=T, by = "location_id")

ex_underweight_df$age_group_name <- factor(ex_underweight_df$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))

# run age-, sex-, measure-specific splines

out_dt <- data.table()

pdf(file.path(out_dir, "underweight_extreme_spline_plots_sdi.pdf"), height = 7.5, width = 14)

for(mid in unique(ex_underweight_df$measure_id)){
  for(sid in unique(ex_underweight_df$sex_id)){
    for(agid in unique(ex_underweight_df$age_group_id)){
      
      data_dt <- ex_underweight_df[age_group_id == agid & sex_id == sid & measure_id == mid]
      
      
      # use offset before logit
      data_dt[val<1e-7, val := 1e-7]
      data_dt[val>(1-1e-7), val := (1-1e-7)]
      data_dt[, logit_val := logit(val)]
      
      # make SD standard across all locations
      data_dt[, se := 1]
      
      
      
      n_knots=6 
      start=min(uhc$mean_value) 
      prior.start <- max(data_dt[val > 1e-7]$uhc)
      end = max(uhc$mean_value)
      
      
      
      min_dist=0.1
      
      knots_samples <- utils$sample_knots(
        num_intervals = 5L, # n_knots-1,
        knot_bounds = matrix(rep(c(start, end), each=n_knots-2), ncol=2),  # num_intervals-1 knot_bounds specified
        # num rows=num knots n_knots-2=num interior knots and repeat by num of iterior knot times; same for end
        # rbind(start,end repeated for x times internal knots)
        interval_sizes = rbind(c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.)),  # OR like: matrix(rep(c(min_dist, 1.), each=n_knots-1), ncol=2),  # num_intervals = interval_sizes specified
        # ^controls intervals between knots , interval length
        # all proportional to domain; maximum distance of knots is 1 follow same set-up logic as b
        num_samples = 20L # change to 20L once done testing
      )
      ensemble_cov_model1 <- LinearCovModel(
        alt_cov = "uhc",
        use_spline = TRUE,
        spline_knots = array(c(0,0.2, .4, .6, .8, 1)),  # number of specified splines needs to match n_knots; e.g. array(seq(0, 1, by = 0.25)); specifications OVERWRITTEN by ensemble splines specified when using MRBeRT!
        spline_degree = 2L, #3L
        spline_knots_type = 'domain',
        # prior_spline_monotonicity = "increasing",
        prior_spline_derval_gaussian = array(c(1e-7, 1e-7)),
        prior_spline_derval_gaussian_domain = array(c(prior.start, end)),
        #prior_spline_funval_uniform = array(c(1e-6, 1e-6)),
        #prior_spline_funval_uniform_domain = array(c(prior.start, end)),
        use_re = FALSE,
        use_spline_intercept = FALSE,
        spline_r_linear = TRUE,
        spline_l_linear = TRUE,
        prior_spline_monotonicity = "decreasing"
        ## HERE is where can add more priors and other features to splines!
      )
      
      
      
      dat_1 <- MRData()
      dat_1$load_df(
        data = data_dt,  col_obs = "logit_val", col_obs_se = "se",
        col_covs = list("uhc", "sdi"), col_study_id = "super_region_name"
      )
      
      
      
      
      model <- MRBeRT(data = dat_1,  # e for ensemble splines
                      ensemble_cov_model = ensemble_cov_model1,
                      ensemble_knots = knots_samples,
                      cov_models = list(LinearCovModel("intercept", use_re=FALSE),
                                        LinearCovModel("sdi")), inlier_pct = .99)
      
      model$fit_model(inner_max_iter = 30L) 
      
      
      
      
      
      pred_dt <- rbind(data_dt, data.table(uhc = c(0,1)), fill = T)
      
      pred_dt <- pred_dt[order(uhc)]
      
      pred_dt <- rbind(data.table(pred_dt, sdi=0.2),
                       data.table(pred_dt, sdi=0.4),
                       data.table(pred_dt, sdi=0.6),
                       data.table(pred_dt, sdi=0.8),
                       data.table(pred_dt, sdi=1))
      
      dat_pred <- MRData()
      
      dat_pred$load_df(
        data = pred_dt,
        col_covs=list("uhc", "sdi")
      )
      
      #pred_dt$expected <- model$predict(dat_pred) %>% invlogit
      pred_dt$expected <- invlogit(model$predict(dat_pred))
      
      
      out_dt <- rbind(out_dt, pred_dt[!is.na(location_id)])
      
      print(paste0("Done with sex_id ", sid, " age_group_id ", agid))
      
      
      knot.placement <- data.table(model$ensemble_knots, weights = model$weights)
      setnames(knot.placement, old = c("V1", "V2", "V3", "V4", "V5", "V6"), new = c("knot1", "knot2", "knot3", "knot4", "knot5", "knot6"))
      knot.placement[, knot1 := start]
      knot.placement[, knot6 := end]
      write.csv(knot.placement, paste0(out_dir, "knot_placement_", sid, "_", agid, "_sdi.csv"), row.names = F)
      
      
    }
    
    out_dt$age_group_name <- factor(out_dt$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))
    
    
    
    ex_underweight_plot_df <- copy(ex_underweight_df)
    
    ex_underweight_plot_df[val<1e-7, val := 1e-7]
    ex_underweight_plot_df[val>(1-1e-7), val := (1-1e-7)]
    ex_underweight_plot_df[, logit_val := logit(val)]
    
    
    
    
    gg <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=logit_val, x=uhc * 100))+
      geom_point(data = ex_underweight_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = logit(expected), group = sdi))+
      theme_bw()+
      scale_color_manual(values = custom.col.sr)+
      labs(title = paste0("Extreme Underweight (WAZ < -4 SD) ", unique(data_dt$measure_name), " among ", 
                          unique(data_dt$sex), "s, 1990-2020"),
           y = paste(unique(data_dt$measure_name), "(logit)"), 
           x = "Universal Health Coverage Index",
           color = "Super Region")+
      facet_wrap(~age_group_name, scales = 'free')+
      theme(legend.position="bottom", 
            legend.title.align = .5,
            legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90")) +
      guides(color = guide_legend(override.aes = list(alpha = .5), title.position = "top")) +
      #scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    
    gg2 <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=val, x=uhc * 100))+
      geom_point(data = ex_underweight_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = expected, group = sdi))+
      theme_bw()+
      scale_color_manual(values = custom.col.sr)+
      labs(title = paste0("Extreme Underweight (WAZ < -4 SD) ", unique(data_dt$measure_name), " among ", 
                          unique(data_dt$sex), "s, 1990-2020"),
           y = paste(unique(data_dt$measure_name)), 
           x = "Universal Health Coverage Index",
           color = "Super Region")+
      facet_wrap(~age_group_name, scales = 'free')+
      theme(legend.position="bottom", 
            legend.title.align = .5,
            legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90")) +
      guides(color = guide_legend(override.aes = list(alpha = .5), title.position = "top")) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    
    
    print(gg)
    print(gg2)
  }
}

dev.off()



write.csv(out_dt, file.path(out_dir, "observed_and_expected_sdi.csv"), row.names = F)

out_dt <- fread(file.path(out_dir, "observed_and_expected_sdi.csv"))


out_dt <- out_dt[, .(location_id, age_group_id, year_id, sex_id, measure_id, metric_id, expected, observed = val, measure_name, metric_name, sdi)]


# Create aggregates -------------------------------------------------------

# merge on populations

population <- get_population(gbd_round_id = 7, decomp_step = "iterative",
                             location_id = unique(out_dt$location_id), 
                             age_group_id = unique(out_dt$age_group_id),
                             sex_id = unique(out_dt$sex_id),
                             year_id = unique(out_dt$year_id))

population[, run_id := NULL]

out_dt <- merge(out_dt, population, by = c("location_id", "age_group_id", "year_id", "sex_id"), all.x=T)

# merge on age weights
out_dt <- merge(out_dt, age_weights, by = "age_group_id", all.x=T)

# Aggreagate up location hierarchy

for(loc in hierarchy[most_detailed == 0, location_id]){
  
  if(loc == 1){
    agg_dt <- copy(out_dt)
  }else{
    child_locs <- hierarchy[most_detailed == 1 & (parent_id == loc | grepl(paste0(",", loc, ","), path_to_top_parent)), location_id]
    
    agg_dt <- out_dt[location_id %in% child_locs] 
  }
  
  agg_dt <- agg_dt[, .(location_id = loc, expected = weighted.mean(expected, w = population), observed = weighted.mean(observed, w = population),
                       population = sum(population)),
                   by = c("age_group_id", "year_id", "sex_id", "measure_id", "metric_id", "measure_name", "metric_name", "age_group_weight_value", "sdi")]
  
  out_dt <- rbind(out_dt, agg_dt, use.names = T)
  
}


# Aggregate to desired age groups

ages <- list("Under 1" = c(2, 3, 388, 389),
             "Under 5" = c(2, 3, 388, 389, 238, 34),
             "15 - 49" = 8:14,
             "50 - 69" = 15:18,
             "70+" = c(19,20, 20:32, 235), 
             "All Ages" = unique(out_dt$age_group_id),
             "Age-standardized" = unique(out_dt$age_group_id))

sum_dt <- data.table()

for(i in 1:length(ages)){
  
  agg_dt <- out_dt[age_group_id %in% ages[[i]]]
  
  if(names(ages)[i]=="Age-standardized"){
    agg_dt[, weight := age_group_weight_value]
  }else{
    agg_dt[, weight := population]
  }
  
  agg_dt <- agg_dt[, .(expected = weighted.mean(expected, w = weight), observed = weighted.mean(observed, w = weight), age_group_name = names(ages)[i]),
                   by = c("location_id", "year_id", "measure_id", "metric_id", "measure_name", "metric_name", "sdi")]
  
  sum_dt <- rbind(sum_dt, agg_dt, use.names = T)
  
}


# merge on location names
sum_dt <- merge(sum_dt, hierarchy[, .(location_id, location_name, ihme_loc_id, level, super_region_name, region_name)], by = "location_id", all.x=T)

sum_dt[, ratio := observed/expected]

sum_dt <- merge(sum_dt, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))


write.csv(sum_dt, file.path(out_dir, "summary_results_sdi.csv"), row.names = F)


















