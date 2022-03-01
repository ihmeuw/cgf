# CGF Paper Epidemiological Transisition Analysis



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

mod_stunting_df <- merge(mod_stunting_df, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))

mod_stunting_df <- merge(mod_stunting_df, hierarchy[, .(location_id, super_region_name)], all.x=T, by = "location_id")

mod_stunting_df$age_group_name <- factor(mod_stunting_df$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))

# run age-, sex-, measure-specific splines

out_dt <- data.table()

pdf(file.path(out_dir, "stunting_overall_spline_plots.pdf"), height = 7.5, width = 14)

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
        col_covs = list("uhc"), col_study_id = "super_region_name"
      )
      
      
      
      
      model <- MRBeRT(data = dat_1,  # e for ensemble splines
                      ensemble_cov_model = ensemble_cov_model1,
                      ensemble_knots = knots_samples,
                      cov_models = list(LinearCovModel("intercept", use_re=FALSE)), inlier_pct = .99)
      
      model$fit_model(inner_max_iter = 30L) 
      
      
      
      
      
      pred_dt <- rbind(data_dt, data.table(uhc = c(0,1)), fill = T)
      
      pred_dt <- pred_dt[order(uhc)]
      
      dat_pred <- MRData()
      
      dat_pred$load_df(
        data = pred_dt,
        col_covs=list("uhc")
      )
      
      #pred_dt$expected <- model$predict(dat_pred) %>% invlogit
      pred_dt$expected <- invlogit(model$predict(dat_pred))
      
      
      out_dt <- rbind(out_dt, pred_dt[!is.na(location_id)])
      
      print(paste0("Done with sex_id ", sid, " age_group_id ", agid))
      
      
      knot.placement <- data.table(model$ensemble_knots, weights = model$weights)
      setnames(knot.placement, old = c("V1", "V2", "V3", "V4", "V5", "V6"), new = c("knot1", "knot2", "knot3", "knot4", "knot5", "knot6"))
      knot.placement[, knot1 := start]
      knot.placement[, knot6 := end]
      write.csv(knot.placement, paste0(out_dir, "knot_placement_", sid, "_", agid, ".csv"), row.names = F)
    }
    
    out_dt$age_group_name <- factor(out_dt$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))
    
    mod_stunting_plot_df <- copy(mod_stunting_df)
    
    mod_stunting_plot_df[val<1e-7, val := 1e-7]
    mod_stunting_plot_df[val>(1-1e-7), val := (1-1e-7)]
    mod_stunting_plot_df[, logit_val := logit(val)]
    
    
    gg <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=logit_val, x=uhc*100))+
      geom_point(data = mod_stunting_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = logit(expected)))+
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
      geom_line(aes(y = expected))+
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


write.csv(out_dt, file.path(out_dir, "observed_and_expected.csv"), row.names = F)

out_dt <- fread(file.path(out_dir, "observed_and_expected.csv"))



out_dt <- out_dt[, .(location_id, age_group_id, year_id, sex_id, measure_id, metric_id, expected, observed = val, measure_name, metric_name)]


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
                   by = c("age_group_id", "year_id", "sex_id", "measure_id", "metric_id", "measure_name", "metric_name", "age_group_weight_value")]
  
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
                   by = c("location_id", "year_id", "measure_id", "metric_id", "measure_name", "metric_name")]
  
  sum_dt <- rbind(sum_dt, agg_dt, use.names = T)
  
}


# merge on location names
sum_dt <- merge(sum_dt, hierarchy[, .(location_id, location_name, ihme_loc_id, level, super_region_name, region_name)], by = "location_id", all.x=T)

sum_dt[, ratio := observed/expected]

sum_dt <- merge(sum_dt, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))


write.csv(sum_dt, file.path(out_dir, "summary_results.csv"), row.names = F)



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


sev_stunting_df <- merge(sev_stunting_df, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))

sev_stunting_df <- merge(sev_stunting_df, hierarchy[, .(location_id, super_region_name)], all.x=T, by = "location_id")

sev_stunting_df$age_group_name <- factor(sev_stunting_df$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))

# run age-, sex-, measure-specific splines

out_dt <- data.table()

pdf(file.path(out_dir, "stunting_severe_spline_plots.pdf"), height = 7.5, width = 14)

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
        col_covs = list("uhc"), col_study_id = "super_region_name"
      )
      
      
      
      
      model <- MRBeRT(data = dat_1,  # e for ensemble splines
                      ensemble_cov_model = ensemble_cov_model1,
                      ensemble_knots = knots_samples,
                      cov_models = list(LinearCovModel("intercept", use_re=FALSE)), inlier_pct = .99)
      
      model$fit_model(inner_max_iter = 30L) 
      
      
      
      
      
      pred_dt <- rbind(data_dt, data.table(uhc = c(0,1)), fill = T)
      
      pred_dt <- pred_dt[order(uhc)]
      
      dat_pred <- MRData()
      
      dat_pred$load_df(
        data = pred_dt,
        col_covs=list("uhc")
      )
      
      #pred_dt$expected <- model$predict(dat_pred) %>% invlogit
      pred_dt$expected <- invlogit(model$predict(dat_pred))
      
      
      out_dt <- rbind(out_dt, pred_dt[!is.na(location_id)])
      
      print(paste0("Done with sex_id ", sid, " age_group_id ", agid))
      
      
      knot.placement <- data.table(model$ensemble_knots, weights = model$weights)
      setnames(knot.placement, old = c("V1", "V2", "V3", "V4", "V5", "V6"), new = c("knot1", "knot2", "knot3", "knot4", "knot5", "knot6"))
      knot.placement[, knot1 := start]
      knot.placement[, knot6 := end]
      write.csv(knot.placement, paste0(out_dir, "knot_placement_", sid, "_", agid, ".csv"), row.names = F)
    }
    
    out_dt$age_group_name <- factor(out_dt$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))
    
    sev_stunting_plot_df <- copy(sev_stunting_df)
    
    sev_stunting_plot_df[val<1e-7, val := 1e-7]
    sev_stunting_plot_df[val>(1-1e-7), val := (1-1e-7)]
    sev_stunting_plot_df[, logit_val := logit(val)]
    
    
    gg <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=logit_val, x=uhc* 100))+
      geom_point(data = sev_stunting_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = logit(expected)))+
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
      geom_line(aes(y = expected))+
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


write.csv(out_dt, file.path(out_dir, "observed_and_expected.csv"), row.names = F)

out_dt <- fread(file.path(out_dir, "observed_and_expected.csv"))


out_dt <- out_dt[, .(location_id, age_group_id, year_id, sex_id, measure_id, metric_id, expected, observed = val, measure_name, metric_name)]


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
                   by = c("age_group_id", "year_id", "sex_id", "measure_id", "metric_id", "measure_name", "metric_name", "age_group_weight_value")]
  
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
                   by = c("location_id", "year_id", "measure_id", "metric_id", "measure_name", "metric_name")]
  
  sum_dt <- rbind(sum_dt, agg_dt, use.names = T)
  
}


# merge on location names
sum_dt <- merge(sum_dt, hierarchy[, .(location_id, location_name, ihme_loc_id, level, super_region_name, region_name)], by = "location_id", all.x=T)

sum_dt[, ratio := observed/expected]

sum_dt <- merge(sum_dt, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))


write.csv(sum_dt, file.path(out_dir, "summary_results.csv"), row.names = F)





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


ex_stunting_df <- merge(ex_stunting_df, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))

ex_stunting_df <- merge(ex_stunting_df, hierarchy[, .(location_id, super_region_name)], all.x=T, by = "location_id")

ex_stunting_df$age_group_name <- factor(ex_stunting_df$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))

# run age-, sex-, measure-specific splines

out_dt <- data.table()

pdf(file.path(out_dir, "stunting_extreme_spline_plots.pdf"), height = 7.5, width = 14)

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
                      cov_models = list(LinearCovModel("intercept", use_re=FALSE)), inlier_pct = .99)
      
      model$fit_model(inner_max_iter = 30L) 
      
      
      
      
      
      pred_dt <- rbind(data_dt, data.table(uhc = c(0,1)), fill = T)
      
      pred_dt <- pred_dt[order(uhc)]
      
      dat_pred <- MRData()
      
      dat_pred$load_df(
        data = pred_dt,
        col_covs=list("uhc")
      )
      
      #pred_dt$expected <- model$predict(dat_pred) %>% invlogit
      pred_dt$expected <- invlogit(model$predict(dat_pred))
      
      
      out_dt <- rbind(out_dt, pred_dt[!is.na(location_id)])
      
      print(paste0("Done with sex_id ", sid, " age_group_id ", agid))
      
      
      knot.placement <- data.table(model$ensemble_knots, weights = model$weights)
      setnames(knot.placement, old = c("V1", "V2", "V3", "V4", "V5", "V6"), new = c("knot1", "knot2", "knot3", "knot4", "knot5", "knot6"))
      knot.placement[, knot1 := start]
      knot.placement[, knot6 := end]
      write.csv(knot.placement, paste0(out_dir, "knot_placement_", sid, "_", agid, ".csv"), row.names = F)
    }
    
    out_dt$age_group_name <- factor(out_dt$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))
    
    ex_stunting_plot_df <- copy(ex_stunting_df)
    
    ex_stunting_plot_df[val<1e-7, val := 1e-7]
    ex_stunting_plot_df[val>(1-1e-7), val := (1-1e-7)]
    ex_stunting_plot_df[, logit_val := logit(val)]
    
    
    gg <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=logit_val, x=uhc*100))+
      geom_point(data = ex_stunting_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = logit(expected)))+
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
      geom_line(aes(y = expected))+
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



write.csv(out_dt, file.path(out_dir, "observed_and_expected.csv"), row.names = F)

out_dt <- fread(file.path(out_dir, "observed_and_expected.csv"))


out_dt <- out_dt[, .(location_id, age_group_id, year_id, sex_id, measure_id, metric_id, expected, observed = val, measure_name, metric_name)]


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
                   by = c("age_group_id", "year_id", "sex_id", "measure_id", "metric_id", "measure_name", "metric_name", "age_group_weight_value")]
  
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
                   by = c("location_id", "year_id", "measure_id", "metric_id", "measure_name", "metric_name")]
  
  sum_dt <- rbind(sum_dt, agg_dt, use.names = T)
  
}


# merge on location names
sum_dt <- merge(sum_dt, hierarchy[, .(location_id, location_name, ihme_loc_id, level, super_region_name, region_name)], by = "location_id", all.x=T)

sum_dt[, ratio := observed/expected]

sum_dt <- merge(sum_dt, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))


write.csv(sum_dt, file.path(out_dir, "summary_results.csv"), row.names = F)




###################################################################################################
###################################################################################################
####################################### PLOTTING STUNTING #########################################
###################################################################################################
###################################################################################################


# reading in epi-transition output for all stunting severities
overall.stunting.epitrans <- fread("filepath")
severe.stunting.epitrans <- fread("filepath")
extreme.stunting.epitrans <- fread("filepath")


overall.stunting.epitrans[, cgf.type := "HAZ"]
severe.stunting.epitrans[, cgf.type := "HAZ"]
extreme.stunting.epitrans[, cgf.type := "HAZ"]

overall.stunting.epitrans[, severity := "Overall"]
severe.stunting.epitrans[, severity := "Severe"]
extreme.stunting.epitrans[, severity := "Extreme"]


haz.epitrans <- rbind(overall.stunting.epitrans, severe.stunting.epitrans, extreme.stunting.epitrans)
haz.epitrans <- haz.epitrans[age_group_name == "Under 5"]
haz.epitrans[, age_group_id := 1]


# need to aggregate UHC levels up to the regions, super regions, and global to add this on the dataframe

uhc <- get_covariate_estimates(1097, gbd_round_id = 7, decomp_step = "iterative")
uhc <- uhc[year_id %in% c(1990:2020)]

total.pop <- get_population(gbd_round_id = 7, decomp_step = 'iterative', location_id = unique(uhc$location_id), age_group_id = 22, sex_id = 3, year_id = c(1990:2020))
total.pop$run_id <- NULL
uhc <- merge(uhc, total.pop, by = c("age_group_id", "location_id", "year_id", "sex_id"))
uhc <- uhc[, c("age_group_id", "location_id", "year_id", "sex_id", "mean_value", "population")]


agg.locs.uhc <- lapply(hierarchy[most_detailed == 0 & location_id != 1]$location_id, function(loc){
  
  
  if(loc == 1){
    agg_dt <- copy(uhc)
  }else{
    child_locs <- hierarchy[most_detailed == 1 & (parent_id == loc | grepl(paste0(",", loc, ","), path_to_top_parent)), location_id]
    
    child_uhcs <- uhc[location_id %in% child_locs] 
  }
  
  agg.uhc <- child_uhcs[, .(location_id = loc, uhc = weighted.mean(mean_value, w = population),
                            population = sum(population)),
                        by = c("age_group_id", "year_id", "sex_id")]
  
  return(agg.uhc)
  
})

agg.locs.uhc <- rbindlist(agg.locs.uhc)
setnames(agg.locs.uhc, "uhc", "agg.uhc")
agg.locs.uhc <- agg.locs.uhc[, -c("age_group_id", "population", "sex_id")]


# now need to insert these aggregated uhc estimates into the epitrans dataframe

haz.epitrans <- merge(haz.epitrans, agg.locs.uhc, by = c("location_id", "year_id"), all.x = T)
haz.epitrans[level <3, uhc:= agg.uhc]
haz.epitrans$agg.uhc <- NULL
haz.epitrans <- haz.epitrans[location_id != 1]


#doing this to help with plotting eventually
haz.epitrans[, loc.sev := paste0(location_id, "_", severity)]





# first need to create scaled versions of the three splines and smooth them

overall.haz.spline <- haz.epitrans[level > 2 & severity == "Overall", c("uhc", "expected")]
overall.haz.spline <- overall.haz.spline[order(uhc),]

severe.haz.spline <- haz.epitrans[level > 2 & severity == "Severe", c("uhc", "expected")]
severe.haz.spline <- severe.haz.spline[order(uhc),]

extreme.haz.spline <- haz.epitrans[level > 2 & severity == "Extreme", c("uhc", "expected")]
extreme.haz.spline <- extreme.haz.spline[order(uhc),]

#getting 500 UHC values along this span to smooth out
min.uhc <- min(overall.haz.spline$uhc)
max.uhc <- max(overall.haz.spline$uhc)
uhc.vals <- seq(from = min.uhc, to = max.uhc, length.out = 500)


#getting the initial overall spline values at those UHC points
overall.uhc.spaced.vals <- lapply(uhc.vals, function(uval){
  
  estimate <- overall.haz.spline[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, overall.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial severe spline values at those UHC points
severe.uhc.spaced.vals <- lapply(uhc.vals, function(uval){
  
  estimate <- severe.haz.spline[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, severe.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial extreme spline values at those UHC points
extreme.uhc.spaced.vals <- lapply(uhc.vals, function(uval){
  
  estimate <- extreme.haz.spline[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, extreme.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#now calculating a rolling average 
overall.uhc.spaced.vals <- overall.uhc.spaced.vals %>% 
  mutate(roll_mean = rollmean(overall.spline, 10, na.pad = T))



severe.uhc.spaced.vals <- severe.uhc.spaced.vals %>% 
  mutate(roll_mean = rollmean(severe.spline, 10, na.pad = T))


extreme.uhc.spaced.vals <- extreme.uhc.spaced.vals %>% 
  mutate(roll_mean = rollmean(extreme.spline, 10, na.pad = T))



overall.uhc.spaced.vals$overall.spline <- NULL
severe.uhc.spaced.vals$severe.spline <- NULL
extreme.uhc.spaced.vals$extreme.spline <- NULL


overall.uhc.spaced.vals <- data.table(overall.uhc.spaced.vals)
severe.uhc.spaced.vals <- data.table(severe.uhc.spaced.vals)
extreme.uhc.spaced.vals <- data.table(extreme.uhc.spaced.vals)

overall.uhc.spaced.vals <- overall.uhc.spaced.vals[!is.na(roll_mean)]
severe.uhc.spaced.vals <- severe.uhc.spaced.vals[!is.na(roll_mean)]
extreme.uhc.spaced.vals <- extreme.uhc.spaced.vals[!is.na(roll_mean)]

# merging all smoothed splines together
setnames(overall.uhc.spaced.vals, "roll_mean", "overall_expected")
setnames(severe.uhc.spaced.vals, "roll_mean", "severe_expected")
setnames(extreme.uhc.spaced.vals, "roll_mean", "extreme_expected")

stunting.smoothed.splines <- merge(overall.uhc.spaced.vals, severe.uhc.spaced.vals, by = "uhc_value")
stunting.smoothed.splines <- merge(stunting.smoothed.splines, extreme.uhc.spaced.vals, by = "uhc_value")

stunt.severe.to.overall.scalar <- stunting.smoothed.splines[1]$overall_expected / stunting.smoothed.splines[1]$severe_expected
stunt.exreme.to.overall.scalar <- stunting.smoothed.splines[1]$overall_expected / stunting.smoothed.splines[1]$extreme_expected
stunt.extreme.to.severe.scalar <- stunting.smoothed.splines[1]$severe_expected / stunting.smoothed.splines[1]$extreme_expected

# columns named as <what they're scaled to match>.scaled.<what they were originally>
stunting.smoothed.splines[, overall.scaled.severe := severe_expected * stunt.severe.to.overall.scalar]
stunting.smoothed.splines[, overall.scaled.extreme := extreme_expected * stunt.exreme.to.overall.scalar]

stunting.smoothed.splines[, severe.scaled.overall := overall_expected / stunt.severe.to.overall.scalar]
stunting.smoothed.splines[, severe.scaled.extreme := extreme_expected * stunt.extreme.to.severe.scalar]

stunting.smoothed.splines[, extreme.scaled.overall := overall_expected / stunt.exreme.to.overall.scalar]
stunting.smoothed.splines[, extreme.scaled.severe := severe_expected / stunt.extreme.to.severe.scalar]






# these plots would be for main body


haz.epitrans$region_name <- factor(haz.epitrans$region_name, levels = c("Central Asia", "Central Europe", "Eastern Europe",
                                                                        "Australasia", "High-income Asia Pacific", "High-income North America",
                                                                        "Southern Latin America", "Western Europe",
                                                                        "Andean Latin America", "Caribbean", "Central Latin America", "Tropical Latin America",
                                                                        "North Africa and Middle East", "South Asia",
                                                                        "East Asia", "Oceania", "Southeast Asia", 
                                                                        "Central Sub-Saharan Africa", "Eastern Sub-Saharan Africa",
                                                                        "Southern Sub-Saharan Africa", "Western Sub-Saharan Africa"))


haz.epitrans <- haz.epitrans[order(year_id)]




stunting.smoothed.splines.test.melted <- melt(stunting.smoothed.splines, id.vars = "uhc_value")
stunting.smoothed.splines.test.melted$variable <- factor(stunting.smoothed.splines.test.melted$variable, 
                                                         levels = c("overall_expected", "overall.scaled.severe", "overall.scaled.extreme",
                                                                    "severe.scaled.overall", "severe_expected", "severe.scaled.extreme",
                                                                    "extreme.scaled.overall", "extreme.scaled.severe", "extreme_expected"))


# these are for supplemental materials

haz.epitrans[, cgf.type.label := "Stunting (HAZ)"]



#this plot shows the smoothed splines for SEVERE
region.plot.severe.stunting<- ggplot() +
  geom_line(data = stunting.smoothed.splines.test.melted[variable == "severe_expected"], aes(x = uhc_value, y = value ), color = "black", size = 1.5) +
  geom_path(data = haz.epitrans[level == 2 & severity == "Severe" & cgf.type == "HAZ" & !is.na(region_name)], aes(x = uhc, y = observed, group = location_id, color = region_name), alpha = 0.9, size = 1)+
  geom_point(data = haz.epitrans[level == 2 & severity == "Severe" & cgf.type == "HAZ" & !is.na(region_name) & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id, fill = region_name, shape = region_name), color = 'black', alpha = 0.9, size = 3)+
  scale_shape_manual(values = c(21, 22, 23, # Central Europe
                                21, 22, 23, 24, 25, #High Income
                                21, 22, 23, 24, # Latin America
                                21, #North Africa Middle East
                                21, # South Asia
                                21, 22, 23, #SE Asia
                                21, 22, 23, 24 ),guide=guide_legend(title="Region",ncol=1, title.hjust = 0.5) ) + #Sub Saharan Africa
  scale_color_manual(values = c("#E6AB02" , "#E6AB02" , "#E6AB02" , #Central Asia and Europe
                                "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , #High Income
                                "#7570B3" , "#7570B3" , "#7570B3" , "#7570B3" , # Latin America
                                "#E7298A" , #North Africa Middle East
                                "#66A61E"   , # South Asia
                                "#1B9E77" , "#1B9E77" , "#1B9E77" , # SE Asia 
                                "#A6761D" ,  "#A6761D" ,  "#A6761D" ,  "#A6761D"  ), guide=guide_legend(title="Region",ncol=1, title.hjust = 0.5)) + #Sub Saharan Africa
  scale_fill_manual(values = c("#E6AB02" , "#E6AB02" , "#E6AB02" , #Central Asia and Europe
                               "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , #High Income
                               "#7570B3" , "#7570B3" , "#7570B3" , "#7570B3" , # Latin America
                               "#E7298A" , #North Africa Middle East
                               "#66A61E"   , # South Asia
                               "#1B9E77" , "#1B9E77" , "#1B9E77" , # SE Asia 
                               "#A6761D" ,  "#A6761D" ,  "#A6761D" ,  "#A6761D"  ), guide=guide_legend(title="Region",ncol=1, title.hjust = 0.5)) + #Sub Saharan Africa
  labs(x = "Universal Health Coverage Index", y = "Prevalence", color = "Region", shape = "Region", fill = "Region", tag = "A", title = "Severe CGF Epidemiological Transition Analysis by Region") +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 18),
        legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
        legend.key = element_rect(fill = "grey90"),
        plot.tag = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 22, hjust = .95, face = "bold", vjust = 15)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0, .1, .2, .3), limits = c(0, .365)) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  facet_wrap(~cgf.type.label) +
  guides(linetype = guide_legend(keywidth = unit(5, 'cm')))





#this plot shows the smoothed splines for EXTREME 
region.plot.extreme.stunting<- ggplot() +
  geom_line(data = stunting.smoothed.splines.test.melted[variable == "extreme_expected"], aes(x = uhc_value, y = value ), color = "black", size = 1.5) +
  geom_path(data = haz.epitrans[level == 2 & severity == "Extreme" & cgf.type == "HAZ" & !is.na(region_name)], aes(x = uhc, y = observed, group = location_id, color = region_name), alpha = 0.9, size = 1)+
  geom_point(data = haz.epitrans[level == 2 & severity == "Extreme" & cgf.type == "HAZ" & !is.na(region_name) & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id, fill = region_name, shape = region_name), color = 'black', alpha = 0.9, size = 3)+
  scale_shape_manual(values = c(21, 22, 23, # Central Europe
                                21, 22, 23, 24, 25, #High Income
                                21, 22, 23, 24, # Latin America
                                21, #North Africa Middle East
                                21, # South Asia
                                21, 22, 23, #SE Asia
                                21, 22, 23, 24 ),guide=guide_legend(title="Region",ncol=1, title.hjust = 0.5) ) + #Sub Saharan Africa
  scale_color_manual(values = c("#E6AB02" , "#E6AB02" , "#E6AB02" , #Central Asia and Europe
                                "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , #High Income
                                "#7570B3" , "#7570B3" , "#7570B3" , "#7570B3" , # Latin America
                                "#E7298A" , #North Africa Middle East
                                "#66A61E"   , # South Asia
                                "#1B9E77" , "#1B9E77" , "#1B9E77" , # SE Asia 
                                "#A6761D" ,  "#A6761D" ,  "#A6761D" ,  "#A6761D"  ), guide=guide_legend(title="Region",ncol=1, title.hjust = 0.5)) + #Sub Saharan Africa
  scale_fill_manual(values = c("#E6AB02" , "#E6AB02" , "#E6AB02" , #Central Asia and Europe
                               "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , #High Income
                               "#7570B3" , "#7570B3" , "#7570B3" , "#7570B3" , # Latin America
                               "#E7298A" , #North Africa Middle East
                               "#66A61E"   , # South Asia
                               "#1B9E77" , "#1B9E77" , "#1B9E77" , # SE Asia 
                               "#A6761D" ,  "#A6761D" ,  "#A6761D" ,  "#A6761D"  ), guide=guide_legend(title="Region",ncol=1, title.hjust = 0.5)) + #Sub Saharan Africa
  labs(x = "Universal Health Coverage Index", y = "Prevalence", color = "Region", shape = "Region", fill = "Region", tag = "A",title = "Extreme CGF Epidemiological Transition Analysis by Region") +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 18),
        legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
        legend.key = element_rect(fill = "grey90"),
        plot.tag = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 22, hjust = .95, face = "bold", vjust = 15)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0, .02, .04, .06, .08, .1, .12), limits = c(0, .13)) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  facet_wrap(~cgf.type.label) +
  guides(linetype = guide_legend(keywidth = unit(5, 'cm')))





















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

mod_wasting_df <- merge(mod_wasting_df, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))

mod_wasting_df <- merge(mod_wasting_df, hierarchy[, .(location_id, super_region_name)], all.x=T, by = "location_id")

mod_wasting_df$age_group_name <- factor(mod_wasting_df$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))

# run age-, sex-, measure-specific splines

out_dt <- data.table()

pdf(file.path(out_dir, "wasting_overall_spline_plots.pdf"), height = 7.5, width = 14)

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
                      cov_models = list(LinearCovModel("intercept", use_re=FALSE)), inlier_pct = .99)
      
      model$fit_model(inner_max_iter = 30L) 
      
      
      
      
      
      pred_dt <- rbind(data_dt, data.table(uhc = c(0,1)), fill = T)
      
      pred_dt <- pred_dt[order(uhc)]
      
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
      write.csv(knot.placement, paste0(out_dir, "knot_placement_", sid, "_", agid, ".csv"), row.names = F)
    }
    
    out_dt$age_group_name <- factor(out_dt$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))
    
    mod_wasting_plot_df <- copy(mod_wasting_df)
    mod_wasting_plot_df[val<1e-7, val := 1e-7]
    mod_wasting_plot_df[val>(1-1e-7), val := (1-1e-7)]
    mod_wasting_plot_df[, logit_val := logit(val)]
    
    gg <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=logit_val, x=uhc * 100))+
      geom_point(data = mod_wasting_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
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
      geom_line(aes(y = expected))+
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

write.csv(out_dt, file.path(out_dir, "observed_and_expected.csv"), row.names = F)

out_dt <- fread(file.path(out_dir, "observed_and_expected.csv"))



out_dt <- out_dt[, .(location_id, age_group_id, year_id, sex_id, measure_id, metric_id, expected, observed = val, measure_name, metric_name)]


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
                   by = c("age_group_id", "year_id", "sex_id", "measure_id", "metric_id", "measure_name", "metric_name", "age_group_weight_value")]
  
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
                   by = c("location_id", "year_id", "measure_id", "metric_id", "measure_name", "metric_name")]
  
  sum_dt <- rbind(sum_dt, agg_dt, use.names = T)
  
}


# merge on location names
sum_dt <- merge(sum_dt, hierarchy[, .(location_id, location_name, ihme_loc_id, level, super_region_name, region_name)], by = "location_id", all.x=T)

sum_dt[, ratio := observed/expected]

sum_dt <- merge(sum_dt, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))


write.csv(sum_dt, file.path(out_dir, "summary_results.csv"), row.names = F)






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

sev_wasting_df <- merge(sev_wasting_df, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))

sev_wasting_df <- merge(sev_wasting_df, hierarchy[, .(location_id, super_region_name)], all.x=T, by = "location_id")

sev_wasting_df$age_group_name <- factor(sev_wasting_df$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))

# run age-, sex-, measure-specific splines

out_dt <- data.table()

pdf(file.path(out_dir, "wasting_severe_spline_plots.pdf"), height = 7.5, width = 14)

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
        col_covs = list("uhc"), col_study_id = "super_region_name"
      )
      
      
      
      
      model <- MRBeRT(data = dat_1,  # e for ensemble splines
                      ensemble_cov_model = ensemble_cov_model1,
                      ensemble_knots = knots_samples,
                      cov_models = list(LinearCovModel("intercept", use_re=FALSE)), inlier_pct = .99)
      
      model$fit_model(inner_max_iter = 30L) 
      
      
      
      
      
      pred_dt <- rbind(data_dt, data.table(uhc = c(0,1)), fill = T)
      
      pred_dt <- pred_dt[order(uhc)]
      
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
      write.csv(knot.placement, paste0(out_dir, "knot_placement_", sid, "_", agid, ".csv"), row.names = F)
    }
    
    out_dt$age_group_name <- factor(out_dt$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))
    
    sev_wasting_plot_df <- copy(sev_wasting_df)
    
    sev_wasting_plot_df[val<1e-7, val := 1e-7]
    sev_wasting_plot_df[val>(1-1e-7), val := (1-1e-7)]
    sev_wasting_plot_df[, logit_val := logit(val)]
    
    
    gg <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=logit_val, x=uhc*100))+
      geom_point(data = sev_wasting_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = logit(expected)))+
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
      geom_line(aes(y = expected))+
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



write.csv(out_dt, file.path(out_dir, "observed_and_expected.csv"), row.names = F)

out_dt <- fread(file.path(out_dir, "observed_and_expected.csv"))


out_dt <- out_dt[, .(location_id, age_group_id, year_id, sex_id, measure_id, metric_id, expected, observed = val, measure_name, metric_name)]


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
                   by = c("age_group_id", "year_id", "sex_id", "measure_id", "metric_id", "measure_name", "metric_name", "age_group_weight_value")]
  
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
                   by = c("location_id", "year_id", "measure_id", "metric_id", "measure_name", "metric_name")]
  
  sum_dt <- rbind(sum_dt, agg_dt, use.names = T)
  
}


# merge on location names
sum_dt <- merge(sum_dt, hierarchy[, .(location_id, location_name, ihme_loc_id, level, super_region_name, region_name)], by = "location_id", all.x=T)

sum_dt[, ratio := observed/expected]

sum_dt <- merge(sum_dt, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))


write.csv(sum_dt, file.path(out_dir, "summary_results.csv"), row.names = F)








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

ex_wasting_df <- merge(ex_wasting_df, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))

ex_wasting_df <- merge(ex_wasting_df, hierarchy[, .(location_id, super_region_name)], all.x=T, by = "location_id")

ex_wasting_df$age_group_name <- factor(ex_wasting_df$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))

# run age-, sex-, measure-specific splines

out_dt <- data.table()

pdf(file.path(out_dir, "wasting_extreme_spline_plots.pdf"), height = 7.5, width = 14)

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
        col_covs = list("uhc"), col_study_id = "super_region_name"
      )
      
      
      
      
      model <- MRBeRT(data = dat_1,  # e for ensemble splines
                      ensemble_cov_model = ensemble_cov_model1,
                      ensemble_knots = knots_samples,
                      cov_models = list(LinearCovModel("intercept", use_re=FALSE)), inlier_pct = .99)
      
      model$fit_model(inner_max_iter = 30L) 
      
      
      
      
      
      pred_dt <- rbind(data_dt, data.table(uhc = c(0,1)), fill = T)
      
      pred_dt <- pred_dt[order(uhc)]
      
      dat_pred <- MRData()
      
      dat_pred$load_df(
        data = pred_dt,
        col_covs=list("uhc")
      )
      
      #pred_dt$expected <- model$predict(dat_pred) %>% invlogit
      pred_dt$expected <- invlogit(model$predict(dat_pred))
      
      
      out_dt <- rbind(out_dt, pred_dt[!is.na(location_id)])
      
      print(paste0("Done with sex_id ", sid, " age_group_id ", agid))
      
      
      knot.placement <- data.table(model$ensemble_knots, weights = model$weights)
      setnames(knot.placement, old = c("V1", "V2", "V3", "V4", "V5", "V6"), new = c("knot1", "knot2", "knot3", "knot4", "knot5", "knot6"))
      knot.placement[, knot1 := start]
      knot.placement[, knot6 := end]
      write.csv(knot.placement, paste0(out_dir, "knot_placement_", sid, "_", agid, ".csv"), row.names = F)
      
    }
    
    
    
    out_dt$age_group_name <- factor(out_dt$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))
    
    ex_wasting_plot_df <- copy(ex_wasting_df)
    
    ex_wasting_plot_df[val<1e-7, val := 1e-7]
    ex_wasting_plot_df[val>(1-1e-7), val := (1-1e-7)]
    ex_wasting_plot_df[, logit_val := logit(val)]
    
    
    gg <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=logit_val, x=uhc*100))+
      geom_point(data = ex_wasting_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = logit(expected)))+
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
      geom_line(aes(y = expected))+
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



write.csv(out_dt, file.path(out_dir, "observed_and_expected.csv"), row.names = F)

out_dt <- fread(file.path(out_dir, "observed_and_expected.csv"))


out_dt <- out_dt[, .(location_id, age_group_id, year_id, sex_id, measure_id, metric_id, expected, observed = val, measure_name, metric_name)]


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
                   by = c("age_group_id", "year_id", "sex_id", "measure_id", "metric_id", "measure_name", "metric_name", "age_group_weight_value")]
  
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
                   by = c("location_id", "year_id", "measure_id", "metric_id", "measure_name", "metric_name")]
  
  sum_dt <- rbind(sum_dt, agg_dt, use.names = T)
  
}


# merge on location names
sum_dt <- merge(sum_dt, hierarchy[, .(location_id, location_name, ihme_loc_id, level, super_region_name, region_name)], by = "location_id", all.x=T)

sum_dt[, ratio := observed/expected]

sum_dt <- merge(sum_dt, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))


write.csv(sum_dt, file.path(out_dir, "summary_results.csv"), row.names = F)










###################################################################################################
###################################################################################################
####################################### PLOTTING WASTING ##########################################
###################################################################################################
###################################################################################################


# reading in epi-transition output for all wasting severities
overall.wasting.epitrans <- fread("filepath")
severe.wasting.epitrans <- fread("filepath")
extreme.wasting.epitrans <- fread("filepath")


overall.wasting.epitrans[, cgf.type := "WHZ"]
severe.wasting.epitrans[, cgf.type := "WHZ"]
extreme.wasting.epitrans[, cgf.type := "WHZ"]

overall.wasting.epitrans[, severity := "Overall"]
severe.wasting.epitrans[, severity := "Severe"]
extreme.wasting.epitrans[, severity := "Extreme"]


whz.epitrans <- rbind(overall.wasting.epitrans, severe.wasting.epitrans, extreme.wasting.epitrans)
whz.epitrans <- whz.epitrans[age_group_name == "Under 5"]
whz.epitrans[, age_group_id := 1]


# need to aggregate UHC levels up to the regions, super regions, and global to add this on the dataframe

uhc <- get_covariate_estimates(1097, gbd_round_id = 7, decomp_step = "iterative")
uhc <- uhc[year_id %in% c(1990:2020)]

total.pop <- get_population(gbd_round_id = 7, decomp_step = 'iterative', location_id = unique(uhc$location_id), age_group_id = 22, sex_id = 3, year_id = c(1990:2020))
total.pop$run_id <- NULL
uhc <- merge(uhc, total.pop, by = c("age_group_id", "location_id", "year_id", "sex_id"))
uhc <- uhc[, c("age_group_id", "location_id", "year_id", "sex_id", "mean_value", "population")]


agg.locs.uhc <- lapply(hierarchy[most_detailed == 0 & location_id != 1]$location_id, function(loc){
  
  
  if(loc == 1){
    agg_dt <- copy(uhc)
  }else{
    child_locs <- hierarchy[most_detailed == 1 & (parent_id == loc | grepl(paste0(",", loc, ","), path_to_top_parent)), location_id]
    
    child_uhcs <- uhc[location_id %in% child_locs] 
  }
  
  agg.uhc <- child_uhcs[, .(location_id = loc, uhc = weighted.mean(mean_value, w = population),
                            population = sum(population)),
                        by = c("age_group_id", "year_id", "sex_id")]
  
  return(agg.uhc)
  
})

agg.locs.uhc <- rbindlist(agg.locs.uhc)
setnames(agg.locs.uhc, "uhc", "agg.uhc")
agg.locs.uhc <- agg.locs.uhc[, -c("age_group_id", "population", "sex_id")]


# now need to insert these aggregated uhc estimates into the epitrans dataframe

whz.epitrans <- merge(whz.epitrans, agg.locs.uhc, by = c("location_id", "year_id"), all.x = T)
whz.epitrans[level <3, uhc:= agg.uhc]
whz.epitrans$agg.uhc <- NULL
whz.epitrans <- whz.epitrans[location_id != 1]


#doing this to help with plotting eventually
whz.epitrans[, loc.sev := paste0(location_id, "_", severity)]




# first need to create scaled versions of the three splines and smooth them

overall.whz.spline <- whz.epitrans[level > 2 & severity == "Overall", c("uhc", "expected")]
overall.whz.spline <- overall.whz.spline[order(uhc),]

severe.whz.spline <- whz.epitrans[level > 2 & severity == "Severe", c("uhc", "expected")]
severe.whz.spline <- severe.whz.spline[order(uhc),]

extreme.whz.spline <- whz.epitrans[level > 2 & severity == "Extreme", c("uhc", "expected")]
extreme.whz.spline <- extreme.whz.spline[order(uhc),]

#getting 500 UHC values along this span to smooth out
min.uhc <- min(overall.whz.spline$uhc)
max.uhc <- max(overall.whz.spline$uhc)
uhc.vals <- seq(from = min.uhc, to = max.uhc, length.out = 500)



#getting the initial overall spline values at those UHC points
overall.uhc.spaced.vals <- lapply(uhc.vals, function(uval){
  
  estimate <- overall.whz.spline[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, overall.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial severe spline values at those UHC points
severe.uhc.spaced.vals <- lapply(uhc.vals, function(uval){
  
  estimate <- severe.whz.spline[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, severe.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial extreme spline values at those UHC points
extreme.uhc.spaced.vals <- lapply(uhc.vals, function(uval){
  
  estimate <- extreme.whz.spline[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, extreme.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#now calculating a rolling average 
overall.uhc.spaced.vals <- overall.uhc.spaced.vals %>% 
  mutate(roll_mean = rollmean(overall.spline, 10, na.pad = T))



severe.uhc.spaced.vals <- severe.uhc.spaced.vals %>% 
  mutate(roll_mean = rollmean(severe.spline, 10, na.pad = T))



extreme.uhc.spaced.vals <- extreme.uhc.spaced.vals %>% 
  mutate(roll_mean = rollmean(extreme.spline, 10, na.pad = T))



overall.uhc.spaced.vals$overall.spline <- NULL
severe.uhc.spaced.vals$severe.spline <- NULL
extreme.uhc.spaced.vals$extreme.spline <- NULL


overall.uhc.spaced.vals <- data.table(overall.uhc.spaced.vals)
severe.uhc.spaced.vals <- data.table(severe.uhc.spaced.vals)
extreme.uhc.spaced.vals <- data.table(extreme.uhc.spaced.vals)

overall.uhc.spaced.vals <- overall.uhc.spaced.vals[!is.na(roll_mean)]
severe.uhc.spaced.vals <- severe.uhc.spaced.vals[!is.na(roll_mean)]
extreme.uhc.spaced.vals <- extreme.uhc.spaced.vals[!is.na(roll_mean)]

# merging all smoothed splines together
setnames(overall.uhc.spaced.vals, "roll_mean", "overall_expected")
setnames(severe.uhc.spaced.vals, "roll_mean", "severe_expected")
setnames(extreme.uhc.spaced.vals, "roll_mean", "extreme_expected")

wasting.smoothed.splines <- merge(overall.uhc.spaced.vals, severe.uhc.spaced.vals, by = "uhc_value")
wasting.smoothed.splines <- merge(wasting.smoothed.splines, extreme.uhc.spaced.vals, by = "uhc_value")

waste.severe.to.overall.scalar <- wasting.smoothed.splines[1]$overall_expected / wasting.smoothed.splines[1]$severe_expected
waste.exreme.to.overall.scalar <- wasting.smoothed.splines[1]$overall_expected / wasting.smoothed.splines[1]$extreme_expected
waste.extreme.to.severe.scalar <- wasting.smoothed.splines[1]$severe_expected / wasting.smoothed.splines[1]$extreme_expected

# columns named as <what they're scaled to match>.scaled.<what they were originally>
wasting.smoothed.splines[, overall.scaled.severe := severe_expected * waste.severe.to.overall.scalar]
wasting.smoothed.splines[, overall.scaled.extreme := extreme_expected * waste.exreme.to.overall.scalar]

wasting.smoothed.splines[, severe.scaled.overall := overall_expected / waste.severe.to.overall.scalar]
wasting.smoothed.splines[, severe.scaled.extreme := extreme_expected * waste.extreme.to.severe.scalar]

wasting.smoothed.splines[, extreme.scaled.overall := overall_expected / waste.exreme.to.overall.scalar]
wasting.smoothed.splines[, extreme.scaled.severe := severe_expected / waste.extreme.to.severe.scalar]






# these plots would be for main body


whz.epitrans$region_name <- factor(whz.epitrans$region_name, levels = c("Central Asia", "Central Europe", "Eastern Europe",
                                                                        "Australasia", "High-income Asia Pacific", "High-income North America",
                                                                        "Southern Latin America", "Western Europe",
                                                                        "Andean Latin America", "Caribbean", "Central Latin America", "Tropical Latin America",
                                                                        "North Africa and Middle East", "South Asia",
                                                                        "East Asia", "Oceania", "Southeast Asia", 
                                                                        "Central Sub-Saharan Africa", "Eastern Sub-Saharan Africa",
                                                                        "Southern Sub-Saharan Africa", "Western Sub-Saharan Africa"))


whz.epitrans <- whz.epitrans[order(year_id)]





wasting.smoothed.splines.test.melted <- melt(wasting.smoothed.splines, id.vars = "uhc_value")
wasting.smoothed.splines.test.melted$variable <- factor(wasting.smoothed.splines.test.melted$variable, 
                                                        levels = c("overall_expected", "overall.scaled.severe", "overall.scaled.extreme",
                                                                   "severe.scaled.overall", "severe_expected", "severe.scaled.extreme",
                                                                   "extreme.scaled.overall", "extreme.scaled.severe", "extreme_expected"))



# these are for supplemental materials

whz.epitrans[, cgf.type.label := "Wasting (WHZ)"]

#this plot shows the smoothed splines for SEVERE
region.plot.severe.wasting<- ggplot() +
  geom_line(data = wasting.smoothed.splines.test.melted[variable == "severe_expected"], aes(x = uhc_value, y = value ), color = "black", size = 1.5) +
  geom_path(data = whz.epitrans[level == 2 & severity == "Severe" & cgf.type == "WHZ" & !is.na(region_name)], aes(x = uhc, y = observed, group = location_id, color = region_name), alpha = 0.9, size = 1)+
  geom_point(data = whz.epitrans[level == 2 & severity == "Severe" & cgf.type == "WHZ" & !is.na(region_name) & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id, fill = region_name, shape = region_name), color = 'black', alpha = 0.9, size = 3)+
  scale_shape_manual(values = c(21, 22, 23, # Central Europe
                                21, 22, 23, 24, 25, #High Income
                                21, 22, 23, 24, # Latin America
                                21, #North Africa Middle East
                                21, # South Asia
                                21, 22, 23, #SE Asia
                                21, 22, 23, 24 ),guide=guide_legend(title="Region",ncol=1) ) + #Sub Saharan Africa
  scale_color_manual(values = c("#E6AB02" , "#E6AB02" , "#E6AB02" , #Central Asia and Europe
                                "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , #High Income
                                "#7570B3" , "#7570B3" , "#7570B3" , "#7570B3" , # Latin America
                                "#E7298A" , #North Africa Middle East
                                "#66A61E"   , # South Asia
                                "#1B9E77" , "#1B9E77" , "#1B9E77" , # SE Asia 
                                "#A6761D" ,  "#A6761D" ,  "#A6761D" ,  "#A6761D"  ), guide=guide_legend(title="Region",ncol=1)) + #Sub Saharan Africa
  scale_fill_manual(values = c("#E6AB02" , "#E6AB02" , "#E6AB02" , #Central Asia and Europe
                               "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , #High Income
                               "#7570B3" , "#7570B3" , "#7570B3" , "#7570B3" , # Latin America
                               "#E7298A" , #North Africa Middle East
                               "#66A61E"   , # South Asia
                               "#1B9E77" , "#1B9E77" , "#1B9E77" , # SE Asia 
                               "#A6761D" ,  "#A6761D" ,  "#A6761D" ,  "#A6761D"  ), guide=guide_legend(title="Region",ncol=1)) + #Sub Saharan Africa
  labs(x = "Universal Health Coverage Index", y = "Prevalence", color = "Region", shape = "Region", fill = "Region", tag = "B") +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 18),
        legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
        legend.key = element_rect(fill = "grey90"),
        plot.tag = element_text(size = 20, face = "bold")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0, .02, .04, .06, .08), limits = c(0, .09)) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  facet_wrap(~cgf.type.label) +
  guides(linetype = guide_legend(keywidth = unit(5, 'cm')))





#this plot shows the smoothed splines for EXTREME 
region.plot.extreme.wasting<- ggplot() +
  geom_line(data = wasting.smoothed.splines.test.melted[variable == "extreme_expected"], aes(x = uhc_value, y = value ), color = "black", size = 1.5) +
  geom_path(data = whz.epitrans[level == 2 & severity == "Extreme" & cgf.type == "WHZ" & !is.na(region_name)], aes(x = uhc, y = observed, group = location_id, color = region_name), alpha = 0.9, size = 1)+
  geom_point(data = whz.epitrans[level == 2 & severity == "Extreme" & cgf.type == "WHZ" & !is.na(region_name) & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id, fill = region_name, shape = region_name), color = 'black', alpha = 0.9, size = 3)+
  scale_shape_manual(values = c(21, 22, 23, # Central Europe
                                21, 22, 23, 24, 25, #High Income
                                21, 22, 23, 24, # Latin America
                                21, #North Africa Middle East
                                21, # South Asia
                                21, 22, 23, #SE Asia
                                21, 22, 23, 24 ),guide=guide_legend(title="Region",ncol=1) ) + #Sub Saharan Africa
  scale_color_manual(values = c("#E6AB02" , "#E6AB02" , "#E6AB02" , #Central Asia and Europe
                                "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , #High Income
                                "#7570B3" , "#7570B3" , "#7570B3" , "#7570B3" , # Latin America
                                "#E7298A" , #North Africa Middle East
                                "#66A61E"   , # South Asia
                                "#1B9E77" , "#1B9E77" , "#1B9E77" , # SE Asia 
                                "#A6761D" ,  "#A6761D" ,  "#A6761D" ,  "#A6761D"  ), guide=guide_legend(title="Region",ncol=1)) + #Sub Saharan Africa
  scale_fill_manual(values = c("#E6AB02" , "#E6AB02" , "#E6AB02" , #Central Asia and Europe
                               "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , #High Income
                               "#7570B3" , "#7570B3" , "#7570B3" , "#7570B3" , # Latin America
                               "#E7298A" , #North Africa Middle East
                               "#66A61E"   , # South Asia
                               "#1B9E77" , "#1B9E77" , "#1B9E77" , # SE Asia 
                               "#A6761D" ,  "#A6761D" ,  "#A6761D" ,  "#A6761D"  ), guide=guide_legend(title="Region",ncol=1)) + #Sub Saharan Africa
  labs(x = "Universal Health Coverage Index", y = "Prevalence", color = "Region", shape = "Region", fill = "Region", tag = "B") +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 18),
        legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
        legend.key = element_rect(fill = "grey90"),
        plot.tag = element_text(size = 20, face = "bold")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0, .01, .02, .03), limits = c(0, .031)) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  facet_wrap(~cgf.type.label) +
  guides(linetype = guide_legend(keywidth = unit(5, 'cm')))

















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

mod_underweight_df <- merge(mod_underweight_df, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))

mod_underweight_df <- merge(mod_underweight_df, hierarchy[, .(location_id, super_region_name)], all.x=T, by = "location_id")

mod_underweight_df$age_group_name <- factor(mod_underweight_df$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))

# run age-, sex-, measure-specific splines

out_dt <- data.table()

pdf(file.path(out_dir, "underweight_overall_spline_plots.pdf"), height = 7.5, width = 14)

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
        col_covs = list("uhc"), col_study_id = "super_region_name"
      )
      
      
      
      
      model <- MRBeRT(data = dat_1,  # e for ensemble splines
                      ensemble_cov_model = ensemble_cov_model1,
                      ensemble_knots = knots_samples,
                      cov_models = list(LinearCovModel("intercept", use_re=FALSE)), inlier_pct = .99)
      
      model$fit_model(inner_max_iter = 30L) 
      
      
      
      
      
      pred_dt <- rbind(data_dt, data.table(uhc = c(0,1)), fill = T)
      
      pred_dt <- pred_dt[order(uhc)]
      
      dat_pred <- MRData()
      
      dat_pred$load_df(
        data = pred_dt,
        col_covs=list("uhc")
      )
      
      #pred_dt$expected <- model$predict(dat_pred) %>% invlogit
      pred_dt$expected <- invlogit(model$predict(dat_pred))
      
      
      out_dt <- rbind(out_dt, pred_dt[!is.na(location_id)])
      
      print(paste0("Done with sex_id ", sid, " age_group_id ", agid))
      
      
      knot.placement <- data.table(model$ensemble_knots, weights = model$weights)
      setnames(knot.placement, old = c("V1", "V2", "V3", "V4", "V5", "V6"), new = c("knot1", "knot2", "knot3", "knot4", "knot5", "knot6"))
      knot.placement[, knot1 := start]
      knot.placement[, knot6 := end]
      write.csv(knot.placement, paste0(out_dir, "knot_placement_", sid, "_", agid, ".csv"), row.names = F)
    }
    
    out_dt$age_group_name <- factor(out_dt$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))
    
    mod_underweight_plot_df <- copy(mod_underweight_df)
    
    mod_underweight_plot_df[val<1e-7, val := 1e-7]
    mod_underweight_plot_df[val>(1-1e-7), val := (1-1e-7)]
    mod_underweight_plot_df[, logit_val := logit(val)]
    
    
    gg <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=logit_val, x=uhc*100))+
      geom_point(data = mod_underweight_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = logit(expected)))+
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
      geom_line(aes(y = expected))+
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


write.csv(out_dt, file.path(out_dir, "observed_and_expected.csv"), row.names = F)

out_dt <- fread(file.path(out_dir, "observed_and_expected.csv"))



out_dt <- out_dt[, .(location_id, age_group_id, year_id, sex_id, measure_id, metric_id, expected, observed = val, measure_name, metric_name)]


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
                   by = c("age_group_id", "year_id", "sex_id", "measure_id", "metric_id", "measure_name", "metric_name", "age_group_weight_value")]
  
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
                   by = c("location_id", "year_id", "measure_id", "metric_id", "measure_name", "metric_name")]
  
  sum_dt <- rbind(sum_dt, agg_dt, use.names = T)
  
}


# merge on location names
sum_dt <- merge(sum_dt, hierarchy[, .(location_id, location_name, ihme_loc_id, level, super_region_name, region_name)], by = "location_id", all.x=T)

sum_dt[, ratio := observed/expected]

sum_dt <- merge(sum_dt, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))


write.csv(sum_dt, file.path(out_dir, "summary_results.csv"), row.names = F)









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

sev_underweight_df <- merge(sev_underweight_df, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))

sev_underweight_df <- merge(sev_underweight_df, hierarchy[, .(location_id, super_region_name)], all.x=T, by = "location_id")

sev_underweight_df$age_group_name <- factor(sev_underweight_df$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))

# run age-, sex-, measure-specific splines

out_dt <- data.table()

pdf(file.path(out_dir, "underweight_severe_spline_plots.pdf"), height = 7.5, width = 14)

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
        col_covs = list("uhc"), col_study_id = "super_region_name"
      )
      
      
      
      
      model <- MRBeRT(data = dat_1,  # e for ensemble splines
                      ensemble_cov_model = ensemble_cov_model1,
                      ensemble_knots = knots_samples,
                      cov_models = list(LinearCovModel("intercept", use_re=FALSE)), inlier_pct = .99)
      
      model$fit_model(inner_max_iter = 30L) 
      
      
      
      
      
      pred_dt <- rbind(data_dt, data.table(uhc = c(0,1)), fill = T)
      
      pred_dt <- pred_dt[order(uhc)]
      
      dat_pred <- MRData()
      
      dat_pred$load_df(
        data = pred_dt,
        col_covs=list("uhc")
      )
      
      #pred_dt$expected <- model$predict(dat_pred) %>% invlogit
      pred_dt$expected <- invlogit(model$predict(dat_pred))
      
      
      out_dt <- rbind(out_dt, pred_dt[!is.na(location_id)])
      
      print(paste0("Done with sex_id ", sid, " age_group_id ", agid))
      
      
      knot.placement <- data.table(model$ensemble_knots, weights = model$weights)
      setnames(knot.placement, old = c("V1", "V2", "V3", "V4", "V5", "V6"), new = c("knot1", "knot2", "knot3", "knot4", "knot5", "knot6"))
      knot.placement[, knot1 := start]
      knot.placement[, knot6 := end]
      write.csv(knot.placement, paste0(out_dir, "knot_placement_", sid, "_", agid, ".csv"), row.names = F)
      
    }
    
    out_dt$age_group_name <- factor(out_dt$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))
    
    
    
    sev_underweight_plot_df <- copy(sev_underweight_df)
    
    sev_underweight_plot_df[val<1e-7, val := 1e-7]
    sev_underweight_plot_df[val>(1-1e-7), val := (1-1e-7)]
    sev_underweight_plot_df[, logit_val := logit(val)]
    
    
    
    
    gg <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=logit_val, x=uhc *100))+
      geom_point(data = sev_underweight_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = logit(expected)))+
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
      geom_line(aes(y = expected))+
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


write.csv(out_dt, file.path(out_dir, "observed_and_expected.csv"), row.names = F)

out_dt <- fread(file.path(out_dir, "observed_and_expected.csv"))


out_dt <- out_dt[, .(location_id, age_group_id, year_id, sex_id, measure_id, metric_id, expected, observed = val, measure_name, metric_name)]


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
                   by = c("age_group_id", "year_id", "sex_id", "measure_id", "metric_id", "measure_name", "metric_name", "age_group_weight_value")]
  
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
                   by = c("location_id", "year_id", "measure_id", "metric_id", "measure_name", "metric_name")]
  
  sum_dt <- rbind(sum_dt, agg_dt, use.names = T)
  
}


# merge on location names
sum_dt <- merge(sum_dt, hierarchy[, .(location_id, location_name, ihme_loc_id, level, super_region_name, region_name)], by = "location_id", all.x=T)

sum_dt[, ratio := observed/expected]

sum_dt <- merge(sum_dt, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))


write.csv(sum_dt, file.path(out_dir, "summary_results.csv"), row.names = F)








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

ex_underweight_df <- merge(ex_underweight_df, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))

ex_underweight_df <- merge(ex_underweight_df, hierarchy[, .(location_id, super_region_name)], all.x=T, by = "location_id")

ex_underweight_df$age_group_name <- factor(ex_underweight_df$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))

# run age-, sex-, measure-specific splines

out_dt <- data.table()

pdf(file.path(out_dir, "underweight_extreme_spline_plots.pdf"), height = 7.5, width = 14)

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
        col_covs = list("uhc"), col_study_id = "super_region_name"
      )
      
      
      
      
      model <- MRBeRT(data = dat_1,  # e for ensemble splines
                      ensemble_cov_model = ensemble_cov_model1,
                      ensemble_knots = knots_samples,
                      cov_models = list(LinearCovModel("intercept", use_re=FALSE)), inlier_pct = .99)
      
      model$fit_model(inner_max_iter = 30L) 
      
      
      
      
      
      pred_dt <- rbind(data_dt, data.table(uhc = c(0,1)), fill = T)
      
      pred_dt <- pred_dt[order(uhc)]
      
      dat_pred <- MRData()
      
      dat_pred$load_df(
        data = pred_dt,
        col_covs=list("uhc")
      )
      
      #pred_dt$expected <- model$predict(dat_pred) %>% invlogit
      pred_dt$expected <- invlogit(model$predict(dat_pred))
      
      
      out_dt <- rbind(out_dt, pred_dt[!is.na(location_id)])
      
      print(paste0("Done with sex_id ", sid, " age_group_id ", agid))
      
      
      knot.placement <- data.table(model$ensemble_knots, weights = model$weights)
      setnames(knot.placement, old = c("V1", "V2", "V3", "V4", "V5", "V6"), new = c("knot1", "knot2", "knot3", "knot4", "knot5", "knot6"))
      knot.placement[, knot1 := start]
      knot.placement[, knot6 := end]
      write.csv(knot.placement, paste0(out_dir, "knot_placement_", sid, "_", agid, ".csv"), row.names = F)
      
      
    }
    
    out_dt$age_group_name <- factor(out_dt$age_group_name, levels = c("Early Neonatal", "Late Neonatal", "1-5 Months", "6-11 Months", "12-23 Months", "2-4 Years"))
    
    
    
    ex_underweight_plot_df <- copy(ex_underweight_df)
    
    ex_underweight_plot_df[val<1e-7, val := 1e-7]
    ex_underweight_plot_df[val>(1-1e-7), val := (1-1e-7)]
    ex_underweight_plot_df[, logit_val := logit(val)]
    
    
    
    
    gg <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=logit_val, x=uhc * 100))+
      geom_point(data = ex_underweight_plot_df[measure_id == mid & sex_id == sid & year_id %in% c(1990, 2000, 2010, 2020)], 
                 aes(color = super_region_name), alpha = 0.3)+
      geom_line(aes(y = logit(expected)))+
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
      geom_line(aes(y = expected))+
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



write.csv(out_dt, file.path(out_dir, "observed_and_expected.csv"), row.names = F)

out_dt <- fread(file.path(out_dir, "observed_and_expected.csv"))


out_dt <- out_dt[, .(location_id, age_group_id, year_id, sex_id, measure_id, metric_id, expected, observed = val, measure_name, metric_name)]


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
                   by = c("age_group_id", "year_id", "sex_id", "measure_id", "metric_id", "measure_name", "metric_name", "age_group_weight_value")]
  
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
                   by = c("location_id", "year_id", "measure_id", "metric_id", "measure_name", "metric_name")]
  
  sum_dt <- rbind(sum_dt, agg_dt, use.names = T)
  
}


# merge on location names
sum_dt <- merge(sum_dt, hierarchy[, .(location_id, location_name, ihme_loc_id, level, super_region_name, region_name)], by = "location_id", all.x=T)

sum_dt[, ratio := observed/expected]

sum_dt <- merge(sum_dt, uhc[,.(location_id, year_id, uhc = mean_value)], all.x=T, by = c("location_id", "year_id"))


write.csv(sum_dt, file.path(out_dir, "summary_results.csv"), row.names = F)













###################################################################################################
###################################################################################################
################################### PLOTTING UNDERWEIGHT ##########################################
###################################################################################################
###################################################################################################


# reading in epi-transition output for all underweight severities
overall.underweight.epitrans <- fread("filepath")
severe.underweight.epitrans <- fread("filepath")
extreme.underweight.epitrans <- fread("filepath")


overall.underweight.epitrans[, cgf.type := "WAZ"]
severe.underweight.epitrans[, cgf.type := "WAZ"]
extreme.underweight.epitrans[, cgf.type := "WAZ"]

overall.underweight.epitrans[, severity := "Overall"]
severe.underweight.epitrans[, severity := "Severe"]
extreme.underweight.epitrans[, severity := "Extreme"]


waz.epitrans <- rbind(overall.underweight.epitrans, severe.underweight.epitrans, extreme.underweight.epitrans)
waz.epitrans <- waz.epitrans[age_group_name == "Under 5"]
waz.epitrans[, age_group_id := 1]


# need to aggregate UHC levels up to the regions, super regions, and global to add this on the dataframe

uhc <- get_covariate_estimates(1097, gbd_round_id = 7, decomp_step = "iterative")
uhc <- uhc[year_id %in% c(1990:2020)]

total.pop <- get_population(gbd_round_id = 7, decomp_step = 'iterative', location_id = unique(uhc$location_id), age_group_id = 22, sex_id = 3, year_id = c(1990:2020))
total.pop$run_id <- NULL
uhc <- merge(uhc, total.pop, by = c("age_group_id", "location_id", "year_id", "sex_id"))
uhc <- uhc[, c("age_group_id", "location_id", "year_id", "sex_id", "mean_value", "population")]


agg.locs.uhc <- lapply(hierarchy[most_detailed == 0 & location_id != 1]$location_id, function(loc){
  
  
  if(loc == 1){
    agg_dt <- copy(uhc)
  }else{
    child_locs <- hierarchy[most_detailed == 1 & (parent_id == loc | grepl(paste0(",", loc, ","), path_to_top_parent)), location_id]
    
    child_uhcs <- uhc[location_id %in% child_locs] 
  }
  
  agg.uhc <- child_uhcs[, .(location_id = loc, uhc = weighted.mean(mean_value, w = population),
                            population = sum(population)),
                        by = c("age_group_id", "year_id", "sex_id")]
  
  return(agg.uhc)
  
})

agg.locs.uhc <- rbindlist(agg.locs.uhc)
setnames(agg.locs.uhc, "uhc", "agg.uhc")
agg.locs.uhc <- agg.locs.uhc[, -c("age_group_id", "population")]


# now need to insert these aggregated uhc estimates into the epitrans dataframe

waz.epitrans <- merge(waz.epitrans, agg.locs.uhc, by = c("location_id", "year_id"), all.x = T)
waz.epitrans[level <3, uhc:= agg.uhc]
waz.epitrans$agg.uhc <- NULL
waz.epitrans <- waz.epitrans[location_id != 1]


#doing this to help with plotting eventually
waz.epitrans[, loc.sev := paste0(location_id, "_", severity)]




# first need to create scaled versions of the three splines and smooth them

overall.waz.spline <- waz.epitrans[level > 2 & severity == "Overall", c("uhc", "expected")]
overall.waz.spline <- overall.waz.spline[order(uhc),]

severe.waz.spline <- waz.epitrans[level > 2 & severity == "Severe", c("uhc", "expected")]
severe.waz.spline <- severe.waz.spline[order(uhc),]

extreme.waz.spline <- waz.epitrans[level > 2 & severity == "Extreme", c("uhc", "expected")]
extreme.waz.spline <- extreme.waz.spline[order(uhc),]

#getting 500 UHC values along this span to smooth out
min.uhc <- min(overall.whz.spline$uhc)
max.uhc <- max(overall.whz.spline$uhc)
uhc.vals <- seq(from = min.uhc, to = max.uhc, length.out = 500)



#getting the initial overall spline values at those UHC points
overall.uhc.spaced.vals <- lapply(uhc.vals, function(uval){
  
  estimate <- overall.waz.spline[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, overall.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial severe spline values at those UHC points
severe.uhc.spaced.vals <- lapply(uhc.vals, function(uval){
  
  estimate <- severe.waz.spline[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, severe.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial extreme spline values at those UHC points
extreme.uhc.spaced.vals <- lapply(uhc.vals, function(uval){
  
  estimate <- extreme.waz.spline[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, extreme.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#now calculating a rolling average
overall.uhc.spaced.vals <- overall.uhc.spaced.vals %>% 
  mutate(roll_mean = rollmean(overall.spline, 10, na.pad = T))


severe.uhc.spaced.vals <- severe.uhc.spaced.vals %>% 
  mutate(roll_mean = rollmean(severe.spline, 10, na.pad = T))



extreme.uhc.spaced.vals <- extreme.uhc.spaced.vals %>% 
  mutate(roll_mean = rollmean(extreme.spline, 10, na.pad = T))



overall.uhc.spaced.vals$overall.spline <- NULL
severe.uhc.spaced.vals$severe.spline <- NULL
extreme.uhc.spaced.vals$extreme.spline <- NULL


overall.uhc.spaced.vals <- data.table(overall.uhc.spaced.vals)
severe.uhc.spaced.vals <- data.table(severe.uhc.spaced.vals)
extreme.uhc.spaced.vals <- data.table(extreme.uhc.spaced.vals)

overall.uhc.spaced.vals <- overall.uhc.spaced.vals[!is.na(roll_mean)]
severe.uhc.spaced.vals <- severe.uhc.spaced.vals[!is.na(roll_mean)]
extreme.uhc.spaced.vals <- extreme.uhc.spaced.vals[!is.na(roll_mean)]

# merging all smoothed splines together
setnames(overall.uhc.spaced.vals, "roll_mean", "overall_expected")
setnames(severe.uhc.spaced.vals, "roll_mean", "severe_expected")
setnames(extreme.uhc.spaced.vals, "roll_mean", "extreme_expected")

underweight.smoothed.splines <- merge(overall.uhc.spaced.vals, severe.uhc.spaced.vals, by = "uhc_value")
underweight.smoothed.splines <- merge(underweight.smoothed.splines, extreme.uhc.spaced.vals, by = "uhc_value")

under.severe.to.overall.scalar <- underweight.smoothed.splines[1]$overall_expected / underweight.smoothed.splines[1]$severe_expected
under.exreme.to.overall.scalar <- underweight.smoothed.splines[1]$overall_expected / underweight.smoothed.splines[1]$extreme_expected
under.extreme.to.severe.scalar <- underweight.smoothed.splines[1]$severe_expected / underweight.smoothed.splines[1]$extreme_expected

# columns named as <what they're scaled to match>.scaled.<what they were originally>
underweight.smoothed.splines[, overall.scaled.severe := severe_expected * under.severe.to.overall.scalar]
underweight.smoothed.splines[, overall.scaled.extreme := extreme_expected * under.exreme.to.overall.scalar]

underweight.smoothed.splines[, severe.scaled.overall := overall_expected / under.severe.to.overall.scalar]
underweight.smoothed.splines[, severe.scaled.extreme := extreme_expected * under.extreme.to.severe.scalar]

underweight.smoothed.splines[, extreme.scaled.overall := overall_expected / under.exreme.to.overall.scalar]
underweight.smoothed.splines[, extreme.scaled.severe := severe_expected / under.extreme.to.severe.scalar]






# these plots would be for main body


waz.epitrans$region_name <- factor(waz.epitrans$region_name, levels = c("Central Asia", "Central Europe", "Eastern Europe",
                                                                        "Australasia", "High-income Asia Pacific", "High-income North America",
                                                                        "Southern Latin America", "Western Europe",
                                                                        "Andean Latin America", "Caribbean", "Central Latin America", "Tropical Latin America",
                                                                        "North Africa and Middle East", "South Asia",
                                                                        "East Asia", "Oceania", "Southeast Asia", 
                                                                        "Central Sub-Saharan Africa", "Eastern Sub-Saharan Africa",
                                                                        "Southern Sub-Saharan Africa", "Western Sub-Saharan Africa"))


waz.epitrans <- waz.epitrans[order(year_id)]



















underweight.smoothed.splines.test.melted <- melt(underweight.smoothed.splines, id.vars = "uhc_value")
underweight.smoothed.splines.test.melted$variable <- factor(underweight.smoothed.splines.test.melted$variable, 
                                                            levels = c("overall_expected", "overall.scaled.severe", "overall.scaled.extreme",
                                                                       "severe.scaled.overall", "severe_expected", "severe.scaled.extreme",
                                                                       "extreme.scaled.overall", "extreme.scaled.severe", "extreme_expected"))




# these are for supplemental materials


waz.epitrans[, cgf.type.label := "Underweight (WAZ)"]

#this plot shows the smoothed splines for SEVERE
region.plot.severe.underweight<- ggplot() +
  geom_line(data = underweight.smoothed.splines.test.melted[variable == "severe_expected"], aes(x = uhc_value, y = value ), color = "black", size = 1.5) +
  geom_path(data = waz.epitrans[level == 2 & severity == "Severe" & cgf.type == "WAZ" & !is.na(region_name)], aes(x = uhc, y = observed, group = location_id, color = region_name), alpha = 0.9, size = 1)+
  geom_point(data = waz.epitrans[level == 2 & severity == "Severe" & cgf.type == "WAZ" & !is.na(region_name) & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id, fill = region_name, shape = region_name), color = 'black', alpha = 0.9, size = 3)+
  scale_shape_manual(values = c(21, 22, 23, # Central Europe
                                21, 22, 23, 24, 25, #High Income
                                21, 22, 23, 24, # Latin America
                                21, #North Africa Middle East
                                21, # South Asia
                                21, 22, 23, #SE Asia
                                21, 22, 23, 24 ),guide=guide_legend(title="Region",ncol=1) ) + #Sub Saharan Africa
  scale_color_manual(values = c("#E6AB02" , "#E6AB02" , "#E6AB02" , #Central Asia and Europe
                                "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , #High Income
                                "#7570B3" , "#7570B3" , "#7570B3" , "#7570B3" , # Latin America
                                "#E7298A" , #North Africa Middle East
                                "#66A61E"   , # South Asia
                                "#1B9E77" , "#1B9E77" , "#1B9E77" , # SE Asia 
                                "#A6761D" ,  "#A6761D" ,  "#A6761D" ,  "#A6761D"  ), guide=guide_legend(title="Region",ncol=1)) + #Sub Saharan Africa
  scale_fill_manual(values = c("#E6AB02" , "#E6AB02" , "#E6AB02" , #Central Asia and Europe
                               "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , #High Income
                               "#7570B3" , "#7570B3" , "#7570B3" , "#7570B3" , # Latin America
                               "#E7298A" , #North Africa Middle East
                               "#66A61E"   , # South Asia
                               "#1B9E77" , "#1B9E77" , "#1B9E77" , # SE Asia 
                               "#A6761D" ,  "#A6761D" ,  "#A6761D" ,  "#A6761D"  ), guide=guide_legend(title="Region",ncol=1)) + #Sub Saharan Africa
  labs(x = "Universal Health Coverage Index", y = "Prevalence", color = "Region", shape = "Region", fill = "Region", tag = "C") +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 18), 
        legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
        legend.key = element_rect(fill = "grey90"),
        plot.tag = element_text(size = 20, face = 'bold')) +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0, .1, .2), limits = c(0, .205)) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  facet_wrap(~cgf.type.label) +
  guides(linetype = guide_legend(keywidth = unit(5, 'cm')))





#this plot shows the smoothed splines for EXTREME 
region.plot.extreme.underweight<- ggplot() +
  geom_line(data = underweight.smoothed.splines.test.melted[variable == "extreme_expected"], aes(x = uhc_value, y = value ), color = "black", size = 1.5) +
  geom_path(data = waz.epitrans[level == 2 & severity == "Extreme" & cgf.type == "WAZ" & !is.na(region_name)], aes(x = uhc, y = observed, group = location_id, color = region_name), alpha = 0.9, size = 1)+
  geom_point(data = waz.epitrans[level == 2 & severity == "Extreme" & cgf.type == "WAZ" & !is.na(region_name) & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id, fill = region_name, shape = region_name), color = 'black', alpha = 0.9, size = 3)+
  scale_shape_manual(values = c(21, 22, 23, # Central Europe
                                21, 22, 23, 24, 25, #High Income
                                21, 22, 23, 24, # Latin America
                                21, #North Africa Middle East
                                21, # South Asia
                                21, 22, 23, #SE Asia
                                21, 22, 23, 24 ),guide=guide_legend(title="Region",ncol=1) ) + #Sub Saharan Africa
  scale_color_manual(values = c("#E6AB02" , "#E6AB02" , "#E6AB02" , #Central Asia and Europe
                                "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , #High Income
                                "#7570B3" , "#7570B3" , "#7570B3" , "#7570B3" , # Latin America
                                "#E7298A" , #North Africa Middle East
                                "#66A61E"   , # South Asia
                                "#1B9E77" , "#1B9E77" , "#1B9E77" , # SE Asia 
                                "#A6761D" ,  "#A6761D" ,  "#A6761D" ,  "#A6761D"  ), guide=guide_legend(title="Region",ncol=1)) + #Sub Saharan Africa
  scale_fill_manual(values = c("#E6AB02" , "#E6AB02" , "#E6AB02" , #Central Asia and Europe
                               "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , #High Income
                               "#7570B3" , "#7570B3" , "#7570B3" , "#7570B3" , # Latin America
                               "#E7298A" , #North Africa Middle East
                               "#66A61E"   , # South Asia
                               "#1B9E77" , "#1B9E77" , "#1B9E77" , # SE Asia 
                               "#A6761D" ,  "#A6761D" ,  "#A6761D" ,  "#A6761D"  ), guide=guide_legend(title="Region",ncol=1)) + #Sub Saharan Africa
  labs(x = "Universal Health Coverage Index", y = "Prevalence", color = "Region", shape = "Region", fill = "Region", tag = "C") +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 18),
        legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
        legend.key = element_rect(fill = "grey90"),
        plot.tag = element_text(size = 20, face = 'bold')) +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0, .02, .04, .06, .08), limits = c(0, .08)) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  facet_wrap(~cgf.type.label) +
  guides(linetype = guide_legend(keywidth = unit(5, 'cm')))











###################################################################################################
###################################################################################################
#################### GRID ARRANGE OF APPENDIX PLOTS FOR SEVERE AND EXTREME ########################
###################################################################################################
###################################################################################################



layout <- rbind(c(2, 2,2),
                c(2, 1,2),
                c(2, 3,4),
                c(2, 5,2),
                c(2, 6,6))


empty <- ggplot() + theme_void()

regions.legend <- get_legend(region.plot.severe.stunting)



caption.plot <- ggplot() +
  theme_void() +
  labs(caption = expression(paste(bold("Appendix G Figure 1: \n \n "), " The expected values of severe stunting, wasting, and underweight based on UHC Index are represented by the black \n lines in Figures 1a, 1b, and 1c respectively. Estimated values of severe CGF are shown for each region, and colored\n by super region. Points are shown every five years from 1990-2020."))) +
  theme(plot.caption = element_text(hjust = 0, size = 14))




appendixgfigure1 <- plot_grid(arrangeGrob(region.plot.severe.stunting +  theme(legend.position = "none"), empty, region.plot.severe.wasting +  theme(legend.position = "none"),
                                          regions.legend, region.plot.severe.underweight +  theme(legend.position = "none"), caption.plot,
                                          nrow=5, ncol = 3, widths = c(.1, 5,2.5), heights = c(.2,1, 1, 1, .4), layout_matrix = layout))





#Severe CGF Epi Transition Plots
pdf("filepath", height = 15, width = 14)



print(appendixgfigure1)


dev.off()







caption.plot.2 <- ggplot() +
  theme_void() +
  labs(caption = expression(paste(bold("Appendix G Figure 2: \n \n"), " The expected values of extreme stunting, wasting, and underweight based on UHC Index are represented by the black \n lines in Figures 2a, 2b, and 2c respectively. Estimated values of extreme CGF are shown for each region, and colored\n by super region. Points are shown every five years from 1990-2020."))) +
  theme(plot.caption = element_text(hjust = 0, size = 14)) 




appendixgfigure2 <- plot_grid(arrangeGrob(region.plot.extreme.stunting +  theme(legend.position = "none"), empty, region.plot.extreme.wasting +  theme(legend.position = "none"),
                                          regions.legend, region.plot.extreme.underweight +  theme(legend.position = "none"), caption.plot.2,
                                          nrow=5, ncol = 3, widths = c(.1, 5,2), heights = c(.2,1, 1, 1, .4), layout_matrix = layout))





#Extreme CGF Epi Transition Plots
pdf("filepath", height = 15, width = 14)



print(appendixgfigure2)


dev.off()








###################################################################################################
###################################################################################################
################### GRID ARRANGE FOR MAIN TEXT OVERALL CGF AND SCALED SPLINES #####################
###################################################################################################
###################################################################################################




haz.epitrans$region_name <- factor(haz.epitrans$region_name, levels = c("Central Asia", "Central Europe", "Eastern Europe",
                                                                        "Australasia", "High-income Asia Pacific", "High-income North America",
                                                                        "Southern Latin America", "Western Europe",
                                                                        "Andean Latin America", "Caribbean", "Central Latin America", "Tropical Latin America",
                                                                        "North Africa and Middle East", "South Asia",
                                                                        "East Asia", "Oceania", "Southeast Asia", 
                                                                        "Central Sub-Saharan Africa", "Eastern Sub-Saharan Africa",
                                                                        "Southern Sub-Saharan Africa", "Western Sub-Saharan Africa"))

stunting.smoothed.splines[, cgf.type := "HAZ"]
haz.epitrans[, cgf.type.label := "Stunting (HAZ)"]

region.plot.overall.stunting.onespline<- ggplot() +
  geom_line(data = stunting.smoothed.splines, aes(x = uhc_value, y = overall_expected ), color = "black", size = 2) +
  geom_path(data = haz.epitrans[level == 2 & severity == "Overall" & cgf.type == "HAZ" & !is.na(region_name)], aes(x = uhc, y = observed, group = location_id, color = region_name), alpha = 0.9, size = 1)+
  geom_point(data = haz.epitrans[level == 2 & severity == "Overall" & cgf.type == "HAZ" & !is.na(region_name) & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id, fill = region_name, shape = region_name), color = 'black', alpha = 0.9, size = 3)+
  scale_shape_manual(values = c(21, 22, 23, # Central Europe
                                21, 22, 23, 24, 25, #High Income
                                21, 22, 23, 24, # Latin America
                                21, #North Africa Middle East
                                21, # South Asia
                                21, 22, 23, #SE Asia
                                21, 22, 23, 24 ),guide=guide_legend(title="Region",ncol=1, title.hjust = 0.5) ) + #Sub Saharan Africa
  scale_color_manual(values = c("#E6AB02" , "#E6AB02" , "#E6AB02" , #Central Asia and Europe
                                "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , #High Income
                                "#7570B3" , "#7570B3" , "#7570B3" , "#7570B3" , # Latin America
                                "#E7298A" , #North Africa Middle East
                                "#66A61E"   , # South Asia
                                "#1B9E77" , "#1B9E77" , "#1B9E77" , # SE Asia 
                                "#A6761D" ,  "#A6761D" ,  "#A6761D" ,  "#A6761D"  ), guide=guide_legend(title="Region",ncol=1, title.hjust = 0.5)) + #Sub Saharan Africa
  scale_fill_manual(values = c("#E6AB02" , "#E6AB02" , "#E6AB02" , #Central Asia and Europe
                               "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , #High Income
                               "#7570B3" , "#7570B3" , "#7570B3" , "#7570B3" , # Latin America
                               "#E7298A" , #North Africa Middle East
                               "#66A61E"   , # South Asia
                               "#1B9E77" , "#1B9E77" , "#1B9E77" , # SE Asia 
                               "#A6761D" ,  "#A6761D" ,  "#A6761D" ,  "#A6761D"  ), guide=guide_legend(title="Region",ncol=1, title.hjust = 0.5)) + #Sub Saharan Africa
  labs(x = "Universal Health Coverage Index", y = "Prevalence", color = "Region", shape = "Region", fill = "Region", tag = "A") +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 18),
        legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
        legend.key = element_rect(fill = "grey90"),
        plot.tag = element_text(size = 20, face = "bold")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0, .1, .2, .3, .4, .5), limits = c(0, .56)) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  facet_wrap(~cgf.type.label)






whz.epitrans$region_name <- factor(whz.epitrans$region_name, levels = c("Central Asia", "Central Europe", "Eastern Europe",
                                                                        "Australasia", "High-income Asia Pacific", "High-income North America",
                                                                        "Southern Latin America", "Western Europe",
                                                                        "Andean Latin America", "Caribbean", "Central Latin America", "Tropical Latin America",
                                                                        "North Africa and Middle East", "South Asia",
                                                                        "East Asia", "Oceania", "Southeast Asia", 
                                                                        "Central Sub-Saharan Africa", "Eastern Sub-Saharan Africa",
                                                                        "Southern Sub-Saharan Africa", "Western Sub-Saharan Africa"))

wasting.smoothed.splines[, cgf.type := "WHZ"]
whz.epitrans[, cgf.type.label := "Wasting (WHZ)"]


region.plot.overall.wasting.onespline<- ggplot() +
  geom_line(data = wasting.smoothed.splines, aes(x = uhc_value, y = overall_expected ), color = "black", size = 2) +
  geom_path(data = whz.epitrans[level == 2 & severity == "Overall" & cgf.type == "WHZ" & !is.na(region_name)], aes(x = uhc, y = observed, group = location_id, color = region_name), alpha = 0.9, size = 1)+
  geom_point(data = whz.epitrans[level == 2 & severity == "Overall" & cgf.type == "WHZ" & !is.na(region_name) & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id, fill = region_name, shape = region_name), color = 'black', alpha = 0.9, size = 3)+
  scale_shape_manual(values = c(21, 22, 23, # Central Europe
                                21, 22, 23, 24, 25, #High Income
                                21, 22, 23, 24, # Latin America
                                21, #North Africa Middle East
                                21, # South Asia
                                21, 22, 23, #SE Asia
                                21, 22, 23, 24 ),guide=guide_legend(title="Region",ncol=1) ) + #Sub Saharan Africa
  scale_color_manual(values = c("#E6AB02" , "#E6AB02" , "#E6AB02" , #Central Asia and Europe
                                "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , #High Income
                                "#7570B3" , "#7570B3" , "#7570B3" , "#7570B3" , # Latin America
                                "#E7298A" , #North Africa Middle East
                                "#66A61E"   , # South Asia
                                "#1B9E77" , "#1B9E77" , "#1B9E77" , # SE Asia 
                                "#A6761D" ,  "#A6761D" ,  "#A6761D" ,  "#A6761D"  ), guide=guide_legend(title="Region",ncol=1)) + #Sub Saharan Africa
  scale_fill_manual(values = c("#E6AB02" , "#E6AB02" , "#E6AB02" , #Central Asia and Europe
                               "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , #High Income
                               "#7570B3" , "#7570B3" , "#7570B3" , "#7570B3" , # Latin America
                               "#E7298A" , #North Africa Middle East
                               "#66A61E"   , # South Asia
                               "#1B9E77" , "#1B9E77" , "#1B9E77" , # SE Asia 
                               "#A6761D" ,  "#A6761D" ,  "#A6761D" ,  "#A6761D"  ), guide=guide_legend(title="Region",ncol=1)) + #Sub Saharan Africa
  labs(x = "Universal Health Coverage Index", y = "Prevalence", color = "Region", shape = "Region", fill = "Region", tag = "B") +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 18),
        plot.tag = element_text(size = 20, face = "bold")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0, .05, .1, .15, .2), limits = c(0, .235)) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  facet_wrap(~cgf.type.label)





waz.epitrans$region_name <- factor(waz.epitrans$region_name, levels = c("Central Asia", "Central Europe", "Eastern Europe",
                                                                        "Australasia", "High-income Asia Pacific", "High-income North America",
                                                                        "Southern Latin America", "Western Europe",
                                                                        "Andean Latin America", "Caribbean", "Central Latin America", "Tropical Latin America",
                                                                        "North Africa and Middle East", "South Asia",
                                                                        "East Asia", "Oceania", "Southeast Asia", 
                                                                        "Central Sub-Saharan Africa", "Eastern Sub-Saharan Africa",
                                                                        "Southern Sub-Saharan Africa", "Western Sub-Saharan Africa"))

underweight.smoothed.splines[, cgf.type := "WAZ"]
waz.epitrans[, cgf.type.label := "Underweight (WAZ)"]

region.plot.overall.underweight.onespline<- ggplot() +
  geom_line(data = underweight.smoothed.splines, aes(x = uhc_value, y = overall_expected ), color = "black", size = 2) +
  geom_path(data = waz.epitrans[level == 2 & severity == "Overall" & cgf.type == "WAZ" & !is.na(region_name)], aes(x = uhc, y = observed, group = location_id, color = region_name), alpha = 0.9, size = 1)+
  geom_point(data = waz.epitrans[level == 2 & severity == "Overall" & cgf.type == "WAZ" & !is.na(region_name) & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id, fill = region_name, shape = region_name), color = 'black', alpha = 0.9, size = 3)+
  scale_shape_manual(values = c(21, 22, 23, # Central Europe
                                21, 22, 23, 24, 25, #High Income
                                21, 22, 23, 24, # Latin America
                                21, #North Africa Middle East
                                21, # South Asia
                                21, 22, 23, #SE Asia
                                21, 22, 23, 24 ),guide=guide_legend(title="Region",ncol=1) ) + #Sub Saharan Africa
  scale_color_manual(values = c("#E6AB02" , "#E6AB02" , "#E6AB02" , #Central Asia and Europe
                                "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , #High Income
                                "#7570B3" , "#7570B3" , "#7570B3" , "#7570B3" , # Latin America
                                "#E7298A" , #North Africa Middle East
                                "#66A61E"   , # South Asia
                                "#1B9E77" , "#1B9E77" , "#1B9E77" , # SE Asia 
                                "#A6761D" ,  "#A6761D" ,  "#A6761D" ,  "#A6761D"  ), guide=guide_legend(title="Region",ncol=1)) + #Sub Saharan Africa
  scale_fill_manual(values = c("#E6AB02" , "#E6AB02" , "#E6AB02" , #Central Asia and Europe
                               "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , "#D95F02" , #High Income
                               "#7570B3" , "#7570B3" , "#7570B3" , "#7570B3" , # Latin America
                               "#E7298A" , #North Africa Middle East
                               "#66A61E"   , # South Asia
                               "#1B9E77" , "#1B9E77" , "#1B9E77" , # SE Asia 
                               "#A6761D" ,  "#A6761D" ,  "#A6761D" ,  "#A6761D"  ), guide=guide_legend(title="Region",ncol=1)) + #Sub Saharan Africa
  labs(x = "Universal Health Coverage Index", y = "Prevalence", color = "Region", shape = "Region", fill = "Region", tag = "C") +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 18),
        plot.tag = element_text(size = 20, face = "bold")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0, .1, .2, .3, .4), limits = c(0, .425)) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  facet_wrap(~cgf.type.label)







epi.trans.legend.2 <- get_legend(region.plot.overall.stunting.onespline)




temp.df <- data.table(cgf.type = c("Overall CGF (<-2SD)", "Severe CGF (<-3SD)", "Extreme CGF (<-4SD)"),
                      val = c(1, 2, 3))

temp.df$cgf.type <- factor(temp.df$cgf.type, levels = c("Overall CGF (<-2SD)", "Severe CGF (<-3SD)", "Extreme CGF (<-4SD)"))


severity.legend.plot <- ggplot() +
  geom_histogram(data = temp.df, aes(x = val, fill = cgf.type), alpha = 1) +
  scale_fill_manual(values = c("#eda109", "#e36f10", "#bf1313"),
                    guide=guide_legend(title="Severity", title.position = "top", title.hjust = .5, ncol = 3)) +
  theme(legend.title = element_text(size = 24),
        legend.text = element_text(size = 20),
        legend.position = "bottom",
        legend.justification = 'center',
        legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
        legend.key = element_rect(fill = "grey90"),
        legend.key.height = unit(1, "cm"))


severity.legend <- get_legend(severity.legend.plot)




layout <- rbind(c(2, 1,2),
                c(2, 3,4),
                c(2, 5,2),
                c(2, 6,6))


empty <- ggplot()



caption.plot <- ggplot() +
  labs(caption = expression(paste(bold("Figure 4: \n \n"), "The expected values of overall stunting, wasting, and underweight based on UHC Index are represented by the\nsolid black lines in figures 4a, 4b, and 4c respectively. Estimated values of overall CGF are shown for each region,\nand colored by super region. Points are shown every five years from 1990-2020."))) +
  theme(plot.caption = element_text(hjust = 0, size = 16))




figure4 <- plot_grid(arrangeGrob(region.plot.overall.stunting.onespline+ theme(legend.position="none"), empty, 
                                 region.plot.overall.wasting.onespline+ theme(legend.position="none"), epi.trans.legend.2, 
                                 region.plot.overall.underweight.onespline+ theme(legend.position="none"),caption.plot,
                                 nrow=4, ncol = 3, widths = c(.1, 5,2), heights = c(1, 1, 1, .4), layout_matrix = layout))




#overall CGF Epi Transition Plots
pdf("filepath", height = 15, width = 14)



print(figure4)

dev.off()















#getting y labels for the spline plot since they're really going to be shown in relative space
stunting.spline.y.labels <- seq(0, max(stunting.smoothed.splines$overall_expected), length.out = 6)
stunting.smoothed.splines[, cgf.type.label := "Stunting (HAZ)"]

stunting.splines.plot <- ggplot() +
  geom_line(data = stunting.smoothed.splines, aes(x = uhc_value, y = overall_expected), color = "#eda109", size = 2.5) +
  geom_line(data = stunting.smoothed.splines, aes(x = uhc_value, y = overall.scaled.severe), color = "#e36f10", size = 2.5) +
  geom_line(data = stunting.smoothed.splines, aes(x = uhc_value, y = overall.scaled.extreme), color = "#bf1313", size = 2.5) +
  scale_y_continuous(breaks = stunting.spline.y.labels, labels = c("-100%", "-80%", "-60%", "-40%", "-20%", "0%")) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        plot.tag = element_text(size = 24, face = "bold")) +
  labs(x = "Universal Health Coverage Index", y = "Relative Decrease", tag = "A") +
  facet_wrap(~cgf.type.label)



#getting y labels for the spline plot since they're really going to be shown in relative space
wasting.spline.y.labels <- seq(0, max(wasting.smoothed.splines$overall_expected), length.out = 6)
wasting.smoothed.splines[, cgf.type.label := "Wasting (WHZ)"]

wasting.splines.plot <- ggplot() +
  geom_line(data = wasting.smoothed.splines, aes(x = uhc_value, y = overall_expected), color = "#eda109", size = 2.5) +
  geom_line(data = wasting.smoothed.splines, aes(x = uhc_value, y = overall.scaled.severe), color = "#e36f10", size = 2.5) +
  geom_line(data = wasting.smoothed.splines, aes(x = uhc_value, y = overall.scaled.extreme), color = "#bf1313", size = 2.5) +
  scale_y_continuous(breaks = wasting.spline.y.labels, labels = c("-100%", "-80%", "-60%", "-40%", "-20%", "0%")) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        plot.tag = element_text(size = 24, face = "bold")) +
  labs(x = "Universal Health Coverage Index", y = "Relative Decrease", tag = "B") +
  facet_wrap(~cgf.type.label)



#getting y labels for the spline plot since they're really going to be shown in relative space
underweight.spline.y.labels <- seq(0, max(underweight.smoothed.splines$overall_expected), length.out = 6)
underweight.smoothed.splines[, cgf.type.label := "Underweight (WAZ)"]

underweight.splines.plot <- ggplot() +
  geom_line(data = underweight.smoothed.splines, aes(x = uhc_value, y = overall_expected), color = "#eda109", size = 2.5) +
  geom_line(data = underweight.smoothed.splines, aes(x = uhc_value, y = overall.scaled.severe), color = "#e36f10", size = 2.5) +
  geom_line(data = underweight.smoothed.splines, aes(x = uhc_value, y = overall.scaled.extreme), color = "#bf1313", size = 2.5) +
  scale_y_continuous(breaks = underweight.spline.y.labels, labels = c("-100%", "-80%", "-60%", "-40%", "-20%", "0%")) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        plot.tag = element_text(size = 24, face = "bold")) +
  labs(x = "Universal Health Coverage Index", y = "Relative Decrease", tag = "C") +
  facet_wrap(~cgf.type.label)










layout2 <- rbind(c(1, 2, 3),
                 c(4, 4, 4),
                 c(5, 5, 5))


caption.plot.2 <- ggplot() +
  labs(caption = expression(paste(bold("Figure 5: \n"), " Expected values of overall, severe, and extreme CGF scaled to show how trajectories in stunting (figure 5a), wasting (figure 5b), and underweight (figure 5c) change as UHC Index improves. Prevalences have\n been scaled to demonstrate expected relative changes as UHC Index increases from the minimum observed UHC Index value."))) +
  theme(plot.caption = element_text(hjust = 0, size = 18))



figure5 <- plot_grid(arrangeGrob(stunting.splines.plot, wasting.splines.plot, underweight.splines.plot, severity.legend, caption.plot.2,
                                 nrow=3, ncol = 3, widths = c(1, 1, 1), heights = c(1, .2, .1), layout_matrix = layout2))





#Scaled Spline Plots
pdf("filepath", height = 10, width = 26)



print(figure5)


dev.off()










###########################





waz.epitrans$sex_id <- NULL
all.epitrans <- rbind(haz.epitrans, whz.epitrans, waz.epitrans)

stunting.smoothed.splines.test.melted[, cgf.type.label := "Stunting (HAZ)"]
wasting.smoothed.splines.test.melted[, cgf.type.label := "Wasting (WHZ)"]
underweight.smoothed.splines.test.melted[, cgf.type.label := "Underweight (WAZ)"]

stunting.smoothed.splines.test.melted[variable == "overall_expected", severity := "Overall"]
stunting.smoothed.splines.test.melted[variable == "severe_expected", severity := "Severe"]
stunting.smoothed.splines.test.melted[variable == "extreme_expected", severity := "Extreme"]

wasting.smoothed.splines.test.melted[variable == "overall_expected", severity := "Overall"]
wasting.smoothed.splines.test.melted[variable == "severe_expected", severity := "Severe"]
wasting.smoothed.splines.test.melted[variable == "extreme_expected", severity := "Extreme"]

underweight.smoothed.splines.test.melted[variable == "overall_expected", severity := "Overall"]
underweight.smoothed.splines.test.melted[variable == "severe_expected", severity := "Severe"]
underweight.smoothed.splines.test.melted[variable == "extreme_expected", severity := "Extreme"]

stunting.smoothed.splines.test.melted <- stunting.smoothed.splines.test.melted[!is.na(severity)]
wasting.smoothed.splines.test.melted <- wasting.smoothed.splines.test.melted[!is.na(severity)]
underweight.smoothed.splines.test.melted <- underweight.smoothed.splines.test.melted[!is.na(severity)]


all.splines.melted <- rbind(stunting.smoothed.splines.test.melted, wasting.smoothed.splines.test.melted, underweight.smoothed.splines.test.melted)
all.splines.melted[severity == "Overall", severity := "Overall (<-2SD)"]
all.splines.melted[severity == "Severe", severity := "Severe (<-3SD)"]
all.splines.melted[severity == "Extreme", severity := "Extreme (<-4SD)"]

all.epitrans[severity == "Overall", severity := "Overall (<-2SD)"]
all.epitrans[severity == "Severe", severity := "Severe (<-3SD)"]
all.epitrans[severity == "Extreme", severity := "Extreme (<-4SD)"]


all.epitrans$region_name <- as.character(all.epitrans$region_name)
empty <- ggplot() + theme_void()



#add in 4 nations of UK 
uhc.uk <- get_covariate_estimates(1097, gbd_round_id = 7, decomp_step = "iterative", location_id = c(433, 434, 4636, 4749))
setnames(uhc.uk, "mean_value", "uhc")
uhc.uk <- uhc.uk[, c("location_id", "year_id", "uhc")]
uhc.uk <- uhc.uk[year_id > 1989]
uhc.uk <- uhc.uk[year_id < 2021]

uk.loc.merge <- loc.met[location_id %in% c(433, 434, 4636, 4749), c("location_id", "location_name", "ihme_loc_id", "level", "super_region_name", "region_name")]


combined <- lapply(c(10556, 8949, 26941, 10558, 8945, 26943, 10560, 2540, 26942), function(meid){
  
  print(meid)
  
  uk_cgf <- get_draws("modelable_entity_id", meid, year_id=c(1990:2020),
                      source="epi", gbd_round_id=7, decomp_step="iterative",
                      age_group_id = c(1), sex_id = c(3), location_id = c(433, 434, 4636, 4749))
  
  uk_cgf <- melt(uk_cgf, id.vars = id.vars)
  uk_cgf[, mean.val := mean(value), by = c("location_id", "year_id")]
  uk_cgf[, loc.year := paste0(location_id, "_", year_id)]
  uk_cgf <- uk_cgf[!duplicated(uk_cgf$loc.year)]
  uk_cgf$modelable_entity_id <- NULL
  uk_cgf$sex_id <- NULL
  uk_cgf$model_version_id <- NULL
  uk_cgf$variable <- NULL
  uk_cgf$loc.year <- NULL
  uk_cgf$value <- NULL
  
  uk_cgf[, measure_name := "Prevalence"]
  uk_cgf[, metric_name := "Rate"]
  setnames(uk_cgf, "mean.val", "observed")
  uk_cgf[, age_group_name := "Under 5"]
  
  uk_cgf$uhc <- uhc.uk$uhc
  uk_cgf <- merge(uk_cgf, uk.loc.merge, by = "location_id")
  
  if(meid == 10556){
    uk_cgf[, cgf.type := "HAZ"]
    uk_cgf[, severity := "Overall (<-2SD)"]
    uk_cgf[, loc.sev := paste0(location_id, "_Overall")]
    uk_cgf[, cgf.type.label := "Stunting (HAZ)"]
    
  }
  
  if(meid == 8949){
    uk_cgf[, cgf.type := "HAZ"]
    uk_cgf[, severity := "Severe (<-3SD)"]
    uk_cgf[, loc.sev := paste0(location_id, "_Severe")]
    uk_cgf[, cgf.type.label := "Stunting (HAZ)"]
    
  }
  if(meid == 26941){
    uk_cgf[, cgf.type := "HAZ"]
    uk_cgf[, severity := "Extreme (<-4SD)"]
    uk_cgf[, loc.sev := paste0(location_id, "_Extreme")]
    uk_cgf[, cgf.type.label := "Stunting (HAZ)"]
    
  }
  if(meid == 10558){
    uk_cgf[, cgf.type := "WHZ"]
    uk_cgf[, severity := "Overall (<-2SD)"]
    uk_cgf[, loc.sev:= paste0(location_id, "_Overall")]
    uk_cgf[, cgf.type.label:= "Wasting (WHZ)"]
    
  }
  if(meid == 8945){
    uk_cgf[, cgf.type := "WHZ"]
    uk_cgf[, severity := "Severe (<-3SD)"]
    uk_cgf[, loc.sev:= paste0(location_id, "_Severe")]
    uk_cgf[, cgf.type.label := "Wasting (WHZ)"]
    
  }
  
  if(meid == 26943){
    uk_cgf[, cgf.type := "WHZ"]
    uk_cgf[, severity := "Extreme (<-4SD)"]
    uk_cgf[, loc.sev := paste0(location_id, "_Extreme")]
    uk_cgf[, cgf.type.label := "Wasting (WHZ)"]
    
  }
  if(meid == 10560){
    uk_cgf[, cgf.type := "WAZ"]
    uk_cgf[, severity:= "Overall (<-2SD)"]
    uk_cgf[, loc.sev := paste0(location_id, "_Overall")]
    uk_cgf[, cgf.type.label := "Underweight (WAZ)"]
    
  }
  if(meid == 2540){
    uk_cgf[, cgf.type := "WAZ"]
    uk_cgf[, severity := "Severe (<-3SD)"]
    uk_cgf[, loc.sev := paste0(location_id, "_Severe")]
    uk_cgf[, cgf.type.label:= "Underweight (WAZ)"]
    
  }
  if(meid == 26942){
    uk_cgf[, cgf.type := "WAZ"]
    uk_cgf[, severity := "Extreme (<-4SD)"]
    uk_cgf[, loc.sev := paste0(location_id, "_Extreme")]
    uk_cgf[, cgf.type.label := "Underweight (WAZ)"]
    
  }
  
  return(uk_cgf)
  
})

combined <- rbindlist(combined)

all.epitrans <- rbind(combined, all.epitrans, fill = TRUE)


#NA's for ratio

all.epitrans$region_name <- factor(all.epitrans$region_name, levels = c("Central Asia", "Central Europe", "Eastern Europe",
                                                                        "Australasia", "High-income Asia Pacific", 
                                                                        "High-income North America", "Southern Latin America",
                                                                        "Western Europe", "Andean Latin America", "Caribbean",
                                                                        "Central Latin America", "Tropical Latin America", "North Africa and Middle East",
                                                                        "South Asia", "East Asia", "Oceania", "Southeast Asia", "Central Sub-Saharan Africa",
                                                                        "Eastern Sub-Saharan Africa", "Southern Sub-Saharan Africa", 
                                                                        "Western Sub-Saharan Africa"))


all.splines.melted$region_name <- factor(all.splines.melted$region_name, levels = c("Central Asia", "Central Europe", "Eastern Europe",
                                                                                    "Australasia", "High-income Asia Pacific", 
                                                                                    "High-income North America", "Southern Latin America",
                                                                                    "Western Europe", "Andean Latin America", "Caribbean",
                                                                                    "Central Latin America", "Tropical Latin America", "North Africa and Middle East",
                                                                                    "South Asia", "East Asia", "Oceania", "Southeast Asia", "Central Sub-Saharan Africa",
                                                                                    "Eastern Sub-Saharan Africa", "Southern Sub-Saharan Africa", 
                                                                                    "Western Sub-Saharan Africa"))


all.regions <- c("Central Asia", "Central Europe", "Eastern Europe",
                 "Australasia", "High-income Asia Pacific", 
                 "High-income North America", "Southern Latin America",
                 "Western Europe", "Andean Latin America", "Caribbean",
                 "Central Latin America", "Tropical Latin America", "North Africa and Middle East",
                 "South Asia", "East Asia", "Oceania", "Southeast Asia", "Central Sub-Saharan Africa",
                 "Eastern Sub-Saharan Africa", "Southern Sub-Saharan Africa", 
                 "Western Sub-Saharan Africa")

all.epitrans <- all.epitrans[location_id != 95]
all.epitrans[location_id %in% c(433, 434, 4636, 4749), level := 3]


pdf("filepath", height = 15, width = 20)
for (rid in all.regions) {
  
  number.of.countries <- length(unique(all.epitrans[severity == "Overall (<-2SD)" & cgf.type.label == "Stunting (HAZ)" & level == 3 & !is.na(super_region_name) & region_name == rid]$location_name))
  legend.cols <- ceiling(number.of.countries/3)
  
  
  one.region.epitrans.1 <- ggplot() +
    geom_line(data = all.splines.melted[severity == "Overall (<-2SD)" & cgf.type.label == "Stunting (HAZ)"], aes(x = uhc_value, y = value ), color = "black", size = 1.5) +
    geom_path(data = all.epitrans[severity == "Overall (<-2SD)" & cgf.type.label == "Stunting (HAZ)" & level == 3 & !is.na(super_region_name) & region_name == rid], aes(x = uhc, y = observed, group = location_id, color = location_name), alpha = 0.9, size = 1)+
    geom_point(data = all.epitrans[severity == "Overall (<-2SD)" & cgf.type.label == "Stunting (HAZ)"& level == 3& !is.na(super_region_name) & region_name == rid & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id, fill = location_name), color = 'black', alpha = 0.9, size = 3, shape = 21)+
    geom_path(data = all.epitrans[severity == "Overall (<-2SD)" & cgf.type.label == "Stunting (HAZ)"& level == 2  & !is.na(super_region_name) & region_name == rid], aes(x = uhc, y = observed, group = location_id), alpha = 0.9, size = 1, color = "black")+
    geom_point(data = all.epitrans[severity == "Overall (<-2SD)" & cgf.type.label == "Stunting (HAZ)"& level == 2 & !is.na(super_region_name) & region_name == rid & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id), fill = "grey50", color = 'black', alpha = 0.9, size = 4, shape = 22)+
    labs(x = "", y = "", color = "Country", fill = "Country") +
    theme_bw() +
    theme(axis.title = element_text(size = 22), 
          axis.text = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 20),
          strip.text = element_text(size = 18),
          plot.title = element_text(size = 20),
          legend.position = "bottom",
          legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"), 
          legend.key = element_rect(fill = "grey90"),
          legend.box = 'vertical') +
    guides(color = guide_legend(title.position="top", title.hjust = 0.5),
           fill = guide_legend(title.position = 'top', title.hjust = 0.5)) +
    scale_x_continuous(breaks = c(0, .2, .4, .6, .8, 1), labels = c("0", "20", "40", "60", "80", "100"), limits = c(0, 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    facet_grid(cols = vars(cgf.type.label))
  
  one.region.epitrans.2 <- ggplot() +
    geom_line(data = all.splines.melted[severity == "Overall (<-2SD)" & cgf.type.label == "Wasting (WHZ)"], aes(x = uhc_value, y = value ), color = "black", size = 1.5) +
    geom_path(data = all.epitrans[severity == "Overall (<-2SD)" & cgf.type.label == "Wasting (WHZ)" & level == 3 & !is.na(super_region_name) & region_name == rid], aes(x = uhc, y = observed, group = location_id, color = location_name), alpha = 0.9, size = 1)+
    geom_point(data = all.epitrans[severity == "Overall (<-2SD)" & cgf.type.label == "Wasting (WHZ)"& level == 3& !is.na(super_region_name) & region_name == rid & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id, fill = location_name), color = 'black', alpha = 0.9, size = 3, shape = 21)+
    geom_path(data = all.epitrans[severity == "Overall (<-2SD)" & cgf.type.label == "Wasting (WHZ)"& level == 2  & !is.na(super_region_name) & region_name == rid], aes(x = uhc, y = observed, group = location_id), alpha = 0.9, size = 1, color = "black")+
    geom_point(data = all.epitrans[severity == "Overall (<-2SD)" & cgf.type.label == "Wasting (WHZ)"& level == 2 & !is.na(super_region_name) & region_name == rid & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id), fill = "grey50", color = 'black', alpha = 0.9, size = 4, shape = 22)+
    labs(x = "", y = "", color = "Country", fill = "Country") +
    theme_bw() +
    theme(axis.title = element_text(size = 22), 
          axis.text = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 20),
          strip.text = element_text(size = 18),
          plot.title = element_text(size = 20),
          legend.position = "bottom",
          legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"), 
          legend.key = element_rect(fill = "grey90"),
          legend.box = 'vertical', 
          title = element_text(size = 30, hjust = .5)) +
    guides(color = guide_legend(title.position="top", title.hjust = 0.5),
           fill = guide_legend(title.position = 'top', title.hjust = 0.5)) +
    scale_x_continuous(breaks = c(0, .2, .4, .6, .8, 1), labels = c("0", "20", "40", "60", "80", "100"), limits = c(0, 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    facet_grid(cols = vars(cgf.type.label))
  
  one.region.epitrans.3 <- ggplot() +
    geom_line(data = all.splines.melted[severity == "Overall (<-2SD)" & cgf.type.label == "Underweight (WAZ)"], aes(x = uhc_value, y = value ), color = "black", size = 1.5) +
    geom_path(data = all.epitrans[severity == "Overall (<-2SD)" & cgf.type.label == "Underweight (WAZ)" & level == 3 & !is.na(super_region_name) & region_name == rid], aes(x = uhc, y = observed, group = location_id, color = location_name), alpha = 0.9, size = 1)+
    geom_point(data = all.epitrans[severity == "Overall (<-2SD)" & cgf.type.label == "Underweight (WAZ)"& level == 3& !is.na(super_region_name) & region_name == rid & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id, fill = location_name), color = 'black', alpha = 0.9, size = 3, shape = 21)+
    geom_path(data = all.epitrans[severity == "Overall (<-2SD)" & cgf.type.label == "Underweight (WAZ)"& level == 2  & !is.na(super_region_name) & region_name == rid], aes(x = uhc, y = observed, group = location_id), alpha = 0.9, size = 1, color = "black")+
    geom_point(data = all.epitrans[severity == "Overall (<-2SD)" & cgf.type.label == "Underweight (WAZ)"& level == 2 & !is.na(super_region_name) & region_name == rid & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id), fill = "grey50", color = 'black', alpha = 0.9, size = 4, shape = 22)+
    labs(x = "", color = "Country", fill = "Country", y = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 22), 
          axis.text = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 20),
          strip.text = element_text(size = 18),
          plot.title = element_text(size = 20),
          legend.position = "bottom",
          legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"), 
          legend.key = element_rect(fill = "grey90"),
          legend.box = 'vertical') +
    guides(color = guide_legend(title.position="top", title.hjust = 0.5),
           fill = guide_legend(title.position = 'top', title.hjust = 0.5)) +
    scale_x_continuous(breaks = c(0, .2, .4, .6, .8, 1), labels = c("0", "20", "40", "60", "80", "100"), limits = c(0, 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    facet_grid(cols = vars(cgf.type.label), rows = vars(severity))
  
  one.region.epitrans.4 <- ggplot() +
    geom_line(data = all.splines.melted[severity == "Severe (<-3SD)" & cgf.type.label == "Stunting (HAZ)"], aes(x = uhc_value, y = value ), color = "black", size = 1.5) +
    geom_path(data = all.epitrans[severity == "Severe (<-3SD)" & cgf.type.label == "Stunting (HAZ)" & level == 3 & !is.na(super_region_name) & region_name == rid], aes(x = uhc, y = observed, group = location_id, color = location_name), alpha = 0.9, size = 1)+
    geom_point(data = all.epitrans[severity == "Severe (<-3SD)" & cgf.type.label == "Stunting (HAZ)"& level == 3& !is.na(super_region_name) & region_name == rid & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id, fill = location_name), color = 'black', alpha = 0.9, size = 3, shape = 21)+
    geom_path(data = all.epitrans[severity == "Severe (<-3SD)" & cgf.type.label == "Stunting (HAZ)"& level == 2  & !is.na(super_region_name) & region_name == rid], aes(x = uhc, y = observed, group = location_id), alpha = 0.9, size = 1, color = "black")+
    geom_point(data = all.epitrans[severity == "Severe (<-3SD)" & cgf.type.label == "Stunting (HAZ)"& level == 2 & !is.na(super_region_name) & region_name == rid & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id), fill = "grey50", color = 'black', alpha = 0.9, size = 4, shape = 22)+
    labs(x = "", y = "Prevalence", color = "Country", fill = "Country") +
    theme_bw() +
    theme(axis.title = element_text(size = 22), 
          axis.text = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 20),
          strip.text = element_text(size = 18),
          plot.title = element_text(size = 20),
          legend.position = "bottom",
          legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"), 
          legend.key = element_rect(fill = "grey90"),
          legend.box = 'vertical') +
    guides(color = guide_legend(title.position="top", title.hjust = 0.5),
           fill = guide_legend(title.position = 'top', title.hjust = 0.5)) +
    scale_x_continuous(breaks = c(0, .2, .4, .6, .8, 1), labels = c("0", "20", "40", "60", "80", "100"), limits = c(0, 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1))
  
  one.region.epitrans.5 <- ggplot() +
    geom_line(data = all.splines.melted[severity == "Severe (<-3SD)" & cgf.type.label == "Wasting (WHZ)"], aes(x = uhc_value, y = value ), color = "black", size = 1.5) +
    geom_path(data = all.epitrans[severity == "Severe (<-3SD)" & cgf.type.label == "Wasting (WHZ)" & level == 3 & !is.na(super_region_name) & region_name == rid], aes(x = uhc, y = observed, group = location_id, color = location_name), alpha = 0.9, size = 1)+
    geom_point(data = all.epitrans[severity == "Severe (<-3SD)" & cgf.type.label == "Wasting (WHZ)"& level == 3& !is.na(super_region_name) & region_name == rid & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id, fill = location_name), color = 'black', alpha = 0.9, size = 3, shape = 21)+
    geom_path(data = all.epitrans[severity == "Severe (<-3SD)" & cgf.type.label == "Wasting (WHZ)"& level == 2  & !is.na(super_region_name) & region_name == rid], aes(x = uhc, y = observed, group = location_id), alpha = 0.9, size = 1, color = "black")+
    geom_point(data = all.epitrans[severity == "Severe (<-3SD)" & cgf.type.label == "Wasting (WHZ)"& level == 2 & !is.na(super_region_name) & region_name == rid & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id), fill = "grey50", color = 'black', alpha = 0.9, size = 4, shape = 22)+
    labs(x = "", y = "", color = "Country", fill = "Country") +
    theme_bw() +
    theme(axis.title = element_text(size = 22), 
          axis.text = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 20),
          strip.text = element_text(size = 18),
          plot.title = element_text(size = 20),
          legend.position = "bottom",
          legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"), 
          legend.key = element_rect(fill = "grey90"),
          legend.box = 'vertical') +
    guides(color = guide_legend(title.position="top", title.hjust = 0.5),
           fill = guide_legend(title.position = 'top', title.hjust = 0.5)) +
    scale_x_continuous(breaks = c(0, .2, .4, .6, .8, 1), labels = c("0", "20", "40", "60", "80", "100"), limits = c(0, 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) 
  
  one.region.epitrans.6 <- ggplot() +
    geom_line(data = all.splines.melted[severity == "Severe (<-3SD)" & cgf.type.label == "Underweight (WAZ)"], aes(x = uhc_value, y = value ), color = "black", size = 1.5) +
    geom_path(data = all.epitrans[severity == "Severe (<-3SD)" & cgf.type.label == "Underweight (WAZ)" & level == 3 & !is.na(super_region_name) & region_name == rid], aes(x = uhc, y = observed, group = location_id, color = location_name), alpha = 0.9, size = 1)+
    geom_point(data = all.epitrans[severity == "Severe (<-3SD)" & cgf.type.label == "Underweight (WAZ)"& level == 3& !is.na(super_region_name) & region_name == rid & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id, fill = location_name), color = 'black', alpha = 0.9, size = 3, shape = 21)+
    geom_path(data = all.epitrans[severity == "Severe (<-3SD)" & cgf.type.label == "Underweight (WAZ)"& level == 2  & !is.na(super_region_name) & region_name == rid], aes(x = uhc, y = observed, group = location_id), alpha = 0.9, size = 1, color = "black")+
    geom_point(data = all.epitrans[severity == "Severe (<-3SD)" & cgf.type.label == "Underweight (WAZ)"& level == 2 & !is.na(super_region_name) & region_name == rid & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id), fill = "grey50", color = 'black', alpha = 0.9, size = 4, shape = 22)+
    labs(x = "", y = "", color = "Country", fill = "Country") +
    theme_bw() +
    theme(axis.title = element_text(size = 22), 
          axis.text = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 20),
          strip.text = element_text(size = 18),
          plot.title = element_text(size = 20),
          legend.position = "bottom",
          legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"), 
          legend.key = element_rect(fill = "grey90"),
          legend.box = 'vertical') +
    guides(color = guide_legend(title.position="top", title.hjust = 0.5),
           fill = guide_legend(title.position = 'top', title.hjust = 0.5)) +
    scale_x_continuous(breaks = c(0, .2, .4, .6, .8, 1), labels = c("0", "20", "40", "60", "80", "100"), limits = c(0, 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    facet_grid(rows = vars(severity))
  
  one.region.epitrans.7 <- ggplot() +
    geom_line(data = all.splines.melted[severity == "Extreme (<-4SD)" & cgf.type.label == "Stunting (HAZ)"], aes(x = uhc_value, y = value ), color = "black", size = 1.5) +
    geom_path(data = all.epitrans[severity == "Extreme (<-4SD)" & cgf.type.label == "Stunting (HAZ)" & level == 3 & !is.na(super_region_name) & region_name == rid], aes(x = uhc, y = observed, group = location_id, color = location_name), alpha = 0.9, size = 1)+
    geom_point(data = all.epitrans[severity == "Extreme (<-4SD)" & cgf.type.label == "Stunting (HAZ)"& level == 3& !is.na(super_region_name) & region_name == rid & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id, fill = location_name), color = 'black', alpha = 0.9, size = 3, shape = 21)+
    geom_path(data = all.epitrans[severity == "Extreme (<-4SD)" & cgf.type.label == "Stunting (HAZ)"& level == 2  & !is.na(super_region_name) & region_name == rid], aes(x = uhc, y = observed, group = location_id), alpha = 0.9, size = 1, color = "black")+
    geom_point(data = all.epitrans[severity == "Extreme (<-4SD)" & cgf.type.label == "Stunting (HAZ)"& level == 2 & !is.na(super_region_name) & region_name == rid & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id), fill = "grey50", color = 'black', alpha = 0.9, size = 4, shape = 22)+
    labs(x = "", y = "", color = "Country", fill = "Country") +
    theme_bw() +
    theme(axis.title = element_text(size = 22), 
          axis.text = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 20),
          strip.text = element_text(size = 18),
          plot.title = element_text(size = 20),
          legend.position = "bottom",
          legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"), 
          legend.key = element_rect(fill = "grey90"),
          legend.box = 'vertical') +
    guides(color = guide_legend(title.position="top", title.hjust = 0.5),
           fill = guide_legend(title.position = 'top', title.hjust = 0.5)) +
    scale_x_continuous(breaks = c(0, .2, .4, .6, .8, 1), labels = c("0", "20", "40", "60", "80", "100"), limits = c(0, 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1))
  
  
  
  one.region.epitrans.8 <- ggplot() +
    geom_line(data = all.splines.melted[severity == "Extreme (<-4SD)" & cgf.type.label == "Wasting (WHZ)"], aes(x = uhc_value, y = value ), color = "black", size = 1.5) +
    geom_path(data = all.epitrans[severity == "Extreme (<-4SD)" & cgf.type.label == "Wasting (WHZ)" & level == 3 & !is.na(super_region_name) & region_name == rid], aes(x = uhc, y = observed, group = location_id, color = location_name), alpha = 0.9, size = 1)+
    geom_point(data = all.epitrans[severity == "Extreme (<-4SD)" & cgf.type.label == "Wasting (WHZ)"& level == 3& !is.na(super_region_name) & region_name == rid & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id, fill = location_name), color = 'black', alpha = 0.9, size = 3, shape = 21)+
    geom_path(data = all.epitrans[severity == "Extreme (<-4SD)" & cgf.type.label == "Wasting (WHZ)"& level == 2  & !is.na(super_region_name) & region_name == rid], aes(x = uhc, y = observed, group = location_id), alpha = 0.9, size = 1, color = "black")+
    geom_point(data = all.epitrans[severity == "Extreme (<-4SD)" & cgf.type.label == "Wasting (WHZ)"& level == 2 & !is.na(super_region_name) & region_name == rid & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id), fill = "grey50", color = 'black', alpha = 0.9, size = 4, shape = 22)+
    labs(x = "Universal Health Coverage Index", y = "", color = "Country", fill = "Country") +
    theme_bw() +
    theme(axis.title = element_text(size = 22), 
          axis.text = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 20),
          strip.text = element_text(size = 18),
          plot.title = element_text(size = 20),
          legend.position = "bottom",
          legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"), 
          legend.key = element_rect(fill = "grey90"),
          legend.box = 'vertical') +
    guides(color = guide_legend(title.position="top", title.hjust = 0.5),
           fill = guide_legend(title.position = 'top', title.hjust = 0.5)) +
    scale_x_continuous(breaks = c(0, .2, .4, .6, .8, 1), labels = c("0", "20", "40", "60", "80", "100"), limits = c(0, 1)) +
    scale_y_continuous(labels = percent_format(accuracy = .5))
  
  one.region.epitrans.9 <- ggplot() +
    geom_line(data = all.splines.melted[severity == "Extreme (<-4SD)" & cgf.type.label == "Underweight (WAZ)"], aes(x = uhc_value, y = value ), color = "black", size = 1.5) +
    geom_path(data = all.epitrans[severity == "Extreme (<-4SD)" & cgf.type.label == "Underweight (WAZ)" & level == 3 & !is.na(super_region_name) & region_name == rid], aes(x = uhc, y = observed, group = location_id, color = location_name), alpha = 0.9, size = 1)+
    geom_point(data = all.epitrans[severity == "Extreme (<-4SD)" & cgf.type.label == "Underweight (WAZ)"& level == 3& !is.na(super_region_name) & region_name == rid & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id, fill = location_name), color = 'black', alpha = 0.9, size = 3, shape = 21)+
    geom_path(data = all.epitrans[severity == "Extreme (<-4SD)" & cgf.type.label == "Underweight (WAZ)"& level == 2  & !is.na(super_region_name) & region_name == rid], aes(x = uhc, y = observed, group = location_id), alpha = 0.9, size = 1, color = "black")+
    geom_point(data = all.epitrans[severity == "Extreme (<-4SD)" & cgf.type.label == "Underweight (WAZ)"& level == 2 & !is.na(super_region_name) & region_name == rid & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = uhc, y = observed, group = location_id), fill = "grey50", color = 'black', alpha = 0.9, size = 4, shape = 22)+
    labs(x = "", y = "", color = "Location", fill = "Location") +
    theme_bw() +
    theme(axis.title = element_text(size = 22), 
          axis.text = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 22),
          strip.text = element_text(size = 18),
          plot.title = element_text(size = 20),
          legend.position = "bottom",
          legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"), 
          legend.key = element_rect(fill = "grey90"),
          legend.box = 'vertical') +
    guides(color = guide_legend(title.position="top", title.hjust = 0.5, ncol = legend.cols),
           fill = guide_legend(title.position = 'top', title.hjust = 0.5, ncol = legend.cols)) +
    scale_x_continuous(breaks = c(0, .2, .4, .6, .8, 1), labels = c("0", "20", "40", "60", "80", "100"), limits = c(0, 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    facet_grid(rows = vars(severity))
  
  

  
  
  
  
  
  
  
  
  layout <- rbind(c(13, 1, 1, 1, 13),
                  c(13, 2, 3, 4, 13),
                  c(13, 5, 6, 7, 13),
                  c(13, 8, 9, 10, 13),
                  c(13, 13, 13, 13, 13),
                  c(13, 11, 11, 11, 13),
                  c(13, 12, 12, 12, 13),
                  c(13, 13, 13, 13, 13))
  
  title.plot <- ggplot() +
    labs(caption = rid) +
    theme_void() +
    theme(plot.caption = element_text(hjust = .5, size = 28, face = 'bold'))
  
  
  caption.plot <- ggplot() +
    theme_void() +
    labs(caption = expression(paste(bold("Appendix H: \n"), "Estimated values of all forms and severities of CGF are shown compared to expected values based on UHC Index for all locations with each region.\nPoints are shown for every five years from 1990-2020 with the grey line and grey squares representing the estimate for the aggregated region."))) +
    theme(plot.caption = element_text(hjust = 0, size = 18))
  
  country.legend <- get_legend(one.region.epitrans.9)
  
  empty <- ggplot() + theme_void()
  
  
  
  appendixh <- plot_grid(arrangeGrob(title.plot,
                                     one.region.epitrans.1 + theme(legend.position = "none"),
                                     one.region.epitrans.2 + theme(legend.position = "none"),
                                     one.region.epitrans.3 + theme(legend.position = "none"),
                                     one.region.epitrans.4 + theme(legend.position = "none"),
                                     one.region.epitrans.5 + theme(legend.position = "none"),
                                     one.region.epitrans.6 + theme(legend.position = "none"),
                                     one.region.epitrans.7 + theme(legend.position = "none"),
                                     one.region.epitrans.8 + theme(legend.position = "none"),
                                     one.region.epitrans.9 + theme(legend.position = "none"),
                                     country.legend, 
                                     caption.plot,
                                     empty,
                                     nrow=8, ncol = 5, widths = c(.05, 1, 1, 1, .05), heights = c(.2, 1, 1, 1,.05, .2, .3, .03), layout_matrix = layout))
  
  
  

  print(appendixh)
  
  
  
  
}

dev.off()












