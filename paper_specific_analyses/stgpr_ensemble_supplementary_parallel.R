
library(ggpubr, lib.loc = "filepath")
library(cowplot, lib.loc =  "filepath")
source("filepath")
library(scales)


'%notlike%' <- Negate('%like%')
'%notin%' <- Negate('%in%')

task_id <- as.integer(Sys.getenv("SGE_TASK_ID"))

args <- commandArgs(trailingOnly = TRUE)
param_map_filepath <- args[1]
param_map <- fread(param_map_filepath)


mod.run_id <- param_map[task_id, mod.id]
sev.run_id <- param_map[task_id, sev.id]
mean.run_id <- param_map[task_id, mean.id]

cgf.type <- param_map[task_id, cgf.type.name]
cgf.type.symbol <- param_map[task_id, cgf.type.letters]

mod.xw <- param_map[task_id, mod.cross]
sev.xw <- param_map[task_id, sev.cross]
mean.xw <- param_map[task_id, mean.cross]




pdf_filepath = "filepath"


stage_diagnostics <- function(mod.run_id, sev.run_id, mean.run_id, mod.xw, sev.xw, mean.xw, pdf_filepath, cgf.type,cgf.type.symbol, axis = "None", min_yaxis = NULL, max_yaxis = NULL){
  
  
  source("filepath"  )
  source("filepath"  )
  
  #dealing with age groups
  
  age_metadata <- get_age_metadata(age_group_set_id = 19)
  age_metadata.old <- get_age_metadata(age_group_set_id = 12)
  
  age_ids_in_common <- intersect(age_metadata$age_group_id, age_metadata.old$age_group_id)
  
  '%ni%' <- Negate('%in%')
  
  age_metadata.old.to.add <- age_metadata.old[age_group_id %ni% age_ids_in_common]
  
  age_metadata <- rbind(age_metadata.old.to.add, age_metadata)
  
  age_metadata <- age_metadata[order(age_group_years_start)]
  
  #labeling age columns and sex rows
  
  age_group_labels <- get_ids("age_group")
  sex_labels <- get_ids("sex")
  
  
  #dealing with moderate data and est
  
  mod.data <- model_load(mod.run_id, "data")
  mod.data[is.na(data), data := outlier_value]
  mod.data[, `:=` (type = "mod.data", comparison = mod.run_id)]
  
  mod.est <- model_load(mod.run_id, "raked")
  t <- model_load(mod.run_id, "raked")
  
  mod.est[, `:=` (type = "mod.estimates", comparison = mod.run_id)]
  
  setnames(mod.data, "data", "mean")
  setnames(mod.est, "gpr_mean", "mean")
  
  
  #dealing with severe data and est
  
  sev.data <- model_load(sev.run_id, "data")
  sev.data[is.na(data), data := outlier_value]
  sev.data[, `:=` (type = "sev.data", comparison = sev.run_id)]
  
  sev.est <- model_load(sev.run_id, "raked")
  sev.est[, `:=` (type = "sev.estimates", comparison = sev.run_id)]
  
  setnames(sev.data, "data", "mean")
  setnames(sev.est, "gpr_mean", "mean")
  
  
  #dealing with mean data and est
  
  mean.data <- model_load(mean.run_id, "data")
  mean.data[is.na(data), data := outlier_value]
  mean.data[, `:=` (type = "mean.data", comparison = mean.run_id)]
  
  mean.est <- model_load(mean.run_id, "raked")
  mean.est[, `:=` (type = "mean.estimates", comparison = mean.run_id)]
  
  setnames(mean.data, "data", "mean")
  setnames(mean.est, "gpr_mean", "mean")
  
  
  
  
  
  
  
  # comparison.est
  data_and_est <- rbind(mod.data, mod.est, sev.data, sev.est, mean.data, mean.est,
                        use.names = T, fill = T)
  
  
  
  
  data_and_est[, Stage := factor(x = type, levels = c("mod.data", "mod.estimates", "sev.data", "sev.estimates", "mean.estimates"), 
                                 labels = c("Moderate Data", "Moderate Estimates", "Severe Data", "Severe Estimates", "Mean Estimates"))]
  
  data_and_est[, is_outlier := as.factor(is_outlier)]
  
  data_and_est[, comparison := factor(comparison, levels = c(mod.run_id, sev.run_id, mean.run_id), labels = c("Moderate", "Severe",  "Mean"))]
  
  # Transform data into logit space  
  data_and_est<-data_and_est[type == "mod.data",`:=` (lower= exp(log(mean / (1-mean)) - 1.96* (variance*(1/(mean*(1-mean)))^2)^(1/2)) / (exp(log(mean / (1-mean)) - 1.96* (variance*(1/(mean*(1-mean)))^2)^(1/2)) + 1), 
                                                      upper=exp(log(mean / (1-mean)) + 1.96* (variance*(1/(mean*(1-mean)))^2)^(1/2)) / (exp(log(mean / (1-mean)) + 1.96* (variance*(1/(mean*(1-mean)))^2)^(1/2)) + 1))]
  
  data_and_est<-data_and_est[type == "sev.data",`:=` (lower= exp(log(mean / (1-mean)) - 1.96* (variance*(1/(mean*(1-mean)))^2)^(1/2)) / (exp(log(mean / (1-mean)) - 1.96* (variance*(1/(mean*(1-mean)))^2)^(1/2)) + 1), 
                                                      upper=exp(log(mean / (1-mean)) + 1.96* (variance*(1/(mean*(1-mean)))^2)^(1/2)) / (exp(log(mean / (1-mean)) + 1.96* (variance*(1/(mean*(1-mean)))^2)^(1/2)) + 1))]
  
  data_and_est<-data_and_est[type == "mean.data",`:=` (lower= exp(log(mean / (1-mean)) - 1.96* (variance*(1/(mean*(1-mean)))^2)^(1/2)) / (exp(log(mean / (1-mean)) - 1.96* (variance*(1/(mean*(1-mean)))^2)^(1/2)) + 1), 
                                                       upper=exp(log(mean / (1-mean)) + 1.96* (variance*(1/(mean*(1-mean)))^2)^(1/2)) / (exp(log(mean / (1-mean)) + 1.96* (variance*(1/(mean*(1-mean)))^2)^(1/2)) + 1))]
  
  
  data_and_est<-data_and_est[type == "mean.data",`:=` (lower= mean - 1.96* ((sqrt(variance))/sqrt(sample_size)), 
                                                       upper= mean+ 1.96* ((sqrt(variance))/sqrt(sample_size)))]
  
  
  # Add location data
  loc.met <- get_location_metadata(location_set_id = 35, release_id = 9)
  #countries <- loc.met[level == 3]
  countries <- loc.met[parent_id == 95]
  countries <- countries[order(super_region_name, region_name, path_to_top_parent )]
  
  
  
  data_and_est <- merge(data_and_est, loc.met, by = "location_id")
  data_and_est <- data_and_est[order(super_region_name, region_name, path_to_top_parent )]
  
  # Add sex labels
  data_and_est <- merge(data_and_est, sex_labels, all.x = T)
  
  # Add age_group_id labels
  data_and_est <- merge(data_and_est, age_group_labels, all.x = T, by = "age_group_id")
  data_and_est[, age_group_name := factor(age_group_name, levels = age_metadata$age_group_name, ordered = TRUE)]
  
  data_and_est[is.na(upper), upper:= max(mean), by = c("location_id")]
  data_and_est[age_group_id == 1, age_group_name := "Under 5 Aggregated"]
  
  # -----
  
  
  
  
  
  
  # doing some prep work for the study tables that will be on the right side of this page
  
  study.names <- fread("filepath")
  
  study.names[, survey_title := gsub("Multiple Indicator Cluster Survey", "MICS", survey_title)]
  study.names[, survey_title := gsub("Multiple Cluster Indicator Survey", "MICS", survey_title)]
  study.names[, survey_title := gsub("Demographic and Health Survey", "DHS", survey_title)]
  study.names[, survey_title := gsub("Survey of Diet and Nutritional Status", "SDNS", survey_title)]
  study.names[, survey_title := gsub("C\U3e34663cte d'Ivoire", "Côte d'Ivoire", survey_title)]
  study.names[, survey_title := gsub("WHO Global Database on Child Growth and Malnutrition", "WHO CGM Database", survey_title)]
  study.names[, survey_title := gsub("[0-9]{4}-[0-9]{4}", "", survey_title)]
  study.names[, survey_title := gsub("[0-9]{4}", "", survey_title)]
  
  #doing some spot checks on specific surveys labels that were giving issues
  study.names[, survey_title := gsub("Serbia and Montenegro - ", "", survey_title)]
  study.names[, survey_title := gsub("Serbia and Montenegro", "", survey_title)]
  study.names[, survey_title := gsub("and Montenegro −  ", "", survey_title)]
  study.names[, survey_title := gsub("Serbia and  ", "", survey_title)]
  study.names[, survey_title := gsub("Montenegro - ", "", survey_title)]
  study.names[, survey_title := gsub("Montenegro", "", survey_title)]
  study.names[, survey_title := gsub("Serbia", "", survey_title)]
  study.names[, survey_title := gsub("South Korea", "", survey_title)]
  study.names[, survey_title := gsub("North Korea", "", survey_title)]
  study.names[, survey_title := gsub("Bolivia", "", survey_title)]
  study.names[, survey_title := gsub("Somaliland", "", survey_title)]
  study.names[, survey_title := gsub("Sudan - South", "", survey_title)]
  study.names[, survey_title := gsub("Sudan - North", "", survey_title)]
  study.names[, survey_title := gsub("North Sudan", "", survey_title)]
  study.names[, survey_title := gsub("South Sudan", "", survey_title)]
  study.names[, survey_title := gsub("Sudan", "", survey_title)]
  study.names[, survey_title := gsub("Papau New Guinea", "", survey_title)]
  study.names[, survey_title := gsub("Laos", "", survey_title)]
  study.names[, survey_title := gsub("Laos", "", survey_title)]
  study.names[, survey_title := gsub("Vietnam", "", survey_title)]
  study.names[, survey_title := gsub("Zaire", "", survey_title)]
  study.names[, survey_title := gsub("Tanzania - ", "", survey_title)]
  study.names[, survey_title := gsub("Tanzania", "", survey_title)]
  study.names[, survey_title := gsub("Swaziland", "", survey_title)]
  study.names[, survey_title := gsub("Togo", "", survey_title)]
  study.names[, survey_title := gsub("Madagascar", "", survey_title)]
  study.names[, survey_title := gsub("Hungary", "", survey_title)]
  
  
  
  
  
  invisible(sapply(list.files("filepath", full.names = T), source))
  
  mod.crosswalk <- get_crosswalk_version(mod.xw)
  sev.crosswalk <- get_crosswalk_version(sev.xw)
  mean.crosswalk <- get_crosswalk_version(mean.xw)
  
  mod.nids <- unique(mod.crosswalk$nid)
  sev.nids <- unique(sev.crosswalk$nid)
  mean.nids <- unique(mean.crosswalk$nid)
  
  all.nids <- append(mod.nids, sev.nids)
  all.nids <- append(all.nids, mean.nids)
  
  
  
  mod.xw.nid.years <- mod.crosswalk[, c("nid", "year_id")]
  sev.xw.nid.years <- sev.crosswalk[, c("nid", "year_id")]
  mean.xw.nid.years <- mean.crosswalk[, c("nid", "year_id")]
  
  all.nid.years <- rbind(mod.xw.nid.years, sev.xw.nid.years, mean.xw.nid.years)
  
  
  boldsplit <- function( string , split ){
    require( stringr )
    blurb <- paste( string )
    blurbs <- strsplit( blurb , paste(split) )
    annot <- bquote( paste( bold( .( blurbs[[1]][1] ) ) , .(split) , .(blurbs[[1]][2]) , sep = "" ) )
    return( annot )
  }
  
  
  output.param <- fread("filepath")
  
  
  
  
  
  for(loc.id in unique(countries$location_id)){
    
    
    
    output.file <- paste0(output.param[location_id == loc.id & plot.type == cgf.type]$filename, ".pdf")
    
    final.path <- paste0(pdf_filepath, output.file)
    
    
    pdf(final.path, width = 24.1, height = 15.7)
    
    lname <- countries[location_id == loc.id]$location_name
    
    print(paste0("Starting Location ID ", loc.id, " : ", lname))
    
    
    
    title <- boldsplit(string = paste0(lname, " - ", cgf.type, " (", cgf.type.symbol,")"), split = "-")
    
    
    subtitle = paste0("Overall and Severe ", cgf.type, " Prevalence")
    
    location.plot <- ggplot() +
      geom_ribbon(data = data_and_est[location_id == loc.id  & type == "mod.estimates" & comparison == "Moderate" , ], aes(x = year_id, ymin = gpr_lower, ymax = gpr_upper, group = location_id), fill = "#cf8417", alpha = .2) +
      geom_ribbon(data = data_and_est[location_id == loc.id  & type == "sev.estimates" & comparison == "Severe" , ], aes(x = year_id, ymin = gpr_lower, ymax = gpr_upper, group = location_id), fill = "#8a1228", alpha = .2) +
      geom_line(data = data_and_est[location_id == loc.id  & type == "mod.estimates" & comparison == "Moderate" , ], aes(x = year_id, y = mean, group = location_id), color = "#f5ac40", alpha = 0.8, size = 1) +
      geom_line(data = data_and_est[location_id == loc.id  & type == "sev.estimates" & comparison == "Severe" , ], aes(x = year_id, y = mean, group = location_id), color = "#8a1228", alpha = 0.8, size = 1) +
      geom_segment(data = data_and_est[location_id == loc.id & type %like% "data" & type %notlike% "mean" & is_outlier == 0 & nid %in% study.names[confidentiality %in% c("Public", "Composite source, cite")]$nid, ],aes(x = year_id, y = lower, xend = year_id, yend = upper), color = "Black",alpha = 0.7)+
      geom_point(data = data_and_est[location_id == loc.id  & type == "mod.data"  & comparison == "Moderate" & is_outlier == 0 & nid %in% study.names[confidentiality %in% c("Public", "Composite source, cite")]$nid, ], aes(x = year_id, y = mean), shape = 21, fill = "#cf8417", alpha = 0.9, size = 2.5) +
      geom_point(data = data_and_est[location_id == loc.id & type == "sev.data"  & comparison == "Severe" & is_outlier == 0 & nid %in% study.names[confidentiality %in% c("Public", "Composite source, cite")]$nid, ], aes(x = year_id, y = mean), shape = 21, fill = "#8a1228", alpha = 0.9, size = 2.5) +
      geom_point(data = data_and_est[location_id == loc.id  & type == "mod.data"  & comparison == "Moderate" & is_outlier == 1 & nid %in% study.names[confidentiality %in% c("Public", "Composite source, cite")]$nid, ], aes(x = year_id, y = mean), shape = 4, color = "#cf8417", alpha = 0.8, show.legend = F) +
      geom_point(data = data_and_est[location_id == loc.id  & type == "sev.data"  & comparison == "Severe" & is_outlier == 1 & nid %in% study.names[confidentiality %in% c("Public", "Composite source, cite")]$nid, ], aes(x = year_id, y = mean), shape = 4, color = "#8a1228", alpha = 0.8, show.legend = F) +
      facet_grid(sex ~ age_group_name) + 
      theme_bw() + 
      
      # Theme Control
      xlim(1990, 2020) + 
      labs(x = "Year", y = "Prevalence", title = title, subtitle = subtitle) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size = 22, face = "bold"),
            axis.title = element_text(size = 16, face = "bold"),
            strip.text = element_text(size =14),
            plot.subtitle = element_text(size = 16)) + 
      scale_shape_manual(name = "Outliers") 
    
    if(axis == "global_yaxis"){
      ylim_max = max(data_and_est[is.na(is_outlier) | is_outlier==0, mean], na.rm = T)
      location.plot <- location.plot + ylim(0, ylim_max)
    } else if(axis == "0_1"){
      ylim_max = 1
      location.plot <- location.plot + ylim(0, ylim_max)
    } else if(axis == "custom" & !is.null(min_yaxis) & !is.null(max_yaxis)){
      location.plot <- location.plot + ylim(min_yaxis, max_yaxis)
    } else {
      dy<-data_and_est[location_id == loc.id & type %like% "data"&(is_outlier == 0) & type %notlike% "mean",]
      if((dy[,.N]==0)){
        max_y <- max(data_and_est[location_id == loc.id & type %like% "estimate" & type %notlike% "mean", upper], na.rm = T)
        
      }
      else{
        max_y <-max( max(data_and_est[location_id == loc.id & type %like% "data" &(is_outlier == 0) & type %notlike% "mean", mean], na.rm = T),
                     max(data_and_est[location_id == loc.id & type %like% "data" &(is_outlier == 0) & type %notlike% "mean",upper], na.rm = T),
                     max(data_and_est[location_id == loc.id & type %like% "estimate" & type %notlike% "mean", gpr_upper], na.rm = T))
        
      }
      ylim_max <-1.02* max_y
      #max( data_and_est[is.na(is_outlier)|is_outlier==0, mean], na.rm = T)  #change Y-axis maximum to the next gridline after the highest non-outlier data point
      location.plot <- location.plot +       scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, ylim_max))
      
      
    }
    
    
    
    
    mean.plot <- ggplot() +
      geom_ribbon(data = data_and_est[location_id == loc.id & type == "mean.estimates" & sex_id == 1,], aes(x = year_id, ymin = gpr_lower, ymax = gpr_upper, group = location_id), fill = "dodgerblue3", alpha = .2) +
      geom_ribbon(data = data_and_est[location_id == loc.id & type == "mean.estimates" & sex_id == 2,], aes(x = year_id, ymin = gpr_lower, ymax = gpr_upper, group = location_id), fill = "firebrick3", alpha = .2) +
      geom_line(data = data_and_est[location_id == loc.id & type == "mean.estimates" & sex_id == 1,], aes(x = year_id, y = mean, group = location_id), color = "dodgerblue3", alpha = .8, size = 1) +
      geom_line(data = data_and_est[location_id == loc.id & type == "mean.estimates" & sex_id == 2,], aes(x = year_id, y = mean, group = location_id), color = "firebrick3", alpha = .8, size = 1) +
      geom_segment(data = data_and_est[location_id == loc.id & type %like% "mean.data" & is_outlier == 0 & nid %in% study.names[confidentiality %in% c("Public", "Composite source, cite")], ],aes(x = year_id, y = lower, xend = year_id, yend = upper), color = "Black",alpha = 0.7)+
      geom_point(data = data_and_est[location_id == loc.id  & type == "mean.data"   & is_outlier == 0 & sex_id == 1 & nid %in% study.names[confidentiality %in% c("Public", "Composite source, cite")]$nid, ], aes(x = year_id, y = mean), shape = 21, fill = "dodgerblue3", alpha = 0.9, size = 2.5) +
      geom_point(data = data_and_est[location_id == loc.id  & type == "mean.data"   & is_outlier == 1 & sex_id == 1 & nid %in% study.names[confidentiality %in% c("Public", "Composite source, cite")]$nid, ], aes(x = year_id, y = mean), shape = 4, color = "dodgerblue3", alpha = 0.8, show.legend = F) +
      geom_point(data = data_and_est[location_id == loc.id  & type == "mean.data"   & is_outlier == 0 & sex_id == 2 & nid %in% study.names[confidentiality %in% c("Public", "Composite source, cite")]$nid, ], aes(x = year_id, y = mean), shape = 21, fill = "firebrick3", alpha = 0.9, size = 2.5) +
      geom_point(data = data_and_est[location_id == loc.id  & type == "mean.data"   & is_outlier == 1 & sex_id == 2 & nid %in% study.names[confidentiality %in% c("Public", "Composite source, cite")]$nid, ], aes(x = year_id, y = mean), shape = 4, color = "firebrick3", alpha = 0.8, show.legend = F) +
      #geom_ribbon(data = data_and_est[location_id == loc.id & type == "estimates" & comparison == reference.run_id, ], aes(x = year_id, ymin = gpr_lower, ymax = gpr_upper), fill = "purple", alpha = 0.1) +
      #geom_line(data = data_and_est[location_id == loc.id & type != "data", ], aes(x = year_id, y = mean, color = Stage, linetype = comparison)) +
      facet_wrap(~age_group_name, ncol = 7) + 
      theme_bw() + 
      labs(subtitle = paste0("Transformed Mean ", cgf.type, " Z Scores: (Estimate * 10) - 10 = Mean Z Score")) + 
      
      
      # Theme Control
      xlab("Year") + xlim(1990, 2020) + ylab("Transformed Mean Z Score") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 16, face = "bold"),
            strip.text = element_text(size  =14),
            plot.subtitle = element_text(size = 16)) + 
      scale_shape_manual(name = "Outliers", values= c(19,4)) 
    

    
    caption.plot <- ggplot() +
      theme_void() +
      labs(caption = expression(paste(bold("       Appendix B: \n \n \n \n \n"), "Spatio-temporal regression results for overall, severe, and mean CGF are shown for each location. Surveys included in the models are shown in the table on the right. Surveys that \nwere outliered are shown with X's on all plots. Surveys prior to 1990 may have been inputs to the models to inform trends, but estimates are only produced and shown for 1990-2020.\nDHS is Demographic and Health Surveys. MICS is Multiple Indicator Cluster Survey. WHO CGM is the WHO Global Database on Child Growth and Malnutrition. SDNS is Survey of \nDiet and Nutritional Status. Surveys conducted over a range of years were assigned to the midpoint year from that interval, which is the year reflected in the table and the plots. For \nlocations that are modeled nationally and subnationally, sources that are only included subnationally are not included in the plots of national level estimates. These sources were \nincluded in subnational models that influence national level models. Note that due to the transformation on mean Z scores, increasing values reflect improvements in mean Z score."))) +
      theme(plot.caption = element_text(size = 13, hjust = 0))
    
    
    
    
    test.dt <- data.table(row = c(1:4), name = c("Overall (<-2SD)", "Severe (<-3SD)", "Overall (<-2SD)", "Severe (<-3SD)"), val = c(90, 15, 36, 122), sex = c("Male", "Male", "Female", "Female"))
    
    severity.fake.plot <-  ggplot(test.dt) +
      geom_line(aes(x = row, y = val, color = name)) + 
      scale_color_manual(values = c("#cf8417", "#8a1228")) +
      guides(color = guide_legend(override.aes = list(size = 2), title.hjust = .5)) +
      labs(color = "Severity") +
      theme(legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90"),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 16))
    
    severity.legend <- get_legend(severity.fake.plot)
    
    
    
    severity.with.legend <- ggarrange(location.plot, severity.legend, ncol = 2, nrow = 1, widths = c(7.4, 1))
    
    
    
    
    sex.fake.plot <-  ggplot(test.dt) +
      geom_line(aes(x = row, y = val, color = sex)) + 
      scale_color_manual(values = c("firebrick3", "dodgerblue3")) +
      guides(color = guide_legend(override.aes = list(size = 2), title.hjust = .5)) +
      labs(color = "Sex") +
      theme(legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
            legend.key = element_rect(fill = "grey90"),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 16))
    
    
    sex.legend <- get_legend(sex.fake.plot)
    
    
    
    
    empty <- ggplot() + theme_void()
    
    sex.with.legend <- ggarrange(mean.plot, sex.legend, ncol = 2, nrow = 1, widths = c(7, 1))
    sex.with.legend <- ggarrange(sex.with.legend, empty, caption.plot, ncol = 1, nrow = 3, heights = c(1, .05, .3))
    
    
    
    all.combined.plot <- ggarrange(severity.with.legend, sex.with.legend, ncol = 1, nrow = 2, heights = c(2, 1.3))
    
    
    
    
    # creating table listing studies next to that
    
    
    ihme.loc.id <- loc.met[location_id == loc.id]$ihme_loc_id
    
    
    testing.if.subnats <- nrow(loc.met[ihme_loc_id %like% ihme.loc.id])
    
    #formatting one table for all countries that we do NOT model subnationally
    if (testing.if.subnats == 1 | loc.id %in% c(63, 93)){
      
      mod.crosswalk[,nid.year := paste0(nid, "_", year_id)]
      sev.crosswalk[,nid.year := paste0(nid, "_", year_id)]
      mean.crosswalk[,nid.year := paste0(nid, "_", year_id)]
      
      
      mod.country.nids <- unique(mod.crosswalk[location_id == loc.id]$nid.year)
      sev.country.nids <- unique(sev.crosswalk[location_id == loc.id]$nid.year)
      mean.country.nids <- unique(mean.crosswalk[location_id == loc.id]$nid.year)
      
      all.country.nids <- append(mod.country.nids, sev.country.nids)
      all.country.nids <- append(all.country.nids, mean.country.nids)
      all.country.nids <- unique(all.country.nids)
      
      data.test <- copy(all.country.nids)
      
      if(length(data.test)>0){
        
        all.country.nids <- as.data.table(transpose(str_split(all.country.nids, "_")))
        setnames(all.country.nids, c("V1", "V2"), c("nid", "year_id"))
        all.country.nids$nid <- as.integer(all.country.nids$nid)
        all.country.nids$year_id <- as.integer(all.country.nids$year_id)
        
        
        reporting.nids <- merge(all.country.nids, study.names, by = "nid")
        
        reporting.nids <- reporting.nids[confidentiality %in% c("Public", "Cite, don't share data", "Composite source, cite")]
        
        reporting.nids <- reporting.nids[order(year_id)]
        
        
        reporting.nids <- reporting.nids[, c("survey_title", "year_id")]
        setnames(reporting.nids, c("survey_title", "year_id"), c("Source", "Year"))
        
        
        reporting.nids[, Source :=str_wrap(Source,52)]
        
        
        lname.dash <- paste0(lname, " -")
        
        reporting.nids[, Source := gsub(lname.dash, "", Source)]
        reporting.nids[, Source := gsub(lname, "", Source)]
        
        
        reporting.nids <- reporting.nids[,c(2,1)]
        
        if(nrow(reporting.nids) < 35){nid.table <- ggtexttable(reporting.nids, rows = NULL, theme = ttheme(base_size = 17))}
        if(nrow(reporting.nids) > 34 & nrow(reporting.nids) < 50){nid.table <- ggtexttable(reporting.nids, rows = NULL, theme = ttheme(base_size = 14))}
        if(nrow(reporting.nids) > 49){nid.table <- ggtexttable(reporting.nids, rows = NULL, theme = ttheme(base_size = 12))}
      }else if(length(data.test) == 0){
        reporting.nids <- data.table(Source = "No sources for this location")
        nid.table <- ggtexttable(reporting.nids, rows = NULL, theme = ttheme(base_size = 18))
        
      }
      
      
      
    }
    
    
    
    #formatting one table for all countries that we DO model subnationally
    if (testing.if.subnats > 1 & loc.id %ni% c(63, 93)){
      
      
      all.subnat.locs <- loc.met[ihme_loc_id %like% ihme.loc.id & location_id != loc.id]$location_id
      
      
      mod.crosswalk[,nid.year := paste0(nid, "_", year_id)]
      sev.crosswalk[,nid.year := paste0(nid, "_", year_id)]
      mean.crosswalk[,nid.year := paste0(nid, "_", year_id)]
      
      mod.subnat.nids <- unique(mod.crosswalk[location_id %in% all.subnat.locs]$nid.year)
      sev.subnat.nids <- unique(sev.crosswalk[location_id %in% all.subnat.locs]$nid.year)
      mean.subnat.nids <- unique(mean.crosswalk[location_id %in% all.subnat.locs]$nid.year)
      
      all.subnat.nids <- append(mod.subnat.nids, sev.subnat.nids)
      all.subnat.nids <- append(all.subnat.nids, mean.subnat.nids)
      all.subnat.nids <- unique(all.subnat.nids)
      
      if(length(all.subnat.nids) > 0){
        
        
        all.subnat.nids <- as.data.table(transpose(str_split(all.subnat.nids, "_")))
        setnames(all.subnat.nids, c("V1", "V2"), c("nid", "year_id"))
        all.subnat.nids$nid <- as.integer(all.subnat.nids$nid)
        all.subnat.nids$year_id <- as.integer(all.subnat.nids$year_id)
        
        
        
        
        # this is the total set of NIDs used SUBNATIONALLY
        
        reporting.subnat.nids <- merge(all.subnat.nids, study.names, by = "nid")
        
        
        reporting.subnat.nids <- reporting.subnat.nids[confidentiality %in% c("Public", "Cite, don't share data", "Composite source, cite")]
        
      }
      
      mod.country.nids <- unique(mod.crosswalk[location_id == loc.id]$nid.year)
      sev.country.nids <- unique(sev.crosswalk[location_id == loc.id]$nid.year)
      mean.country.nids <- unique(mean.crosswalk[location_id == loc.id]$nid.year)
      
      all.country.nids <- append(mod.country.nids, sev.country.nids)
      all.country.nids <- append(all.country.nids, mean.country.nids)
      all.country.nids <- unique(all.country.nids)
      
      if(length(all.country.nids) > 0){
        
        all.country.nids <- as.data.table(transpose(str_split(all.country.nids, "_")))
        setnames(all.country.nids, c("V1", "V2"), c("nid", "year_id"))
        all.country.nids$nid <- as.integer(all.country.nids$nid)
        all.country.nids$year_id <- as.integer(all.country.nids$year_id)
        
      }
      
      
      
      
      if(length(all.subnat.nids) > 0 & length(all.country.nids) > 0){ # these are countries with data both nationally AND subnationally
        
        # this is the total set of NIDs used NATIONALLY
        
        reporting.country.nids <- merge(all.country.nids, study.names, by = "nid")
        
        
        reporting.country.nids <- reporting.country.nids[confidentiality %in% c("Public", "Cite, don't share data", "Composite source, cite")]
        
        
        # we now know which sources were included subnationally and nationally
        
        reporting.subnat.nids[, Subnational := "X"]
        reporting.country.nids[, National := "X"]
        
        final.dt <- merge(reporting.country.nids, reporting.subnat.nids, by = c("nid", "year_id", "survey_title", "confidentiality"), all = TRUE)
        
        
        final.dt <- final.dt[order(year_id)]
        final.dt$nid <- NULL
        final.dt$confidentiality <- NULL
        setnames(final.dt, "survey_title", "Source")
        setnames(final.dt, "year_id", "Year")
        final.dt[is.na(National), National := ""]
        final.dt[is.na(Subnational), Subnational := ""]
        
        lname.dash <- paste0(lname, " -")
        
        final.dt[, Source := gsub(lname.dash, "", Source)]
        final.dt[, Source := gsub(lname, "", Source)]
        
        
        if(nrow(final.dt) > 37){
          final.dt[, Source :=str_wrap(Source,60)]
          nid.table <- ggtexttable(final.dt, rows = NULL)
        }else if(nrow(final.dt) <38 & nrow(final.dt) > 20){
          final.dt[, Source :=str_wrap(Source,42)]
          nid.table <- ggtexttable(final.dt, rows = NULL, theme = ttheme(base_size = 14))
        }else if(nrow(final.dt) < 21){
          final.dt[, Source :=str_wrap(Source,30)]
          nid.table <- ggtexttable(final.dt, rows = NULL, theme = ttheme(base_size = 15))
        }
        
        
      }else if(length(all.subnat.nids) == 0 & length(all.country.nids) > 0){ #these are countries with no subnational data, but have national data
        
        reporting.country.nids <- merge(all.country.nids, study.names, by = "nid")
        reporting.country.nids <- reporting.country.nids[confidentiality %in% c("Public", "Cite, don't share data", "Composite source, cite")]
        reporting.country.nids[, National := "X"]
        reporting.country.nids[, Subnational := ""]
        final.dt <- reporting.country.nids[order(year_id)]
        final.dt$nid <- NULL
        final.dt$confidentiality <- NULL
        setnames(final.dt, "survey_title", "Source")
        setnames(final.dt, "year_id", "Year")
        
        lname.dash <- paste0(lname, " -")
        
        final.dt[, Source := gsub(lname.dash, "", Source)]
        final.dt[, Source := gsub(lname, "", Source)]
        
        
        if(nrow(final.dt) > 39){
          final.dt[, Source :=str_wrap(Source,60)]
          nid.table <- ggtexttable(final.dt, rows = NULL)
        }else if(nrow(final.dt) <40 & nrow(final.dt) > 20){
          final.dt[, Source :=str_wrap(Source,42)]
          nid.table <- ggtexttable(final.dt, rows = NULL, theme = ttheme(base_size = 14))
        }else if(nrow(final.dt) < 21){
          final.dt[, Source :=str_wrap(Source,30)]
          nid.table <- ggtexttable(final.dt, rows = NULL, theme = ttheme(base_size = 17))
        }
        
      }else if(length(all.subnat.nids) >0 & length(all.country.nids) == 0){ # these are countries with no national data, but have subnational data
        
        reporting.subnat.nids <- merge(reporting.subnat.nids, study.names, by = "nid")
        reporting.subnat.nids <- reporting.subnat.nids[confidentiality %in% c("Public", "Cite, don't share data", "Composite source, cite")]
        reporting.subnat.nids[, National := ""]
        reporting.subnat.nids[, Subnational := "X"]
        final.dt <- reporting.subnat.nids[order(year_id)]
        final.dt$nid <- NULL
        final.dt$confidentiality <- NULL
        setnames(final.dt, "survey_title", "Source")
        setnames(final.dt, "year_id", "Year")
        
        lname.dash <- paste0(lname, " -")
        
        final.dt[, Source := gsub(lname.dash, "", Source)]
        final.dt[, Source := gsub(lname, "", Source)]
        
        
        if(nrow(final.dt) > 39){
          final.dt[, Source :=str_wrap(Source,60)]
          nid.table <- ggtexttable(final.dt, rows = NULL)
        }else if(nrow(final.dt) <40 & nrow(final.dt) > 20){
          final.dt[, Source :=str_wrap(Source,42)]
          nid.table <- ggtexttable(final.dt, rows = NULL, theme = ttheme(base_size = 14))
        }else if(nrow(final.dt) < 21){
          final.dt[, Source :=str_wrap(Source,30)]
          nid.table <- ggtexttable(final.dt, rows = NULL, theme = ttheme(base_size = 17))
        }
        
        
      }else if(length(all.subnat.nids) == 0 & length(all.country.nids) == 0){ # these are countries with NO data at all
        reporting.nids <- data.table(Source = "No national or subnational sources for this location")
        nid.table <- ggtexttable(reporting.nids, rows = NULL, theme = ttheme(base_size = 17))
        
      }
      
      
      
      
    }
    
    
    
    
    final.plot <- plot_grid(all.combined.plot, nid.table,empty, nrow = 1, ncol = 3, rel_widths = c(7.8,3.2, .27))
    
    

    print(final.plot)
    
    dev.off()
    
  }
  
  
  
}


stage_diagnostics(mod.run_id, sev.run_id, mean.run_id, mod.xw, sev.xw, mean.xw, pdf_filepath, cgf.type,cgf.type.symbol, axis = "None", min_yaxis = NULL, max_yaxis = NULL)






