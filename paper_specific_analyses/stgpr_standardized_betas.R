
rm(list=ls())
Sys.umask(mode = 002)
os <- .Platform$OS.type
library(data.table)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(lme4)
source("filepath"  )
source("filepath"  )
source("filepath"  )
# ----- Params







param <- data.table(me_name = c("stunting_2", "stunting_3", "stunting_mean",
                                "wasting_2", "wasting_3", "wasting_mean",
                                "underweight_2", "underweight_3", "underweight_mean"),
                    me_title = c("Overall Stunting", "Severe Stunting", "Mean Stunting",
                                 "Overall Wasting", "Severe Wasting", "Mean Wasting",
                                 "Overall Underweight", "Severe Underweight", "Mean Underweight"),
                    stg.1.fp = c("filepath",
                                 "filepath",
                                 "filepath",
                                 "filepath",
                                 "filepath",
                                 "filepath",
                                 "filepath",
                                 "filepath",
                                 "filepath",))

param[, data.fp := paste0("filepath", me_name, "/", me_name, "_best_data.csv")]

param[, index := 1:.N]

for (i in unique(param$index)) {
  
  
  
  me_name = param[index == i]$me_name
  me_title = param[index == i]$me_title
  covids <- c(977, 205, 1099, 208, 881, 866)
  prior_signlist <- c(-1, 1, -1, -1, -1, 1) * -1
  pdf_dir <- "filepath" #
  data_transform = "logit"
  n_mods = 500
  gbd7_rmses_fp = param[index == i]$stg.1.fp
  
  dfp <- param[index == i]$data.fp
  
  
  
  
  
  
  # ----- Plot 
  prior_signs <- data.table(covariate_id = covids, 
                            prior_sign = prior_signlist )
  covs <- get_ids(table = "covariate")
  covs_map <- unique(covs[,.(covariate_id = covariate_id, variable = covariate_name_short)])
  covs_map <- covs_map[covariate_id %in% covids]
  prior_signs <- merge(covs_map, prior_signs)
  prior_signs[, variable := paste0(variable, "_fixd")]
  p2 <- fread(gbd7_rmses_fp)
  
  
  
  
  # Check out what was dropped
  p2[, .N, .(drop)]
  p2 <- p2[drop==0]
  
  p2[, weights := 1/out_rmse]
  p2[, weights := weights/sum(weights)]
  p2[, gbd_round_id := 7]
  p2.f <- p2[sex == "F" ]
  p2.m <- p2[sex == "M" ]
  p2.m <- p2.m[order(out_rmse)]
  
  if(nrow(p2.m) > n_mods){p2.m <- p2.m[1:n_mods, ]}
  if(nrow(p2.m) < n_mods){p2.m <- p2.m[1:nrow(p2.m), ]}
  if(nrow(p2.f) > n_mods){p2.f <- p2.f[1:n_mods, ]}
  if(nrow(p2.f) < n_mods){p2.f <- p2.f[1:nrow(p2.f), ]}
  prior_betas <- rbind(p2.m, p2.f, fill=TRUE)
  # Ratio of standardized betas
  ratio_data <- fread(dfp)
  ratio_data <- ratio_data[, c("sex_id", "data", covs_map$variable), with = F]
  ratio_data <- ratio_data[, lapply(.SD, sd), by = sex_id]
  ratio_data <- melt(ratio_data, id.vars = "sex_id", measure.vars = c(covs_map$variable, "data"))
  ratio_data <- merge(ratio_data, ratio_data[variable == "data", .(sex_id, denom = value)])
  ratio_data <- ratio_data[variable != "data"]
  ratio_data[, ratio := value / denom ]
  ratio_data[, variable := paste0(variable, "_fixd")]
  ratio_data <- ratio_data[, .(sex_id, variable, ratio)]
  ratio_data[sex_id == 1, sex := "M"]
  ratio_data[sex_id == 2, sex := "F"]
  prior_betas[, draws := round(weights * 10000)]
  #
  # Construct data
  
  
  
  expanded <- prior_betas[, .(draw = 1:draws), by = .(sex, covs, gbd_round_id)]
  
  
  prior_betas <- prior_betas[,c("out_rmse", "in_rmse", "sex", "gbd_round_id", "covs", "(Intercept)_fixd", paste0(unique(covs_map$variable), "_fixd")), with = F]
  prior_betas <- merge(prior_betas, expanded)
  prior_betas <- melt(prior_betas, measure.vars = paste0(unique(covs_map$variable), "_fixd"))
  prior_betas <- prior_betas[!is.na(value)]
  prior_betas[, gbd_round_id := as.factor(gbd_round_id)]
  prior_betas <- merge(prior_betas, prior_signs, all.x = T, by = "variable")
  prior_betas <- merge(prior_betas, ratio_data, by = c("sex", "variable"), all.x = T)
  prior_betas[, standardized_beta := value * ratio]
  # Check out which covs got more weight
  prior_betas[,.N,.(gbd_round_id, variable, prior_sign)][order(variable, gbd_round_id)]
  prior_betas[gbd_round_id == 7, GBD := "GBD 2020 Initial Models"]
  
  
  prior_betas[sex == "M", sex := "Males"]
  prior_betas[sex == "F", sex := "Females"]
  
  prior_betas[covariate_id == 208, variable := "Maternal Care and Immunization"]
  prior_betas[covariate_id == 866, variable := "Unsafe Sanitation SEV"]
  prior_betas[covariate_id == 977, variable := "Energy Unadjusted (kcal)"]
  prior_betas[covariate_id == 1099, variable := "Heathcare Access and Quality Index"]
  prior_betas[covariate_id == 205, variable := "Prevalence of Severe Anemia"]
  prior_betas[covariate_id == 881, variable := "Socio-demographic Index"]
  
  boldsplit <- function( string , split ){
    require( stringr )
    blurb <- paste( string )
    blurbs <- strsplit( blurb , paste(split) )
    annot <- bquote( paste( bold( .( blurbs[[1]][1] ) ) , .(split) , .(blurbs[[1]][2]) , sep = "" ) )
    return( annot )
  }
  
  
  
  empty <- ggplot() + theme_void()
  
  plot.st <- boldsplit(string = paste0(me_title, ": ", "Standardized ST-GPR Ensemble Prior Betas"), split = ":")
  
  
  caption <- ggplot() +
    labs(caption =expression(paste(bold("  Appendix C: \n"), "Standardized betas for ST-GPR stage 1 priors are shown by sex for all covariates included in ensemble priors. Note that because of a transformation to\nmean Z-score models, those relationships are in the opposite direction.", ))) +
    theme_void() +
    theme(plot.caption = element_text(hjust = 0, size = 14))
  
  
  
  pdf(file.path(pdf_dir, paste0(i,"betas.pdf")), width = 15, height = 10)
  gg1 <- ggplot(prior_betas) + 
    geom_vline(xintercept = 0, color = "grey65") +
    geom_histogram(aes(x=standardized_beta, y=..count../sum(..count..), group = sex, fill = sex), alpha = 0.5, 
                   position = "identity") + 
    facet_wrap(~variable) + 
    theme_bw() + 
    xlab("Standardized Betas") + 
    ylab("Density") +
    labs(fill = "Sex", subtitle = plot.st) +
    theme(strip.text = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          plot.subtitle = element_text(size = 16, face = "bold"),
          legend.title.align = .5,
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"), 
          legend.key = element_rect(fill = "grey90")) +
    theme(panel.spacing = unit(2, "lines"))
  
  
  
  final.plot <- ggarrange(empty, gg1, caption, nrow = 3, ncol = 1, heights = c(.03, 1, .1))
  
  print(final.plot)
  
  
  
  dev.off()
  
  
  
  
}



















