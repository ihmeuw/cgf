# CGF Paper Figure 1 Niger Example




slocs <- get_location_metadata(location_set_id = 101, release_id = 9)


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




########################################################################################################################
# Getting Overall, Severe, and Extreme Stunting Prevalences in 1990 and 2020 and Percent Changes Over that Span 
########################################################################################################################



stunting.id.names <- data.table(idval = c(10556, 8949, 26941, 23510),
                                name = c("Overall Stunting", "Severe Stunting", "Extreme Stunting", "Mean Stunting Z Score"))


for (meid in unique(stunting.id.names$idval)) {
  
  
  
  niger.stunting <- get_draws("modelable_entity_id", meid, year_id=c(1990,2020), 
                              source="epi", gbd_round_id=7, decomp_step="iterative", 
                              age_group_id = 1, sex_id = c(3), location_id = 213)
  
  
  niger.stunting <- melt(niger.stunting, id.vars = id.vars)
  niger.stunting <- merge(niger.stunting, populations)
  
  if(meid == 23510){niger.stunting[, value:= (value * 10) -10]}
  
  niger.stunting.1990.mean <- mean(niger.stunting[year_id == 1990]$value)
  niger.stunting.1990.lower <- quantile(niger.stunting[year_id == 1990]$value, probs = .025)
  niger.stunting.1990.upper <- quantile(niger.stunting[year_id == 1990]$value, probs = .975)
  
  niger.stunting.2020.mean <- mean(niger.stunting[year_id == 2020]$value)
  niger.stunting.2020.lower <- quantile(niger.stunting[year_id == 2020]$value, probs = .025)
  niger.stunting.2020.upper <- quantile(niger.stunting[year_id == 2020]$value, probs = .975)
  
  name <- stunting.id.names[idval == meid]$name
  
  
  if(meid %in% c(10556, 8949, 26941)){
    
    niger.stunting[, pct.change := (value-lag(value))/lag(value), by = c("age_group_id", "location_id", "sex_id", "metric_id", "measure_id", "modelable_entity_id", "model_version_id", "variable")]
    
    niger.stunting.pc.median <- median(niger.stunting[year_id == 2020 & !is.na(pct.change)]$pct.change)
    niger.stunting.pc.lower <- quantile(niger.stunting[year_id == 2020 & !is.na(pct.change)]$pct.change, probs = .025)
    niger.stunting.pc.upper <- quantile(niger.stunting[year_id == 2020 & !is.na(pct.change)]$pct.change, probs = .975)
    
    print(paste0("Niger ", name, " Prevalence in 1990: ", signif(niger.stunting.1990.mean*100, 3), "% (", signif(niger.stunting.1990.lower*100, 3), "% - ", signif(niger.stunting.1990.upper*100, 3), "%)."))
    print(paste0("Niger ", name, " Prevalence in 2020: ", signif(niger.stunting.2020.mean*100, 3), "% (", signif(niger.stunting.2020.lower*100, 3), "% - ", signif(niger.stunting.2020.upper*100, 3), "%)."))
    
    print(paste0("Niger ", name, " Percent Change 1990-2020: ", signif(niger.stunting.pc.median*100, 3), "% (", signif(niger.stunting.pc.lower*100, 3), "% - ", signif(niger.stunting.pc.upper*100, 3), "%)."))
    
  }
  
  if(meid == 23510){
    print(paste0("Niger ", name, " in 1990: ", round(niger.stunting.1990.mean, 2), " (", round(niger.stunting.1990.lower, 2), " - ", round(niger.stunting.1990.upper, 2), ")."))
    print(paste0("Niger ", name, " in 2020: ", round(niger.stunting.2020.mean, 2), " (", round(niger.stunting.2020.lower, 2), " - ", round(niger.stunting.2020.upper, 2), ")."))
    
  }
  
  
}




# calculating percent of overall stunting that is extreme stunting 


niger.overall.stunting <- get_draws("modelable_entity_id", 10556, year_id=c(1990,2020), 
                                    source="epi", gbd_round_id=7, decomp_step="iterative", 
                                    age_group_id = 1, sex_id = c(3), location_id = 213)

niger.extreme.stunting <- get_draws("modelable_entity_id", 26941, year_id=c(1990,2020), 
                                    source="epi", gbd_round_id=7, decomp_step="iterative", 
                                    age_group_id = 1, sex_id = c(3), location_id = 213)

niger.overall.stunting <- melt(niger.overall.stunting, id.vars = id.vars)
niger.overall.stunting <- merge(niger.overall.stunting, populations)

niger.extreme.stunting <- melt(niger.extreme.stunting, id.vars = id.vars)
niger.extreme.stunting <- merge(niger.extreme.stunting, populations)

niger.overall.stunting <- niger.overall.stunting[, -c("modelable_entity_id", "model_version_id")]
niger.extreme.stunting <- niger.extreme.stunting[, -c("modelable_entity_id", "model_version_id")]

setnames(niger.overall.stunting, "value", "overall.value")
setnames(niger.extreme.stunting, "value", "extreme.value")

niger.overall.extreme.stunting <- merge(niger.overall.stunting, niger.extreme.stunting, by = c("variable", "year_id", "age_group_id", "location_id", "sex_id", "metric_id", "measure_id", "population"))

niger.overall.extreme.stunting[, extreme.prop := extreme.value/overall.value]

niger.extreme.stunting.prop.1990.mean <- mean(niger.overall.extreme.stunting[year_id == 1990]$extreme.prop) * 100
niger.extreme.stunting.prop.1990.lower <- unname(quantile(niger.overall.extreme.stunting[year_id == 1990]$extreme.prop, probs = .025) * 100)
niger.extreme.stunting.prop.1990.upper <- unname(quantile(niger.overall.extreme.stunting[year_id == 1990]$extreme.prop, probs = .975) * 100)

niger.extreme.stunting.prop.2020.mean <- mean(niger.overall.extreme.stunting[year_id == 2020]$extreme.prop) * 100
niger.extreme.stunting.prop.2020.lower <- unname(quantile(niger.overall.extreme.stunting[year_id == 2020]$extreme.prop, probs = .025) * 100)
niger.extreme.stunting.prop.2020.upper <- unname(quantile(niger.overall.extreme.stunting[year_id == 2020]$extreme.prop, probs = .975) * 100)


print(paste0("In 1990 in Niger, ", round(niger.extreme.stunting.prop.1990.mean, 1), "% (", round(niger.extreme.stunting.prop.1990.lower, 1), "% - ", 
             round(niger.extreme.stunting.prop.1990.upper, 1), "%) of stunted children experienced extreme stunting."))

print(paste0("In 2020 in Niger, ", round(niger.extreme.stunting.prop.2020.mean, 1), "% (", round(niger.extreme.stunting.prop.2020.lower, 2), "% - ", 
             round(niger.extreme.stunting.prop.2020.upper, 1), "%) of stunted children experienced extreme stunting."))


###################################################################################################################
# Creating the Stunting Curves for Niger
###################################################################################################################
source("filepath.R")
distlist <- c(classA, classM) 

offset = 10
bottom_cutoff = -7
top_cutoff = 7
bottom_cutoff = bottom_cutoff + offset
top_cutoff = top_cutoff + offset

cgf.type <- "HAZ"



create_pdf <- function(distlist, weights, CDF_Data_Params) {
  for(z in 1:length(weights)){
    LENGTH <- length(formals(unlist(distlist[[z]]$mv2par)))
    if (LENGTH==4) {
      est <- try((unlist(distlist[[z]]$mv2par(CDF_Data_Params$mn,CDF_Data_Params$variance,XMIN=CDF_Data_Params$XMIN,XMAX=CDF_Data_Params$XMAX))),silent=T)
    } else {
      est <- try((unlist(distlist[[z]]$mv2par(CDF_Data_Params$mn,CDF_Data_Params$variance))),silent=T)
    }
    fxj <- try(distlist[[z]]$dF(CDF_Data_Params$x,est),silent=T)
    if(class(est)=="try-error") {
      fxj <- rep(0,length(CDF_Data_Params$fx))
    }
    fxj[!is.finite(fxj)] <- 0
    CDF_Data_Params$fx <- (CDF_Data_Params$fx + fxj*weights[[z]])
  }
  CDF_Data_Params$fx[!is.finite(CDF_Data_Params$fx)] <- 0
  CDF_Data_Params$fx[length(CDF_Data_Params$fx)] <- 0
  CDF_Data_Params$fx[1] <- 0
  den <- approxfun(CDF_Data_Params$x, CDF_Data_Params$fx, yleft=0, yright=0) #density function
  
  return(den)
}

integrate_pdf <- function(den, CDF_Data_Params) {
  CDF_Data_Params$cdfFITYout <- c()
  for(val in CDF_Data_Params$x) {
    v<-try(integrate(den, min(CDF_Data_Params$x), val)$value)
    if (class(v)=="try-error") {
      v <- NA
    }
    CDF_Data_Params$cdfFITYout <- append(CDF_Data_Params$cdfFITYout,v)
  }
  
  
  return(CDF_Data_Params)
} 

create_pdf_points <- function(den, CDF_Data_Params) {
  CDF_Data_Params$cdfFITYout <- c()
  for(val in CDF_Data_Params$x) {
    v<-den(val)
    CDF_Data_Params$cdfFITYout <- append(CDF_Data_Params$cdfFITYout,v)
  }
  
  
  return(CDF_Data_Params)
} 

#enter weights as a vector
calculate_curve <- function(nid_loc_yr_i, mean_val, sd_val, bottom_cutoff, top_cutoff, weights, offset){
  
  CDF_Data_Params <- list()
  CDF_Data_Params$mn <- mean_val
  CDF_Data_Params$variance <- sd_val ^2
  CDF_Data_Params$XMIN <- bottom_cutoff
  CDF_Data_Params$XMAX <- top_cutoff
  CDF_Data_Params$x <- seq(CDF_Data_Params$XMIN, CDF_Data_Params$XMAX, length = 5000)
  CDF_Data_Params$fx <- 0*CDF_Data_Params$x
  CDF_Data_Params$cdfDATAYout <- sort(CDF_Data_Params$x)
  CDF_Data_Params$nid <- nid_loc_yr_i
  
  den <- create_pdf(distlist = distlist, weights = weights, CDF_Data_Params = CDF_Data_Params)
  
  CDF_Data_Params <- integrate_pdf(den, CDF_Data_Params = CDF_Data_Params)
  
  storing <- copy(CDF_Data_Params)
  
  new <- create_pdf_points(den, CDF_Data_Params = CDF_Data_Params)
  
  output <- data.table(xpoints = storing$x, pdf.points = new$cdfFITYout, cdf.points = storing$cdfFITYout, nid_loc_yr_i = nid_loc_yr_i)
  
  
  
  
  
  return(output)
  
}





country.files <- expand.grid(location_id = 213,
                             age_group_id = c(2, 3, 388, 389, 238, 34),
                             sex_id = c(1,2),
                             year_id = c(1990, 1995, 2000, 2005,2006, 2010, 2015, 2020),
                             location_name = "Niger")

country.files <- data.table(country.files)
country.files[, files:= paste0("meanSD_", location_id, "_", age_group_id, "_", sex_id, "_", year_id, ".rds")]
country.files[, index:= 1:.N]





ensemble.dist.output <- lapply(country.files$index, function(loop.index){
  
  if(loop.index == 1){print("Calculating Curves...")}
  
  #reading in the mean and sd for the curve
  file = country.files[index == loop.index]$files
  meansdfile <- readRDS(paste0("filepath", cgf.type, "filepath", file))
  
  # For labeling purposes of the output
  loc = country.files[index == loop.index]$location_id
  age = country.files[index == loop.index]$age_group_id
  year = country.files[index == loop.index]$year_id
  sex = country.files[index == loop.index]$sex_id
  loc_name = country.files[index == loop.index]$location_name
  ind = country.files[index == loop.index]$index
  
  #if HAZ, WAZ, WHZ, make sure to add in that this mean needs to be multiplied by 10 because of mean Z score transformation in modeling
  mean_val <- mean((meansdfile$mean)) *10
  sd_val <- mean(meansdfile$sd)
  
  
  
  #reading in the weights
  weights <- fread(paste0("filepath", cgf.type, "filepath"), stringsAsFactors = F)
  #names(distlist) to make sure these weights are in the right order
  weights <- data.table(weights = c(weights$exp, weights$gamma, weights$invgamma, weights$llogis, weights$gumbel, weights$weibull, weights$lnorm, weights$norm, weights$mgamma, weights$mgumbel))
  weights <- weights$weights
  
  ensemble.dist.output <- calculate_curve(nid_loc_yr_i = ind, mean_val, sd_val, bottom_cutoff, top_cutoff, weights, offset)
  
  #labeling the peak of the curve
  max.pdf <- max(ensemble.dist.output$pdf.points)
  ensemble.dist.output[pdf.points == max.pdf, max.flag := 1]
  ensemble.dist.output[is.na(max.flag), max.flag := 0]
  
  
  ensemble.dist.output[, location_id := loc]
  ensemble.dist.output[, age_group_id := age]
  ensemble.dist.output[, year_id := year]
  ensemble.dist.output[, sex_id := sex]
  ensemble.dist.output[, location_name := loc_name]
  ensemble.dist.output[, sd.val := sd_val]
  ensemble.dist.output[, mean.val := mean_val - 10] # minus ten here because of the transformation done in mean CGF modeling 
  ensemble.dist.output[, index := ind]
  
  if(loop.index == nrow(country.files)){print("Done Calculating Curves.")}
  
  
  return(ensemble.dist.output)
  
})



ensemble.dist.output <- rbindlist(ensemble.dist.output)









ensemble.dist.output <- merge(ensemble.dist.output, populations, by = c("location_id", "year_id", "sex_id", "age_group_id"))


# population age and sex aggregating the curves up to all kids under 5

ensemble.dist.output[, weighted.pdf := pdf.points * population]
ensemble.dist.output[, sum.weighted.pdf := sum(weighted.pdf), by = c("location_id", "year_id", "xpoints")]
ensemble.dist.output[, under.5.pop := sum(population), by = c("location_id", "year_id", "xpoints")]

ensemble.dist.output[, aggregated.pdf := sum.weighted.pdf/under.5.pop]
ensemble.dist.output[, loc.year.x.cgf := paste0(location_id, "_", year_id, "_", xpoints)]

ensemble.dist.output <- ensemble.dist.output[!duplicated(ensemble.dist.output$loc.year.x)]





########################################################################################################################
#creating Figure 1 with Niger Curves next to Overall Stunting Timeseries
########################################################################################################################


left <- c(-7, -4, -3)
right <- c(-4, -3, -2)
top <- c(.3, .3, .3)
bottom <- c(0, 0, 0)
color <- c('red4', 'orangered4', 'darkorange2')
rectangles <- data.table(x1 = left, x2 = right, y1 = bottom, y2 = top, col = color)


niger.pdf.plot <- ggplot() +
  geom_rect(data = rectangles, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = col)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ensemble.dist.output[year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020)], aes(x = xpoints-offset, y = aggregated.pdf, group = as.character(index), color = year_id), size = 1.5) +
  theme_bw() +
  labs(x = "HAZ", y = "Probability Density", title = "HAZ Distributions in Niger 1990-2020", color = "Year", tag = "D") +
  scale_x_continuous(limits=c(-7, 7), breaks = c(-7:7), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), labels = percent_format(accuracy = 1), limits = c(0, .3), breaks = c(0, .1, .2, .3)) +
  guides(fill = F)+
  scale_fill_manual(values = alpha(c("#e69c12", "#ed6d28", '#c22c1b'), .3)) +
  theme(title = element_text(size = 20)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size=22), axis.title = element_text(size = 24)) + 
  theme(legend.position = c(.92, .68),
        legend.key.size = unit(1.8, 'cm'),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 22),
        legend.title.align = .5,
        legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30")) +
  geom_vline(xintercept = -2, color = "grey", size = 1.3, linetype = "dashed") +
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        plot.tag = element_text(face = "bold")) 






niger.2006.males <- fread("filepath")
niger.2006.females <- fread("filepath")
niger.2006.survey <- rbind(niger.2006.males, niger.2006.females)







niger.pdf.plot.2006.survey <- ggplot() +
  geom_rect(data = rectangles, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = col)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_histogram(data = niger.2006.survey, aes(x = HAZ, y = ..density..), fill = "lightskyblue", binwidth = .2, alpha = .8) +
  geom_line(data = ensemble.dist.output[year_id %in% c(2006)], aes(x = xpoints-offset, y = aggregated.pdf, group = as.character(index)), size = 1) +
  theme_bw() +
  labs(x = "HAZ", y = "Probability Density", title = "HAZ Distribution in Niger 2006", tag = "C") +
  scale_x_continuous(limits=c(-7, 7), breaks = c(-7:7), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), labels = percent_format(accuracy = 1), limits = c(0, .3), breaks = c(0, .1, .2, .3)) +
  guides(fill = F)+
  scale_fill_manual(values = alpha(c("#e69c12", "#ed6d28", '#c22c1b'), .3)) +
  theme(title = element_text(size = 20)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size=22), axis.title = element_text(size = 24)) + 
  theme(legend.position = c(.92, .764),
        legend.key.size = unit(1.8, 'cm'),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 22),
        legend.title.align = .5,
        legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30")) +
  geom_vline(xintercept = -2, color = "grey", size = 1.3, linetype = "dashed") +
  theme(plot.margin = unit(c(0,1,1,1), "cm"),
        plot.tag = element_text(face = "bold")) 












# for comparison, making a plot where you can't see the details, just the overall line

overall.niger.stunting.timeseries <- get_draws("modelable_entity_id", 10556, year_id=c(1990:2020), 
                                               source="epi", gbd_round_id=7, decomp_step="iterative", 
                                               age_group_id = 1, sex_id = c(3), location_id = 213)

overall.niger.stunting.timeseries <- melt(overall.niger.stunting.timeseries, id.vars = id.vars)
overall.niger.stunting.timeseries <- merge(overall.niger.stunting.timeseries, populations)

overall.niger.stunting.timeseries[, mean.val := mean(value), by = year_id]
overall.niger.stunting.timeseries[, lower := quantile(value, probs = .025), by = year_id]
overall.niger.stunting.timeseries[, upper := quantile(value, probs = .975), by = year_id]

severe.niger.stunting.timeseries <- get_draws("modelable_entity_id", 8949, year_id=c(1990:2020), 
                                              source="epi", gbd_round_id=7, decomp_step="iterative", 
                                              age_group_id = 1, sex_id = c(3), location_id = 213)

severe.niger.stunting.timeseries <- melt(severe.niger.stunting.timeseries, id.vars = id.vars)
severe.niger.stunting.timeseries <- merge(severe.niger.stunting.timeseries, populations)

severe.niger.stunting.timeseries[, mean.val := mean(value), by = year_id]
severe.niger.stunting.timeseries[, lower := quantile(value, probs = .025), by = year_id]
severe.niger.stunting.timeseries[, upper := quantile(value, probs = .975), by = year_id]


extreme.niger.stunting.timeseries <- get_draws("modelable_entity_id", 26941, year_id=c(1990:2020), 
                                               source="epi", gbd_round_id=7, decomp_step="iterative", 
                                               age_group_id = 1, sex_id = c(3), location_id = 213)

extreme.niger.stunting.timeseries <- melt(extreme.niger.stunting.timeseries, id.vars = id.vars)
extreme.niger.stunting.timeseries <- merge(extreme.niger.stunting.timeseries, populations)

extreme.niger.stunting.timeseries[, mean.val := mean(value), by = year_id]
extreme.niger.stunting.timeseries[, lower := quantile(value, probs = .025), by = year_id]
extreme.niger.stunting.timeseries[, upper := quantile(value, probs = .975), by = year_id]




niger.overall.timeseries <- ggplot() +
  geom_ribbon(data = overall.niger.stunting.timeseries, aes(x = year_id, ymin = lower, ymax = upper), fill = "#e69c12", alpha = .3) +
  geom_line(data = overall.niger.stunting.timeseries, aes(x = year_id, y = mean.val), color = "#e69c12", size = 1) +
  geom_ribbon(data = severe.niger.stunting.timeseries, aes(x = year_id, ymin = lower, ymax = upper), fill = "#ed6d28", alpha = .3) +
  geom_line(data = severe.niger.stunting.timeseries, aes(x = year_id, y = mean.val), color = "#ed6d28", size = 1) +
  geom_ribbon(data = extreme.niger.stunting.timeseries, aes(x = year_id, ymin = lower, ymax = upper), fill = "#c22c1b", alpha = .3) +
  geom_line(data = extreme.niger.stunting.timeseries, aes(x = year_id, y = mean.val), color = "#c22c1b", size = 1) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, .6), labels = percent_format(accuracy = 1)) +
  theme_bw() +
  labs(x = "Year", y = "Stunting Prevalence", title = "Prevalence 1990-2020", tag = "A") +
  theme(title = element_text(size = 18)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size=18), axis.title = element_text(size = 22), axis.text.x = element_text(vjust = -.5)) +
  theme(plot.margin = unit(c(1,1,0,1), "cm"),
        plot.tag = element_text(face = "bold")) 




######## percent change since 1990 figure

overall.niger.1990 <- overall.niger.stunting.timeseries[year_id == 1990]

setnames(overall.niger.1990, c("value"), c("value.1990"))
overall.niger.1990.prev <- overall.niger.1990$value.1990

overall.niger.stunting.timeseries$value.1990 <- rep(overall.niger.1990.prev, times = 31)

overall.niger.stunting.timeseries[, pc := ((value-value.1990)/value.1990)]
overall.niger.stunting.timeseries[, median.pc := median(pc), by = c("year_id")]
overall.niger.stunting.timeseries[, lower.pc := quantile(pc, probs = .025), by = c("year_id")]
overall.niger.stunting.timeseries[, upper.pc := quantile(pc, probs = .975), by = c("year_id")]



severe.niger.1990 <- severe.niger.stunting.timeseries[year_id == 1990]

setnames(severe.niger.1990, c("value"), c("value.1990"))
severe.niger.1990.prev <- severe.niger.1990$value.1990

severe.niger.stunting.timeseries$value.1990 <- rep(severe.niger.1990.prev, times = 31)

severe.niger.stunting.timeseries[, pc := ((value-value.1990)/value.1990)]
severe.niger.stunting.timeseries[, median.pc := median(pc), by = c("year_id")]
severe.niger.stunting.timeseries[, lower.pc := quantile(pc, probs = .025), by = c("year_id")]
severe.niger.stunting.timeseries[, upper.pc := quantile(pc, probs = .975), by = c("year_id")]


extreme.niger.1990 <- extreme.niger.stunting.timeseries[year_id == 1990]

setnames(extreme.niger.1990, c("value"), c("value.1990"))
extreme.niger.1990.prev <- extreme.niger.1990$value.1990

extreme.niger.stunting.timeseries$value.1990 <- rep(extreme.niger.1990.prev, times = 31)

extreme.niger.stunting.timeseries[, pc := ((value-value.1990)/value.1990)]
extreme.niger.stunting.timeseries[, median.pc := median(pc), by = c("year_id")]
extreme.niger.stunting.timeseries[, lower.pc := quantile(pc, probs = .025), by = c("year_id")]
extreme.niger.stunting.timeseries[, upper.pc := quantile(pc, probs = .975), by = c("year_id")]





niger.overall.timeseries.percent.change <- ggplot() +
  geom_hline(yintercept = 0, color = "grey40", size = .5) +
  geom_ribbon(data = overall.niger.stunting.timeseries, aes(x = year_id, ymin = lower.pc, ymax = upper.pc), fill = "#e69c12", alpha = .3) +
  geom_line(data = overall.niger.stunting.timeseries, aes(x = year_id, y = median.pc), color = "#e69c12", size = 1) +
  geom_ribbon(data = severe.niger.stunting.timeseries, aes(x = year_id, ymin = lower.pc, ymax = upper.pc), fill = "#ed6d28", alpha = .3) +
  geom_line(data = severe.niger.stunting.timeseries, aes(x = year_id, y = median.pc), color = "#ed6d28", size = 1) +
  geom_ribbon(data = extreme.niger.stunting.timeseries, aes(x = year_id, ymin = lower.pc, ymax = upper.pc), fill = "#c22c1b", alpha = .3) +
  geom_line(data = extreme.niger.stunting.timeseries, aes(x = year_id, y = median.pc), color = "#c22c1b", size = 1) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(-1, 0.4), labels = percent_format(accuracy = 1), breaks = c(-1, -.75, -.5, -.25, 0, .25)) +
  theme_bw() +
  labs(x = "Year", y = "Percent Change since 1990", title = "Relative Change 1990-2020", tag = "B") +
  theme(title = element_text(size = 18)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size=18), axis.title = element_text(size = 22), axis.text.x = element_text(vjust = -.5)) +
  theme(plot.margin = unit(c(1,1,0,1), "cm"),
        plot.tag = element_text(face = "bold")) 
















# creating a plot I can use for the legend
temp.df <- data.table(cgf.type = c("Overall Stunting (HAZ <-2)", "Severe Stunting (HAZ <-3)", "Extreme Stunting (HAZ <-4)"),
                      val = c(1, 2, 3))

temp.df$cgf.type <- factor(temp.df$cgf.type, levels = c("Extreme Stunting (HAZ <-4)", "Severe Stunting (HAZ <-3)", "Overall Stunting (HAZ <-2)"))


legend.plot <- ggplot() +
  geom_histogram(data = temp.df, aes(x = val, fill = cgf.type), alpha = .5) +
  scale_fill_manual(values = c("#c22c1b", "#ed6d28","#e69c12"), 
                    guide=guide_legend(title="Severity", title.position = "top", title.hjust = .5, ncol = 3)) +
  theme(legend.title = element_text(size = 24),
        legend.text = element_text(size = 20),
        legend.position = "bottom",
        legend.justification = 'center',
        legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
        legend.key = element_rect(fill = "grey90"),
        legend.key.height = unit(1, "cm"))


severity.legend <- get_legend(legend.plot)





layout <- rbind(c(1,2, 3),
                c(4, 4, 3),
                c(4, 4, 5),
                c(6, 6, 6))








empty.caption <- ggplot() +
  theme_void() +
  labs(caption = expression(paste(bold(" Figure 1: \n \n \n"), " Trends in each severity of stunting in Niger from 1990-2020 are shown in Figure 1a. The yellow line for overall stunting reflects the conventional stunting monitoring metric.\n Figure 1b shows relative changes in each severity since 1990. Figure 1c represents a step in our methodology by which we incorporate surveys with individual measurements\n of HAZ to paramaterize characteristic curves of HAZ. Here, individual anthropometric measurements from a 2006 DHS survey in Niger are overlaid with the curve of final HAZ\n estimates for Niger in 2006. This process is completed for all years, yielding a sequence of changing HAZ curves over time, shown in Figure 1d."))) +
  theme(plot.caption = element_text(size = 22, hjust = 0), 
        plot.caption.position = "plot") 




figure1 <- plot_grid(arrangeGrob(niger.overall.timeseries, niger.overall.timeseries.percent.change, niger.pdf.plot, niger.pdf.plot.2006.survey, severity.legend, empty.caption,
                                 nrow=4, ncol = 3, widths = c(2, 2,5), heights = c(1.2, 1, .2, .4), layout_matrix = layout))





pdf("filepath/figure_1.pdf", height = 12, width = 26)




print(figure1)

dev.off()



























