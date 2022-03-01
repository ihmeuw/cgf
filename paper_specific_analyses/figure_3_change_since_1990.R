# CGF paper analysis

# load functions and libraries

invisible(sapply(list.files("filepath", full.names = T), source))

library(data.table)
library(ggplot2)
library(dplyr)
library(scales)
library('cowplot', lib.loc = "filepath")
library(gridExtra)
library("ggridges", lib.loc = "filepath")
library("ggExtra", lib.loc = "filepath")


"%ni%" <- Negate("%in%")
# loading populations

populations <- get_population(gbd_round_id = 7, decomp_step = 'iterative', location_id = 'all', location_set_id = 35, 
                              year_id = c(1990:2020), age_group_id = c(1, 2, 3, 388, 389, 238, 34, 22), sex_id = c(1, 2, 3))
populations$run_id <- NULL


# setting id vars to use in melts for this section

id.vars <- c("metric_id", "age_group_id", "location_id", "measure_id", "modelable_entity_id", "sex_id", "year_id", "model_version_id")



loc.met <- get_location_metadata(release_id = 9, location_set_id = 35)

figures.loc.met <- get_location_metadata(release_id = 9, location_set_id = 91)
figure.locs <- figures.loc.met[level < 6]$location_id # no England UTLAs in tables






##############################################################################################################
# Stunting
##############################################################################################################

# prepping overall (moderate) stunting
#stunting.rate.compare.overall <- get_draws("modelable_entity_id", 10556, year_id=c(1990:2020), 
#                                           source="epi", gbd_round_id=7, decomp_step="iterative", 
#                                           age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = figure.locs)

stunting.rate.compare.overall <- fread("filepath")


stunting.rate.compare.overall <- melt(stunting.rate.compare.overall, id.vars = id.vars)

#set any 0 values to extremely small prevalences to avoid infinite numbers in calculations
stunting.rate.compare.overall[value == 0, value:= 0.000000001]

stunting.rate.compare.overall <- merge(stunting.rate.compare.overall, populations, by = c("age_group_id", "location_id", "year_id", "sex_id"))
stunting.rate.compare.overall[, weighted.val := value*population]

stunting.rate.compare.overall[, agg.weighted.val := sum(weighted.val), by = c("location_id", "year_id", "variable")]
stunting.rate.compare.overall[, under.5.pop := sum(population), by = c("location_id", "year_id", "variable")]
stunting.rate.compare.overall[, aggregated.value := agg.weighted.val/under.5.pop]



overall.stunting.1990 <- stunting.rate.compare.overall[year_id == 1990]
overall.stunting.1990$year_id <- NULL
setnames(overall.stunting.1990, "aggregated.value", "aggregated.value.1990")
overall.stunting.1990 <- overall.stunting.1990[, -c("population", "weighted.val", "agg.weighted.val", "under.5.pop", "value")]

stunting.rate.compare.overall <- merge(stunting.rate.compare.overall, overall.stunting.1990, by = c("age_group_id", "location_id", "sex_id", "metric_id", "measure_id", "modelable_entity_id",  "model_version_id", "variable"))

stunting.rate.compare.overall[, overall.pc.1990 := ((aggregated.value - aggregated.value.1990)/aggregated.value.1990)*100]




#picking one age, one sex. Doesn't matter, we've just age-sex aggregated going to change labels after this
stunting.rate.compare.u5.overall <- stunting.rate.compare.overall[age_group_id == 34 & sex_id == 1]
stunting.rate.compare.u5.overall[, age_group_id := 1]
stunting.rate.compare.u5.overall[, sex_id := 3]
stunting.rate.compare.u5.overall <- stunting.rate.compare.u5.overall[, -c("value", "weighted.val", "agg.weighted.val", "population")]

overall.stunting.meanpcs <- copy(stunting.rate.compare.u5.overall)



overall.stunting.meanpcs[, median.overall.pc := median(overall.pc.1990), by = c("location_id", "year_id")]
overall.stunting.meanpcs[, lower.overall.pc := quantile(overall.pc.1990, probs = .025, na.rm = T), by = c("location_id", "year_id")]
overall.stunting.meanpcs[, upper.overall.pc := quantile(overall.pc.1990, probs = .975, na.rm = T), by = c("location_id", "year_id")]
overall.stunting.hold <- copy(overall.stunting.meanpcs)
overall.stunting.meanpcs[, loc.year := paste0(location_id, "_", year_id)]
overall.stunting.meanpcs <- overall.stunting.meanpcs[!duplicated(overall.stunting.meanpcs$loc.year)]
overall.stunting.meanpcs$loc.year <- NULL





# prepping severe stunting
#stunting.rate.compare.severe <- get_draws("modelable_entity_id", 8949, year_id=c(1990:2020), 
#                                          source="epi", gbd_round_id=7, decomp_step="iterative", 
#                                          age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1,2), location_id = figure.locs)

stunting.rate.compare.severe <- fread("filepath")

stunting.rate.compare.severe <- melt(stunting.rate.compare.severe, id.vars = id.vars)

#set any 0 values to extremely small prevalences to avoid infinite numbers in calculations
stunting.rate.compare.severe[value == 0, value:= 0.000000001]

stunting.rate.compare.severe <- merge(stunting.rate.compare.severe, populations, by = c("age_group_id", "location_id", "year_id", "sex_id"))
stunting.rate.compare.severe[, weighted.val := value*population]

stunting.rate.compare.severe[, agg.weighted.val := sum(weighted.val), by = c("location_id", "year_id", "variable")]
stunting.rate.compare.severe[, under.5.pop := sum(population), by = c("location_id", "year_id", "variable")]
stunting.rate.compare.severe[, aggregated.value := agg.weighted.val/under.5.pop]



severe.stunting.1990 <- stunting.rate.compare.severe[year_id == 1990]
severe.stunting.1990$year_id <- NULL
setnames(severe.stunting.1990, "aggregated.value", "aggregated.value.1990")
severe.stunting.1990 <- severe.stunting.1990[, -c("population", "weighted.val", "agg.weighted.val", "under.5.pop", "value")]

stunting.rate.compare.severe <- merge(stunting.rate.compare.severe, severe.stunting.1990, by = c("age_group_id", "location_id", "sex_id", "metric_id", "measure_id", "modelable_entity_id",  "model_version_id", "variable"))

stunting.rate.compare.severe[, severe.pc.1990 := ((aggregated.value - aggregated.value.1990)/aggregated.value.1990)*100]




#picking one age, one sex. Doesn't matter, we've just age-sex aggregated going to change labels after this
stunting.rate.compare.u5.severe <- stunting.rate.compare.severe[age_group_id == 34 & sex_id == 1]
stunting.rate.compare.u5.severe[, age_group_id := 1]
stunting.rate.compare.u5.severe[, sex_id := 3]
stunting.rate.compare.u5.severe <- stunting.rate.compare.u5.severe[, -c("value", "weighted.val", "agg.weighted.val", "population")]

severe.stunting.meanpcs <- copy(stunting.rate.compare.u5.severe)



severe.stunting.meanpcs[, median.severe.pc := median(severe.pc.1990), by = c("location_id", "year_id")]
severe.stunting.meanpcs[, lower.severe.pc := quantile(severe.pc.1990, probs = .025, na.rm = T), by = c("location_id", "year_id")]
severe.stunting.meanpcs[, upper.severe.pc := quantile(severe.pc.1990, probs = .975, na.rm = T), by = c("location_id", "year_id")]
severe.stunting.hold <- copy(severe.stunting.meanpcs)
severe.stunting.meanpcs[, loc.year := paste0(location_id, "_", year_id)]
severe.stunting.meanpcs <- severe.stunting.meanpcs[!duplicated(severe.stunting.meanpcs$loc.year)]
severe.stunting.meanpcs$loc.year <- NULL






# prepping extreme stunting
#stunting.rate.compare.extreme <- get_draws("modelable_entity_id", 26941, year_id=c(1990:2020), 
#                                           source="epi", gbd_round_id=7, decomp_step="iterative", 
#                                           age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = figure.locs)

stunting.rate.compare.extreme <- fread("filepath")

stunting.rate.compare.extreme <- melt(stunting.rate.compare.extreme, id.vars = id.vars)

#set any 0 values to extremely small prevalences to avoid infinite numbers in calculations
stunting.rate.compare.extreme[value == 0, value:= 0.000000001]

stunting.rate.compare.extreme <- merge(stunting.rate.compare.extreme, populations, by = c("age_group_id", "location_id", "year_id", "sex_id"))
stunting.rate.compare.extreme[, weighted.val := value*population]

stunting.rate.compare.extreme[, agg.weighted.val := sum(weighted.val), by = c("location_id", "year_id", "variable")]
stunting.rate.compare.extreme[, under.5.pop := sum(population), by = c("location_id", "year_id", "variable")]
stunting.rate.compare.extreme[, aggregated.value := agg.weighted.val/under.5.pop]



extreme.stunting.1990 <- stunting.rate.compare.extreme[year_id == 1990]
extreme.stunting.1990$year_id <- NULL
setnames(extreme.stunting.1990, "aggregated.value", "aggregated.value.1990")
extreme.stunting.1990 <- extreme.stunting.1990[, -c("population", "weighted.val", "agg.weighted.val", "under.5.pop", "value")]

stunting.rate.compare.extreme <- merge(stunting.rate.compare.extreme, extreme.stunting.1990, by = c("age_group_id", "location_id", "sex_id", "metric_id", "measure_id", "modelable_entity_id",  "model_version_id", "variable"))

stunting.rate.compare.extreme[, extreme.pc.1990 := ((aggregated.value - aggregated.value.1990)/aggregated.value.1990)*100]




#picking one age, one sex. Doesn't matter, we've just age-sex aggregated going to change labels after this
stunting.rate.compare.u5.extreme <- stunting.rate.compare.extreme[age_group_id == 34 & sex_id == 1]
stunting.rate.compare.u5.extreme[, age_group_id := 1]
stunting.rate.compare.u5.extreme[, sex_id := 3]
stunting.rate.compare.u5.extreme <- stunting.rate.compare.u5.extreme[, -c("value", "weighted.val", "agg.weighted.val", "population")]

extreme.stunting.meanpcs <- copy(stunting.rate.compare.u5.extreme)



extreme.stunting.meanpcs[, median.extreme.pc := median(extreme.pc.1990), by = c("location_id", "year_id")]
extreme.stunting.meanpcs[, lower.extreme.pc := quantile(extreme.pc.1990, probs = .025, na.rm = T), by = c("location_id", "year_id")]
extreme.stunting.meanpcs[, upper.extreme.pc := quantile(extreme.pc.1990, probs = .975, na.rm = T), by = c("location_id", "year_id")]
extreme.stunting.hold <- copy(extreme.stunting.meanpcs)
extreme.stunting.meanpcs[, loc.year := paste0(location_id, "_", year_id)]
extreme.stunting.meanpcs <- extreme.stunting.meanpcs[!duplicated(extreme.stunting.meanpcs$loc.year)]
extreme.stunting.meanpcs$loc.year <- NULL




######################################################################################




#merging stunting together
overall.stunting.meanpcs$model_version_id <- NULL
overall.stunting.meanpcs$modelable_entity_id <- NULL
severe.stunting.meanpcs$model_version_id <- NULL
severe.stunting.meanpcs$modelable_entity_id <- NULL
extreme.stunting.meanpcs$model_version_id <- NULL
extreme.stunting.meanpcs$modelable_entity_id <- NULL

setnames(severe.stunting.meanpcs, c("overall.pc.1990", "median.overall.pc", "lower.overall.pc", "upper.overall.pc"), 
                                  c("severe.pc.1990", "median.severe.pc", "lower.severe.pc", "upper.severe.pc"))

setnames(extreme.stunting.meanpcs, c("overall.pc.1990", "median.overall.pc", "lower.overall.pc", "upper.overall.pc"), 
         c("extreme.pc.1990", "median.extreme.pc", "lower.extreme.pc", "upper.extreme.pc"))


overall.stunting.meanpcs <- overall.stunting.meanpcs[, -c("metric_id", "measure_id", "variable", "under.5.pop", "overall.pc.1990")] #here's where draw.rank might be needed
severe.stunting.meanpcs <- severe.stunting.meanpcs[, -c("metric_id", "measure_id", "variable", "under.5.pop", "severe.pc.1990")] #here's where draw.rank might be needed
extreme.stunting.meanpcs <- extreme.stunting.meanpcs[, -c("metric_id", "measure_id", "variable", "under.5.pop", "extreme.pc.1990")] #here's where draw.rank might be needed

setnames(overall.stunting.meanpcs, c("aggregated.value", "aggregated.value.1990"), c("aggregated.overall.value", "aggregated.overall.value.1990"))
setnames(severe.stunting.meanpcs, c("aggregated.value", "aggregated.value.1990"), c("aggregated.severe.value", "aggregated.severe.value.1990"))
setnames(extreme.stunting.meanpcs, c("aggregated.value", "aggregated.value.1990"), c("aggregated.extreme.value", "aggregated.extreme.value.1990"))


all.stunting.pcs <- merge(overall.stunting.meanpcs, severe.stunting.meanpcs, by = c("age_group_id", "location_id", "year_id", "sex_id"))
all.stunting.pcs <- merge(all.stunting.pcs, extreme.stunting.meanpcs, by = c("age_group_id", "location_id", "year_id", "sex_id"))







# exclude locations that are regions/superregions
aggregate.locs <- loc.met[level < 3]$location_id


#only include countries that decreased from 1990 - 2020 for all three severities
decreasing.stunting.locs <- unique(all.stunting.pcs[year_id == 2020 & median.overall.pc < 0 & median.severe.pc < 0 & median.extreme.pc < 0]$location_id)



region.locs <- loc.met[level == 2]$location_id
sr.locs <- loc.met[level == 1]$location_id
uk.ids <- c( 4749,  433 , 434 ,4636)
density.plot.locs <- loc.met[level == 3 | location_id %in% uk.ids]$location_id



breaks <- c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100)
labels = paste0(breaks, "%")
labels = replace(labels, labels == "100%", ">100%")


stunting.time.trend.melted <- melt(all.stunting.pcs, id.vars = c("age_group_id", "location_id", "year_id", "sex_id"))
stunting.time.trend.melted[, loc.var := paste0(location_id, "_", variable)]
stunting.time.trend.melted$variable <- as.character(stunting.time.trend.melted$variable)

over.100.changes.stunting <- unique(stunting.time.trend.melted[variable %like% "median" & year_id == 2020 &
                                                        (value > 100 | value < -100)]$location_id)

extreme.stunt.exclude.locs <- unique(stunting.time.trend.melted[variable == "aggregated.extreme.value" & value < 1e-06 & location_id %in% density.plot.locs]$location_id)

population.exclude.locs <- populations[age_group_id == 22 & sex_id == 3 & year_id == 2020 & population < 300000 & location_id %in% density.plot.locs]$location_id





stunting.time.trend.melted[, cgf.type := "Stunting (HAZ)"]
  
stunting.time.trend.melted.plot <- ggplot() +
  geom_hline(yintercept = 0, color = "grey50", size = .9) +
  geom_line(data =  stunting.time.trend.melted[location_id %in% sr.locs &
                                                 variable %like% "median"], aes(x= year_id, y = value,  group = loc.var, color = variable), alpha = .8, size = .4) +
  geom_line(data = stunting.time.trend.melted[location_id == 1 & variable %like% "median"], aes(x= year_id, y = value,  group = loc.var, color = variable), size = 2.2) +
  scale_color_manual(values = c("#a11d0e", "#E69C12", "#e0631f")) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Year", y = "Percent Change since 1990", tag = "A") +
  scale_y_continuous(breaks = breaks, labels = labels, limits = c(-100, 100), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(from = 1990, to = 2020, by = 10), expand = c(0,0)) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(vjust = -.5),
        panel.grid.minor = element_line(size = 0),
        strip.text = element_text(size = 24),
        plot.tag = element_text(face = "bold", size = 26, vjust = -.5)) +
  facet_grid(~cgf.type)


stunting.density.df <- stunting.time.trend.melted[location_id %ni% aggregate.locs &
                                           location_id != 95 &
                                           location_id %in% density.plot.locs &
                                           variable %like% "median" &
                                           location_id %ni% extreme.stunt.exclude.locs &
                                           location_id %ni% population.exclude.locs &
                                           year_id == 2020]

stunting.density.df$variable <- factor(stunting.density.df$variable, levels = c("median.overall.pc", "median.severe.pc", "median.extreme.pc"))

stunting.marginal <- ggplot() +
  geom_density(data = stunting.density.df,
                      aes(x = value, fill = variable),  alpha = .7, bw = 6) +
  scale_fill_manual(values = c("#E69C12","#e0631f", "#a11d0e")) +
  theme_void() +
  theme(legend.position = "none") +
  coord_flip() +
  theme(plot.margin=unit(c(6.62, 0, 1.15, 0),"cm"))




  
  

stunting.combined.plot <- plot_grid(stunting.time.trend.melted.plot, stunting.marginal, ncol = 2, nrow = 1, rel_widths = c(3, .75))



###############################
#calculating how much more severe and extreme stunting improved compared to overall stunting
###############################





setnames(severe.stunting.hold, c("overall.pc.1990", "median.overall.pc", "lower.overall.pc", "upper.overall.pc"), 
         c("severe.pc.1990", "median.severe.pc", "lower.severe.pc", "upper.severe.pc"))

setnames(extreme.stunting.hold, c("overall.pc.1990", "median.overall.pc", "lower.overall.pc", "upper.overall.pc"), 
         c("extreme.pc.1990", "median.extreme.pc", "lower.extreme.pc", "upper.extreme.pc"))






overall.stunting.hold <- overall.stunting.hold[, -c("metric_id", "measure_id",  "under.5.pop", "modelable_entity_id", "model_version_id")] #here's where draw.rank might be needed
severe.stunting.hold <- severe.stunting.hold[, -c("metric_id", "measure_id",  "under.5.pop", "modelable_entity_id", "model_version_id")] #here's where draw.rank might be needed
extreme.stunting.hold <- extreme.stunting.hold[, -c("metric_id", "measure_id",  "under.5.pop", "modelable_entity_id", "model_version_id")] #here's where draw.rank might be needed

setnames(overall.stunting.hold, c("aggregated.value", "aggregated.value.1990"), c("aggregated.overall.value", "aggregated.overall.value.1990"))
setnames(severe.stunting.hold, c("aggregated.value", "aggregated.value.1990"), c("aggregated.severe.value", "aggregated.severe.value.1990"))
setnames(extreme.stunting.hold, c("aggregated.value", "aggregated.value.1990"), c("aggregated.extreme.value", "aggregated.extreme.value.1990"))


all.stunting.rates <- merge(overall.stunting.hold, severe.stunting.hold, by = c("age_group_id", "location_id", "year_id", "sex_id", "variable"))
all.stunting.rates <- merge(all.stunting.rates, extreme.stunting.hold, by = c("age_group_id", "location_id", "year_id", "sex_id", "variable"))





#getting a mean value for 1990 and each subsequent year
all.stunting.rates[, mean.1990.overall.value := mean(aggregated.overall.value.1990), by = c("location_id", "year_id")]
all.stunting.rates[, mean.1990.severe.value := mean(aggregated.severe.value.1990), by = c("location_id", "year_id")]
all.stunting.rates[, mean.1990.extreme.value := mean(aggregated.extreme.value.1990), by = c("location_id", "year_id")]
all.stunting.rates[, mean.overall.value := mean(aggregated.overall.value), by = c("location_id", "year_id")]
all.stunting.rates[, mean.severe.value := mean(aggregated.severe.value), by = c("location_id", "year_id")]
all.stunting.rates[, mean.extreme.value := mean(aggregated.extreme.value), by = c("location_id", "year_id")]

#only going to calculate improvement ratio for places where all 3 severities improved - otherwise you get infinite values. Using those mean values to identify places that decreased
all.stunting.rates[year_id == 2020 &
                     (mean.overall.value < mean.1990.overall.value) &
                     (mean.severe.value < mean.1990.severe.value) &
                     (mean.extreme.value < mean.1990.extreme.value), 
                   severe.to.overall.improvement.ratio := severe.pc.1990/overall.pc.1990]

all.stunting.rates[year_id == 2020 &
                     (mean.overall.value < mean.1990.overall.value) &
                     (mean.severe.value < mean.1990.severe.value) &
                     (mean.extreme.value < mean.1990.extreme.value), 
                   extreme.to.overall.improvement.ratio := extreme.pc.1990/overall.pc.1990]

#getting rid of non-2020 rows and places where all 3 severities didn't improve
improving.stunting.rates <- all.stunting.rates[!is.na(severe.to.overall.improvement.ratio) & !is.na(extreme.to.overall.improvement.ratio)]


#taking the mean, upper, lower of improvement ratios for severe and extreme
improving.stunting.rates[, mean.severe.to.overall.ir := mean(severe.to.overall.improvement.ratio), by = c("location_id", "year_id")]
improving.stunting.rates[, lower.severe.to.overall.ir := quantile(severe.to.overall.improvement.ratio, probs = .025, na.rm = T), by = c("location_id", "year_id")]
improving.stunting.rates[, upper.severe.to.overall.ir := quantile(severe.to.overall.improvement.ratio, probs = .975, na.rm = T), by = c("location_id", "year_id")]

improving.stunting.rates[, mean.extreme.to.overall.ir := mean(extreme.to.overall.improvement.ratio), by = c("location_id", "year_id")]
improving.stunting.rates[, lower.extreme.to.overall.ir := quantile(extreme.to.overall.improvement.ratio, probs = .025, na.rm = T), by = c("location_id", "year_id")]
improving.stunting.rates[, upper.extreme.to.overall.ir := quantile(extreme.to.overall.improvement.ratio, probs = .975, na.rm = T), by = c("location_id", "year_id")]

#subsetting to only have 1 row per location
improving.stunting.rates[, loc.year := paste0(location_id, "_", year_id)]
improving.stunting.rates.collapsed <- improving.stunting.rates[!duplicated(improving.stunting.rates$loc.year)]

#merging on location names
loc.names <- loc.met[, c("location_id", "location_name")]
improving.stunting.rates.collapsed <- merge(loc.names, improving.stunting.rates.collapsed, by = "location_id")

#identifying locations where improvement ratios are significant, meaning even the lower for the improvement ratio is above 1
improving.stunting.rates.collapsed[lower.severe.to.overall.ir > 1, severe.signficant := 1]
improving.stunting.rates.collapsed[lower.extreme.to.overall.ir > 1, extreme.signficant := 1]
stunting.improving.faster.collapsed <- improving.stunting.rates.collapsed[severe.signficant == 1 & extreme.signficant == 1]

print(paste0("Globally since 1990, severe stunting has improved ", round(unique(stunting.improving.faster.collapsed[location_id == 1]$mean.severe.to.overall.ir), 2),
             " (", round(unique(stunting.improving.faster.collapsed[location_id == 1]$lower.severe.to.overall.ir), 2), " - ", 
             round(unique(stunting.improving.faster.collapsed[location_id == 1]$upper.severe.to.overall.ir), 2), ") times more than overall stunting."))

print(paste0("Globally since 1990, extreme stunting has improved ", round(unique(stunting.improving.faster.collapsed[location_id == 1]$mean.extreme.to.overall.ir), 2),
             " (", round(unique(stunting.improving.faster.collapsed[location_id == 1]$lower.extreme.to.overall.ir), 2), " - ", 
             round(unique(stunting.improving.faster.collapsed[location_id == 1]$upper.extreme.to.overall.ir), 2), ") times more than overall stunting."))

print(paste0("In Sub-Saharan Africa since 1990, severe stunting has improved ", round(unique(stunting.improving.faster.collapsed[location_id == 166]$mean.severe.to.overall.ir), 2),
             " (", round(unique(stunting.improving.faster.collapsed[location_id == 166]$lower.severe.to.overall.ir), 2), " - ", 
             round(unique(stunting.improving.faster.collapsed[location_id == 166]$upper.severe.to.overall.ir), 2), ") times more than overall stunting."))

print(paste0("In Sub-Saharan Africa since 1990, extreme stunting has improved ", round(unique(stunting.improving.faster.collapsed[location_id == 166]$mean.extreme.to.overall.ir), 2),
             " (", round(unique(stunting.improving.faster.collapsed[location_id == 166]$lower.extreme.to.overall.ir), 2), " - ", 
             round(unique(stunting.improving.faster.collapsed[location_id == 166]$upper.extreme.to.overall.ir), 2), ") times more than overall stunting."))






##############################################################################################################
# Wasting
##############################################################################################################

# prepping overall (moderate) wasting
wasting.rate.compare.overall <- get_draws("modelable_entity_id", 10558, year_id=c(1990:2020), 
                                           source="epi", gbd_round_id=7, decomp_step="iterative", 
                                           age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = figure.locs)

write.csv(wasting.rate.compare.overall, "filepath", row.names = F)


wasting.rate.compare.overall <- melt(wasting.rate.compare.overall, id.vars = id.vars)

#set any 0 values to extremely small prevalences to avoid infinite numbers in calculations
wasting.rate.compare.overall[value == 0, value:= 0.000000001]

wasting.rate.compare.overall <- merge(wasting.rate.compare.overall, populations, by = c("age_group_id", "location_id", "year_id", "sex_id"))
wasting.rate.compare.overall[, weighted.val := value*population]

wasting.rate.compare.overall[, agg.weighted.val := sum(weighted.val), by = c("location_id", "year_id", "variable")]
wasting.rate.compare.overall[, under.5.pop := sum(population), by = c("location_id", "year_id", "variable")]
wasting.rate.compare.overall[, aggregated.value := agg.weighted.val/under.5.pop]



overall.wasting.1990 <- wasting.rate.compare.overall[year_id == 1990]
overall.wasting.1990$year_id <- NULL
setnames(overall.wasting.1990, "aggregated.value", "aggregated.value.1990")
overall.wasting.1990 <- overall.wasting.1990[, -c("population", "weighted.val", "agg.weighted.val", "under.5.pop", "value")]

wasting.rate.compare.overall <- merge(wasting.rate.compare.overall, overall.wasting.1990, by = c("age_group_id", "location_id", "sex_id", "metric_id", "measure_id", "modelable_entity_id",  "model_version_id", "variable"))

wasting.rate.compare.overall[, overall.pc.1990 := ((aggregated.value - aggregated.value.1990)/aggregated.value.1990)*100]




#picking one age, one sex. Doesn't matter, we've just age-sex aggregated going to change labels after this
wasting.rate.compare.u5.overall <- wasting.rate.compare.overall[age_group_id == 34 & sex_id == 1]
wasting.rate.compare.u5.overall[, age_group_id := 1]
wasting.rate.compare.u5.overall[, sex_id := 3]
wasting.rate.compare.u5.overall <- wasting.rate.compare.u5.overall[, -c("value", "weighted.val", "agg.weighted.val", "population")]

overall.wasting.meanpcs <- copy(wasting.rate.compare.u5.overall)



overall.wasting.meanpcs[, median.overall.pc := median(overall.pc.1990), by = c("location_id", "year_id")]
overall.wasting.meanpcs[, lower.overall.pc := quantile(overall.pc.1990, probs = .025, na.rm = T), by = c("location_id", "year_id")]
overall.wasting.meanpcs[, upper.overall.pc := quantile(overall.pc.1990, probs = .975, na.rm = T), by = c("location_id", "year_id")]
overall.wasting.hold <- copy(overall.wasting.meanpcs)
write.csv(overall.wasting.hold, "filepath", row.names = F)
overall.wasting.hold <- fread("filepath")
overall.wasting.meanpcs[, loc.year := paste0(location_id, "_", year_id)]
overall.wasting.meanpcs <- overall.wasting.meanpcs[!duplicated(overall.wasting.meanpcs$loc.year)]
overall.wasting.meanpcs$loc.year <- NULL





# prepping severe wasting
wasting.rate.compare.severe <- get_draws("modelable_entity_id", 8945, year_id=c(1990:2020), 
                                          source="epi", gbd_round_id=7, decomp_step="iterative", 
                                          age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1,2), location_id = figure.locs)

write.csv(wasting.rate.compare.severe, "filepath", row.names = F)

wasting.rate.compare.severe <- melt(wasting.rate.compare.severe, id.vars = id.vars)

#set any 0 values to extremely small prevalences to avoid infinite numbers in calculations
wasting.rate.compare.severe[value == 0, value:= 0.000000001]

wasting.rate.compare.severe <- merge(wasting.rate.compare.severe, populations, by = c("age_group_id", "location_id", "year_id", "sex_id"))
wasting.rate.compare.severe[, weighted.val := value*population]

wasting.rate.compare.severe[, agg.weighted.val := sum(weighted.val), by = c("location_id", "year_id", "variable")]
wasting.rate.compare.severe[, under.5.pop := sum(population), by = c("location_id", "year_id", "variable")]
wasting.rate.compare.severe[, aggregated.value := agg.weighted.val/under.5.pop]



severe.wasting.1990 <- wasting.rate.compare.severe[year_id == 1990]
severe.wasting.1990$year_id <- NULL
setnames(severe.wasting.1990, "aggregated.value", "aggregated.value.1990")
severe.wasting.1990 <- severe.wasting.1990[, -c("population", "weighted.val", "agg.weighted.val", "under.5.pop", "value")]

wasting.rate.compare.severe <- merge(wasting.rate.compare.severe, severe.wasting.1990, by = c("age_group_id", "location_id", "sex_id", "metric_id", "measure_id", "modelable_entity_id",  "model_version_id", "variable"))

wasting.rate.compare.severe[, severe.pc.1990 := ((aggregated.value - aggregated.value.1990)/aggregated.value.1990)*100]




#picking one age, one sex. Doesn't matter, we've just age-sex aggregated going to change labels after this
wasting.rate.compare.u5.severe <- wasting.rate.compare.severe[age_group_id == 34 & sex_id == 1]
wasting.rate.compare.u5.severe[, age_group_id := 1]
wasting.rate.compare.u5.severe[, sex_id := 3]
wasting.rate.compare.u5.severe <- wasting.rate.compare.u5.severe[, -c("value", "weighted.val", "agg.weighted.val", "population")]

severe.wasting.meanpcs <- copy(wasting.rate.compare.u5.severe)



severe.wasting.meanpcs[, median.severe.pc := median(severe.pc.1990), by = c("location_id", "year_id")]
severe.wasting.meanpcs[, lower.severe.pc := quantile(severe.pc.1990, probs = .025, na.rm = T), by = c("location_id", "year_id")]
severe.wasting.meanpcs[, upper.severe.pc := quantile(severe.pc.1990, probs = .975, na.rm = T), by = c("location_id", "year_id")]
severe.wasting.hold <- copy(severe.wasting.meanpcs)
write.csv(severe.wasting.hold, "filepath", row.names = F)
severe.wasting.hold <- fread("filepath")
severe.wasting.meanpcs[, loc.year := paste0(location_id, "_", year_id)]
severe.wasting.meanpcs <- severe.wasting.meanpcs[!duplicated(severe.wasting.meanpcs$loc.year)]
severe.wasting.meanpcs$loc.year <- NULL





# prepping extreme wasting
wasting.rate.compare.extreme <- get_draws("modelable_entity_id", 26943, year_id=c(1990:2020), 
                                           source="epi", gbd_round_id=7, decomp_step="iterative", 
                                           age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = figure.locs)

write.csv(wasting.rate.compare.extreme, "filepath", row.names = F)

wasting.rate.compare.extreme <- melt(wasting.rate.compare.extreme, id.vars = id.vars)

#set any 0 values to extremely small prevalences to avoid infinite numbers in calculations
wasting.rate.compare.extreme[value == 0, value:= 0.000000001]

wasting.rate.compare.extreme <- merge(wasting.rate.compare.extreme, populations, by = c("age_group_id", "location_id", "year_id", "sex_id"))
wasting.rate.compare.extreme[, weighted.val := value*population]

wasting.rate.compare.extreme[, agg.weighted.val := sum(weighted.val), by = c("location_id", "year_id", "variable")]
wasting.rate.compare.extreme[, under.5.pop := sum(population), by = c("location_id", "year_id", "variable")]
wasting.rate.compare.extreme[, aggregated.value := agg.weighted.val/under.5.pop]



extreme.wasting.1990 <- wasting.rate.compare.extreme[year_id == 1990]
extreme.wasting.1990$year_id <- NULL
setnames(extreme.wasting.1990, "aggregated.value", "aggregated.value.1990")
extreme.wasting.1990 <- extreme.wasting.1990[, -c("population", "weighted.val", "agg.weighted.val", "under.5.pop", "value")]

wasting.rate.compare.extreme <- merge(wasting.rate.compare.extreme, extreme.wasting.1990, by = c("age_group_id", "location_id", "sex_id", "metric_id", "measure_id", "modelable_entity_id",  "model_version_id", "variable"))

wasting.rate.compare.extreme[, extreme.pc.1990 := ((aggregated.value - aggregated.value.1990)/aggregated.value.1990)*100]




#picking one age, one sex. Doesn't matter, we've just age-sex aggregated going to change labels after this
wasting.rate.compare.u5.extreme <- wasting.rate.compare.extreme[age_group_id == 34 & sex_id == 1]
wasting.rate.compare.u5.extreme[, age_group_id := 1]
wasting.rate.compare.u5.extreme[, sex_id := 3]
wasting.rate.compare.u5.extreme <- wasting.rate.compare.u5.extreme[, -c("value", "weighted.val", "agg.weighted.val", "population")]

extreme.wasting.meanpcs <- copy(wasting.rate.compare.u5.extreme)



extreme.wasting.meanpcs[, median.extreme.pc := median(extreme.pc.1990), by = c("location_id", "year_id")]
extreme.wasting.meanpcs[, lower.extreme.pc := quantile(extreme.pc.1990, probs = .025, na.rm = T), by = c("location_id", "year_id")]
extreme.wasting.meanpcs[, upper.extreme.pc := quantile(extreme.pc.1990, probs = .975, na.rm = T), by = c("location_id", "year_id")]
extreme.wasting.hold <- copy(extreme.wasting.meanpcs)
write.csv(extreme.wasting.hold, "filepath", row.names = F)
extreme.wasting.hold <- fread("filepath")
extreme.wasting.meanpcs[, loc.year := paste0(location_id, "_", year_id)]
extreme.wasting.meanpcs <- extreme.wasting.meanpcs[!duplicated(extreme.wasting.meanpcs$loc.year)]
extreme.wasting.meanpcs$loc.year <- NULL








######################################################################################



#merging wasting together
overall.wasting.meanpcs$model_version_id <- NULL
overall.wasting.meanpcs$modelable_entity_id <- NULL
severe.wasting.meanpcs$model_version_id <- NULL
severe.wasting.meanpcs$modelable_entity_id <- NULL
extreme.wasting.meanpcs$model_version_id <- NULL
extreme.wasting.meanpcs$modelable_entity_id <- NULL

setnames(severe.wasting.meanpcs, c("overall.pc.1990", "median.overall.pc", "lower.overall.pc", "upper.overall.pc"), 
         c("severe.pc.1990", "median.severe.pc", "lower.severe.pc", "upper.severe.pc"))

setnames(extreme.wasting.meanpcs, c("overall.pc.1990", "median.overall.pc", "lower.overall.pc", "upper.overall.pc"), 
         c("extreme.pc.1990", "median.extreme.pc", "lower.extreme.pc", "upper.extreme.pc"))




overall.wasting.meanpcs <- overall.wasting.meanpcs[, -c("metric_id", "measure_id", "variable", "under.5.pop", "overall.pc.1990")] #here's where draw.rank might be needed
severe.wasting.meanpcs <- severe.wasting.meanpcs[, -c("metric_id", "measure_id", "variable", "under.5.pop", "severe.pc.1990")] #here's where draw.rank might be needed
extreme.wasting.meanpcs <- extreme.wasting.meanpcs[, -c("metric_id", "measure_id", "variable", "under.5.pop", "extreme.pc.1990")] #here's where draw.rank might be needed

setnames(overall.wasting.meanpcs, c("aggregated.value", "aggregated.value.1990"), c("aggregated.overall.value", "aggregated.overall.value.1990"))
setnames(severe.wasting.meanpcs, c("aggregated.value", "aggregated.value.1990"), c("aggregated.severe.value", "aggregated.severe.value.1990"))
setnames(extreme.wasting.meanpcs, c("aggregated.value", "aggregated.value.1990"), c("aggregated.extreme.value", "aggregated.extreme.value.1990"))


all.wasting.pcs <- merge(overall.wasting.meanpcs, severe.wasting.meanpcs, by = c("age_group_id", "location_id", "year_id", "sex_id"))
all.wasting.pcs <- merge(all.wasting.pcs, extreme.wasting.meanpcs, by = c("age_group_id", "location_id", "year_id", "sex_id"))




# exclude locations that are regions/superregions
aggregate.locs <- loc.met[level < 3]$location_id


#only include countries that decreased from 1990 - 2020 for all three severities
decreasing.wasting.locs <- unique(all.wasting.pcs[year_id == 2020 & median.overall.pc < 0 & median.severe.pc < 0 & median.extreme.pc < 0]$location_id)



region.locs <- loc.met[level == 2]$location_id
sr.locs <- loc.met[level == 1]$location_id
uk.ids <- c( 4749,  433 , 434 ,4636)
density.plot.locs <- loc.met[level == 3 | location_id %in% uk.ids]$location_id



breaks <- c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100)
labels = paste0(breaks, "%")
labels = replace(labels, labels == "100%", ">100%")


wasting.time.trend.melted <- melt(all.wasting.pcs, id.vars = c("age_group_id", "location_id", "year_id", "sex_id"))
wasting.time.trend.melted[, loc.var := paste0(location_id, "_", variable)]
wasting.time.trend.melted$variable <- as.character(wasting.time.trend.melted$variable)



extreme.waste.exclude.locs <- unique(wasting.time.trend.melted[variable == "aggregated.extreme.value" & value < 1e-06 & location_id %in% density.plot.locs]$location_id)

population.exclude.locs <- populations[age_group_id == 22 & sex_id == 3 & year_id == 2020 & population < 300000 & location_id %in% density.plot.locs]$location_id


over.100.changes.wasting <- unique(wasting.time.trend.melted[variable %like% "median" & year_id == 2020 & location_id %in% density.plot.locs & 
                                                               location_id %ni% extreme.waste.exclude.locs &
                                                               location_id %ni% population.exclude.locs &
                                                               (value > 100 | value < -100)]$location_id)




wasting.time.trend.melted[, cgf.type := "Wasting (WHZ)"]

wasting.time.trend.melted.plot <- ggplot() +
  geom_hline(yintercept = 0, color = "grey50", size = .9) +
  geom_line(data =  wasting.time.trend.melted[location_id %in% sr.locs &
                                                variable %like% "median"], aes(x= year_id, y = value,  group = loc.var, color = variable), alpha = .8, size = .4) +
  geom_line(data = wasting.time.trend.melted[location_id == 1 & variable %like% "median"], aes(x= year_id, y = value,  group = loc.var, color = variable), size = 2.2) +
  scale_color_manual(values = c("#a11d0e", "#E69C12", "#e0631f")) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Year", y = "Percent Change since 1990", tag = "B") +
  scale_y_continuous(breaks = breaks, labels = labels, limits = c(-100, 100), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(from = 1990, to = 2020, by = 10), expand = c(0,0)) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(vjust = -.5),
        panel.grid.minor = element_line(size = 0),
        strip.text = element_text(size = 24),
        plot.tag = element_text(face = "bold", size = 26, vjust = -.5)) +
  facet_grid(~cgf.type)


wasting.time.trend.melted[location_id %in% over.100.changes.wasting & variable == "median.extreme.pc", value := 100]

wasting.density.df <- wasting.time.trend.melted[location_id %ni% aggregate.locs &
                                                  location_id != 95 &
                                                  location_id %in% density.plot.locs &
                                                  variable %like% "median" &
                                                  location_id %ni% extreme.waste.exclude.locs &
                                                  location_id %ni% population.exclude.locs &
                                                  year_id == 2020]

wasting.density.df$variable <- factor(wasting.density.df$variable, levels = c("median.overall.pc", "median.severe.pc", "median.extreme.pc"))


wasting.marginal <- ggplot() +
  geom_density(data = wasting.density.df,
               aes(x = value, fill = variable),  alpha = .6, bw = 6) +
  scale_fill_manual(values = c("#E69C12","#e0631f", "#a11d0e")) +
  theme_void() +
  theme(legend.position = "none") +
  coord_flip() +
  theme(plot.margin=unit(c(1.41, 0, .95, 0),"cm"))








wasting.combined.plot <- plot_grid(wasting.time.trend.melted.plot, wasting.marginal, ncol = 2, nrow = 1, rel_widths = c(3, .75))




###############################
#calculating how much more severe and extreme wasting improved compared to overall wasting
###############################



setnames(severe.wasting.hold, c("overall.pc.1990", "median.overall.pc", "lower.overall.pc", "upper.overall.pc"), 
         c("severe.pc.1990", "median.severe.pc", "lower.severe.pc", "upper.severe.pc"))

setnames(extreme.wasting.hold, c("overall.pc.1990", "median.overall.pc", "lower.overall.pc", "upper.overall.pc"), 
         c("extreme.pc.1990", "median.extreme.pc", "lower.extreme.pc", "upper.extreme.pc"))




overall.wasting.hold <- overall.wasting.hold[, -c("metric_id", "measure_id",  "under.5.pop", "modelable_entity_id", "model_version_id")] #here's where draw.rank might be needed
severe.wasting.hold <- severe.wasting.hold[, -c("metric_id", "measure_id",  "under.5.pop", "modelable_entity_id", "model_version_id")] #here's where draw.rank might be needed
extreme.wasting.hold <- extreme.wasting.hold[, -c("metric_id", "measure_id",  "under.5.pop", "modelable_entity_id", "model_version_id")] #here's where draw.rank might be needed

setnames(overall.wasting.hold, c("aggregated.value", "aggregated.value.1990"), c("aggregated.overall.value", "aggregated.overall.value.1990"))
setnames(severe.wasting.hold, c("aggregated.value", "aggregated.value.1990"), c("aggregated.severe.value", "aggregated.severe.value.1990"))
setnames(extreme.wasting.hold, c("aggregated.value", "aggregated.value.1990"), c("aggregated.extreme.value", "aggregated.extreme.value.1990"))


all.wasting.rates <- merge(overall.wasting.hold, severe.wasting.hold, by = c("age_group_id", "location_id", "year_id", "sex_id", "variable"))
all.wasting.rates <- merge(all.wasting.rates, extreme.wasting.hold, by = c("age_group_id", "location_id", "year_id", "sex_id", "variable"))

#getting a mean value for 1990 and each subsequent year
all.wasting.rates[, mean.1990.overall.value := mean(aggregated.overall.value.1990), by = c("location_id", "year_id")]
all.wasting.rates[, mean.1990.severe.value := mean(aggregated.severe.value.1990), by = c("location_id", "year_id")]
all.wasting.rates[, mean.1990.extreme.value := mean(aggregated.extreme.value.1990), by = c("location_id", "year_id")]
all.wasting.rates[, mean.overall.value := mean(aggregated.overall.value), by = c("location_id", "year_id")]
all.wasting.rates[, mean.severe.value := mean(aggregated.severe.value), by = c("location_id", "year_id")]
all.wasting.rates[, mean.extreme.value := mean(aggregated.extreme.value), by = c("location_id", "year_id")]

#only going to calculate improvement ratio for places where all 3 severities improved - otherwise you get infinite values. Using those mean values to identify places that decreased
all.wasting.rates[year_id == 2020 &
                     (mean.overall.value < mean.1990.overall.value) &
                     (mean.severe.value < mean.1990.severe.value) &
                     (mean.extreme.value < mean.1990.extreme.value), 
                   severe.to.overall.improvement.ratio := severe.pc.1990/overall.pc.1990]

all.wasting.rates[year_id == 2020 &
                     (mean.overall.value < mean.1990.overall.value) &
                     (mean.severe.value < mean.1990.severe.value) &
                     (mean.extreme.value < mean.1990.extreme.value), 
                   extreme.to.overall.improvement.ratio := extreme.pc.1990/overall.pc.1990]

#getting rid of non-2020 rows and places where all 3 severities didn't improve
improving.wasting.rates <- all.wasting.rates[!is.na(severe.to.overall.improvement.ratio) & !is.na(extreme.to.overall.improvement.ratio)]


#taking the mean, upper, lower of improvement ratios for severe and extreme
improving.wasting.rates[, mean.severe.to.overall.ir := mean(severe.to.overall.improvement.ratio), by = c("location_id", "year_id")]
improving.wasting.rates[, lower.severe.to.overall.ir := quantile(severe.to.overall.improvement.ratio, probs = .025, na.rm = T), by = c("location_id", "year_id")]
improving.wasting.rates[, upper.severe.to.overall.ir := quantile(severe.to.overall.improvement.ratio, probs = .975, na.rm = T), by = c("location_id", "year_id")]

improving.wasting.rates[, mean.extreme.to.overall.ir := mean(extreme.to.overall.improvement.ratio), by = c("location_id", "year_id")]
improving.wasting.rates[, lower.extreme.to.overall.ir := quantile(extreme.to.overall.improvement.ratio, probs = .025, na.rm = T), by = c("location_id", "year_id")]
improving.wasting.rates[, upper.extreme.to.overall.ir := quantile(extreme.to.overall.improvement.ratio, probs = .975, na.rm = T), by = c("location_id", "year_id")]

#subsetting to only have 1 row per location
improving.wasting.rates[, loc.year := paste0(location_id, "_", year_id)]
improving.wasting.rates.collapsed <- improving.wasting.rates[!duplicated(improving.wasting.rates$loc.year)]

#merging on location names
loc.names <- loc.met[, c("location_id", "location_name")]
improving.wasting.rates.collapsed <- merge(loc.names, improving.wasting.rates.collapsed, by = "location_id")

#identifying locations where improvement ratios are significant, meaning even the lower for the improvement ratio is above 1
improving.wasting.rates.collapsed[lower.severe.to.overall.ir > 1, severe.signficant := 1]
improving.wasting.rates.collapsed[lower.extreme.to.overall.ir > 1, extreme.signficant := 1]
wasting.improving.faster.collapsed <- improving.wasting.rates.collapsed[severe.signficant == 1 & extreme.signficant == 1]

print(paste0("Globally since 1990, severe wasting has improved ", round(unique(wasting.improving.faster.collapsed[location_id == 1]$mean.severe.to.overall.ir), 2),
             " (", round(unique(wasting.improving.faster.collapsed[location_id == 1]$lower.severe.to.overall.ir), 2), " - ", 
             round(unique(wasting.improving.faster.collapsed[location_id == 1]$upper.severe.to.overall.ir), 2), ") times more than overall wasting"))

print(paste0("Globally since 1990, extreme wasting has improved ", round(unique(wasting.improving.faster.collapsed[location_id == 1]$mean.extreme.to.overall.ir), 2),
             " (", round(unique(wasting.improving.faster.collapsed[location_id == 1]$lower.extreme.to.overall.ir), 2), " - ", 
             round(unique(wasting.improving.faster.collapsed[location_id == 1]$upper.extreme.to.overall.ir), 2), ") times more than overall wasting"))

print(paste0("In Yemen since 1990, severe wasting has improved ", round(unique(wasting.improving.faster.collapsed[location_id == 157]$mean.severe.to.overall.ir), 2),
             " (", round(unique(wasting.improving.faster.collapsed[location_id == 157]$lower.severe.to.overall.ir), 2), " - ", 
             round(unique(wasting.improving.faster.collapsed[location_id == 157]$upper.severe.to.overall.ir), 2), ") times more than overall wasting"))

print(paste0("In Yemen since 1990, extreme wasting has improved ", round(unique(wasting.improving.faster.collapsed[location_id == 157]$mean.extreme.to.overall.ir), 2),
             " (", round(unique(wasting.improving.faster.collapsed[location_id == 157]$lower.extreme.to.overall.ir), 2), " - ", 
             round(unique(wasting.improving.faster.collapsed[location_id == 157]$upper.extreme.to.overall.ir), 2), ") times more than overall wasting"))


print(paste0("In South Sudan since 1990, severe wasting has improved ", round(unique(wasting.improving.faster.collapsed[location_id == 435]$mean.severe.to.overall.ir), 2),
             " (", round(unique(wasting.improving.faster.collapsed[location_id == 435]$lower.severe.to.overall.ir), 2), " - ", 
             round(unique(wasting.improving.faster.collapsed[location_id == 435]$upper.severe.to.overall.ir), 2), ") times more than overall wasting"))

print(paste0("In South Sudan since 1990, extreme wasting has improved ", round(unique(wasting.improving.faster.collapsed[location_id == 435]$mean.extreme.to.overall.ir), 2),
             " (", round(unique(wasting.improving.faster.collapsed[location_id == 435]$lower.extreme.to.overall.ir), 2), " - ", 
             round(unique(wasting.improving.faster.collapsed[location_id == 435]$upper.extreme.to.overall.ir), 2), ") times more than overall wasting"))










##############################################################################################################
# Underweight
##############################################################################################################

# prepping overall (moderate) underweight
underweight.rate.compare.overall <- get_draws("modelable_entity_id", 10560, year_id=c(1990:2020), 
                                           source="epi", gbd_round_id=7, decomp_step="iterative", 
                                           age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = figure.locs)

write.csv(underweight.rate.compare.overall, "filepath", row.names = F)
underweight.rate.compare.overall <- fread("filepath")
underweight.rate.compare.overall <- melt(underweight.rate.compare.overall, id.vars = id.vars)

#set any 0 values to extremely small prevalences to avoid infinite numbers in calculations
underweight.rate.compare.overall[value == 0, value:= 0.000000001]

underweight.rate.compare.overall <- merge(underweight.rate.compare.overall, populations, by = c("age_group_id", "location_id", "year_id", "sex_id"))
underweight.rate.compare.overall[, weighted.val := value*population]

underweight.rate.compare.overall[, agg.weighted.val := sum(weighted.val), by = c("location_id", "year_id", "variable")]
underweight.rate.compare.overall[, under.5.pop := sum(population), by = c("location_id", "year_id", "variable")]
underweight.rate.compare.overall[, aggregated.value := agg.weighted.val/under.5.pop]



overall.underweight.1990 <- underweight.rate.compare.overall[year_id == 1990]
overall.underweight.1990$year_id <- NULL
setnames(overall.underweight.1990, "aggregated.value", "aggregated.value.1990")
overall.underweight.1990 <- overall.underweight.1990[, -c("population", "weighted.val", "agg.weighted.val", "under.5.pop", "value")]

underweight.rate.compare.overall <- merge(underweight.rate.compare.overall, overall.underweight.1990, by = c("age_group_id", "location_id", "sex_id", "metric_id", "measure_id", "modelable_entity_id",  "model_version_id", "variable"))

underweight.rate.compare.overall[, overall.pc.1990 := ((aggregated.value - aggregated.value.1990)/aggregated.value.1990)*100]




#picking one age, one sex. Doesn't matter, we've just age-sex aggregated going to change labels after this
underweight.rate.compare.u5.overall <- underweight.rate.compare.overall[age_group_id == 34 & sex_id == 1]
underweight.rate.compare.u5.overall[, age_group_id := 1]
underweight.rate.compare.u5.overall[, sex_id := 3]
underweight.rate.compare.u5.overall <- underweight.rate.compare.u5.overall[, -c("value", "weighted.val", "agg.weighted.val", "population")]

overall.underweight.meanpcs <- copy(underweight.rate.compare.u5.overall)



overall.underweight.meanpcs[, median.overall.pc := median(overall.pc.1990), by = c("location_id", "year_id")]
overall.underweight.meanpcs[, lower.overall.pc := quantile(overall.pc.1990, probs = .025, na.rm = T), by = c("location_id", "year_id")]
overall.underweight.meanpcs[, upper.overall.pc := quantile(overall.pc.1990, probs = .975, na.rm = T), by = c("location_id", "year_id")]
overall.underweight.hold <- copy(overall.underweight.meanpcs)
overall.underweight.meanpcs[, loc.year := paste0(location_id, "_", year_id)]
overall.underweight.meanpcs <- overall.underweight.meanpcs[!duplicated(overall.underweight.meanpcs$loc.year)]
overall.underweight.meanpcs$loc.year <- NULL





# prepping severe underweight
underweight.rate.compare.severe <- get_draws("modelable_entity_id", 8949, year_id=c(1990:2020), 
                                          source="epi", gbd_round_id=7, decomp_step="iterative", 
                                          age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1,2), location_id = figure.locs)

write.csv(stunting.rate.compare.severe, "filepath", row.names = F)

underweight.rate.compare.severe <- melt(underweight.rate.compare.severe, id.vars = id.vars)

#set any 0 values to extremely small prevalences to avoid infinite numbers in calculations
underweight.rate.compare.severe[value == 0, value:= 0.000000001]

underweight.rate.compare.severe <- merge(underweight.rate.compare.severe, populations, by = c("age_group_id", "location_id", "year_id", "sex_id"))
underweight.rate.compare.severe[, weighted.val := value*population]

underweight.rate.compare.severe[, agg.weighted.val := sum(weighted.val), by = c("location_id", "year_id", "variable")]
underweight.rate.compare.severe[, under.5.pop := sum(population), by = c("location_id", "year_id", "variable")]
underweight.rate.compare.severe[, aggregated.value := agg.weighted.val/under.5.pop]



severe.underweight.1990 <- underweight.rate.compare.severe[year_id == 1990]
severe.underweight.1990$year_id <- NULL
setnames(severe.underweight.1990, "aggregated.value", "aggregated.value.1990")
severe.underweight.1990 <- severe.underweight.1990[, -c("population", "weighted.val", "agg.weighted.val", "under.5.pop", "value")]

underweight.rate.compare.severe <- merge(underweight.rate.compare.severe, severe.underweight.1990, by = c("age_group_id", "location_id", "sex_id", "metric_id", "measure_id", "modelable_entity_id",  "model_version_id", "variable"))

underweight.rate.compare.severe[, severe.pc.1990 := ((aggregated.value - aggregated.value.1990)/aggregated.value.1990)*100]




#picking one age, one sex. Doesn't matter, we've just age-sex aggregated going to change labels after this
underweight.rate.compare.u5.severe <- underweight.rate.compare.severe[age_group_id == 34 & sex_id == 1]
underweight.rate.compare.u5.severe[, age_group_id := 1]
underweight.rate.compare.u5.severe[, sex_id := 3]
underweight.rate.compare.u5.severe <- underweight.rate.compare.u5.severe[, -c("value", "weighted.val", "agg.weighted.val", "population")]

severe.underweight.meanpcs <- copy(underweight.rate.compare.u5.severe)



severe.underweight.meanpcs[, median.severe.pc := median(severe.pc.1990), by = c("location_id", "year_id")]
severe.underweight.meanpcs[, lower.severe.pc := quantile(severe.pc.1990, probs = .025, na.rm = T), by = c("location_id", "year_id")]
severe.underweight.meanpcs[, upper.severe.pc := quantile(severe.pc.1990, probs = .975, na.rm = T), by = c("location_id", "year_id")]
severe.underweight.hold <- copy(severe.underweight.meanpcs)
severe.underweight.meanpcs[, loc.year := paste0(location_id, "_", year_id)]
severe.underweight.meanpcs <- severe.underweight.meanpcs[!duplicated(severe.underweight.meanpcs$loc.year)]
severe.underweight.meanpcs$loc.year <- NULL






# prepping extreme underweight
underweight.rate.compare.extreme <- get_draws("modelable_entity_id", 26941, year_id=c(1990:2020), 
                                           source="epi", gbd_round_id=7, decomp_step="iterative", 
                                           age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = figure.locs)

write.csv(underweight.rate.compare.extreme, "filepath", row.names = F)

underweight.rate.compare.extreme <- melt(underweight.rate.compare.extreme, id.vars = id.vars)

#set any 0 values to extremely small prevalences to avoid infinite numbers in calculations
underweight.rate.compare.extreme[value == 0, value:= 0.000000001]

underweight.rate.compare.extreme <- merge(underweight.rate.compare.extreme, populations, by = c("age_group_id", "location_id", "year_id", "sex_id"))
underweight.rate.compare.extreme[, weighted.val := value*population]

underweight.rate.compare.extreme[, agg.weighted.val := sum(weighted.val), by = c("location_id", "year_id", "variable")]
underweight.rate.compare.extreme[, under.5.pop := sum(population), by = c("location_id", "year_id", "variable")]
underweight.rate.compare.extreme[, aggregated.value := agg.weighted.val/under.5.pop]



extreme.underweight.1990 <- underweight.rate.compare.extreme[year_id == 1990]
extreme.underweight.1990$year_id <- NULL
setnames(extreme.underweight.1990, "aggregated.value", "aggregated.value.1990")
extreme.underweight.1990 <- extreme.underweight.1990[, -c("population", "weighted.val", "agg.weighted.val", "under.5.pop", "value")]

underweight.rate.compare.extreme <- merge(underweight.rate.compare.extreme, extreme.underweight.1990, by = c("age_group_id", "location_id", "sex_id", "metric_id", "measure_id", "modelable_entity_id",  "model_version_id", "variable"))

underweight.rate.compare.extreme[, extreme.pc.1990 := ((aggregated.value - aggregated.value.1990)/aggregated.value.1990)*100]




#picking one age, one sex. Doesn't matter, we've just age-sex aggregated going to change labels after this
underweight.rate.compare.u5.extreme <- underweight.rate.compare.extreme[age_group_id == 34 & sex_id == 1]
underweight.rate.compare.u5.extreme[, age_group_id := 1]
underweight.rate.compare.u5.extreme[, sex_id := 3]
underweight.rate.compare.u5.extreme <- underweight.rate.compare.u5.extreme[, -c("value", "weighted.val", "agg.weighted.val", "population")]

extreme.underweight.meanpcs <- copy(underweight.rate.compare.u5.extreme)



extreme.underweight.meanpcs[, median.extreme.pc := median(extreme.pc.1990), by = c("location_id", "year_id")]
extreme.underweight.meanpcs[, lower.extreme.pc := quantile(extreme.pc.1990, probs = .025, na.rm = T), by = c("location_id", "year_id")]
extreme.underweight.meanpcs[, upper.extreme.pc := quantile(extreme.pc.1990, probs = .975, na.rm = T), by = c("location_id", "year_id")]
extreme.underweight.hold <- copy(extreme.underweight.meanpcs)
extreme.underweight.meanpcs[, loc.year := paste0(location_id, "_", year_id)]
extreme.underweight.meanpcs <- extreme.underweight.meanpcs[!duplicated(extreme.underweight.meanpcs$loc.year)]
extreme.underweight.meanpcs$loc.year <- NULL







######################################################################################



#merging underweight together
overall.underweight.meanpcs$model_version_id <- NULL
overall.underweight.meanpcs$modelable_entity_id <- NULL
severe.underweight.meanpcs$model_version_id <- NULL
severe.underweight.meanpcs$modelable_entity_id <- NULL
extreme.underweight.meanpcs$model_version_id <- NULL
extreme.underweight.meanpcs$modelable_entity_id <- NULL

setnames(severe.underweight.meanpcs, c("overall.pc.1990", "median.overall.pc", "lower.overall.pc", "upper.overall.pc"), 
         c("severe.pc.1990", "median.severe.pc", "lower.severe.pc", "upper.severe.pc"))

setnames(extreme.underweight.meanpcs, c("overall.pc.1990", "median.overall.pc", "lower.overall.pc", "upper.overall.pc"), 
         c("extreme.pc.1990", "median.extreme.pc", "lower.extreme.pc", "upper.extreme.pc"))







overall.underweight.meanpcs <- overall.underweight.meanpcs[, -c("metric_id", "measure_id", "variable", "under.5.pop", "overall.pc.1990")] #here's where draw.rank might be needed
severe.underweight.meanpcs <- severe.underweight.meanpcs[, -c("metric_id", "measure_id", "variable", "under.5.pop", "severe.pc.1990")] #here's where draw.rank might be needed
extreme.underweight.meanpcs <- extreme.underweight.meanpcs[, -c("metric_id", "measure_id", "variable", "under.5.pop", "extreme.pc.1990")] #here's where draw.rank might be needed

setnames(overall.underweight.meanpcs, c("aggregated.value", "aggregated.value.1990"), c("aggregated.overall.value", "aggregated.overall.value.1990"))
setnames(severe.underweight.meanpcs, c("aggregated.value", "aggregated.value.1990"), c("aggregated.severe.value", "aggregated.severe.value.1990"))
setnames(extreme.underweight.meanpcs, c("aggregated.value", "aggregated.value.1990"), c("aggregated.extreme.value", "aggregated.extreme.value.1990"))


all.underweight.pcs <- merge(overall.underweight.meanpcs, severe.underweight.meanpcs, by = c("age_group_id", "location_id", "year_id", "sex_id"))
all.underweight.pcs <- merge(all.underweight.pcs, extreme.underweight.meanpcs, by = c("age_group_id", "location_id", "year_id", "sex_id"))






# exclude locations that are regions/superregions
aggregate.locs <- loc.met[level < 3]$location_id


#only include countries that decreased from 1990 - 2020 for all three severities
decreasing.underweight.locs <- unique(all.wasting.pcs[year_id == 2020 & median.overall.pc < 0 & median.severe.pc < 0 & median.extreme.pc < 0]$location_id)



region.locs <- loc.met[level == 2]$location_id
sr.locs <- loc.met[level == 1]$location_id
uk.ids <- c( 4749,  433 , 434 ,4636)
density.plot.locs <- loc.met[level == 3 | location_id %in% uk.ids]$location_id



breaks <- c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100)
labels = paste0(breaks, "%")
labels = replace(labels, labels == "100%", ">100%")


underweight.time.trend.melted <- melt(all.underweight.pcs, id.vars = c("age_group_id", "location_id", "year_id", "sex_id"))
underweight.time.trend.melted[, loc.var := paste0(location_id, "_", variable)]
underweight.time.trend.melted$variable <- as.character(underweight.time.trend.melted$variable)

over.100.changes.underweight <- unique(underweight.time.trend.melted[variable %like% "median" & year_id == 2020 &
                                                               (value > 100 | value < -100)]$location_id)

extreme.underweight.exclude.locs <- unique(underweight.time.trend.melted[variable == "aggregated.extreme.value" & value < 1e-06 & location_id %in% density.plot.locs]$location_id)

population.exclude.locs <- populations[age_group_id == 22 & sex_id == 3 & year_id == 2020 & population < 300000 & location_id %in% density.plot.locs]$location_id







underweight.time.trend.melted[, cgf.type := "Underweight (WAZ)"]


underweight.time.trend.melted.plot <- ggplot() +
  geom_hline(yintercept = 0, color = "grey50", size = .9) +
  geom_line(data =  underweight.time.trend.melted[location_id %in% sr.locs &
                                                    variable %like% "median"], aes(x= year_id, y = value,  group = loc.var, color = variable), alpha = .8, size = .4) +
  geom_line(data = underweight.time.trend.melted[location_id == 1 & variable %like% "median"], aes(x= year_id, y = value,  group = loc.var, color = variable), size = 2.2) +
  scale_color_manual(values = c("#a11d0e", "#E69C12", "#e0631f")) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Year", y = "Percent Change since 1990", tag = "C") +
  scale_y_continuous(breaks = breaks, labels = labels, limits = c(-100, 100), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(from = 1990, to = 2020, by = 10), expand = c(0,0)) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(vjust = -.5),
        panel.grid.minor = element_line(size = 0),
        strip.text = element_text(size = 24),
        plot.tag = element_text(face = "bold", size = 26, vjust = -.5)) +
  facet_grid(~cgf.type)

underweight.density.df <- underweight.time.trend.melted[location_id %ni% aggregate.locs &
                                                          location_id != 95 &
                                                          location_id %in% density.plot.locs &
                                                          variable %like% "median" &
                                                          location_id %ni% extreme.waste.exclude.locs &
                                                          location_id %ni% population.exclude.locs &
                                                          year_id == 2020]

underweight.density.df$variable <- factor(underweight.density.df$variable, levels = c("median.overall.pc", "median.severe.pc", "median.extreme.pc"))


underweight.marginal <- ggplot() +
  geom_density(data = underweight.density.df,
               aes(x = value, fill = variable),  alpha = .6, bw = 6) +
  scale_fill_manual(values = c("#E69C12","#e0631f", "#a11d0e")) +
  theme_void() +
  theme(legend.position = "none") +
  coord_flip() +
  theme(plot.margin=unit(c(7, 0, 1.2, 0),"cm"))






underweight.combined.plot <- plot_grid(underweight.time.trend.melted.plot, underweight.marginal, ncol = 2, nrow = 1, rel_widths = c(3, .75))







###############################
#calculating how much more severe and extreme underweight improved compared to overall wasting
###############################



setnames(severe.underweight.hold, c("overall.pc.1990", "median.overall.pc", "lower.overall.pc", "upper.overall.pc"), 
         c("severe.pc.1990", "median.severe.pc", "lower.severe.pc", "upper.severe.pc"))

setnames(extreme.underweight.hold, c("overall.pc.1990", "median.overall.pc", "lower.overall.pc", "upper.overall.pc"), 
         c("extreme.pc.1990", "median.extreme.pc", "lower.extreme.pc", "upper.extreme.pc"))



overall.underweight.hold <- overall.underweight.hold[, -c("metric_id", "measure_id",  "under.5.pop", "modelable_entity_id", "model_version_id")] #here's where draw.rank might be needed
severe.underweight.hold <- severe.underweight.hold[, -c("metric_id", "measure_id",  "under.5.pop", "modelable_entity_id", "model_version_id")] #here's where draw.rank might be needed
extreme.underweight.hold <- extreme.underweight.hold[, -c("metric_id", "measure_id",  "under.5.pop", "modelable_entity_id", "model_version_id")] #here's where draw.rank might be needed

setnames(overall.underweight.hold, c("aggregated.value", "aggregated.value.1990"), c("aggregated.overall.value", "aggregated.overall.value.1990"))
setnames(severe.underweight.hold, c("aggregated.value", "aggregated.value.1990"), c("aggregated.severe.value", "aggregated.severe.value.1990"))
setnames(extreme.underweight.hold, c("aggregated.value", "aggregated.value.1990"), c("aggregated.extreme.value", "aggregated.extreme.value.1990"))


all.underweight.rates <- merge(overall.underweight.hold, severe.underweight.hold, by = c("age_group_id", "location_id", "year_id", "sex_id", "variable"))
all.underweight.rates <- merge(all.underweight.rates, extreme.underweight.hold, by = c("age_group_id", "location_id", "year_id", "sex_id", "variable"))

#getting a mean value for 1990 and each subsequent year
all.underweight.rates[, mean.1990.overall.value := mean(aggregated.overall.value.1990), by = c("location_id", "year_id")]
all.underweight.rates[, mean.1990.severe.value := mean(aggregated.severe.value.1990), by = c("location_id", "year_id")]
all.underweight.rates[, mean.1990.extreme.value := mean(aggregated.extreme.value.1990), by = c("location_id", "year_id")]
all.underweight.rates[, mean.overall.value := mean(aggregated.overall.value), by = c("location_id", "year_id")]
all.underweight.rates[, mean.severe.value := mean(aggregated.severe.value), by = c("location_id", "year_id")]
all.underweight.rates[, mean.extreme.value := mean(aggregated.extreme.value), by = c("location_id", "year_id")]

#only going to calculate improvement ratio for places where all 3 severities improved - otherwise you get infinite values. Using those mean values to identify places that decreased
all.underweight.rates[year_id == 2020 &
                    (mean.overall.value < mean.1990.overall.value) &
                    (mean.severe.value < mean.1990.severe.value) &
                    (mean.extreme.value < mean.1990.extreme.value), 
                  severe.to.overall.improvement.ratio := severe.pc.1990/overall.pc.1990]

all.underweight.rates[year_id == 2020 &
                    (mean.overall.value < mean.1990.overall.value) &
                    (mean.severe.value < mean.1990.severe.value) &
                    (mean.extreme.value < mean.1990.extreme.value), 
                  extreme.to.overall.improvement.ratio := extreme.pc.1990/overall.pc.1990]

#getting rid of non-2020 rows and places where all 3 severities didn't improve
improving.underweight.rates <- all.underweight.rates[!is.na(severe.to.overall.improvement.ratio) & !is.na(extreme.to.overall.improvement.ratio)]


#taking the mean, upper, lower of improvement ratios for severe and extreme
improving.underweight.rates[, mean.severe.to.overall.ir := mean(severe.to.overall.improvement.ratio), by = c("location_id", "year_id")]
improving.underweight.rates[, lower.severe.to.overall.ir := quantile(severe.to.overall.improvement.ratio, probs = .025, na.rm = T), by = c("location_id", "year_id")]
improving.underweight.rates[, upper.severe.to.overall.ir := quantile(severe.to.overall.improvement.ratio, probs = .975, na.rm = T), by = c("location_id", "year_id")]

improving.underweight.rates[, mean.extreme.to.overall.ir := mean(extreme.to.overall.improvement.ratio), by = c("location_id", "year_id")]
improving.underweight.rates[, lower.extreme.to.overall.ir := quantile(extreme.to.overall.improvement.ratio, probs = .025, na.rm = T), by = c("location_id", "year_id")]
improving.underweight.rates[, upper.extreme.to.overall.ir := quantile(extreme.to.overall.improvement.ratio, probs = .975, na.rm = T), by = c("location_id", "year_id")]

#subsetting to only have 1 row per location
improving.underweight.rates[, loc.year := paste0(location_id, "_", year_id)]
improving.underweight.rates.collapsed <- improving.underweight.rates[!duplicated(improving.underweight.rates$loc.year)]

#merging on location names
loc.names <- loc.met[, c("location_id", "location_name")]
improving.underweight.rates.collapsed <- merge(loc.names, improving.underweight.rates.collapsed, by = "location_id")

#identifying locations where improvement ratios are significant, meaning even the lower for the improvement ratio is above 1
improving.underweight.rates.collapsed[lower.severe.to.overall.ir > 1, severe.signficant := 1]
improving.underweight.rates.collapsed[lower.extreme.to.overall.ir > 1, extreme.signficant := 1]
underweight.improving.faster.collapsed <- improving.underweight.rates.collapsed[severe.signficant == 1 & extreme.signficant == 1]

print(paste0("Globally since 1990, severe underweight has improved ", round(unique(underweight.improving.faster.collapsed[location_id == 1]$mean.severe.to.overall.ir), 2),
             " (", round(unique(underweight.improving.faster.collapsed[location_id == 1]$lower.severe.to.overall.ir), 2), " - ", 
             round(unique(underweight.improving.faster.collapsed[location_id == 1]$upper.severe.to.overall.ir), 2), ") times more than overall underweight"))

print(paste0("Globally since 1990, extreme underweight has improved ", round(unique(underweight.improving.faster.collapsed[location_id == 1]$mean.extreme.to.overall.ir), 2),
             " (", round(unique(underweight.improving.faster.collapsed[location_id == 1]$lower.extreme.to.overall.ir), 2), " - ", 
             round(unique(underweight.improving.faster.collapsed[location_id == 1]$upper.extreme.to.overall.ir), 2), ") times more than overall underweight"))

print(paste0("In Niger since 1990, severe underweight has improved ", round(unique(underweight.improving.faster.collapsed[location_id == 213]$mean.severe.to.overall.ir), 2),
             " (", round(unique(underweight.improving.faster.collapsed[location_id == 213]$lower.severe.to.overall.ir), 2), " - ", 
             round(unique(underweight.improving.faster.collapsed[location_id == 213]$upper.severe.to.overall.ir), 2), ") times more than overall underweight"))

print(paste0("In Niger since 1990, extreme underweight has improved ", round(unique(underweight.improving.faster.collapsed[location_id == 213]$mean.extreme.to.overall.ir), 2),
             " (", round(unique(underweight.improving.faster.collapsed[location_id == 213]$lower.extreme.to.overall.ir), 2), " - ", 
             round(unique(underweight.improving.faster.collapsed[location_id == 213]$upper.extreme.to.overall.ir), 2), ") times more than overall underweight"))


print(paste0("In Burundi since 1990, severe underweight has improved ", round(unique(underweight.improving.faster.collapsed[location_id == 175]$mean.severe.to.overall.ir), 2),
             " (", round(unique(underweight.improving.faster.collapsed[location_id == 175]$lower.severe.to.overall.ir), 2), " - ", 
             round(unique(underweight.improving.faster.collapsed[location_id == 175]$upper.severe.to.overall.ir), 2), ") times more than overall underweight"))

print(paste0("In Burundi since 1990, extreme underweight has improved ", round(unique(underweight.improving.faster.collapsed[location_id == 175]$mean.extreme.to.overall.ir), 2),
             " (", round(unique(underweight.improving.faster.collapsed[location_id == 175]$lower.extreme.to.overall.ir), 2), " - ", 
             round(unique(underweight.improving.faster.collapsed[location_id == 175]$upper.extreme.to.overall.ir), 2), ") times more than overall underweight"))




















# now combining stunting, wasting, and underweight plots

empty <- ggplot() + theme_void()

all.cgf.timetrends <- plot_grid(stunting.combined.plot, empty, wasting.combined.plot, empty, underweight.combined.plot, ncol = 5, nrow = 1, rel_widths = c(1, .05, 1, .05, 1))




# creating a plot I can use for the legend
temp.df <- data.table(cgf.type = c("Overall CGF (<-2SD)", "Severe CGF (<-3SD)", "Extreme CGF (<-4SD)"),
                      val = c(1, 2, 3))

temp.df$cgf.type <- factor(temp.df$cgf.type, levels = c("Overall CGF (<-2SD)", "Severe CGF (<-3SD)", "Extreme CGF (<-4SD)"))


legend.plot <- ggplot() +
  geom_histogram(data = temp.df, aes(x = val, fill = cgf.type), alpha = .8) +
  scale_fill_manual(values = c("#E69C12", "#E0631F", "#A11D0E"), 
                    guide=guide_legend(title="Severity", title.position = "top", title.hjust = .5, ncol = 3)) +
  theme(legend.title = element_text(size = 24),
        legend.text = element_text(size = 20),
        legend.position = "bottom",
        legend.justification = 'center',
        legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
        legend.key = element_rect(fill = "grey90"),
        legend.key.height = unit(1, "cm"))




severity.legend <- get_legend(legend.plot)


caption <- ggplot() +
  theme_void() +
  labs(caption = expression(paste(bold("  Figure 3: \n \n"), " Relative changes in stunting, wasting, and underweight prevalences since 1990 are shown in figures 3a, 3b, and 3c respectively. Bold lines reflect global trends in CGF \n prevalence, while thin lines represent trends for seven super regions. Density plots on the right display the range of percent change values estimated in 2020 for countries \n with extreme CGF prevalences greater than one per million and populations larger than 300 thousand."))) +
  theme(plot.caption = element_text(hjust = 0, size = 22))


all.cgf.timetrends <- plot_grid(all.cgf.timetrends, empty, severity.legend, caption, nrow = 4, ncol = 1, rel_heights = c(1, .02, .2, .18))





pdf("filepath", height = 10, width = 26)

print(all.cgf.timetrends)

dev.off()



















