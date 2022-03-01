# CGF Severities Paper Results



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



loc.met <- get_location_metadata(gbd_round_id = 7, decomp_step = 'iterative', location_set_id = 35)

figures.loc.met <- get_location_metadata(gbd_round_id = 7, decomp_step = 'iterative', location_set_id = 92)
figure.locs <- unique(figures.loc.met$location_id)









##################################################################################################################
##################################################################################################################
##################################################################################################################
############################# STUNTING --  HEALTH SYSTEMS DISRUPTION ANALYSIS ####################################
##################################################################################################################
##################################################################################################################
##################################################################################################################




overall.stunting <- get_draws("modelable_entity_id", 10556, year_id=c(1990:2019), 
                                           source="epi", gbd_round_id=7, decomp_step="iterative", 
                                           age_group_id = c(2, 3, 388, 389), sex_id = c(1, 2), location_id = figure.locs)

severe.stunting <- get_draws("modelable_entity_id", 8949, year_id=c(1990:2019), 
                              source="epi", gbd_round_id=7, decomp_step="iterative", 
                              age_group_id = c(2, 3, 388, 389), sex_id = c(1, 2), location_id = figure.locs)

extreme.stunting <- get_draws("modelable_entity_id", 26941, year_id=c(1990:2019), 
                             source="epi", gbd_round_id=7, decomp_step="iterative", 
                             age_group_id = c(2, 3, 388, 389), sex_id = c(1, 2), location_id = figure.locs)








overall.stunting <- melt(overall.stunting, id.vars = id.vars)
severe.stunting <- melt(severe.stunting, id.vars = id.vars)
extreme.stunting<- melt(extreme.stunting, id.vars = id.vars)


setnames(overall.stunting, "value", "overall.value")
setnames(severe.stunting, "value", "severe.value")
setnames(extreme.stunting, "value", "extreme.value")

overall.stunting$model_version_id <- NULL
overall.stunting$modelable_entity_id <- NULL
severe.stunting$model_version_id <- NULL
severe.stunting$modelable_entity_id <- NULL
extreme.stunting$model_version_id <- NULL
extreme.stunting$modelable_entity_id <- NULL



all.stunting <- merge(overall.stunting, severe.stunting, by = c("metric_id", "age_group_id", "location_id", "measure_id", "sex_id", "year_id", "variable"))
all.stunting <- merge(all.stunting, extreme.stunting, by = c("metric_id", "age_group_id", "location_id", "measure_id", "sex_id", "year_id", "variable"))


all.stunting <- all.stunting[order(year_id)]


#changing prevalence values of 0 to be very small to avoid infinite values
all.stunting[overall.value == 0, overall.value := .000000001]
all.stunting[severe.value == 0, severe.value := .000000001]
all.stunting[extreme.value == 0, extreme.value := .000000001]





all.stunting[, overall.pct.change := (overall.value-lag(overall.value))/lag(overall.value), by = c("age_group_id", "location_id", "sex_id", "variable")]
all.stunting[, severe.pct.change := (severe.value-lag(severe.value))/lag(severe.value), by = c("age_group_id", "location_id", "sex_id", "variable")]
all.stunting[, extreme.pct.change := (extreme.value-lag(extreme.value))/lag(extreme.value), by = c("age_group_id", "location_id", "sex_id", "variable")]



uhc <- get_covariate_estimates(1097, gbd_round_id = 7, decomp_step = "iterative")
uhc <- uhc[, c("location_id", "year_id", "mean_value")]
setnames(uhc, "mean_value", "uhc.val")

all.stunting <- merge(all.stunting, uhc, by = c("location_id", "year_id"))

country.locs <- figures.loc.met[level >2]
country.locs <- country.locs[location_id != 95] # we use the four countries within the UK, not the UK itself, that would be double counting

plot.locations <- unique(country.locs$location_id)

all.stunting <- all.stunting[location_id %in% plot.locations]

#anything with positive uhc.change had an increase in uhc value from year to year, whereas negative had uhc disruptions
all.stunting[, uhc.change := uhc.val - lag(uhc.val), by = c("age_group_id", "location_id", "sex_id", "variable")]


pops <- get_population(gbd_round_id = 7, decomp_step = 'iterative', location_id = 'all', location_set_id = 35, 
                       year_id = c(1990:2020), age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1,2))
pops$run_id <- NULL



all.stunting <- merge(all.stunting, pops, by = c("age_group_id", "location_id", "year_id", "sex_id"))
all.stunting <- all.stunting[year_id %in% c(1991:2019)]



all.stunting[,overall.pct.change.weighted := overall.pct.change * population]
all.stunting[,severe.pct.change.weighted := severe.pct.change * population]
all.stunting[,extreme.pct.change.weighted := extreme.pct.change * population]
all.stunting[, under.5.pop := sum(population), by = c("location_id", "year_id", "variable")]

all.stunting[, agg.overall.pct.change.sum := sum(overall.pct.change.weighted), by = c("location_id", "year_id", "variable")]
all.stunting[, agg.severe.pct.change.sum := sum(severe.pct.change.weighted), by = c("location_id", "year_id", "variable")]
all.stunting[, agg.extreme.pct.change.sum := sum(extreme.pct.change.weighted), by = c("location_id", "year_id", "variable")]

all.stunting[, agg.overall.pct.change.final := agg.overall.pct.change.sum / under.5.pop]
all.stunting[, agg.severe.pct.change.final := agg.severe.pct.change.sum / under.5.pop]
all.stunting[, agg.extreme.pct.change.final := agg.extreme.pct.change.sum / under.5.pop]



#also need to get moderate, severe, and extreme CGF prevalences so we can filter out ones with low prevalences from the plot
all.stunting[, overall.val.weighted := overall.value * population]
all.stunting[, severe.val.weighted := severe.value * population]
all.stunting[, extreme.val.weighted := extreme.value * population]

all.stunting[, agg.overall.val.sum := sum(overall.val.weighted), by = c("location_id", "year_id", "variable")]
all.stunting[, agg.severe.val.sum := sum(severe.val.weighted), by = c("location_id", "year_id", "variable")]
all.stunting[, agg.extreme.val.sum := sum(extreme.val.weighted), by = c("location_id", "year_id", "variable")]

all.stunting[, agg.overall.val.final := agg.overall.val.sum / under.5.pop]
all.stunting[, agg.severe.val.final := agg.severe.val.sum / under.5.pop]
all.stunting[, agg.extreme.val.final := agg.extreme.val.sum / under.5.pop]








#now that i've aggregated, we're taking one age-sex and we'll change the labels. 
stunting.changes <- all.stunting[age_group_id == 3 & sex_id == 1]
stunting.changes[, age_group_id := 1]
stunting.changes[, sex_id := 3]


stunting.changes[, median.agg.overall.pct.change := median(agg.overall.pct.change.final), by = c("location_id", "year_id")]
stunting.changes[, median.agg.severe.pct.change := median(agg.severe.pct.change.final), by = c("location_id", "year_id")]
stunting.changes[, median.agg.extreme.pct.change := median(agg.extreme.pct.change.final), by = c("location_id", "year_id")]
stunting.changes[, mean.overall.prev := mean(agg.overall.val.final), by = c("location_id", "year_id")]
stunting.changes[, mean.severe.prev := mean(agg.severe.val.final), by = c("location_id", "year_id")]
stunting.changes[, mean.extreme.prev := mean(agg.extreme.val.final), by = c("location_id", "year_id")]



#stopped


stunting.change.plot.df <- stunting.changes[variable == "draw_0"]


temp.stunt.overall <- stunting.change.plot.df[, c("age_group_id", "location_id", "year_id", "sex_id",
                                                  "uhc.val", "uhc.change", "median.agg.overall.pct.change",
                                                  "mean.overall.prev")]
temp.stunt.severe <- stunting.change.plot.df[, c("age_group_id", "location_id", "year_id", "sex_id",
                                                  "uhc.val", "uhc.change",  "median.agg.severe.pct.change",
                                                 "mean.severe.prev")]
temp.stunt.extreme <- stunting.change.plot.df[, c("age_group_id", "location_id", "year_id", "sex_id",
                                                 "uhc.val", "uhc.change",  "median.agg.extreme.pct.change",
                                                 "mean.extreme.prev")]

setnames(temp.stunt.overall, "median.agg.overall.pct.change", "median.pct.change")
setnames(temp.stunt.severe, "median.agg.severe.pct.change", "median.pct.change")
setnames(temp.stunt.extreme, "median.agg.extreme.pct.change", "median.pct.change")

setnames(temp.stunt.overall, "mean.overall.prev", "cgf.prevalence")
setnames(temp.stunt.severe, "mean.severe.prev", "cgf.prevalence")
setnames(temp.stunt.extreme, "mean.extreme.prev", "cgf.prevalence")


temp.stunt.overall[, severity := "Overall"]
temp.stunt.severe[, severity := "Severe"]
temp.stunt.extreme[, severity := "Extreme"]

stunting.change.plot.df <- rbind(temp.stunt.overall, temp.stunt.severe, temp.stunt.extreme)




#merging on total population so we can filter out countries with small populations
totalpop <- get_population(gbd_round_id = 7, decomp_step = 'iterative', location_id = 'all', location_set_id = 35, 
                              year_id = c(1990:2020), age_group_id = 22, sex_id = 3)
totalpop$run_id <- NULL
totalpop$age_group_id <- NULL

stunting.change.plot.df <- merge(stunting.change.plot.df, totalpop, by = c("location_id", "year_id", "sex_id"))

stunting.change.plot.df$severity <- factor(stunting.change.plot.df$severity, levels = c( "Overall", "Severe", "Extreme"))





pop.cutoff <- 300000
cgf.cutoff <- 1e-6

stunting.change.plot.df[, median.pct.change.plotvar := median.pct.change]
stunting.change.plot.df[median.pct.change.plotvar < -.1, median.pct.change.plotvar := -.1]
stunting.change.plot.df[median.pct.change.plotvar > .1, median.pct.change.plotvar := .1]


loc.names <- loc.met[, c("location_id", "location_name")]
stunting.change.plot.df <- merge(loc.names, stunting.change.plot.df, by = "location_id")

stunting.change.plot.df[, cgf.type := "Stunting (HAZ)"]



stunting.change.plot <- ggplot() +
  geom_density_ridges(data = stunting.change.plot.df[uhc.change < 0 & cgf.prevalence > cgf.cutoff & population > pop.cutoff],
                      aes(x = median.pct.change.plotvar, y = severity), fill = "firebrick",  alpha = .6, scale = .95) +
  geom_density_ridges(data = stunting.change.plot.df[uhc.change > 0 & cgf.prevalence > cgf.cutoff & population > pop.cutoff],
                      aes(x = median.pct.change.plotvar, y = severity), fill = "dodgerblue3",  alpha = .6, scale = .95)  +
  labs(x = "Relative Percent Change", y = "Severity", tag = "A") +
  theme_bw() +
  scale_x_continuous(limits = c(-.1, .1), expand = c(0,0), breaks = c(-.1, -.05, 0, .05, .1), labels = c("<-10%", "-5%", "0%", "5%" ,">10%")) +
  scale_y_discrete(expand = expand_scale(add = c(0, 1)), breaks = c("Overall", "Severe", "Extreme")) +
  theme(panel.grid.major.y = element_line(color = "black", size = 1),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 22),
        strip.text = element_text(size = 22),
        axis.text.x = element_text(vjust = -.5),
        axis.text.y = element_text(vjust = .05),
        plot.tag = element_text(size = 22, face = "bold")) +
  facet_wrap(~cgf.type) +
  theme(plot.margin=unit(c(1, 1, 1, 1),"cm"))






ks.test(stunting.change.plot.df[cgf.prevalence > cgf.cutoff & population > pop.cutoff & uhc.change <0 & severity == "Extreme"]$median.pct.change,
        stunting.change.plot.df[cgf.prevalence > cgf.cutoff & population > pop.cutoff & uhc.change >0 & severity == "Extreme"]$median.pct.change)


ks.test(stunting.change.plot.df[cgf.prevalence > cgf.cutoff & population > pop.cutoff & uhc.change <0 & severity == "Severe"]$median.pct.change,
       stunting.change.plot.df[cgf.prevalence > cgf.cutoff & population > pop.cutoff & uhc.change >0 & severity == "Severe"]$median.pct.change)


ks.test(stunting.change.plot.df[cgf.prevalence > cgf.cutoff & population > pop.cutoff & uhc.change <0 & severity == "Overall"]$median.pct.change,
       stunting.change.plot.df[cgf.prevalence > cgf.cutoff & population > pop.cutoff & uhc.change >0 & severity == "Overall"]$median.pct.change)



##################################################################################################################
##################################################################################################################
##################################################################################################################
############################# WASTING --  HEALTH SYSTEMS DISRUPTION ANALYSIS ####################################
##################################################################################################################
##################################################################################################################
##################################################################################################################


# WASTING

overall.wasting <- get_draws("modelable_entity_id", 10558, year_id=c(1990:2019), 
                              source="epi", gbd_round_id=7, decomp_step="iterative", 
                              age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = figure.locs)

severe.wasting <- get_draws("modelable_entity_id", 8945, year_id=c(1990:2019), 
                             source="epi", gbd_round_id=7, decomp_step="iterative", 
                             age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = figure.locs)

extreme.wasting <- get_draws("modelable_entity_id", 26943, year_id=c(1990:2019), 
                              source="epi", gbd_round_id=7, decomp_step="iterative", 
                              age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = figure.locs)




overall.wasting <- melt(overall.wasting, id.vars = id.vars)
severe.wasting <- melt(severe.wasting, id.vars = id.vars)
extreme.wasting<- melt(extreme.wasting, id.vars = id.vars)


setnames(overall.wasting, "value", "overall.value")
setnames(severe.wasting, "value", "severe.value")
setnames(extreme.wasting, "value", "extreme.value")

overall.wasting$model_version_id <- NULL
overall.wasting$modelable_entity_id <- NULL
severe.wasting$model_version_id <- NULL
severe.wasting$modelable_entity_id <- NULL
extreme.wasting$model_version_id <- NULL
extreme.wasting$modelable_entity_id <- NULL



all.wasting <- merge(overall.wasting, severe.wasting, by = c("metric_id", "age_group_id", "location_id", "measure_id", "sex_id", "year_id", "variable"))
all.wasting <- merge(all.wasting, extreme.wasting, by = c("metric_id", "age_group_id", "location_id", "measure_id", "sex_id", "year_id", "variable"))


all.wasting <- all.wasting[order(year_id)]


#changing prevalence values of 0 to be very small to avoid infinite values
all.wasting[overall.value == 0, overall.value := .000000001]
all.wasting[severe.value == 0, severe.value := .000000001]
all.wasting[extreme.value == 0, extreme.value := .000000001]


all.wasting[, overall.pct.change := (overall.value-lag(overall.value))/lag(overall.value), by = c("age_group_id", "location_id", "sex_id", "variable")]
all.wasting[, severe.pct.change := (severe.value-lag(severe.value))/lag(severe.value), by = c("age_group_id", "location_id", "sex_id", "variable")]
all.wasting[, extreme.pct.change := (extreme.value-lag(extreme.value))/lag(extreme.value), by = c("age_group_id", "location_id", "sex_id", "variable")]




uhc <- get_covariate_estimates(1097, gbd_round_id = 7, decomp_step = "iterative")
uhc <- uhc[, c("location_id", "year_id", "mean_value")]
setnames(uhc, "mean_value", "uhc.val")



all.wasting <- merge(all.wasting, uhc, by = c("location_id", "year_id"))

country.locs <- figures.loc.met[level >2]
country.locs <- country.locs[location_id != 95] # we use the four countries within the UK, not the UK itself, that would be double counting

plot.locations <- unique(country.locs$location_id)

all.wasting <- all.wasting[location_id %in% plot.locations]



#anything with positive uhc.change had an increase in uhc value from year to year, whereas negative had uhc disruptions
all.wasting[, uhc.change := uhc.val - lag(uhc.val), by = c("age_group_id", "location_id", "sex_id", "variable")]


pops <- get_population(gbd_round_id = 7, decomp_step = 'iterative', location_id = 'all', location_set_id = 35, 
                       year_id = c(1990:2020), age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1,2))
pops$run_id <- NULL



all.wasting <- merge(all.wasting, pops, by = c("age_group_id", "location_id", "year_id", "sex_id"))
all.wasting <- all.wasting[year_id %in% c(1991:2019)]


all.wasting[,overall.pct.change.weighted := overall.pct.change * population]
all.wasting[,severe.pct.change.weighted := severe.pct.change * population]
all.wasting[,extreme.pct.change.weighted := extreme.pct.change * population]
all.wasting[, under.5.pop := sum(population), by = c("location_id", "year_id", "variable")]

all.wasting[, agg.overall.pct.change.sum := sum(overall.pct.change.weighted), by = c("location_id", "year_id", "variable")]
all.wasting[, agg.severe.pct.change.sum := sum(severe.pct.change.weighted), by = c("location_id", "year_id", "variable")]
all.wasting[, agg.extreme.pct.change.sum := sum(extreme.pct.change.weighted), by = c("location_id", "year_id", "variable")]

all.wasting[, agg.overall.pct.change.final := agg.overall.pct.change.sum / under.5.pop]
all.wasting[, agg.severe.pct.change.final := agg.severe.pct.change.sum / under.5.pop]
all.wasting[, agg.extreme.pct.change.final := agg.extreme.pct.change.sum / under.5.pop]



#also need to get moderate, severe, and extreme CGF prevalences so we can filter out ones with low prevalences from the plot
all.wasting[, overall.val.weighted := overall.value * population]
all.wasting[, severe.val.weighted := severe.value * population]
all.wasting[, extreme.val.weighted := extreme.value * population]

all.wasting[, agg.overall.val.sum := sum(overall.val.weighted), by = c("location_id", "year_id", "variable")]
all.wasting[, agg.severe.val.sum := sum(severe.val.weighted), by = c("location_id", "year_id", "variable")]
all.wasting[, agg.extreme.val.sum := sum(extreme.val.weighted), by = c("location_id", "year_id", "variable")]

all.wasting[, agg.overall.val.final := agg.overall.val.sum / under.5.pop]
all.wasting[, agg.severe.val.final := agg.severe.val.sum / under.5.pop]
all.wasting[, agg.extreme.val.final := agg.extreme.val.sum / under.5.pop]






#now that i've aggregated, we're taking one age-sex and we'll change the labels. 
wasting.changes <- all.wasting[age_group_id == 3 & sex_id == 1]
wasting.changes[, age_group_id := 1]
wasting.changes[, sex_id := 3]


wasting.changes[, median.agg.overall.pct.change := median(agg.overall.pct.change.final), by = c("location_id", "year_id")]
wasting.changes[, median.agg.severe.pct.change := median(agg.severe.pct.change.final), by = c("location_id", "year_id")]
wasting.changes[, median.agg.extreme.pct.change := median(agg.extreme.pct.change.final), by = c("location_id", "year_id")]
wasting.changes[, mean.overall.prev := mean(agg.overall.val.final), by = c("location_id", "year_id")]
wasting.changes[, mean.severe.prev := mean(agg.severe.val.final), by = c("location_id", "year_id")]
wasting.changes[, mean.extreme.prev := mean(agg.extreme.val.final), by = c("location_id", "year_id")]






wasting.change.plot.df <- wasting.changes[variable == "draw_0"]


temp.waste.overall <- wasting.change.plot.df[, c("age_group_id", "location_id", "year_id", "sex_id",
                                                  "uhc.val", "uhc.change", "median.agg.overall.pct.change",
                                                  "mean.overall.prev")]
temp.waste.severe <- wasting.change.plot.df[, c("age_group_id", "location_id", "year_id", "sex_id",
                                                 "uhc.val", "uhc.change",  "median.agg.severe.pct.change",
                                                 "mean.severe.prev")]
temp.waste.extreme <- wasting.change.plot.df[, c("age_group_id", "location_id", "year_id", "sex_id",
                                                  "uhc.val", "uhc.change",  "median.agg.extreme.pct.change",
                                                  "mean.extreme.prev")]

setnames(temp.waste.overall, "median.agg.overall.pct.change", "median.pct.change")
setnames(temp.waste.severe, "median.agg.severe.pct.change", "median.pct.change")
setnames(temp.waste.extreme, "median.agg.extreme.pct.change", "median.pct.change")

setnames(temp.waste.overall, "mean.overall.prev", "cgf.prevalence")
setnames(temp.waste.severe, "mean.severe.prev", "cgf.prevalence")
setnames(temp.waste.extreme, "mean.extreme.prev", "cgf.prevalence")


temp.waste.overall[, severity := "Overall"]
temp.waste.severe[, severity := "Severe"]
temp.waste.extreme[, severity := "Extreme"]

wasting.change.plot.df <- rbind(temp.waste.overall, temp.waste.severe, temp.waste.extreme)




#merging on total population so we can filter out countries with small populations
totalpop <- get_population(gbd_round_id = 7, decomp_step = 'iterative', location_id = 'all', location_set_id = 35, 
                           year_id = c(1990:2020), age_group_id = 22, sex_id = 3)
totalpop$run_id <- NULL
totalpop$age_group_id <- NULL

wasting.change.plot.df <- merge(wasting.change.plot.df, totalpop, by = c("location_id", "year_id", "sex_id"))

wasting.change.plot.df$severity <- factor(wasting.change.plot.df$severity, levels = c( "Overall", "Severe", "Extreme"))


pop.cutoff <- 300000
cgf.cutoff <- 1e-6

wasting.change.plot.df[, median.pct.change.plotvar := median.pct.change]
wasting.change.plot.df[median.pct.change.plotvar < -.1, median.pct.change.plotvar := -.1]
wasting.change.plot.df[median.pct.change.plotvar > .1, median.pct.change.plotvar := .1]




wasting.change.plot.df[, cgf.type := "Wasting (WHZ)"]

wasting.change.plot <- ggplot() +
  geom_density_ridges(data = wasting.change.plot.df[uhc.change < 0 & cgf.prevalence > cgf.cutoff & population > pop.cutoff],
                      aes(x = median.pct.change.plotvar, y = severity), fill = "firebrick",  alpha = .6, scale = .95) +
  geom_density_ridges(data = wasting.change.plot.df[uhc.change > 0 & cgf.prevalence > cgf.cutoff & population > pop.cutoff],
                      aes(x = median.pct.change.plotvar, y = severity), fill = "dodgerblue3",  alpha = .6, scale = .95)  +
  labs(x = "Relative Percent Change", y = "Severity", tag = "B") +
  theme_bw() +
  scale_x_continuous(limits = c(-.1, .1), expand = c(0,0), breaks = c(-.1, -.05, 0, .05, .1), labels = c("<-10%", "-5%", "0%", "5%" ,">10%")) +
  scale_y_discrete(expand = expand_scale(add = c(0, 1)), breaks = c("Overall", "Severe", "Extreme")) +
  theme(panel.grid.major.y = element_line(color = "black", size = 1),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 22),
        strip.text = element_text(size = 22),
        axis.text.x = element_text(vjust = -.5),
        axis.text.y = element_text(vjust = .05),
        plot.tag = element_text(size = 22, face = "bold")) +
  facet_wrap(~cgf.type) +
  theme(plot.margin=unit(c(1, 1, 1, 1),"cm"))




ks.test(wasting.change.plot.df[cgf.prevalence > cgf.cutoff & population > pop.cutoff & uhc.change <0 & severity == "Extreme"]$median.pct.change,
        wasting.change.plot.df[cgf.prevalence > cgf.cutoff & population > pop.cutoff & uhc.change >0 & severity == "Extreme"]$median.pct.change)


ks.test(wasting.change.plot.df[cgf.prevalence > cgf.cutoff & population > pop.cutoff & uhc.change <0 & severity == "Severe"]$median.pct.change,
        wasting.change.plot.df[cgf.prevalence > cgf.cutoff & population > pop.cutoff & uhc.change >0 & severity == "Severe"]$median.pct.change)


ks.test(wasting.change.plot.df[cgf.prevalence > cgf.cutoff & population > pop.cutoff & uhc.change <0 & severity == "Overall"]$median.pct.change,
        wasting.change.plot.df[cgf.prevalence > cgf.cutoff & population > pop.cutoff & uhc.change >0 & severity == "Overall"]$median.pct.change)







##################################################################################################################
##################################################################################################################
##################################################################################################################
############################# UNDERWEIGHT --  HEALTH SYSTEMS DISRUPTION ANALYSIS #################################
##################################################################################################################
##################################################################################################################
##################################################################################################################


# UNDERWEIGHT

overall.underweight <- get_draws("modelable_entity_id", 10560, year_id=c(1990:2019), 
                             source="epi", gbd_round_id=7, decomp_step="iterative", 
                             age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = figure.locs)

severe.underweight <- get_draws("modelable_entity_id", 2540, year_id=c(1990:2019), 
                            source="epi", gbd_round_id=7, decomp_step="iterative", 
                            age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = figure.locs)

extreme.underweight <- get_draws("modelable_entity_id", 26942, year_id=c(1990:2019), 
                             source="epi", gbd_round_id=7, decomp_step="iterative", 
                             age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1, 2), location_id = figure.locs)




overall.underweight <- melt(overall.underweight, id.vars = id.vars)
severe.underweight <- melt(severe.underweight, id.vars = id.vars)
extreme.underweight<- melt(extreme.underweight, id.vars = id.vars)


setnames(overall.underweight, "value", "overall.value")
setnames(severe.underweight, "value", "severe.value")
setnames(extreme.underweight, "value", "extreme.value")

overall.underweight$model_version_id <- NULL
overall.underweight$modelable_entity_id <- NULL
severe.underweight$model_version_id <- NULL
severe.underweight$modelable_entity_id <- NULL
extreme.underweight$model_version_id <- NULL
extreme.underweight$modelable_entity_id <- NULL



all.underweight <- merge(overall.underweight, severe.underweight, by = c("metric_id", "age_group_id", "location_id", "measure_id", "sex_id", "year_id", "variable"))
all.underweight <- merge(all.underweight, extreme.underweight, by = c("metric_id", "age_group_id", "location_id", "measure_id", "sex_id", "year_id", "variable"))


all.underweight <- all.underweight[order(year_id)]


#changing prevalence values of 0 to be very small to avoid infinite values
all.underweight[overall.value == 0, overall.value := .000000001]
all.underweight[severe.value == 0, severe.value := .000000001]
all.underweight[extreme.value == 0, extreme.value := .000000001]


all.underweight[, overall.pct.change := (overall.value-lag(overall.value))/lag(overall.value), by = c("age_group_id", "location_id", "sex_id", "variable")]
all.underweight[, severe.pct.change := (severe.value-lag(severe.value))/lag(severe.value), by = c("age_group_id", "location_id", "sex_id", "variable")]
all.underweight[, extreme.pct.change := (extreme.value-lag(extreme.value))/lag(extreme.value), by = c("age_group_id", "location_id", "sex_id", "variable")]



uhc <- get_covariate_estimates(1097, gbd_round_id = 7, decomp_step = "iterative")
uhc <- uhc[, c("location_id", "year_id", "mean_value")]
setnames(uhc, "mean_value", "uhc.val")

all.underweight <- merge(all.underweight, uhc, by = c("location_id", "year_id"))

country.locs <- figures.loc.met[level >2]
country.locs <- country.locs[location_id != 95] # we use the four countries within the UK, not the UK itself, that would be double counting

plot.locations <- unique(country.locs$location_id)

all.underweight <- all.underweight[location_id %in% plot.locations]

#anything with positive uhc.change had an increase in uhc value from year to year, whereas negative had uhc disruptions
all.underweight[, uhc.change := uhc.val - lag(uhc.val), by = c("age_group_id", "location_id", "sex_id", "variable")]


pops <- get_population(gbd_round_id = 7, decomp_step = 'iterative', location_id = 'all', location_set_id = 35, 
                       year_id = c(1990:2020), age_group_id = c(2, 3, 388, 389, 238, 34), sex_id = c(1,2))
pops$run_id <- NULL



all.underweight <- merge(all.underweight, pops, by = c("age_group_id", "location_id", "year_id", "sex_id"))
all.underweight <- all.underweight[year_id %in% c(1991:2019)]


all.underweight[,overall.pct.change.weighted := overall.pct.change * population]
all.underweight[,severe.pct.change.weighted := severe.pct.change * population]
all.underweight[,extreme.pct.change.weighted := extreme.pct.change * population]
all.underweight[, under.5.pop := sum(population), by = c("location_id", "year_id", "variable")]

all.underweight[, agg.overall.pct.change.sum := sum(overall.pct.change.weighted), by = c("location_id", "year_id", "variable")]
all.underweight[, agg.severe.pct.change.sum := sum(severe.pct.change.weighted), by = c("location_id", "year_id", "variable")]
all.underweight[, agg.extreme.pct.change.sum := sum(extreme.pct.change.weighted), by = c("location_id", "year_id", "variable")]

all.underweight[, agg.overall.pct.change.final := agg.overall.pct.change.sum / under.5.pop]
all.underweight[, agg.severe.pct.change.final := agg.severe.pct.change.sum / under.5.pop]
all.underweight[, agg.extreme.pct.change.final := agg.extreme.pct.change.sum / under.5.pop]



#also need to get moderate, severe, and extreme CGF prevalences so we can filter out ones with low prevalences from the plot
all.underweight[, overall.val.weighted := overall.value * population]
all.underweight[, severe.val.weighted := severe.value * population]
all.underweight[, extreme.val.weighted := extreme.value * population]

all.underweight[, agg.overall.val.sum := sum(overall.val.weighted), by = c("location_id", "year_id", "variable")]
all.underweight[, agg.severe.val.sum := sum(severe.val.weighted), by = c("location_id", "year_id", "variable")]
all.underweight[, agg.extreme.val.sum := sum(extreme.val.weighted), by = c("location_id", "year_id", "variable")]

all.underweight[, agg.overall.val.final := agg.overall.val.sum / under.5.pop]
all.underweight[, agg.severe.val.final := agg.severe.val.sum / under.5.pop]
all.underweight[, agg.extreme.val.final := agg.extreme.val.sum / under.5.pop]






#now that i've aggregated, we're taking one age-sex and we'll change the labels. 
underweight.changes <- all.underweight[age_group_id == 3 & sex_id == 1]
underweight.changes[, age_group_id := 1]
underweight.changes[, sex_id := 3]


underweight.changes[, median.agg.overall.pct.change := median(agg.overall.pct.change.final), by = c("location_id", "year_id")]
underweight.changes[, median.agg.severe.pct.change := median(agg.severe.pct.change.final), by = c("location_id", "year_id")]
underweight.changes[, median.agg.extreme.pct.change := median(agg.extreme.pct.change.final), by = c("location_id", "year_id")]
underweight.changes[, mean.overall.prev := mean(agg.overall.val.final), by = c("location_id", "year_id")]
underweight.changes[, mean.severe.prev := mean(agg.severe.val.final), by = c("location_id", "year_id")]
underweight.changes[, mean.extreme.prev := mean(agg.extreme.val.final), by = c("location_id", "year_id")]






underweight.change.plot.df <- underweight.changes[variable == "draw_0"]


temp.underweight.overall <- underweight.change.plot.df[, c("age_group_id", "location_id", "year_id", "sex_id",
                                                 "uhc.val", "uhc.change", "median.agg.overall.pct.change",
                                                 "mean.overall.prev")]
temp.underweight.severe <- underweight.change.plot.df[, c("age_group_id", "location_id", "year_id", "sex_id",
                                                "uhc.val", "uhc.change",  "median.agg.severe.pct.change",
                                                "mean.severe.prev")]
temp.underweight.extreme <- underweight.change.plot.df[, c("age_group_id", "location_id", "year_id", "sex_id",
                                                 "uhc.val", "uhc.change",  "median.agg.extreme.pct.change",
                                                 "mean.extreme.prev")]

setnames(temp.underweight.overall, "median.agg.overall.pct.change", "median.pct.change")
setnames(temp.underweight.severe, "median.agg.severe.pct.change", "median.pct.change")
setnames(temp.underweight.extreme, "median.agg.extreme.pct.change", "median.pct.change")

setnames(temp.underweight.overall, "mean.overall.prev", "cgf.prevalence")
setnames(temp.underweight.severe, "mean.severe.prev", "cgf.prevalence")
setnames(temp.underweight.extreme, "mean.extreme.prev", "cgf.prevalence")


temp.underweight.overall[, severity := "Overall"]
temp.underweight.severe[, severity := "Severe"]
temp.underweight.extreme[, severity := "Extreme"]

underweight.change.plot.df <- rbind(temp.underweight.overall, temp.underweight.severe, temp.underweight.extreme)




#merging on total population so we can filter out countries with small populations
totalpop <- get_population(gbd_round_id = 7, decomp_step = 'iterative', location_id = 'all', location_set_id = 35, 
                           year_id = c(1990:2020), age_group_id = 22, sex_id = 3)
totalpop$run_id <- NULL
totalpop$age_group_id <- NULL

underweight.change.plot.df <- merge(underweight.change.plot.df, totalpop, by = c("location_id", "year_id", "sex_id"))

underweight.change.plot.df$severity <- factor(underweight.change.plot.df$severity, levels = c( "Overall", "Severe", "Extreme"))


pop.cutoff <- 300000
cgf.cutoff <- 1e-6

underweight.change.plot.df[, median.pct.change.plotvar := median.pct.change]
underweight.change.plot.df[median.pct.change.plotvar < -.1, median.pct.change.plotvar := -.1]
underweight.change.plot.df[median.pct.change.plotvar > .1, median.pct.change.plotvar := .1]

underweight.change.plot.df[, cgf.type := "Underweight (WAZ)"]




underweight.change.plot <- ggplot() +
  geom_density_ridges(data = underweight.change.plot.df[uhc.change < 0 & cgf.prevalence > cgf.cutoff & population > pop.cutoff],
                      aes(x = median.pct.change.plotvar, y = severity), fill = "firebrick",  alpha = .6, scale = .95) +
  geom_density_ridges(data = underweight.change.plot.df[uhc.change > 0 & cgf.prevalence > cgf.cutoff & population > pop.cutoff],
                      aes(x = median.pct.change.plotvar, y = severity), fill = "dodgerblue3",  alpha = .6, scale = .95)  +
  labs(x = "Relative Percent Change", y = "Severity", tag = "C") +
  theme_bw() +
  scale_x_continuous(limits = c(-.1, .1), expand = c(0,0), breaks = c(-.1, -.05, 0, .05, .1), labels = c("<-10%", "-5%", "0%", "5%" ,">10%")) +
  scale_y_discrete(expand = expand_scale(add = c(0, 1)), breaks = c("Overall", "Severe", "Extreme")) +
  theme(panel.grid.major.y = element_line(color = "black", size = 1),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 22),
        strip.text = element_text(size = 22),
        axis.text.x = element_text(vjust = -.5),
        axis.text.y = element_text(vjust = .05),
        plot.tag = element_text(size = 22, face = "bold")) +
  facet_wrap(~cgf.type) +
  theme(plot.margin=unit(c(1, 1, 1, 1),"cm"))






ks.test(underweight.change.plot.df[cgf.prevalence > cgf.cutoff & population > pop.cutoff & uhc.change <0 & severity == "Extreme"]$median.pct.change,
        underweight.change.plot.df[cgf.prevalence > cgf.cutoff & population > pop.cutoff & uhc.change >0 & severity == "Extreme"]$median.pct.change)


ks.test(underweight.change.plot.df[cgf.prevalence > cgf.cutoff & population > pop.cutoff & uhc.change <0 & severity == "Severe"]$median.pct.change,
        underweight.change.plot.df[cgf.prevalence > cgf.cutoff & population > pop.cutoff & uhc.change >0 & severity == "Severe"]$median.pct.change)


ks.test(underweight.change.plot.df[cgf.prevalence > cgf.cutoff & population > pop.cutoff & uhc.change <0 & severity == "Overall"]$median.pct.change,
        underweight.change.plot.df[cgf.prevalence > cgf.cutoff & population > pop.cutoff & uhc.change >0 & severity == "Overall"]$median.pct.change)














empty <- ggplot() + theme_void()

temp.df <- data.table(cgf.type = c("Improving UHC Index", "Worsening UHC Index"),
                      val = c(1, 2))

legend.plot <- ggplot() +
  geom_histogram(data = temp.df, aes(x = val, fill = cgf.type), alpha = .6) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick"), 
                    guide=guide_legend(title="Annual UHC Index Change", title.position = "top", title.hjust = .5, ncol = 3)) +
  theme(legend.title = element_text(size = 24),
        legend.text = element_text(size = 20),
        legend.position = "bottom",
        legend.justification = 'center',
        legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
        legend.key = element_rect(fill = "grey90"),
        legend.key.height = unit(1, "cm"))
  

plot.legend <- get_legend(legend.plot)



caption.plot <- ggplot() +
  theme_void() +
  labs(caption = expression(paste(bold("Figure 6: \n"), " Annual relative changes for each severity of stunting, wasting, and underweight are shown respectively in figure 6a, 6b, and 6c, categorized by whether UHC Index \n improved or worsened in that year. Kolmogorov-Smirnov statistics for each pair of curves are shown, all significant (p < 0.0001 in each case)."))) +
  theme(plot.caption = element_text(hjust = 0, size = 22))


layout <- rbind(c(1, 2, 3),
                 c(4, 4, 4),
                 c(5, 5, 5))







figure6 <- plot_grid(arrangeGrob(stunting.change.plot, wasting.change.plot, underweight.change.plot, plot.legend, caption.plot,
                                 nrow=3, ncol = 3, widths = c(1, 1, 1), heights = c(1, .2, .2), layout_matrix = layout))







pdf("filepath", height = 8, width = 24)



print(figure6)

dev.off()






















# wasting in Djibouti example


overall.wasting.dji <- get_draws("modelable_entity_id", 10558, year_id=c(1993,2001), 
                                  source="epi", gbd_round_id=7, decomp_step="iterative", 
                                  age_group_id = c(1), sex_id = c(3), location_id = 177)

severe.wasting.dji <- get_draws("modelable_entity_id", 8945, year_id=c(1993,2001), 
                                 source="epi", gbd_round_id=7, decomp_step="iterative", 
                                 age_group_id = c(1), sex_id = c(3), location_id = 177)

extreme.wasting.dji <- get_draws("modelable_entity_id", 26943, year_id=c(1993,2001), 
                                  source="epi", gbd_round_id=7, decomp_step="iterative", 
                                  age_group_id = c(1), sex_id = c(3), location_id = 177)

overall.wasting.dji <- melt(overall.wasting.dji, id.vars = id.vars)
severe.wasting.dji <- melt(severe.wasting.dji, id.vars = id.vars)
extreme.wasting.dji<- melt(extreme.wasting.dji, id.vars = id.vars)


overall.wasting.dji[, overall.pct.change := (value-lag(value))/lag(value), by = c("age_group_id", "location_id", "sex_id", "variable")]
severe.wasting.dji[, severe.pct.change := (value-lag(value))/lag(value), by = c("age_group_id", "location_id", "sex_id", "variable")]
extreme.wasting.dji[, extreme.pct.change := (value-lag(value))/lag(value), by = c("age_group_id", "location_id", "sex_id", "variable")]

overall.wasting.dji.median.pc <- median(overall.wasting.dji[!is.na(overall.pct.change)]$overall.pct.change) * 100
overall.wasting.dji.lower.pc <- unname(quantile(overall.wasting.dji[!is.na(overall.pct.change)]$overall.pct.change, .025)) * 100
overall.wasting.dji.upper.pc <- unname(quantile(overall.wasting.dji[!is.na(overall.pct.change)]$overall.pct.change, .975)) * 100

severe.wasting.dji.median.pc <- median(severe.wasting.dji[!is.na(severe.pct.change)]$severe.pct.change) * 100
severe.wasting.dji.lower.pc <- unname(quantile(severe.wasting.dji[!is.na(severe.pct.change)]$severe.pct.change, .025)) * 100
severe.wasting.dji.upper.pc <- unname(quantile(severe.wasting.dji[!is.na(severe.pct.change)]$severe.pct.change, .975)) * 100

extreme.wasting.dji.median.pc <- median(extreme.wasting.dji[!is.na(extreme.pct.change)]$extreme.pct.change) * 100
extreme.wasting.dji.lower.pc <- unname(quantile(extreme.wasting.dji[!is.na(extreme.pct.change)]$extreme.pct.change, .025)) * 100
extreme.wasting.dji.upper.pc <- unname(quantile(extreme.wasting.dji[!is.na(extreme.pct.change)]$extreme.pct.change, .975)) * 100



overall.wasting.dji[, mean.val := mean(value), by = "year_id"]
overall.wasting.dji[, lower.val := quantile(value, .025), by = "year_id"]
overall.wasting.dji[, upper.val := quantile(value, .975), by = "year_id"]

print(paste0("From 1993 to 2001, overall wasting increased ", round(overall.wasting.dji.median.pc, 1), "% (",
             round(overall.wasting.dji.lower.pc, 2), "% - ", round(overall.wasting.dji.upper.pc, 2), "%) from a prevalence of ",
             round(unique(overall.wasting.dji[year_id == 1993]$mean.val*100),1), "% (",
             round(unique(overall.wasting.dji[year_id == 1993]$lower.val*100),1), "% - ",
             round(unique(overall.wasting.dji[year_id == 1993]$upper.val*100),1), "%) to a prevalence of ",
             round(unique(overall.wasting.dji[year_id == 2001]$mean.val*100),1), "% (",
             round(unique(overall.wasting.dji[year_id == 2001]$lower.val*100),1), "% - ",
             round(unique(overall.wasting.dji[year_id == 2001]$upper.val*100),1), "%)."))

severe.wasting.dji[, mean.val := mean(value), by = "year_id"]
severe.wasting.dji[, lower.val := quantile(value, .025), by = "year_id"]
severe.wasting.dji[, upper.val := quantile(value, .975), by = "year_id"]

print(paste0("From 1993 to 2001, severe wasting increased ", round(severe.wasting.dji.median.pc, 1), "% (",
             round(severe.wasting.dji.lower.pc, 2), "% - ", round(severe.wasting.dji.upper.pc, 1), "%) from a prevalence of ",
             round(unique(severe.wasting.dji[year_id == 1993]$mean.val*100),2), "% (",
             round(unique(severe.wasting.dji[year_id == 1993]$lower.val*100),2), "% - ",
             round(unique(severe.wasting.dji[year_id == 1993]$upper.val*100),1), "%) to a prevalence of ",
             round(unique(severe.wasting.dji[year_id == 2001]$mean.val*100),1), "% (",
             round(unique(severe.wasting.dji[year_id == 2001]$lower.val*100),1), "% - ",
             round(unique(severe.wasting.dji[year_id == 2001]$upper.val*100),1), "%)."))

extreme.wasting.dji[, mean.val := mean(value), by = "year_id"]
extreme.wasting.dji[, lower.val := quantile(value, .025), by = "year_id"]
extreme.wasting.dji[, upper.val := quantile(value, .975), by = "year_id"]

print(paste0("From 1993 to 2001, extreme wasting increased ", round(extreme.wasting.dji.median.pc, 1), "% (",
             round(extreme.wasting.dji.lower.pc, 1), "% - ", round(extreme.wasting.dji.upper.pc, 1), "%) from a prevalence of ",
             round(unique(extreme.wasting.dji[year_id == 1993]$mean.val*100),2), "% (",
             round(unique(extreme.wasting.dji[year_id == 1993]$lower.val*100),2), "% - ",
             round(unique(extreme.wasting.dji[year_id == 1993]$upper.val*100),2), "%) to a prevalence of ",
             round(unique(extreme.wasting.dji[year_id == 2001]$mean.val*100),2), "% (",
             round(unique(extreme.wasting.dji[year_id == 2001]$lower.val*100),2), "% - ",
             round(unique(extreme.wasting.dji[year_id == 2001]$upper.val*100),2), "%)."))



































