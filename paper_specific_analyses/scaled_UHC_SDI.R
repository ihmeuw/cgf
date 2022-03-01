#CGF UHC Analysis with SDI covariate scaled plots

library(data.table)
library(plyr)
library(dplyr)
library(zoo)
library(ggplot2)
library(cowplot)
library(gridExtra)
invisible(sapply(list.files("filepath", full.names = T), source))

##### STUNTING

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


age_weights <- get_age_weights(gbd_round_id = 7)
hierarchy <- get_location_metadata(location_set_id = 35, release_id = 9)


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




####### sdi of 0.2


# first need to create scaled versions of the three splines and smooth them

overall.haz.spline.2 <- haz.epitrans[sdi == .2 & level > 2 & severity == "Overall", c("uhc", "expected")]
overall.haz.spline.2 <- overall.haz.spline.2[order(uhc),]

severe.haz.spline.2 <- haz.epitrans[sdi == .2 & level > 2 & severity == "Severe", c("uhc", "expected")]
severe.haz.spline.2 <- severe.haz.spline.2[order(uhc),]

extreme.haz.spline.2 <- haz.epitrans[sdi == .2 & level > 2 & severity == "Extreme", c("uhc", "expected")]
extreme.haz.spline.2 <- extreme.haz.spline.2[order(uhc),]

#getting 500 UHC values along this span to smooth out
min.uhc <- min(overall.haz.spline.2$uhc)
max.uhc <- max(overall.haz.spline.2$uhc)
uhc.vals <- seq(from = min.uhc, to = max.uhc, length.out = 500)


#getting the initial overall spline values at those UHC points
overall.uhc.spaced.vals.2 <- lapply(uhc.vals, function(uval){
  
  estimate <- overall.haz.spline.2[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, overall.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial severe spline values at those UHC points
severe.uhc.spaced.vals.2 <- lapply(uhc.vals, function(uval){
  
  estimate <- severe.haz.spline.2[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, severe.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial extreme spline values at those UHC points
extreme.uhc.spaced.vals.2 <- lapply(uhc.vals, function(uval){
  
  estimate <- extreme.haz.spline.2[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, extreme.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#now calculating a rolling average 
overall.uhc.spaced.vals.2 <- overall.uhc.spaced.vals.2 %>% 
  mutate(roll_mean = rollmean(overall.spline, 10, na.pad = T))



severe.uhc.spaced.vals.2 <- severe.uhc.spaced.vals.2 %>% 
  mutate(roll_mean = rollmean(severe.spline, 10, na.pad = T))



extreme.uhc.spaced.vals.2 <- extreme.uhc.spaced.vals.2 %>% 
  mutate(roll_mean = rollmean(extreme.spline, 10, na.pad = T))




overall.uhc.spaced.vals.2$overall.spline <- NULL
severe.uhc.spaced.vals.2$severe.spline <- NULL
extreme.uhc.spaced.vals.2$extreme.spline <- NULL


overall.uhc.spaced.vals.2 <- data.table(overall.uhc.spaced.vals.2)
severe.uhc.spaced.vals.2 <- data.table(severe.uhc.spaced.vals.2)
extreme.uhc.spaced.vals.2 <- data.table(extreme.uhc.spaced.vals.2)

overall.uhc.spaced.vals.2 <- overall.uhc.spaced.vals.2[!is.na(roll_mean)]
severe.uhc.spaced.vals.2 <- severe.uhc.spaced.vals.2[!is.na(roll_mean)]
extreme.uhc.spaced.vals.2 <- extreme.uhc.spaced.vals.2[!is.na(roll_mean)]

# merging all smoothed splines together
setnames(overall.uhc.spaced.vals.2, "roll_mean", "overall_expected.2")
setnames(severe.uhc.spaced.vals.2, "roll_mean", "severe_expected.2")
setnames(extreme.uhc.spaced.vals.2, "roll_mean", "extreme_expected.2")

stunting.smoothed.splines.2 <- merge(overall.uhc.spaced.vals.2, severe.uhc.spaced.vals.2, by = "uhc_value")
stunting.smoothed.splines.2 <- merge(stunting.smoothed.splines.2, extreme.uhc.spaced.vals.2, by = "uhc_value")

stunt.severe.to.overall.scalar.2 <- stunting.smoothed.splines.2[1]$overall_expected.2 / stunting.smoothed.splines.2[1]$severe_expected.2
stunt.exreme.to.overall.scalar.2 <- stunting.smoothed.splines.2[1]$overall_expected.2 / stunting.smoothed.splines.2[1]$extreme_expected.2
stunt.extreme.to.severe.scalar.2 <- stunting.smoothed.splines.2[1]$severe_expected.2 / stunting.smoothed.splines.2[1]$extreme_expected.2

# columns named as <what they're scaled to match>.scaled.<what they were originally>
stunting.smoothed.splines.2[, overall.scaled.severe.2 := severe_expected.2 * stunt.severe.to.overall.scalar.2]
stunting.smoothed.splines.2[, overall.scaled.extreme.2 := extreme_expected.2 * stunt.exreme.to.overall.scalar.2]

stunting.smoothed.splines.2[, severe.scaled.overall.2 := overall_expected.2 / stunt.severe.to.overall.scalar.2]
stunting.smoothed.splines.2[, severe.scaled.extreme.2 := extreme_expected.2 * stunt.extreme.to.severe.scalar.2]

stunting.smoothed.splines.2[, extreme.scaled.overall.2 := overall_expected.2 / stunt.exreme.to.overall.scalar.2]
stunting.smoothed.splines.2[, extreme.scaled.severe.2 := severe_expected.2 / stunt.extreme.to.severe.scalar.2]







stunting.spline.y.labels <- seq(0, max(stunting.smoothed.splines.2$overall_expected.2), length.out = 6)

stunting.2 <- ggplot() +
  geom_line(data = stunting.smoothed.splines.2, aes(x = uhc_value, y = overall_expected.2), color = "#eda109", size = 2.5) +
  geom_line(data = stunting.smoothed.splines.2, aes(x = uhc_value, y = overall.scaled.severe.2), color = "#e36f10", size = 2.5) +
  geom_line(data = stunting.smoothed.splines.2, aes(x = uhc_value, y = overall.scaled.extreme.2), color = "#bf1313", size = 2.5) +
  scale_y_continuous(breaks = stunting.spline.y.labels, labels = c("-100%", "-80%", "-60%", "-40%", "-20%", "0%")) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        plot.tag = element_text(size = 24, face = "bold")) +
  labs(x = "Universal Health Coverage Index", y = "Relative Decrease", tag = "A")






####### sdi of 0.4


# first need to create scaled versions of the three splines and smooth them

overall.haz.spline.4 <- haz.epitrans[sdi == .4 & level > 2 & severity == "Overall", c("uhc", "expected")]
overall.haz.spline.4 <- overall.haz.spline.4[order(uhc),]

severe.haz.spline.4 <- haz.epitrans[sdi == .4 & level > 2 & severity == "Severe", c("uhc", "expected")]
severe.haz.spline.4 <- severe.haz.spline.4[order(uhc),]

extreme.haz.spline.4 <- haz.epitrans[sdi == .4 & level > 2 & severity == "Extreme", c("uhc", "expected")]
extreme.haz.spline.4 <- extreme.haz.spline.4[order(uhc),]

#getting 500 UHC values along this span to smooth out
min.uhc <- min(overall.haz.spline.4$uhc)
max.uhc <- max(overall.haz.spline.4$uhc)
uhc.vals <- seq(from = min.uhc, to = max.uhc, length.out = 500)


#getting the initial overall spline values at those UHC points
overall.uhc.spaced.vals.4 <- lapply(uhc.vals, function(uval){
  
  estimate <- overall.haz.spline.4[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, overall.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial severe spline values at those UHC points
severe.uhc.spaced.vals.4 <- lapply(uhc.vals, function(uval){
  
  estimate <- severe.haz.spline.4[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, severe.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial extreme spline values at those UHC points
extreme.uhc.spaced.vals.4 <- lapply(uhc.vals, function(uval){
  
  estimate <- extreme.haz.spline.4[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, extreme.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#now calculating a rolling average 
overall.uhc.spaced.vals.4 <- overall.uhc.spaced.vals.4 %>% 
  mutate(roll_mean = rollmean(overall.spline, 10, na.pad = T))



severe.uhc.spaced.vals.4 <- severe.uhc.spaced.vals.4 %>% 
  mutate(roll_mean = rollmean(severe.spline, 10, na.pad = T))



extreme.uhc.spaced.vals.4 <- extreme.uhc.spaced.vals.4 %>% 
  mutate(roll_mean = rollmean(extreme.spline, 10, na.pad = T))




overall.uhc.spaced.vals.4$overall.spline <- NULL
severe.uhc.spaced.vals.4$severe.spline <- NULL
extreme.uhc.spaced.vals.4$extreme.spline <- NULL


overall.uhc.spaced.vals.4 <- data.table(overall.uhc.spaced.vals.4)
severe.uhc.spaced.vals.4 <- data.table(severe.uhc.spaced.vals.4)
extreme.uhc.spaced.vals.4 <- data.table(extreme.uhc.spaced.vals.4)

overall.uhc.spaced.vals.4 <- overall.uhc.spaced.vals.4[!is.na(roll_mean)]
severe.uhc.spaced.vals.4 <- severe.uhc.spaced.vals.4[!is.na(roll_mean)]
extreme.uhc.spaced.vals.4 <- extreme.uhc.spaced.vals.4[!is.na(roll_mean)]

# merging all smoothed splines together
setnames(overall.uhc.spaced.vals.4, "roll_mean", "overall_expected.4")
setnames(severe.uhc.spaced.vals.4, "roll_mean", "severe_expected.4")
setnames(extreme.uhc.spaced.vals.4, "roll_mean", "extreme_expected.4")

stunting.smoothed.splines.4 <- merge(overall.uhc.spaced.vals.4, severe.uhc.spaced.vals.4, by = "uhc_value")
stunting.smoothed.splines.4 <- merge(stunting.smoothed.splines.4, extreme.uhc.spaced.vals.4, by = "uhc_value")

stunt.severe.to.overall.scalar.4 <- stunting.smoothed.splines.4[1]$overall_expected.4 / stunting.smoothed.splines.4[1]$severe_expected.4
stunt.exreme.to.overall.scalar.4 <- stunting.smoothed.splines.4[1]$overall_expected.4 / stunting.smoothed.splines.4[1]$extreme_expected.4
stunt.extreme.to.severe.scalar.4 <- stunting.smoothed.splines.4[1]$severe_expected.4 / stunting.smoothed.splines.4[1]$extreme_expected.4

# columns named as <what they're scaled to match>.scaled.<what they were originally>
stunting.smoothed.splines.4[, overall.scaled.severe.4 := severe_expected.4 * stunt.severe.to.overall.scalar.4]
stunting.smoothed.splines.4[, overall.scaled.extreme.4 := extreme_expected.4 * stunt.exreme.to.overall.scalar.4]

stunting.smoothed.splines.4[, severe.scaled.overall.4 := overall_expected.4 / stunt.severe.to.overall.scalar.4]
stunting.smoothed.splines.4[, severe.scaled.extreme.4 := extreme_expected.4 * stunt.extreme.to.severe.scalar.4]

stunting.smoothed.splines.4[, extreme.scaled.overall.4 := overall_expected.4 / stunt.exreme.to.overall.scalar.4]
stunting.smoothed.splines.4[, extreme.scaled.severe.4 := severe_expected.4 / stunt.extreme.to.severe.scalar.4]







stunting.spline.y.labels <- seq(0, max(stunting.smoothed.splines.4$overall_expected.4), length.out = 6)

stunting.4 <- ggplot() +
  geom_line(data = stunting.smoothed.splines.4, aes(x = uhc_value, y = overall_expected.4), color = "#eda109", size = 2.5) +
  geom_line(data = stunting.smoothed.splines.4, aes(x = uhc_value, y = overall.scaled.severe.4), color = "#e36f10", size = 2.5) +
  geom_line(data = stunting.smoothed.splines.4, aes(x = uhc_value, y = overall.scaled.extreme.4), color = "#bf1313", size = 2.5) +
  scale_y_continuous(breaks = stunting.spline.y.labels, labels = c("-100%", "-80%", "-60%", "-40%", "-20%", "0%")) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        plot.tag = element_text(size = 24, face = "bold")) +
  labs(x = "Universal Health Coverage Index", y = "Relative Decrease", tag = "B")





####### sdi of 0.6


# first need to create scaled versions of the three splines and smooth them

overall.haz.spline.6 <- haz.epitrans[sdi == .6 & level > 2 & severity == "Overall", c("uhc", "expected")]
overall.haz.spline.6 <- overall.haz.spline.6[order(uhc),]

severe.haz.spline.6 <- haz.epitrans[sdi == .6 & level > 2 & severity == "Severe", c("uhc", "expected")]
severe.haz.spline.6 <- severe.haz.spline.6[order(uhc),]

extreme.haz.spline.6 <- haz.epitrans[sdi == .6 & level > 2 & severity == "Extreme", c("uhc", "expected")]
extreme.haz.spline.6 <- extreme.haz.spline.6[order(uhc),]

#getting 500 UHC values along this span to smooth out
min.uhc <- min(overall.haz.spline.6$uhc)
max.uhc <- max(overall.haz.spline.6$uhc)
uhc.vals <- seq(from = min.uhc, to = max.uhc, length.out = 500)


#getting the initial overall spline values at those UHC points
overall.uhc.spaced.vals.6 <- lapply(uhc.vals, function(uval){
  
  estimate <- overall.haz.spline.6[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, overall.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial severe spline values at those UHC points
severe.uhc.spaced.vals.6 <- lapply(uhc.vals, function(uval){
  
  estimate <- severe.haz.spline.6[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, severe.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial extreme spline values at those UHC points
extreme.uhc.spaced.vals.6 <- lapply(uhc.vals, function(uval){
  
  estimate <- extreme.haz.spline.6[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, extreme.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#now calculating a rolling average 
overall.uhc.spaced.vals.6 <- overall.uhc.spaced.vals.6 %>% 
  mutate(roll_mean = rollmean(overall.spline, 10, na.pad = T))



severe.uhc.spaced.vals.6 <- severe.uhc.spaced.vals.6 %>% 
  mutate(roll_mean = rollmean(severe.spline, 10, na.pad = T))



extreme.uhc.spaced.vals.6 <- extreme.uhc.spaced.vals.6 %>% 
  mutate(roll_mean = rollmean(extreme.spline, 10, na.pad = T))




overall.uhc.spaced.vals.6$overall.spline <- NULL
severe.uhc.spaced.vals.6$severe.spline <- NULL
extreme.uhc.spaced.vals.6$extreme.spline <- NULL


overall.uhc.spaced.vals.6 <- data.table(overall.uhc.spaced.vals.6)
severe.uhc.spaced.vals.6 <- data.table(severe.uhc.spaced.vals.6)
extreme.uhc.spaced.vals.6 <- data.table(extreme.uhc.spaced.vals.6)

overall.uhc.spaced.vals.6 <- overall.uhc.spaced.vals.6[!is.na(roll_mean)]
severe.uhc.spaced.vals.6 <- severe.uhc.spaced.vals.6[!is.na(roll_mean)]
extreme.uhc.spaced.vals.6 <- extreme.uhc.spaced.vals.6[!is.na(roll_mean)]

# merging all smoothed splines together
setnames(overall.uhc.spaced.vals.6, "roll_mean", "overall_expected.6")
setnames(severe.uhc.spaced.vals.6, "roll_mean", "severe_expected.6")
setnames(extreme.uhc.spaced.vals.6, "roll_mean", "extreme_expected.6")

stunting.smoothed.splines.6 <- merge(overall.uhc.spaced.vals.6, severe.uhc.spaced.vals.6, by = "uhc_value")
stunting.smoothed.splines.6 <- merge(stunting.smoothed.splines.6, extreme.uhc.spaced.vals.6, by = "uhc_value")

stunt.severe.to.overall.scalar.6 <- stunting.smoothed.splines.6[1]$overall_expected.6 / stunting.smoothed.splines.6[1]$severe_expected.6
stunt.exreme.to.overall.scalar.6 <- stunting.smoothed.splines.6[1]$overall_expected.6 / stunting.smoothed.splines.6[1]$extreme_expected.6
stunt.extreme.to.severe.scalar.6 <- stunting.smoothed.splines.6[1]$severe_expected.6 / stunting.smoothed.splines.6[1]$extreme_expected.6

# columns named as <what they're scaled to match>.scaled.<what they were originally>
stunting.smoothed.splines.6[, overall.scaled.severe.6 := severe_expected.6 * stunt.severe.to.overall.scalar.6]
stunting.smoothed.splines.6[, overall.scaled.extreme.6 := extreme_expected.6 * stunt.exreme.to.overall.scalar.6]

stunting.smoothed.splines.6[, severe.scaled.overall.6 := overall_expected.6 / stunt.severe.to.overall.scalar.6]
stunting.smoothed.splines.6[, severe.scaled.extreme.6 := extreme_expected.6 * stunt.extreme.to.severe.scalar.6]

stunting.smoothed.splines.6[, extreme.scaled.overall.6 := overall_expected.6 / stunt.exreme.to.overall.scalar.6]
stunting.smoothed.splines.6[, extreme.scaled.severe.6 := severe_expected.6 / stunt.extreme.to.severe.scalar.6]







stunting.spline.y.labels <- seq(0, max(stunting.smoothed.splines.6$overall_expected.6), length.out = 6)

stunting.6 <- ggplot() +
  geom_line(data = stunting.smoothed.splines.6, aes(x = uhc_value, y = overall_expected.6), color = "#eda109", size = 2.5) +
  geom_line(data = stunting.smoothed.splines.6, aes(x = uhc_value, y = overall.scaled.severe.6), color = "#e36f10", size = 2.5) +
  geom_line(data = stunting.smoothed.splines.6, aes(x = uhc_value, y = overall.scaled.extreme.6), color = "#bf1313", size = 2.5) +
  scale_y_continuous(breaks = stunting.spline.y.labels, labels = c("-100%", "-80%", "-60%", "-40%", "-20%", "0%")) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        plot.tag = element_text(size = 24, face = "bold")) +
  labs(x = "Universal Health Coverage Index", y = "                                 Relative Decrease", tag = "C")






####### sdi of 0.8


# first need to create scaled versions of the three splines and smooth them

overall.haz.spline.8 <- haz.epitrans[sdi == .8 & level > 2 & severity == "Overall", c("uhc", "expected")]
overall.haz.spline.8 <- overall.haz.spline.8[order(uhc),]

severe.haz.spline.8 <- haz.epitrans[sdi == .8 & level > 2 & severity == "Severe", c("uhc", "expected")]
severe.haz.spline.8 <- severe.haz.spline.8[order(uhc),]

extreme.haz.spline.8 <- haz.epitrans[sdi == .8 & level > 2 & severity == "Extreme", c("uhc", "expected")]
extreme.haz.spline.8 <- extreme.haz.spline.8[order(uhc),]

#getting 500 UHC values along this span to smooth out
min.uhc <- min(overall.haz.spline.8$uhc)
max.uhc <- max(overall.haz.spline.8$uhc)
uhc.vals <- seq(from = min.uhc, to = max.uhc, length.out = 500)


#getting the initial overall spline values at those UHC points
overall.uhc.spaced.vals.8 <- lapply(uhc.vals, function(uval){
  
  estimate <- overall.haz.spline.8[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, overall.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial severe spline values at those UHC points
severe.uhc.spaced.vals.8 <- lapply(uhc.vals, function(uval){
  
  estimate <- severe.haz.spline.8[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, severe.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial extreme spline values at those UHC points
extreme.uhc.spaced.vals.8 <- lapply(uhc.vals, function(uval){
  
  estimate <- extreme.haz.spline.8[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, extreme.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#now calculating a rolling average 
overall.uhc.spaced.vals.8 <- overall.uhc.spaced.vals.8 %>% 
  mutate(roll_mean = rollmean(overall.spline, 10, na.pad = T))



severe.uhc.spaced.vals.8 <- severe.uhc.spaced.vals.8 %>% 
  mutate(roll_mean = rollmean(severe.spline, 10, na.pad = T))



extreme.uhc.spaced.vals.8 <- extreme.uhc.spaced.vals.8 %>% 
  mutate(roll_mean = rollmean(extreme.spline, 10, na.pad = T))




overall.uhc.spaced.vals.8$overall.spline <- NULL
severe.uhc.spaced.vals.8$severe.spline <- NULL
extreme.uhc.spaced.vals.8$extreme.spline <- NULL


overall.uhc.spaced.vals.8 <- data.table(overall.uhc.spaced.vals.8)
severe.uhc.spaced.vals.8 <- data.table(severe.uhc.spaced.vals.8)
extreme.uhc.spaced.vals.8 <- data.table(extreme.uhc.spaced.vals.8)

overall.uhc.spaced.vals.8 <- overall.uhc.spaced.vals.8[!is.na(roll_mean)]
severe.uhc.spaced.vals.8 <- severe.uhc.spaced.vals.8[!is.na(roll_mean)]
extreme.uhc.spaced.vals.8 <- extreme.uhc.spaced.vals.8[!is.na(roll_mean)]

# merging all smoothed splines together
setnames(overall.uhc.spaced.vals.8, "roll_mean", "overall_expected.8")
setnames(severe.uhc.spaced.vals.8, "roll_mean", "severe_expected.8")
setnames(extreme.uhc.spaced.vals.8, "roll_mean", "extreme_expected.8")

stunting.smoothed.splines.8 <- merge(overall.uhc.spaced.vals.8, severe.uhc.spaced.vals.8, by = "uhc_value")
stunting.smoothed.splines.8 <- merge(stunting.smoothed.splines.8, extreme.uhc.spaced.vals.8, by = "uhc_value")

stunt.severe.to.overall.scalar.8 <- stunting.smoothed.splines.8[1]$overall_expected.8 / stunting.smoothed.splines.8[1]$severe_expected.8
stunt.exreme.to.overall.scalar.8 <- stunting.smoothed.splines.8[1]$overall_expected.8 / stunting.smoothed.splines.8[1]$extreme_expected.8
stunt.extreme.to.severe.scalar.8 <- stunting.smoothed.splines.8[1]$severe_expected.8 / stunting.smoothed.splines.8[1]$extreme_expected.8

# columns named as <what they're scaled to match>.scaled.<what they were originally>
stunting.smoothed.splines.8[, overall.scaled.severe.8 := severe_expected.8 * stunt.severe.to.overall.scalar.8]
stunting.smoothed.splines.8[, overall.scaled.extreme.8 := extreme_expected.8 * stunt.exreme.to.overall.scalar.8]

stunting.smoothed.splines.8[, severe.scaled.overall.8 := overall_expected.8 / stunt.severe.to.overall.scalar.8]
stunting.smoothed.splines.8[, severe.scaled.extreme.8 := extreme_expected.8 * stunt.extreme.to.severe.scalar.8]

stunting.smoothed.splines.8[, extreme.scaled.overall.8 := overall_expected.8 / stunt.exreme.to.overall.scalar.8]
stunting.smoothed.splines.8[, extreme.scaled.severe.8 := severe_expected.8 / stunt.extreme.to.severe.scalar.8]







stunting.spline.y.labels <- seq(0, max(stunting.smoothed.splines.8$overall_expected.8), length.out = 6)

stunting.8 <- ggplot() +
  geom_line(data = stunting.smoothed.splines.8, aes(x = uhc_value, y = overall_expected.8), color = "#eda109", size = 2.5) +
  geom_line(data = stunting.smoothed.splines.8, aes(x = uhc_value, y = overall.scaled.severe.8), color = "#e36f10", size = 2.5) +
  geom_line(data = stunting.smoothed.splines.8, aes(x = uhc_value, y = overall.scaled.extreme.8), color = "#bf1313", size = 2.5) +
  scale_y_continuous(breaks = stunting.spline.y.labels, labels = c("-100%", "-80%", "-60%", "-40%", "-20%", "0%")) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        plot.tag = element_text(size = 24, face = "bold")) +
  labs(x = "Universal Health Coverage Index", y = "Relative Decrease", tag = "D")










##### WASTING

# reading in epi-transition output for all stunting severities
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


age_weights <- get_age_weights(gbd_round_id = 7)
hierarchy <- get_location_metadata(location_set_id = 35, release_id = 9)


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




####### sdi of 0.2


# first need to create scaled versions of the three splines and smooth them

overall.whz.spline.2 <- whz.epitrans[sdi == .2 & level > 2 & severity == "Overall", c("uhc", "expected")]
overall.whz.spline.2 <- overall.whz.spline.2[order(uhc),]

severe.whz.spline.2 <- whz.epitrans[sdi == .2 & level > 2 & severity == "Severe", c("uhc", "expected")]
severe.whz.spline.2 <- severe.whz.spline.2[order(uhc),]

extreme.whz.spline.2 <- whz.epitrans[sdi == .2 & level > 2 & severity == "Extreme", c("uhc", "expected")]
extreme.whz.spline.2 <- extreme.whz.spline.2[order(uhc),]

#getting 500 UHC values along this span to smooth out
min.uhc <- min(overall.whz.spline.2$uhc)
max.uhc <- max(overall.whz.spline.2$uhc)
uhc.vals <- seq(from = min.uhc, to = max.uhc, length.out = 500)


#getting the initial overall spline values at those UHC points
overall.uhc.spaced.vals.2 <- lapply(uhc.vals, function(uval){
  
  estimate <- overall.whz.spline.2[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, overall.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial severe spline values at those UHC points
severe.uhc.spaced.vals.2 <- lapply(uhc.vals, function(uval){
  
  estimate <- severe.whz.spline.2[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, severe.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial extreme spline values at those UHC points
extreme.uhc.spaced.vals.2 <- lapply(uhc.vals, function(uval){
  
  estimate <- extreme.whz.spline.2[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, extreme.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#now calculating a rolling average 
overall.uhc.spaced.vals.2 <- overall.uhc.spaced.vals.2 %>% 
  mutate(roll_mean = rollmean(overall.spline, 10, na.pad = T))



severe.uhc.spaced.vals.2 <- severe.uhc.spaced.vals.2 %>% 
  mutate(roll_mean = rollmean(severe.spline, 10, na.pad = T))



extreme.uhc.spaced.vals.2 <- extreme.uhc.spaced.vals.2 %>% 
  mutate(roll_mean = rollmean(extreme.spline, 10, na.pad = T))




overall.uhc.spaced.vals.2$overall.spline <- NULL
severe.uhc.spaced.vals.2$severe.spline <- NULL
extreme.uhc.spaced.vals.2$extreme.spline <- NULL


overall.uhc.spaced.vals.2 <- data.table(overall.uhc.spaced.vals.2)
severe.uhc.spaced.vals.2 <- data.table(severe.uhc.spaced.vals.2)
extreme.uhc.spaced.vals.2 <- data.table(extreme.uhc.spaced.vals.2)

overall.uhc.spaced.vals.2 <- overall.uhc.spaced.vals.2[!is.na(roll_mean)]
severe.uhc.spaced.vals.2 <- severe.uhc.spaced.vals.2[!is.na(roll_mean)]
extreme.uhc.spaced.vals.2 <- extreme.uhc.spaced.vals.2[!is.na(roll_mean)]

# merging all smoothed splines together
setnames(overall.uhc.spaced.vals.2, "roll_mean", "overall_expected.2")
setnames(severe.uhc.spaced.vals.2, "roll_mean", "severe_expected.2")
setnames(extreme.uhc.spaced.vals.2, "roll_mean", "extreme_expected.2")

wasting.smoothed.splines.2 <- merge(overall.uhc.spaced.vals.2, severe.uhc.spaced.vals.2, by = "uhc_value")
wasting.smoothed.splines.2 <- merge(wasting.smoothed.splines.2, extreme.uhc.spaced.vals.2, by = "uhc_value")

waste.severe.to.overall.scalar.2 <- wasting.smoothed.splines.2[1]$overall_expected.2 / wasting.smoothed.splines.2[1]$severe_expected.2
waste.exreme.to.overall.scalar.2 <- wasting.smoothed.splines.2[1]$overall_expected.2 / wasting.smoothed.splines.2[1]$extreme_expected.2
waste.extreme.to.severe.scalar.2 <- wasting.smoothed.splines.2[1]$severe_expected.2 / wasting.smoothed.splines.2[1]$extreme_expected.2

# columns named as <what they're scaled to match>.scaled.<what they were originally>
wasting.smoothed.splines.2[, overall.scaled.severe.2 := severe_expected.2 * waste.severe.to.overall.scalar.2]
wasting.smoothed.splines.2[, overall.scaled.extreme.2 := extreme_expected.2 * waste.exreme.to.overall.scalar.2]

wasting.smoothed.splines.2[, severe.scaled.overall.2 := overall_expected.2 / waste.severe.to.overall.scalar.2]
wasting.smoothed.splines.2[, severe.scaled.extreme.2 := extreme_expected.2 * waste.extreme.to.severe.scalar.2]

wasting.smoothed.splines.2[, extreme.scaled.overall.2 := overall_expected.2 / waste.exreme.to.overall.scalar.2]
wasting.smoothed.splines.2[, extreme.scaled.severe.2 := severe_expected.2 / waste.extreme.to.severe.scalar.2]







wasting.spline.y.labels <- seq(0, max(wasting.smoothed.splines.2$overall_expected.2), length.out = 6)

wasting.2 <- ggplot() +
  geom_line(data = wasting.smoothed.splines.2, aes(x = uhc_value, y = overall_expected.2), color = "#eda109", size = 2.5) +
  geom_line(data = wasting.smoothed.splines.2, aes(x = uhc_value, y = overall.scaled.severe.2), color = "#e36f10", size = 2.5) +
  geom_line(data = wasting.smoothed.splines.2, aes(x = uhc_value, y = overall.scaled.extreme.2), color = "#bf1313", size = 2.5) +
  scale_y_continuous(breaks = wasting.spline.y.labels, labels = c("-100%", "-80%", "-60%", "-40%", "-20%", "0%")) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        plot.tag = element_text(size = 24, face = "bold")) +
  labs(x = "Universal Health Coverage Index", y = "Relative Decrease", tag = "E")









####### sdi of 0.4


# first need to create scaled versions of the three splines and smooth them

overall.whz.spline.4 <- whz.epitrans[sdi == .4 & level > 2 & severity == "Overall", c("uhc", "expected")]
overall.whz.spline.4 <- overall.whz.spline.4[order(uhc),]

severe.whz.spline.4 <- whz.epitrans[sdi == .4 & level > 2 & severity == "Severe", c("uhc", "expected")]
severe.whz.spline.4 <- severe.whz.spline.4[order(uhc),]

extreme.whz.spline.4 <- whz.epitrans[sdi == .4 & level > 2 & severity == "Extreme", c("uhc", "expected")]
extreme.whz.spline.4 <- extreme.whz.spline.4[order(uhc),]

#getting 500 UHC values along this span to smooth out
min.uhc <- min(overall.whz.spline.4$uhc)
max.uhc <- max(overall.whz.spline.4$uhc)
uhc.vals <- seq(from = min.uhc, to = max.uhc, length.out = 500)


#getting the initial overall spline values at those UHC points
overall.uhc.spaced.vals.4 <- lapply(uhc.vals, function(uval){
  
  estimate <- overall.whz.spline.4[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, overall.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial severe spline values at those UHC points
severe.uhc.spaced.vals.4 <- lapply(uhc.vals, function(uval){
  
  estimate <- severe.whz.spline.4[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, severe.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial extreme spline values at those UHC points
extreme.uhc.spaced.vals.4 <- lapply(uhc.vals, function(uval){
  
  estimate <- extreme.whz.spline.4[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, extreme.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#now calculating a rolling average 
overall.uhc.spaced.vals.4 <- overall.uhc.spaced.vals.4 %>% 
  mutate(roll_mean = rollmean(overall.spline, 10, na.pad = T))



severe.uhc.spaced.vals.4 <- severe.uhc.spaced.vals.4 %>% 
  mutate(roll_mean = rollmean(severe.spline, 10, na.pad = T))



extreme.uhc.spaced.vals.4 <- extreme.uhc.spaced.vals.4 %>% 
  mutate(roll_mean = rollmean(extreme.spline, 10, na.pad = T))




overall.uhc.spaced.vals.4$overall.spline <- NULL
severe.uhc.spaced.vals.4$severe.spline <- NULL
extreme.uhc.spaced.vals.4$extreme.spline <- NULL


overall.uhc.spaced.vals.4 <- data.table(overall.uhc.spaced.vals.4)
severe.uhc.spaced.vals.4 <- data.table(severe.uhc.spaced.vals.4)
extreme.uhc.spaced.vals.4 <- data.table(extreme.uhc.spaced.vals.4)

overall.uhc.spaced.vals.4 <- overall.uhc.spaced.vals.4[!is.na(roll_mean)]
severe.uhc.spaced.vals.4 <- severe.uhc.spaced.vals.4[!is.na(roll_mean)]
extreme.uhc.spaced.vals.4 <- extreme.uhc.spaced.vals.4[!is.na(roll_mean)]

# merging all smoothed splines together
setnames(overall.uhc.spaced.vals.4, "roll_mean", "overall_expected.4")
setnames(severe.uhc.spaced.vals.4, "roll_mean", "severe_expected.4")
setnames(extreme.uhc.spaced.vals.4, "roll_mean", "extreme_expected.4")

wasting.smoothed.splines.4 <- merge(overall.uhc.spaced.vals.4, severe.uhc.spaced.vals.4, by = "uhc_value")
wasting.smoothed.splines.4 <- merge(wasting.smoothed.splines.4, extreme.uhc.spaced.vals.4, by = "uhc_value")

waste.severe.to.overall.scalar.4 <- wasting.smoothed.splines.4[1]$overall_expected.4 / wasting.smoothed.splines.4[1]$severe_expected.4
waste.exreme.to.overall.scalar.4 <- wasting.smoothed.splines.4[1]$overall_expected.4 / wasting.smoothed.splines.4[1]$extreme_expected.4
waste.extreme.to.severe.scalar.4 <- wasting.smoothed.splines.4[1]$severe_expected.4 / wasting.smoothed.splines.4[1]$extreme_expected.4

# columns named as <what they're scaled to match>.scaled.<what they were originally>
wasting.smoothed.splines.4[, overall.scaled.severe.4 := severe_expected.4 * waste.severe.to.overall.scalar.4]
wasting.smoothed.splines.4[, overall.scaled.extreme.4 := extreme_expected.4 * waste.exreme.to.overall.scalar.4]

wasting.smoothed.splines.4[, severe.scaled.overall.4 := overall_expected.4 / waste.severe.to.overall.scalar.4]
wasting.smoothed.splines.4[, severe.scaled.extreme.4 := extreme_expected.4 * waste.extreme.to.severe.scalar.4]

wasting.smoothed.splines.4[, extreme.scaled.overall.4 := overall_expected.4 / waste.exreme.to.overall.scalar.4]
wasting.smoothed.splines.4[, extreme.scaled.severe.4 := severe_expected.4 / waste.extreme.to.severe.scalar.4]







wasting.spline.y.labels <- seq(0, max(wasting.smoothed.splines.4$overall_expected.4), length.out = 6)

wasting.4 <- ggplot() +
  geom_line(data = wasting.smoothed.splines.4, aes(x = uhc_value, y = overall_expected.4), color = "#eda109", size = 2.5) +
  geom_line(data = wasting.smoothed.splines.4, aes(x = uhc_value, y = overall.scaled.severe.4), color = "#e36f10", size = 2.5) +
  geom_line(data = wasting.smoothed.splines.4, aes(x = uhc_value, y = overall.scaled.extreme.4), color = "#bf1313", size = 2.5) +
  scale_y_continuous(breaks = wasting.spline.y.labels, labels = c("-100%", "-80%", "-60%", "-40%", "-20%", "0%")) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        plot.tag = element_text(size = 24, face = "bold")) +
  labs(x = "Universal Health Coverage Index", y = "Relative Decrease", tag = "F")








####### sdi of 0.6


# first need to create scaled versions of the three splines and smooth them

overall.whz.spline.6 <- whz.epitrans[sdi == .6 & level > 2 & severity == "Overall", c("uhc", "expected")]
overall.whz.spline.6 <- overall.whz.spline.6[order(uhc),]

severe.whz.spline.6 <- whz.epitrans[sdi == .6 & level > 2 & severity == "Severe", c("uhc", "expected")]
severe.whz.spline.6 <- severe.whz.spline.6[order(uhc),]

extreme.whz.spline.6 <- whz.epitrans[sdi == .6 & level > 2 & severity == "Extreme", c("uhc", "expected")]
extreme.whz.spline.6 <- extreme.whz.spline.6[order(uhc),]

#getting 500 UHC values along this span to smooth out
min.uhc <- min(overall.whz.spline.6$uhc)
max.uhc <- max(overall.whz.spline.6$uhc)
uhc.vals <- seq(from = min.uhc, to = max.uhc, length.out = 500)


#getting the initial overall spline values at those UHC points
overall.uhc.spaced.vals.6 <- lapply(uhc.vals, function(uval){
  
  estimate <- overall.whz.spline.6[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, overall.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial severe spline values at those UHC points
severe.uhc.spaced.vals.6 <- lapply(uhc.vals, function(uval){
  
  estimate <- severe.whz.spline.6[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, severe.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial extreme spline values at those UHC points
extreme.uhc.spaced.vals.6 <- lapply(uhc.vals, function(uval){
  
  estimate <- extreme.whz.spline.6[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, extreme.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#now calculating a rolling average 
overall.uhc.spaced.vals.6 <- overall.uhc.spaced.vals.6 %>% 
  mutate(roll_mean = rollmean(overall.spline, 10, na.pad = T))



severe.uhc.spaced.vals.6 <- severe.uhc.spaced.vals.6 %>% 
  mutate(roll_mean = rollmean(severe.spline, 10, na.pad = T))



extreme.uhc.spaced.vals.6 <- extreme.uhc.spaced.vals.6 %>% 
  mutate(roll_mean = rollmean(extreme.spline, 10, na.pad = T))




overall.uhc.spaced.vals.6$overall.spline <- NULL
severe.uhc.spaced.vals.6$severe.spline <- NULL
extreme.uhc.spaced.vals.6$extreme.spline <- NULL


overall.uhc.spaced.vals.6 <- data.table(overall.uhc.spaced.vals.6)
severe.uhc.spaced.vals.6 <- data.table(severe.uhc.spaced.vals.6)
extreme.uhc.spaced.vals.6 <- data.table(extreme.uhc.spaced.vals.6)

overall.uhc.spaced.vals.6 <- overall.uhc.spaced.vals.6[!is.na(roll_mean)]
severe.uhc.spaced.vals.6 <- severe.uhc.spaced.vals.6[!is.na(roll_mean)]
extreme.uhc.spaced.vals.6 <- extreme.uhc.spaced.vals.6[!is.na(roll_mean)]

# merging all smoothed splines together
setnames(overall.uhc.spaced.vals.6, "roll_mean", "overall_expected.6")
setnames(severe.uhc.spaced.vals.6, "roll_mean", "severe_expected.6")
setnames(extreme.uhc.spaced.vals.6, "roll_mean", "extreme_expected.6")

wasting.smoothed.splines.6 <- merge(overall.uhc.spaced.vals.6, severe.uhc.spaced.vals.6, by = "uhc_value")
wasting.smoothed.splines.6 <- merge(wasting.smoothed.splines.6, extreme.uhc.spaced.vals.6, by = "uhc_value")

waste.severe.to.overall.scalar.6 <- wasting.smoothed.splines.6[1]$overall_expected.6 / wasting.smoothed.splines.6[1]$severe_expected.6
waste.exreme.to.overall.scalar.6 <- wasting.smoothed.splines.6[1]$overall_expected.6 / wasting.smoothed.splines.6[1]$extreme_expected.6
waste.extreme.to.severe.scalar.6 <- wasting.smoothed.splines.6[1]$severe_expected.6 / wasting.smoothed.splines.6[1]$extreme_expected.6

# columns named as <what they're scaled to match>.scaled.<what they were originally>
wasting.smoothed.splines.6[, overall.scaled.severe.6 := severe_expected.6 * waste.severe.to.overall.scalar.6]
wasting.smoothed.splines.6[, overall.scaled.extreme.6 := extreme_expected.6 * waste.exreme.to.overall.scalar.6]

wasting.smoothed.splines.6[, severe.scaled.overall.6 := overall_expected.6 / waste.severe.to.overall.scalar.6]
wasting.smoothed.splines.6[, severe.scaled.extreme.6 := extreme_expected.6 * waste.extreme.to.severe.scalar.6]

wasting.smoothed.splines.6[, extreme.scaled.overall.6 := overall_expected.6 / waste.exreme.to.overall.scalar.6]
wasting.smoothed.splines.6[, extreme.scaled.severe.6 := severe_expected.6 / waste.extreme.to.severe.scalar.6]







wasting.spline.y.labels <- seq(0, max(wasting.smoothed.splines.6$overall_expected.6), length.out = 6)

wasting.6 <- ggplot() +
  geom_line(data = wasting.smoothed.splines.6, aes(x = uhc_value, y = overall_expected.6), color = "#eda109", size = 2.5) +
  geom_line(data = wasting.smoothed.splines.6, aes(x = uhc_value, y = overall.scaled.severe.6), color = "#e36f10", size = 2.5) +
  geom_line(data = wasting.smoothed.splines.6, aes(x = uhc_value, y = overall.scaled.extreme.6), color = "#bf1313", size = 2.5) +
  scale_y_continuous(breaks = wasting.spline.y.labels, labels = c("-100%", "-80%", "-60%", "-40%", "-20%", "0%")) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        plot.tag = element_text(size = 24, face = "bold")) +
  labs(x = "Universal Health Coverage Index", y = "Relative Decrease", tag = "G")








####### sdi of 0.8


# first need to create scaled versions of the three splines and smooth them

overall.whz.spline.8 <- whz.epitrans[sdi == .8 & level > 2 & severity == "Overall", c("uhc", "expected")]
overall.whz.spline.8 <- overall.whz.spline.8[order(uhc),]

severe.whz.spline.8 <- whz.epitrans[sdi == .8 & level > 2 & severity == "Severe", c("uhc", "expected")]
severe.whz.spline.8 <- severe.whz.spline.8[order(uhc),]

extreme.whz.spline.8 <- whz.epitrans[sdi == .8 & level > 2 & severity == "Extreme", c("uhc", "expected")]
extreme.whz.spline.8 <- extreme.whz.spline.8[order(uhc),]

#getting 500 UHC values along this span to smooth out
min.uhc <- min(overall.whz.spline.8$uhc)
max.uhc <- max(overall.whz.spline.8$uhc)
uhc.vals <- seq(from = min.uhc, to = max.uhc, length.out = 500)


#getting the initial overall spline values at those UHC points
overall.uhc.spaced.vals.8 <- lapply(uhc.vals, function(uval){
  
  estimate <- overall.whz.spline.8[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, overall.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial severe spline values at those UHC points
severe.uhc.spaced.vals.8 <- lapply(uhc.vals, function(uval){
  
  estimate <- severe.whz.spline.8[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, severe.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial extreme spline values at those UHC points
extreme.uhc.spaced.vals.8 <- lapply(uhc.vals, function(uval){
  
  estimate <- extreme.whz.spline.8[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, extreme.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#now calculating a rolling average 
overall.uhc.spaced.vals.8 <- overall.uhc.spaced.vals.8 %>% 
  mutate(roll_mean = rollmean(overall.spline, 10, na.pad = T))



severe.uhc.spaced.vals.8 <- severe.uhc.spaced.vals.8 %>% 
  mutate(roll_mean = rollmean(severe.spline, 10, na.pad = T))



extreme.uhc.spaced.vals.8 <- extreme.uhc.spaced.vals.8 %>% 
  mutate(roll_mean = rollmean(extreme.spline, 10, na.pad = T))




overall.uhc.spaced.vals.8$overall.spline <- NULL
severe.uhc.spaced.vals.8$severe.spline <- NULL
extreme.uhc.spaced.vals.8$extreme.spline <- NULL


overall.uhc.spaced.vals.8 <- data.table(overall.uhc.spaced.vals.8)
severe.uhc.spaced.vals.8 <- data.table(severe.uhc.spaced.vals.8)
extreme.uhc.spaced.vals.8 <- data.table(extreme.uhc.spaced.vals.8)

overall.uhc.spaced.vals.8 <- overall.uhc.spaced.vals.8[!is.na(roll_mean)]
severe.uhc.spaced.vals.8 <- severe.uhc.spaced.vals.8[!is.na(roll_mean)]
extreme.uhc.spaced.vals.8 <- extreme.uhc.spaced.vals.8[!is.na(roll_mean)]

# merging all smoothed splines together
setnames(overall.uhc.spaced.vals.8, "roll_mean", "overall_expected.8")
setnames(severe.uhc.spaced.vals.8, "roll_mean", "severe_expected.8")
setnames(extreme.uhc.spaced.vals.8, "roll_mean", "extreme_expected.8")

wasting.smoothed.splines.8 <- merge(overall.uhc.spaced.vals.8, severe.uhc.spaced.vals.8, by = "uhc_value")
wasting.smoothed.splines.8 <- merge(wasting.smoothed.splines.8, extreme.uhc.spaced.vals.8, by = "uhc_value")

waste.severe.to.overall.scalar.8 <- wasting.smoothed.splines.8[1]$overall_expected.8 / wasting.smoothed.splines.8[1]$severe_expected.8
waste.exreme.to.overall.scalar.8 <- wasting.smoothed.splines.8[1]$overall_expected.8 / wasting.smoothed.splines.8[1]$extreme_expected.8
waste.extreme.to.severe.scalar.8 <- wasting.smoothed.splines.8[1]$severe_expected.8 / wasting.smoothed.splines.8[1]$extreme_expected.8

# columns named as <what they're scaled to match>.scaled.<what they were originally>
wasting.smoothed.splines.8[, overall.scaled.severe.8 := severe_expected.8 * waste.severe.to.overall.scalar.8]
wasting.smoothed.splines.8[, overall.scaled.extreme.8 := extreme_expected.8 * waste.exreme.to.overall.scalar.8]

wasting.smoothed.splines.8[, severe.scaled.overall.8 := overall_expected.8 / waste.severe.to.overall.scalar.8]
wasting.smoothed.splines.8[, severe.scaled.extreme.8 := extreme_expected.8 * waste.extreme.to.severe.scalar.8]

wasting.smoothed.splines.8[, extreme.scaled.overall.8 := overall_expected.8 / waste.exreme.to.overall.scalar.8]
wasting.smoothed.splines.8[, extreme.scaled.severe.8 := severe_expected.8 / waste.extreme.to.severe.scalar.8]







wasting.spline.y.labels <- seq(0, max(wasting.smoothed.splines.8$overall_expected.8), length.out = 6)

wasting.8 <- ggplot() +
  geom_line(data = wasting.smoothed.splines.8, aes(x = uhc_value, y = overall_expected.8), color = "#eda109", size = 2.5) +
  geom_line(data = wasting.smoothed.splines.8, aes(x = uhc_value, y = overall.scaled.severe.8), color = "#e36f10", size = 2.5) +
  geom_line(data = wasting.smoothed.splines.8, aes(x = uhc_value, y = overall.scaled.extreme.8), color = "#bf1313", size = 2.5) +
  scale_y_continuous(breaks = wasting.spline.y.labels, labels = c("-100%", "-80%", "-60%", "-40%", "-20%", "0%")) +
  scale_x_continuous(breaks = c(.2, .4, .8, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        plot.tag = element_text(size = 24, face = "bold")) +
  labs(x = "Universal Health Coverage Index", y = "Relative Decrease", tag = "H")








##### UNDERWEIGHT

# reading in epi-transition output for all stunting severities
overall.underweight.epitrans <- fread("filepath")
severe.underweight.epitrans <- fread("filepath")
extreme.underweight.epitrans <- fread("filepath")


overall.underweight.epitrans[, cgf.type := "WHZ"]
severe.underweight.epitrans[, cgf.type := "WHZ"]
extreme.underweight.epitrans[, cgf.type := "WHZ"]

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


age_weights <- get_age_weights(gbd_round_id = 7)
hierarchy <- get_location_metadata(location_set_id = 35, release_id = 9)


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

waz.epitrans <- merge(waz.epitrans, agg.locs.uhc, by = c("location_id", "year_id"), all.x = T)
waz.epitrans[level <3, uhc:= agg.uhc]
waz.epitrans$agg.uhc <- NULL
waz.epitrans <- waz.epitrans[location_id != 1]


#doing this to help with plotting eventually
waz.epitrans[, loc.sev := paste0(location_id, "_", severity)]




####### sdi of 0.2


# first need to create scaled versions of the three splines and smooth them

overall.waz.spline.2 <- waz.epitrans[sdi == .2 & level > 2 & severity == "Overall", c("uhc", "expected")]
overall.waz.spline.2 <- overall.waz.spline.2[order(uhc),]

severe.waz.spline.2 <- waz.epitrans[sdi == .2 & level > 2 & severity == "Severe", c("uhc", "expected")]
severe.waz.spline.2 <- severe.waz.spline.2[order(uhc),]

extreme.waz.spline.2 <- waz.epitrans[sdi == .2 & level > 2 & severity == "Extreme", c("uhc", "expected")]
extreme.waz.spline.2 <- extreme.waz.spline.2[order(uhc),]

#getting 500 UHC values along this span to smooth out
min.uhc <- min(overall.waz.spline.2$uhc)
max.uhc <- max(overall.waz.spline.2$uhc)
uhc.vals <- seq(from = min.uhc, to = max.uhc, length.out = 500)


#getting the initial overall spline values at those UHC points
overall.uhc.spaced.vals.2 <- lapply(uhc.vals, function(uval){
  
  estimate <- overall.waz.spline.2[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, overall.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial severe spline values at those UHC points
severe.uhc.spaced.vals.2 <- lapply(uhc.vals, function(uval){
  
  estimate <- severe.waz.spline.2[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, severe.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial extreme spline values at those UHC points
extreme.uhc.spaced.vals.2 <- lapply(uhc.vals, function(uval){
  
  estimate <- extreme.waz.spline.2[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, extreme.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#now calculating a rolling average 
overall.uhc.spaced.vals.2 <- overall.uhc.spaced.vals.2 %>% 
  mutate(roll_mean = rollmean(overall.spline, 10, na.pad = T))



severe.uhc.spaced.vals.2 <- severe.uhc.spaced.vals.2 %>% 
  mutate(roll_mean = rollmean(severe.spline, 10, na.pad = T))



extreme.uhc.spaced.vals.2 <- extreme.uhc.spaced.vals.2 %>% 
  mutate(roll_mean = rollmean(extreme.spline, 10, na.pad = T))




overall.uhc.spaced.vals.2$overall.spline <- NULL
severe.uhc.spaced.vals.2$severe.spline <- NULL
extreme.uhc.spaced.vals.2$extreme.spline <- NULL


overall.uhc.spaced.vals.2 <- data.table(overall.uhc.spaced.vals.2)
severe.uhc.spaced.vals.2 <- data.table(severe.uhc.spaced.vals.2)
extreme.uhc.spaced.vals.2 <- data.table(extreme.uhc.spaced.vals.2)

overall.uhc.spaced.vals.2 <- overall.uhc.spaced.vals.2[!is.na(roll_mean)]
severe.uhc.spaced.vals.2 <- severe.uhc.spaced.vals.2[!is.na(roll_mean)]
extreme.uhc.spaced.vals.2 <- extreme.uhc.spaced.vals.2[!is.na(roll_mean)]

# merging all smoothed splines together
setnames(overall.uhc.spaced.vals.2, "roll_mean", "overall_expected.2")
setnames(severe.uhc.spaced.vals.2, "roll_mean", "severe_expected.2")
setnames(extreme.uhc.spaced.vals.2, "roll_mean", "extreme_expected.2")

underweight.smoothed.splines.2 <- merge(overall.uhc.spaced.vals.2, severe.uhc.spaced.vals.2, by = "uhc_value")
underweight.smoothed.splines.2 <- merge(underweight.smoothed.splines.2, extreme.uhc.spaced.vals.2, by = "uhc_value")

under.severe.to.overall.scalar.2 <- underweight.smoothed.splines.2[1]$overall_expected.2 / underweight.smoothed.splines.2[1]$severe_expected.2
under.exreme.to.overall.scalar.2 <- underweight.smoothed.splines.2[1]$overall_expected.2 / underweight.smoothed.splines.2[1]$extreme_expected.2
under.extreme.to.severe.scalar.2 <- underweight.smoothed.splines.2[1]$severe_expected.2 / underweight.smoothed.splines.2[1]$extreme_expected.2

# columns named as <what they're scaled to match>.scaled.<what they were originally>
underweight.smoothed.splines.2[, overall.scaled.severe.2 := severe_expected.2 * under.severe.to.overall.scalar.2]
underweight.smoothed.splines.2[, overall.scaled.extreme.2 := extreme_expected.2 * under.exreme.to.overall.scalar.2]

underweight.smoothed.splines.2[, severe.scaled.overall.2 := overall_expected.2 / under.severe.to.overall.scalar.2]
underweight.smoothed.splines.2[, severe.scaled.extreme.2 := extreme_expected.2 * under.extreme.to.severe.scalar.2]

underweight.smoothed.splines.2[, extreme.scaled.overall.2 := overall_expected.2 / under.exreme.to.overall.scalar.2]
underweight.smoothed.splines.2[, extreme.scaled.severe.2 := severe_expected.2 / under.extreme.to.severe.scalar.2]







underweight.spline.y.labels <- seq(0, max(underweight.smoothed.splines.2$overall_expected.2), length.out = 6)

underweight.2 <- ggplot() +
  geom_line(data = underweight.smoothed.splines.2, aes(x = uhc_value, y = overall_expected.2), color = "#eda109", size = 2.5) +
  geom_line(data = underweight.smoothed.splines.2, aes(x = uhc_value, y = overall.scaled.severe.2), color = "#e36f10", size = 2.5) +
  geom_line(data = underweight.smoothed.splines.2, aes(x = uhc_value, y = overall.scaled.extreme.2), color = "#bf1313", size = 2.5) +
  scale_y_continuous(breaks = underweight.spline.y.labels, labels = c("-100%", "-80%", "-60%", "-40%", "-20%", "0%")) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        plot.tag = element_text(size = 24, face = "bold")) +
  labs(x = "Universal Health Coverage Index", y = "Relative Decrease", tag = "I")






####### sdi of 0.4


# first need to create scaled versions of the three splines and smooth them

overall.waz.spline.4 <- waz.epitrans[sdi == .4 & level > 2 & severity == "Overall", c("uhc", "expected")]
overall.waz.spline.4 <- overall.waz.spline.4[order(uhc),]

severe.waz.spline.4 <- waz.epitrans[sdi == .4 & level > 2 & severity == "Severe", c("uhc", "expected")]
severe.waz.spline.4 <- severe.waz.spline.4[order(uhc),]

extreme.waz.spline.4 <- waz.epitrans[sdi == .4 & level > 2 & severity == "Extreme", c("uhc", "expected")]
extreme.waz.spline.4 <- extreme.waz.spline.4[order(uhc),]

#getting 500 UHC values along this span to smooth out
min.uhc <- min(overall.waz.spline.4$uhc)
max.uhc <- max(overall.waz.spline.4$uhc)
uhc.vals <- seq(from = min.uhc, to = max.uhc, length.out = 500)


#getting the initial overall spline values at those UHC points
overall.uhc.spaced.vals.4 <- lapply(uhc.vals, function(uval){
  
  estimate <- overall.waz.spline.4[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, overall.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial severe spline values at those UHC points
severe.uhc.spaced.vals.4 <- lapply(uhc.vals, function(uval){
  
  estimate <- severe.waz.spline.4[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, severe.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial extreme spline values at those UHC points
extreme.uhc.spaced.vals.4 <- lapply(uhc.vals, function(uval){
  
  estimate <- extreme.waz.spline.4[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, extreme.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#now calculating a rolling average 
overall.uhc.spaced.vals.4 <- overall.uhc.spaced.vals.4 %>% 
  mutate(roll_mean = rollmean(overall.spline, 10, na.pad = T))



severe.uhc.spaced.vals.4 <- severe.uhc.spaced.vals.4 %>% 
  mutate(roll_mean = rollmean(severe.spline, 10, na.pad = T))



extreme.uhc.spaced.vals.4 <- extreme.uhc.spaced.vals.4 %>% 
  mutate(roll_mean = rollmean(extreme.spline, 10, na.pad = T))




overall.uhc.spaced.vals.4$overall.spline <- NULL
severe.uhc.spaced.vals.4$severe.spline <- NULL
extreme.uhc.spaced.vals.4$extreme.spline <- NULL


overall.uhc.spaced.vals.4 <- data.table(overall.uhc.spaced.vals.4)
severe.uhc.spaced.vals.4 <- data.table(severe.uhc.spaced.vals.4)
extreme.uhc.spaced.vals.4 <- data.table(extreme.uhc.spaced.vals.4)

overall.uhc.spaced.vals.4 <- overall.uhc.spaced.vals.4[!is.na(roll_mean)]
severe.uhc.spaced.vals.4 <- severe.uhc.spaced.vals.4[!is.na(roll_mean)]
extreme.uhc.spaced.vals.4 <- extreme.uhc.spaced.vals.4[!is.na(roll_mean)]

# merging all smoothed splines together
setnames(overall.uhc.spaced.vals.4, "roll_mean", "overall_expected.4")
setnames(severe.uhc.spaced.vals.4, "roll_mean", "severe_expected.4")
setnames(extreme.uhc.spaced.vals.4, "roll_mean", "extreme_expected.4")

underweight.smoothed.splines.4 <- merge(overall.uhc.spaced.vals.4, severe.uhc.spaced.vals.4, by = "uhc_value")
underweight.smoothed.splines.4 <- merge(underweight.smoothed.splines.4, extreme.uhc.spaced.vals.4, by = "uhc_value")

under.severe.to.overall.scalar.4 <- underweight.smoothed.splines.4[1]$overall_expected.4 / underweight.smoothed.splines.4[1]$severe_expected.4
under.exreme.to.overall.scalar.4 <- underweight.smoothed.splines.4[1]$overall_expected.4 / underweight.smoothed.splines.4[1]$extreme_expected.4
under.extreme.to.severe.scalar.4 <- underweight.smoothed.splines.4[1]$severe_expected.4 / underweight.smoothed.splines.4[1]$extreme_expected.4

# columns named as <what they're scaled to match>.scaled.<what they were originally>
underweight.smoothed.splines.4[, overall.scaled.severe.4 := severe_expected.4 * under.severe.to.overall.scalar.4]
underweight.smoothed.splines.4[, overall.scaled.extreme.4 := extreme_expected.4 * under.exreme.to.overall.scalar.4]

underweight.smoothed.splines.4[, severe.scaled.overall.4 := overall_expected.4 / under.severe.to.overall.scalar.4]
underweight.smoothed.splines.4[, severe.scaled.extreme.4 := extreme_expected.4 * under.extreme.to.severe.scalar.4]

underweight.smoothed.splines.4[, extreme.scaled.overall.4 := overall_expected.4 / under.exreme.to.overall.scalar.4]
underweight.smoothed.splines.4[, extreme.scaled.severe.4 := severe_expected.4 / under.extreme.to.severe.scalar.4]







underweight.spline.y.labels <- seq(0, max(underweight.smoothed.splines.4$overall_expected.4), length.out = 6)

underweight.4 <- ggplot() +
  geom_line(data = underweight.smoothed.splines.4, aes(x = uhc_value, y = overall_expected.4), color = "#eda109", size = 2.5) +
  geom_line(data = underweight.smoothed.splines.4, aes(x = uhc_value, y = overall.scaled.severe.4), color = "#e36f10", size = 2.5) +
  geom_line(data = underweight.smoothed.splines.4, aes(x = uhc_value, y = overall.scaled.extreme.4), color = "#bf1313", size = 2.5) +
  scale_y_continuous(breaks = underweight.spline.y.labels, labels = c("-100%", "-80%", "-60%", "-40%", "-20%", "0%")) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        plot.tag = element_text(size = 24, face = "bold")) +
  labs(x = "Universal Health Coverage Index", y = "Relative Decrease", tag = "J")








####### sdi of 0.6


# first need to create scaled versions of the three splines and smooth them

overall.waz.spline.6 <- waz.epitrans[sdi == .6 & level > 2 & severity == "Overall", c("uhc", "expected")]
overall.waz.spline.6 <- overall.waz.spline.6[order(uhc),]

severe.waz.spline.6 <- waz.epitrans[sdi == .6 & level > 2 & severity == "Severe", c("uhc", "expected")]
severe.waz.spline.6 <- severe.waz.spline.6[order(uhc),]

extreme.waz.spline.6 <- waz.epitrans[sdi == .6 & level > 2 & severity == "Extreme", c("uhc", "expected")]
extreme.waz.spline.6 <- extreme.waz.spline.6[order(uhc),]

#getting 500 UHC values along this span to smooth out
min.uhc <- min(overall.waz.spline.6$uhc)
max.uhc <- max(overall.waz.spline.6$uhc)
uhc.vals <- seq(from = min.uhc, to = max.uhc, length.out = 500)


#getting the initial overall spline values at those UHC points
overall.uhc.spaced.vals.6 <- lapply(uhc.vals, function(uval){
  
  estimate <- overall.waz.spline.6[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, overall.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial severe spline values at those UHC points
severe.uhc.spaced.vals.6 <- lapply(uhc.vals, function(uval){
  
  estimate <- severe.waz.spline.6[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, severe.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial extreme spline values at those UHC points
extreme.uhc.spaced.vals.6 <- lapply(uhc.vals, function(uval){
  
  estimate <- extreme.waz.spline.6[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, extreme.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#now calculating a rolling average 
overall.uhc.spaced.vals.6 <- overall.uhc.spaced.vals.6 %>% 
  mutate(roll_mean = rollmean(overall.spline, 10, na.pad = T))



severe.uhc.spaced.vals.6 <- severe.uhc.spaced.vals.6 %>% 
  mutate(roll_mean = rollmean(severe.spline, 10, na.pad = T))



extreme.uhc.spaced.vals.6 <- extreme.uhc.spaced.vals.6 %>% 
  mutate(roll_mean = rollmean(extreme.spline, 10, na.pad = T))




overall.uhc.spaced.vals.6$overall.spline <- NULL
severe.uhc.spaced.vals.6$severe.spline <- NULL
extreme.uhc.spaced.vals.6$extreme.spline <- NULL


overall.uhc.spaced.vals.6 <- data.table(overall.uhc.spaced.vals.6)
severe.uhc.spaced.vals.6 <- data.table(severe.uhc.spaced.vals.6)
extreme.uhc.spaced.vals.6 <- data.table(extreme.uhc.spaced.vals.6)

overall.uhc.spaced.vals.6 <- overall.uhc.spaced.vals.6[!is.na(roll_mean)]
severe.uhc.spaced.vals.6 <- severe.uhc.spaced.vals.6[!is.na(roll_mean)]
extreme.uhc.spaced.vals.6 <- extreme.uhc.spaced.vals.6[!is.na(roll_mean)]

# merging all smoothed splines together
setnames(overall.uhc.spaced.vals.6, "roll_mean", "overall_expected.6")
setnames(severe.uhc.spaced.vals.6, "roll_mean", "severe_expected.6")
setnames(extreme.uhc.spaced.vals.6, "roll_mean", "extreme_expected.6")

underweight.smoothed.splines.6 <- merge(overall.uhc.spaced.vals.6, severe.uhc.spaced.vals.6, by = "uhc_value")
underweight.smoothed.splines.6 <- merge(underweight.smoothed.splines.6, extreme.uhc.spaced.vals.6, by = "uhc_value")

under.severe.to.overall.scalar.6 <- underweight.smoothed.splines.6[1]$overall_expected.6 / underweight.smoothed.splines.6[1]$severe_expected.6
under.exreme.to.overall.scalar.6 <- underweight.smoothed.splines.6[1]$overall_expected.6 / underweight.smoothed.splines.6[1]$extreme_expected.6
under.extreme.to.severe.scalar.6 <- underweight.smoothed.splines.6[1]$severe_expected.6 / underweight.smoothed.splines.6[1]$extreme_expected.6

# columns named as <what they're scaled to match>.scaled.<what they were originally>
underweight.smoothed.splines.6[, overall.scaled.severe.6 := severe_expected.6 * under.severe.to.overall.scalar.6]
underweight.smoothed.splines.6[, overall.scaled.extreme.6 := extreme_expected.6 * under.exreme.to.overall.scalar.6]

underweight.smoothed.splines.6[, severe.scaled.overall.6 := overall_expected.6 / under.severe.to.overall.scalar.6]
underweight.smoothed.splines.6[, severe.scaled.extreme.6 := extreme_expected.6 * under.extreme.to.severe.scalar.6]

underweight.smoothed.splines.6[, extreme.scaled.overall.6 := overall_expected.6 / under.exreme.to.overall.scalar.6]
underweight.smoothed.splines.6[, extreme.scaled.severe.6 := severe_expected.6 / under.extreme.to.severe.scalar.6]







underweight.spline.y.labels <- seq(0, max(underweight.smoothed.splines.6$overall_expected.6), length.out = 6)

underweight.6 <- ggplot() +
  geom_line(data = underweight.smoothed.splines.6, aes(x = uhc_value, y = overall_expected.6), color = "#eda109", size = 2.5) +
  geom_line(data = underweight.smoothed.splines.6, aes(x = uhc_value, y = overall.scaled.severe.6), color = "#e36f10", size = 2.5) +
  geom_line(data = underweight.smoothed.splines.6, aes(x = uhc_value, y = overall.scaled.extreme.6), color = "#bf1313", size = 2.5) +
  scale_y_continuous(breaks = underweight.spline.y.labels, labels = c("-100%", "-80%", "-60%", "-40%", "-20%", "0%")) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        plot.tag = element_text(size = 24, face = "bold")) +
  labs(x = "Universal Health Coverage Index", y = "Relative Decrease", tag = "K")











####### sdi of 0.8


# first need to create scaled versions of the three splines and smooth them

overall.waz.spline.8 <- waz.epitrans[sdi == .8 & level > 2 & severity == "Overall", c("uhc", "expected")]
overall.waz.spline.8 <- overall.waz.spline.8[order(uhc),]

severe.waz.spline.8 <- waz.epitrans[sdi == .8 & level > 2 & severity == "Severe", c("uhc", "expected")]
severe.waz.spline.8 <- severe.waz.spline.8[order(uhc),]

extreme.waz.spline.8 <- waz.epitrans[sdi == .8 & level > 2 & severity == "Extreme", c("uhc", "expected")]
extreme.waz.spline.8 <- extreme.waz.spline.8[order(uhc),]

#getting 500 UHC values along this span to smooth out
min.uhc <- min(overall.waz.spline.8$uhc)
max.uhc <- max(overall.waz.spline.8$uhc)
uhc.vals <- seq(from = min.uhc, to = max.uhc, length.out = 500)


#getting the initial overall spline values at those UHC points
overall.uhc.spaced.vals.8 <- lapply(uhc.vals, function(uval){
  
  estimate <- overall.waz.spline.8[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, overall.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial severe spline values at those UHC points
severe.uhc.spaced.vals.8 <- lapply(uhc.vals, function(uval){
  
  estimate <- severe.waz.spline.8[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, severe.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#getting the initial extreme spline values at those UHC points
extreme.uhc.spaced.vals.8 <- lapply(uhc.vals, function(uval){
  
  estimate <- extreme.waz.spline.8[which.min(abs(uval-uhc))]$expected
  
  points <- data.table(uhc_value = uval, extreme.spline = estimate)
  
  return(points)
  
}) %>% rbindlist()

#now calculating a rolling average 
overall.uhc.spaced.vals.8 <- overall.uhc.spaced.vals.8 %>% 
  mutate(roll_mean = rollmean(overall.spline, 10, na.pad = T))



severe.uhc.spaced.vals.8 <- severe.uhc.spaced.vals.8 %>% 
  mutate(roll_mean = rollmean(severe.spline, 10, na.pad = T))



extreme.uhc.spaced.vals.8 <- extreme.uhc.spaced.vals.8 %>% 
  mutate(roll_mean = rollmean(extreme.spline, 10, na.pad = T))




overall.uhc.spaced.vals.8$overall.spline <- NULL
severe.uhc.spaced.vals.8$severe.spline <- NULL
extreme.uhc.spaced.vals.8$extreme.spline <- NULL


overall.uhc.spaced.vals.8 <- data.table(overall.uhc.spaced.vals.8)
severe.uhc.spaced.vals.8 <- data.table(severe.uhc.spaced.vals.8)
extreme.uhc.spaced.vals.8 <- data.table(extreme.uhc.spaced.vals.8)

overall.uhc.spaced.vals.8 <- overall.uhc.spaced.vals.8[!is.na(roll_mean)]
severe.uhc.spaced.vals.8 <- severe.uhc.spaced.vals.8[!is.na(roll_mean)]
extreme.uhc.spaced.vals.8 <- extreme.uhc.spaced.vals.8[!is.na(roll_mean)]

# merging all smoothed splines together
setnames(overall.uhc.spaced.vals.8, "roll_mean", "overall_expected.8")
setnames(severe.uhc.spaced.vals.8, "roll_mean", "severe_expected.8")
setnames(extreme.uhc.spaced.vals.8, "roll_mean", "extreme_expected.8")

underweight.smoothed.splines.8 <- merge(overall.uhc.spaced.vals.8, severe.uhc.spaced.vals.8, by = "uhc_value")
underweight.smoothed.splines.8 <- merge(underweight.smoothed.splines.8, extreme.uhc.spaced.vals.8, by = "uhc_value")

under.severe.to.overall.scalar.8 <- underweight.smoothed.splines.8[1]$overall_expected.8 / underweight.smoothed.splines.8[1]$severe_expected.8
under.exreme.to.overall.scalar.8 <- underweight.smoothed.splines.8[1]$overall_expected.8 / underweight.smoothed.splines.8[1]$extreme_expected.8
under.extreme.to.severe.scalar.8 <- underweight.smoothed.splines.8[1]$severe_expected.8 / underweight.smoothed.splines.8[1]$extreme_expected.8

# columns named as <what they're scaled to match>.scaled.<what they were originally>
underweight.smoothed.splines.8[, overall.scaled.severe.8 := severe_expected.8 * under.severe.to.overall.scalar.8]
underweight.smoothed.splines.8[, overall.scaled.extreme.8 := extreme_expected.8 * under.exreme.to.overall.scalar.8]

underweight.smoothed.splines.8[, severe.scaled.overall.8 := overall_expected.8 / under.severe.to.overall.scalar.8]
underweight.smoothed.splines.8[, severe.scaled.extreme.8 := extreme_expected.8 * under.extreme.to.severe.scalar.8]

underweight.smoothed.splines.8[, extreme.scaled.overall.8 := overall_expected.8 / under.exreme.to.overall.scalar.8]
underweight.smoothed.splines.8[, extreme.scaled.severe.8 := severe_expected.8 / under.extreme.to.severe.scalar.8]







underweight.spline.y.labels <- seq(0, max(underweight.smoothed.splines.8$overall_expected.8), length.out = 6)

underweight.8 <- ggplot() +
  geom_line(data = underweight.smoothed.splines.8, aes(x = uhc_value, y = overall_expected.8), color = "#eda109", size = 2.5) +
  geom_line(data = underweight.smoothed.splines.8, aes(x = uhc_value, y = overall.scaled.severe.8), color = "#e36f10", size = 2.5) +
  geom_line(data = underweight.smoothed.splines.8, aes(x = uhc_value, y = overall.scaled.extreme.8), color = "#bf1313", size = 2.5) +
  scale_y_continuous(breaks = underweight.spline.y.labels, labels = c("-100%", "-80%", "-60%", "-40%", "-20%", "0%")) +
  scale_x_continuous(breaks = c(.2, .4, .6, .8), labels = c("20", "40", "60", "80"), limits = c(.1, .95)) +
  theme_bw() +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        plot.tag = element_text(size = 24, face = "bold")) +
  labs(x = "Universal Health Coverage Index", y = "Relative Decrease", tag = "L")










##################################################################################################
##################################################################################################









layout <- rbind(c(1, 2,3),
                c(4, 5,6),
                c(7, 8,9),
                c(10,11,12))





appendixj <- plot_grid(arrangeGrob(stunting.2 + labs(x = "", y = ""), wasting.2 + labs(x = "", y = ""), underweight.2 + labs(x = "", y = ""), 
                                          stunting.4 + labs(x = "", y = ""), wasting.4 + labs(x = "", y = ""), underweight.4 + labs(x = "", y = ""),
                                          stunting.6 + labs(x = ""), wasting.6 + labs(x = "", y = ""), underweight.6+ labs(x = "", y = ""), 
                                          stunting.8+ labs(x = "", y = ""), wasting.8 + labs(y = ""), underweight.8+ labs(x = "", y = ""),
                                          nrow=4, ncol = 3, widths = c(1, 1, 1), heights = c(1, 1, 1, 1), layout_matrix = layout))





#Severe CGF Epi Transition Plots
pdf("filepath", height = 15, width = 14)



print(appendixj)


dev.off()


















