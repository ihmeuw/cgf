
library(data.table)
library(cowplot, lib.loc = "filepath")
library(plyr)
library(dplyr)
library(ggplot2)
library("ggpubr", lib.loc = "filepath")

invisible(sapply(list.files("filepath", full.names = T), source))




source("filepath")

source.counts <- fread("filepath")

loc.met <- get_location_metadata(location_set_id = 35, release_id = 9)

loc.met.ihme.locs <- loc.met[,c("location_id", "ihme_loc_id")]

source.counts <- merge(source.counts, loc.met.ihme.locs, by = "location_id")

stunting.source.counts <- source.counts[rei_id == 241]
wasting.source.counts <- source.counts[rei_id == 240]
underweight.source.counts <- source.counts[rei_id == 94]

stunting.source.counts[, total.sources := sum(source_count), by = "location_id"]
wasting.source.counts[, total.sources := sum(source_count), by = "location_id"]
underweight.source.counts[, total.sources := sum(source_count), by = "location_id"]








map_national_and_subnational(data = stunting.source.counts, map.var = "total.sources", 
                             plot.title = "Stunting (HAZ) Sources in GBD 2020", scale = "cont", 
                             legend.title = "Stunting Sources", subnat.style = "facets", subnat.level = "all",
                             show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                             col.rev = FALSE, na.col = 'grey', 
                             breaks = seq(from = 0, to = 36, by = 4),
                             outline.col = "black", col = "YlGnBu", output.path = "filepath")



map_national_and_subnational(data = wasting.source.counts, map.var = "total.sources", 
                             plot.title = "Wasting (WHZ) Sources in GBD 2020", scale = "cont", 
                             legend.title = "Wasting Sources", subnat.style = "facets", subnat.level = "all",
                             show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                             col.rev = FALSE, na.col = 'grey', 
                             breaks = seq(from = 0, to = 33, by = 3),
                             outline.col = "black", col = "YlGnBu", output.path = "filepath")



map_national_and_subnational(data = underweight.source.counts, map.var = "total.sources", 
                             plot.title = "Underweight (WAZ) Sources in GBD 2020", scale = "cont", 
                             legend.title = "Underweight Sources", subnat.style = "facets", subnat.level = 'all',
                             show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                             col.rev = FALSE, na.col = 'grey', 
                             breaks = seq(from = 0, to = 36, by = 4),
                             outline.col = "black", col = "YlGnBu", output.path = "filepath")







stunting.final <- stunting.source.counts[!duplicated(stunting.source.counts$location_id)]
















source("filepath")





loc.met <- get_location_metadata(location_set_id = 35, release_id = 9)



map_regional_labels(data = loc.met[level > 2], map.var = "region_id",
                    plot.title = "Countries in each Region", scale = "cat",
                    legend.title = "Regions", limits = c(4, 6, 15, 28, 35, 50, 60, 68, 71, 80, 98, 101, 110, 122, 128, 137, 140, 162, 170, 180, 195, 200),
                    labels = c("East Asia", "Southeast Asia", "Oceania", "Central Asia", "Central Europe", "Eastern Europe", 
                               "High-income Asia Pacific", "Australasia", "Western Europe", "Southern Latin America", 
                               "High-income North America", "Caribbean", "Andean Latin America", "Central Latin America", 
                               "Tropical Latin America", "North Africa and Middle East", "South Asia", 
                               "Central Sub-Saharan Africa", "Eastern Sub-Saharan Africa", "Southern Sub-Saharan Africa", 
                               "Western Sub-Saharan Africa"), output.path = 'filepath', col = "Paired", subnat = FALSE)


map_regional_labels(data = loc.met[level > 2], map.var = "super_region_id",
                    plot.title = "Countries in each Super Region", scale = "cat",
                    legend.title = "Super Regions", limits = c(2, 8, 32, 65, 104, 138, 160, 170),
                    labels = c("Southeast Asia, East Asia, and Oceania",
                               "Central Europe, Eastern Europe, and Central Asia",
                               "High-income",
                               "Latin America and Caribbean",
                               "North Africa and Middle East",
                               "South Asia",
                               "Sub-Saharan Africa"), output.path = 'filepath', col = "Dark2", subnat = FALSE)




