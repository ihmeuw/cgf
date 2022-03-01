
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
"%notlike%" <- Negate("%like%")
"%ni%" <- Negate("%in%")


# loading populations

populations <- get_population(gbd_round_id = 7, decomp_step = 'iterative', location_id = 'all', location_set_id = 35, 
                              year_id = c(1990:2020), age_group_id = c(1, 2, 3, 388, 389, 238, 34, 22), sex_id = c(1, 2, 3))
populations$run_id <- NULL


# setting id vars to use in melts for this section
id.vars <- c("metric_id", "age_group_id", "location_id", "measure_id", "modelable_entity_id", "sex_id", "year_id", "model_version_id")


#location data 
loc.met <- get_location_metadata(release_id = 9, location_set_id = 35)
figures.loc.met <- get_location_metadata(release_id = 9, location_set_id = 91)
figure.locs <- unique(figures.loc.met$location_id)
table.locs <- figures.loc.met[level < 6]$location_id # no England UTLAs in tables









##########################################
# STUNTING 
##########################################








# prepping overall (moderate) stunting
stunting.rate.compare.overall <- get_draws("modelable_entity_id", 10556, year_id=c(1990,2000,2010,2020),
                                           source="epi", gbd_round_id=7, decomp_step="iterative",
                                           age_group_id = c(1), sex_id = c(3), location_id = figure.locs)

stunting.rate.compare.overall <- melt(stunting.rate.compare.overall, id.vars = id.vars)

write.csv(stunting.rate.compare.overall, "filepath", row.names = F)
stunting.rate.compare.overall <- fread("filepath")




stunting.rate.compare.overall[, mean.val := mean(value), by = c("location_id", "sex_id", "age_group_id", "year_id")]
stunting.rate.compare.overall[, lower.val := quantile(value, probs = .025), by = c("location_id", "sex_id", "age_group_id", "year_id")]
stunting.rate.compare.overall[, upper.val := quantile(value, probs = .975), by = c("location_id", "sex_id", "age_group_id", "year_id")]
stunting.rate.compare.overall[,loc.year := paste0(location_id, "_", year_id)]
stunting.rate.compare.overall.collapsed <- stunting.rate.compare.overall[!duplicated(stunting.rate.compare.overall$loc.year)]
stunting.rate.compare.overall.collapsed$value <- NULL
stunting.rate.compare.overall.collapsed$variable <- NULL
stunting.rate.compare.overall.collapsed$loc.year <- NULL


stunting.rate.compare.overall.format <- dcast(stunting.rate.compare.overall.collapsed, metric_id + age_group_id + location_id + measure_id + modelable_entity_id + sex_id + model_version_id ~ year_id, value.var = c("mean.val", "lower.val", "upper.val"))






# prepping severe stunting
stunting.rate.compare.severe <- get_draws("modelable_entity_id", 8949, year_id=c(1990,2000,2010,2020),
                                         source="epi", gbd_round_id=7, decomp_step="iterative",
                                         age_group_id = c(1), sex_id = c(3), location_id = figure.locs)

stunting.rate.compare.severe <- melt(stunting.rate.compare.severe, id.vars = id.vars)

write.csv(stunting.rate.compare.severe, "filepath", row.names = F)
stunting.rate.compare.severe <- fread("filepath")




stunting.rate.compare.severe[, mean.val := mean(value), by = c("location_id", "sex_id", "age_group_id", "year_id")]
stunting.rate.compare.severe[, lower.val := quantile(value, probs = .025), by = c("location_id", "sex_id", "age_group_id", "year_id")]
stunting.rate.compare.severe[, upper.val := quantile(value, probs = .975), by = c("location_id", "sex_id", "age_group_id", "year_id")]
stunting.rate.compare.severe[,loc.year := paste0(location_id, "_", year_id)]
stunting.rate.compare.severe.collapsed <- stunting.rate.compare.severe[!duplicated(stunting.rate.compare.severe$loc.year)]
stunting.rate.compare.severe.collapsed$value <- NULL
stunting.rate.compare.severe.collapsed$variable <- NULL
stunting.rate.compare.severe.collapsed$loc.year <- NULL


stunting.rate.compare.severe.format <- dcast(stunting.rate.compare.severe.collapsed, metric_id + age_group_id + location_id + measure_id + modelable_entity_id + sex_id + model_version_id ~ year_id, value.var = c("mean.val", "lower.val", "upper.val"))







# prepping extreme stunting
stunting.rate.compare.extreme <- get_draws("modelable_entity_id", 26941, year_id=c(1990,2000,2010,2020),
                                          source="epi", gbd_round_id=7, decomp_step="iterative",
                                          age_group_id = c(1), sex_id = c(3), location_id = figure.locs)

stunting.rate.compare.extreme <- melt(stunting.rate.compare.extreme, id.vars = id.vars)

write.csv(stunting.rate.compare.extreme, "filepath", row.names = F)
stunting.rate.compare.extreme <- fread("filepath")





stunting.rate.compare.extreme[, mean.val := mean(value), by = c("location_id", "sex_id", "age_group_id", "year_id")]
stunting.rate.compare.extreme[, lower.val := quantile(value, probs = .025), by = c("location_id", "sex_id", "age_group_id", "year_id")]
stunting.rate.compare.extreme[, upper.val := quantile(value, probs = .975), by = c("location_id", "sex_id", "age_group_id", "year_id")]
stunting.rate.compare.extreme[,loc.year := paste0(location_id, "_", year_id)]
stunting.rate.compare.extreme.collapsed <- stunting.rate.compare.extreme[!duplicated(stunting.rate.compare.extreme$loc.year)]
stunting.rate.compare.extreme.collapsed$value <- NULL
stunting.rate.compare.extreme.collapsed$variable <- NULL
stunting.rate.compare.extreme.collapsed$loc.year <- NULL


stunting.rate.compare.extreme.format <- dcast(stunting.rate.compare.extreme.collapsed, metric_id + age_group_id + location_id + measure_id + modelable_entity_id + sex_id + model_version_id ~ year_id, value.var = c("mean.val", "lower.val", "upper.val"))









# subsetting to necessary locations for tables, will use all locations for maps
overall.stunt.table <- stunting.rate.compare.overall.format[location_id %in% table.locs]
severe.stunt.table <- stunting.rate.compare.severe.format[location_id %in% table.locs]
extreme.stunt.table <- stunting.rate.compare.extreme.format[location_id %in% table.locs]

#merging on location names for the table
loc.met.names <- loc.met[, c("location_id", "location_name")]
overall.stunt.table <- merge(overall.stunt.table, loc.met.names, by = "location_id")
severe.stunt.table <- merge(severe.stunt.table, loc.met.names, by = "location_id")
extreme.stunt.table <- merge(extreme.stunt.table, loc.met.names, by = "location_id")


#bringing in location heirarchy just so I can order locations correctly
loc.info <- loc.met[, c("location_name", "region_name", "super_region_name", "path_to_top_parent", "location_id")]
overall.stunt.table <- merge(loc.info, overall.stunt.table, by = c("location_id", "location_name"))
severe.stunt.table <- merge(loc.info, severe.stunt.table, by = c("location_id", "location_name"))
extreme.stunt.table <- merge(loc.info, extreme.stunt.table, by = c("location_id", "location_name"))

#ordering locations now
overall.stunt.table <- overall.stunt.table[order(super_region_name, region_name, path_to_top_parent )]
severe.stunt.table <- severe.stunt.table[order(super_region_name, region_name, path_to_top_parent )]
extreme.stunt.table <- extreme.stunt.table[order(super_region_name, region_name, path_to_top_parent )]



#creating the dataframe that actually becomes the table
#formatting here is very important because it rounds ALL decimals to 2 places after decimal and places the uncertainty interval below the mean estimate
stunting.final.table <- data.table(location_id = overall.stunt.table$location_id,
                                   location_name = overall.stunt.table$location_name,
                                   mod.stunt.1990 = paste(paste0("   ",format(round((overall.stunt.table$mean.val_1990 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((overall.stunt.table$lower.val_1990 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                      format(round((overall.stunt.table$upper.val_1990 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                   
                                   mod.stunt.2000 = paste(paste0("   ",format(round((overall.stunt.table$mean.val_2000 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((overall.stunt.table$lower.val_2000 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                      format(round((overall.stunt.table$upper.val_2000 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                   
                                   mod.stunt.2010 = paste(paste0("   ",format(round((overall.stunt.table$mean.val_2010 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((overall.stunt.table$lower.val_2010 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                      format(round((overall.stunt.table$upper.val_2010 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                   
                                   mod.stunt.2020 = paste(paste0("   ",format(round((overall.stunt.table$mean.val_2020 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((overall.stunt.table$lower.val_2020 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                      format(round((overall.stunt.table$upper.val_2020 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                   
                                   sev.stunt.1990 = paste(paste0("   ",format(round((severe.stunt.table$mean.val_1990 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((severe.stunt.table$lower.val_1990 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                     format(round((severe.stunt.table$upper.val_1990 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                   
                                   sev.stunt.2000 = paste(paste0("   ",format(round((severe.stunt.table$mean.val_2000 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((severe.stunt.table$lower.val_2000 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                     format(round((severe.stunt.table$upper.val_2000 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                   
                                   sev.stunt.2010 = paste(paste0("   ",format(round((severe.stunt.table$mean.val_2010 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((severe.stunt.table$lower.val_2010 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                     format(round((severe.stunt.table$upper.val_2010 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                   
                                   sev.stunt.2020 = paste(paste0("   ",format(round((severe.stunt.table$mean.val_2020 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((severe.stunt.table$lower.val_2020 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                     format(round((severe.stunt.table$upper.val_2020 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                   
                                   ex.stunt.1990 = paste(paste0("   ",format(round((extreme.stunt.table$mean.val_1990 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((extreme.stunt.table$lower.val_1990 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                     format(round((extreme.stunt.table$upper.val_1990 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                   
                                   ex.stunt.2000 = paste(paste0("   ",format(round((extreme.stunt.table$mean.val_2000 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((extreme.stunt.table$lower.val_2000 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                     format(round((extreme.stunt.table$upper.val_2000 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                   
                                   ex.stunt.2010 = paste(paste0("   ",format(round((extreme.stunt.table$mean.val_2010 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((extreme.stunt.table$lower.val_2010 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                     format(round((extreme.stunt.table$upper.val_2010 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                   
                                   ex.stunt.2020 = paste(paste0("   ",format(round((extreme.stunt.table$mean.val_2020 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((extreme.stunt.table$lower.val_2020 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                     format(round((extreme.stunt.table$upper.val_2020 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n")
                                   
)



#so we can know how to indent location names
sr.ids <- loc.met[level == 1]$location_id #super regions
r.ids <- loc.met[level == 2]$location_id  #regions
country.ids <- loc.met[level == 3]$location_id #countries
uk.ids <- c(433, 434, 4749, 4636) # four nations of the UK just in case
subnat.ids <- loc.met[level == 4 & location_id %ni% c(433, 434, 4749, 4636)]$location_id # all subnationals that would be reported except UK nations
uk.subnat.ids <- loc.met[level == 5]$location_id #Subnats of the UK that are NOT the 4 nations


#doing some custom edits on places with particularly long names
stunting.final.table[location_id == 31, location_name := "Central Europe, Eastern Europe,\n  and Central Asia"]
stunting.final.table[location_id == 4, location_name := "Southeast Asia, East Asia,\n  and Oceania"]

#indenting names based on their level in the location heirarchy
stunting.final.table[location_id %in% sr.ids, location_name := paste0("  ", location_name)]
stunting.final.table[location_id %in% r.ids, location_name := paste0("    ", location_name)]
stunting.final.table[location_id %in% country.ids, location_name := paste0("      ", location_name)]
stunting.final.table[location_id %in% uk.ids, location_name := paste0("        ", location_name)]
stunting.final.table[location_id %in% subnat.ids, location_name := paste0("         ", location_name)]
stunting.final.table[location_id %in% uk.subnat.ids, location_name := paste0("           ", location_name)]


#now removing location_id from the table
stunting.final.table$location_id <- NULL

library(ggpubr)
library(grid)
library(gridExtra)
library(gtable)


#telling the table to left align location names (first column), 
#then telling the table to center all other columns. 
#Adjust ncol and nrow to the number of cols and rows in your table, with a correspoinding length of numbers before this
hj <- matrix(c(0.01, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5), ncol=13, nrow=nrow(stunting.final.table),byrow=TRUE)
x <- matrix(c(0.01, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5), ncol=13, nrow=nrow(stunting.final.table), byrow=TRUE)


# this applies those left aligns and center aligns to the theme of the table, as well as sets the column title adjustments and general base font size for the entire table
tt1 <- ttheme_default(core=list(fg_params=list(hjust = as.vector(hj), 
                                               x = as.vector(x))),
                      colhead=list(fg_params=list(hjust=.5, x=.5)), base_size = 8)


#this actually creates the table, using previously made theme, using new column names
stunting.final.table <- tableGrob(stunting.final.table, rows = NULL, theme = tt1,
                                  cols = c("Location",
                                           "Overall Stunting\nPrevalence 1990",
                                           "Overall Stunting\nPrevalence 2000", 
                                           "Overall Stunting\nPrevalence 2010",
                                           "Overall Stunting\nPrevalence 2020",
                                           "Severe Stunting\nPrevalence 1990",
                                           "Severe Stunting\nPrevalence 2000",
                                           "Severe Stunting\nPrevalence 2010",
                                           "Severe Stunting\nPrevalence 2020",
                                           "Extreme Stunting\nPrevalence 1990",
                                           "Extreme Stunting\nPrevalence 2000",
                                           "Extreme Stunting\nPrevalence 2010",
                                           "Extreme Stunting\nPrevalence 2020"))



#here is where we're preparing to loop through and adjust the aesthetics for all cells that are higher in the location heirarchy to make them bold or bold.italic and also bigger
param.setup.table <- loc.met[location_id %in% table.locs, c("location_id", "location_name", "level", "super_region_name", "region_name", "path_to_top_parent")]
param.setup.table <- param.setup.table[order(super_region_name, region_name, path_to_top_parent )]
param.setup.table <- param.setup.table[, c("location_id", "location_name", "level")]


#making a param map of how I will want to adjust those aesthetics
#first adjust font size
param.setup.table[level == 0, fsize := 11]
param.setup.table[level == 1, fsize := 10]
param.setup.table[level == 2, fsize := 9]
param.setup.table[is.na(fsize), fsize := 8]

#then adjust font type
param.setup.table[level == 0, fface := "bold.italic"]
param.setup.table[level == 1, fface := "bold.italic"]
param.setup.table[level == 2, fface := "bold"]
param.setup.table[level > 3 & location_id %ni% uk.ids, fface := "italic"]
param.setup.table[is.na(fface), fface := "plain"]

#have to add one because the title row doesn't count
param.setup.table[, index := (1:.N) + 1]

#this is how i'll identify which cell in the table I'm actually adjusting
find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}

#this loops through the table that you've already made and adjusts the aesthetics (size and face) for the appropriate rows
for (i in unique(param.setup.table$location_id)) {
  
  loc <- param.setup.table[location_id == i]$location_name
  
  cell.row <- param.setup.table[location_id == i]$index
  
  #the 1 here refers to the first column (which has location names)
  cell.of.interest <- find_cell(stunting.final.table, cell.row, 1, "core-fg")
  
  #assigning font size and face from param map
  ftsize <- param.setup.table[location_id == i]$fsize
  ftface <- param.setup.table[location_id == i]$fface
  
  
  stunting.final.table$grobs[cell.of.interest][[1]][["gp"]] <- gpar(fontsize=ftsize, fontface=ftface)
  
}


#here I tell it how many rows to have per page. I've used 18 rows per page.
#important: s.rows is the start rows - it HAS to start with 2. The end number of 620 HAS to be the total number of rows in your table
#important: e.rows is the end rows - it HAS to start with whatever your by argument is + 1 and go to the total number of rows in your table
s.rows <- seq(from = 2, to  = 620, by = 18)
e.rows <- seq(from = 19, to  = 620, by = 18)
e.rows <- append(e.rows[1:length(e.rows)], 620)

row.param <- data.table(starts = s.rows, ends = e.rows)
row.param[, index := 1:.N]


# you may not need this part, i'm using it for my tables for a pretty custom reason
# this part draws a more solid line between some cells of the table
# I use this to make the lines between stunting/wasting/underweight more distinct, whereas the table cell lines between 
# cells of the same type of CGF are softer
# If you do want this, this is equipped to put the lines correctly on the page as you've already broken it up. These will be vertical lines
# the l argument at the end determines which cells to put the vertical lines between
for (i in unique(row.param$index)) {
  
  start.val <- row.param[index == i]$starts
  
  end.val <- row.param[index == i]$ends
  
  stunting.final.table <- gtable_add_grob(stunting.final.table,
                                          grobs = segmentsGrob(x0 = unit(0,"npc"),y0 = unit(0,"npc"),x1 = unit(0,"npc"),y1 = unit(1,"npc"),gp = gpar(lwd = 1)),
                                          t = start.val, b = end.val, l = 2)
  
  stunting.final.table <- gtable_add_grob(stunting.final.table,
                                          grobs = segmentsGrob(x0 = unit(0,"npc"),y0 = unit(0,"npc"),x1 = unit(0,"npc"),y1 = unit(1,"npc"),gp = gpar(lwd = 1)),
                                          t = start.val, b = end.val, l = 6)
  
  stunting.final.table <- gtable_add_grob(stunting.final.table,
                                          grobs = segmentsGrob(x0 = unit(0,"npc"),y0 = unit(0,"npc"),x1 = unit(0,"npc"),y1 = unit(1,"npc"),gp = gpar(lwd = 1)),
                                          t = start.val, b = end.val, l = 10)
  
  
}






#this allows you to print a pdf with special characters. The normal PDF call messes up special characters
library(Cairo)



#IMPORTANT: this pdf call will print out a pdf, but it will also add 1 blank page at the very end. Easy to remove in Adobe, just flagging that 
cairo_pdf("filepath", height = 9, width = 17.5, onefile = TRUE)
for (i in unique(row.param$index)) {
  
  start.val <- row.param[index == i]$starts
  
  end.val <- row.param[index == i]$ends
  
  grid.draw(stunting.final.table[c(1,start.val:end.val)])
  grid.newpage()
  
  
}
dev.off()




##########################################
# WASTING 
##########################################





# prepping overall (moderate) wasting
wasting.rate.compare.overall <- get_draws("modelable_entity_id", 10558, year_id=c(1990,2000,2010,2020),
                                          source="epi", gbd_round_id=7, decomp_step="iterative",
                                          age_group_id = c(1), sex_id = c(3), location_id = figure.locs)

wasting.rate.compare.overall <- melt(wasting.rate.compare.overall, id.vars = id.vars)

write.csv(wasting.rate.compare.overall, "filepath", row.names = F)
wasting.rate.compare.overall <- fread("filepath")




wasting.rate.compare.overall[, mean.val := mean(value), by = c("location_id", "sex_id", "age_group_id", "year_id")]
wasting.rate.compare.overall[, lower.val := quantile(value, probs = .025), by = c("location_id", "sex_id", "age_group_id", "year_id")]
wasting.rate.compare.overall[, upper.val := quantile(value, probs = .975), by = c("location_id", "sex_id", "age_group_id", "year_id")]
wasting.rate.compare.overall[,loc.year := paste0(location_id, "_", year_id)]
wasting.rate.compare.collapsed <- wasting.rate.compare.overall[!duplicated(wasting.rate.compare.overall$loc.year)]
wasting.rate.compare.collapsed$value <- NULL
wasting.rate.compare.collapsed$variable <- NULL
wasting.rate.compare.collapsed$loc.year <- NULL


wasting.rate.compare.overall.format <- dcast(wasting.rate.compare.collapsed, metric_id + age_group_id + location_id + measure_id + modelable_entity_id + sex_id + model_version_id ~ year_id, value.var = c("mean.val", "lower.val", "upper.val"))







# prepping severe wasting
wasting.rate.compare.severe <- get_draws("modelable_entity_id", 8945, year_id=c(1990,2000,2010,2020),
                                         source="epi", gbd_round_id=7, decomp_step="iterative",
                                         age_group_id = c(1), sex_id = c(3), location_id = figure.locs)

wasting.rate.compare.severe <- melt(wasting.rate.compare.severe, id.vars = id.vars)

write.csv(wasting.rate.compare.severe, "filepath", row.names = F)
wasting.rate.compare.severe <- fread("filepath")




wasting.rate.compare.severe[, mean.val := mean(value), by = c("location_id", "sex_id", "age_group_id", "year_id")]
wasting.rate.compare.severe[, lower.val := quantile(value, probs = .025), by = c("location_id", "sex_id", "age_group_id", "year_id")]
wasting.rate.compare.severe[, upper.val := quantile(value, probs = .975), by = c("location_id", "sex_id", "age_group_id", "year_id")]
wasting.rate.compare.severe[,loc.year := paste0(location_id, "_", year_id)]
wasting.rate.compare.severe.collapsed <- wasting.rate.compare.severe[!duplicated(wasting.rate.compare.severe$loc.year)]
wasting.rate.compare.severe.collapsed$value <- NULL
wasting.rate.compare.severe.collapsed$variable <- NULL
wasting.rate.compare.severe.collapsed$loc.year <- NULL


wasting.rate.compare.severe.format <- dcast(wasting.rate.compare.severe.collapsed, metric_id + age_group_id + location_id + measure_id + modelable_entity_id + sex_id + model_version_id ~ year_id, value.var = c("mean.val", "lower.val", "upper.val"))







# prepping extreme wasting
wasting.rate.compare.extreme <- get_draws("modelable_entity_id", 26943, year_id=c(1990,2000,2010,2020),
                                          source="epi", gbd_round_id=7, decomp_step="iterative",
                                          age_group_id = c(1), sex_id = c(3), location_id = figure.locs)

wasting.rate.compare.extreme <- melt(wasting.rate.compare.extreme, id.vars = id.vars)

write.csv(wasting.rate.compare.extreme, "filepath", row.names = F)
wasting.rate.compare.extreme <- fread("filepath")





wasting.rate.compare.extreme[, mean.val := mean(value), by = c("location_id", "sex_id", "age_group_id", "year_id")]
wasting.rate.compare.extreme[, lower.val := quantile(value, probs = .025), by = c("location_id", "sex_id", "age_group_id", "year_id")]
wasting.rate.compare.extreme[, upper.val := quantile(value, probs = .975), by = c("location_id", "sex_id", "age_group_id", "year_id")]
wasting.rate.compare.extreme[,loc.year := paste0(location_id, "_", year_id)]
wasting.rate.compare.extreme.collapsed <- wasting.rate.compare.extreme[!duplicated(wasting.rate.compare.extreme$loc.year)]
wasting.rate.compare.extreme.collapsed$value <- NULL
wasting.rate.compare.extreme.collapsed$variable <- NULL
wasting.rate.compare.extreme.collapsed$loc.year <- NULL


wasting.rate.compare.extreme.format <- dcast(wasting.rate.compare.extreme.collapsed, metric_id + age_group_id + location_id + measure_id + modelable_entity_id + sex_id + model_version_id ~ year_id, value.var = c("mean.val", "lower.val", "upper.val"))









# subsetting to necessary locations for tables, will use all locations for maps
overall.waste.table <- wasting.rate.compare.overall.format[location_id %in% table.locs]
severe.waste.table <- wasting.rate.compare.severe.format[location_id %in% table.locs]
extreme.waste.table <- wasting.rate.compare.extreme.format[location_id %in% table.locs]

#merging on location names for the table
loc.met.names <- loc.met[, c("location_id", "location_name")]
overall.waste.table <- merge(overall.waste.table, loc.met.names, by = "location_id")
severe.waste.table <- merge(severe.waste.table, loc.met.names, by = "location_id")
extreme.waste.table <- merge(extreme.waste.table, loc.met.names, by = "location_id")


#bringing in location heirarchy just so I can order locations correctly
loc.info <- loc.met[, c("location_name", "region_name", "super_region_name", "path_to_top_parent", "location_id")]
overall.waste.table <- merge(loc.info, overall.waste.table, by = c("location_id", "location_name"))
severe.waste.table <- merge(loc.info, severe.waste.table, by = c("location_id", "location_name"))
extreme.waste.table <- merge(loc.info, extreme.waste.table, by = c("location_id", "location_name"))

#ordering locations now
overall.waste.table <- overall.waste.table[order(super_region_name, region_name, path_to_top_parent )]
severe.waste.table <- severe.waste.table[order(super_region_name, region_name, path_to_top_parent )]
extreme.waste.table <- extreme.waste.table[order(super_region_name, region_name, path_to_top_parent )]



#creating the dataframe that actually becomes the table
#formatting here is very important because it rounds ALL decimals to 2 places after decimal and places the uncertainty interval below the mean estimate
wasting.final.table <- data.table(location_id = overall.waste.table$location_id,
                                  location_name = overall.waste.table$location_name,
                                  mod.waste.1990 = paste(paste0("   ",format(round((overall.waste.table$mean.val_1990 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((overall.waste.table$lower.val_1990 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                     format(round((overall.waste.table$upper.val_1990 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                  
                                  mod.waste.2000 = paste(paste0("   ",format(round((overall.waste.table$mean.val_2000 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((overall.waste.table$lower.val_2000 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                     format(round((overall.waste.table$upper.val_2000 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                  
                                  mod.waste.2010 = paste(paste0("   ",format(round((overall.waste.table$mean.val_2010 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((overall.waste.table$lower.val_2010 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                     format(round((overall.waste.table$upper.val_2010 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                  
                                  mod.waste.2020 = paste(paste0("   ",format(round((overall.waste.table$mean.val_2020 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((overall.waste.table$lower.val_2020 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                     format(round((overall.waste.table$upper.val_2020 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                  
                                  sev.waste.1990 = paste(paste0("   ",format(round((severe.waste.table$mean.val_1990 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((severe.waste.table$lower.val_1990 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                    format(round((severe.waste.table$upper.val_1990 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                  
                                  sev.waste.2000 = paste(paste0("   ",format(round((severe.waste.table$mean.val_2000 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((severe.waste.table$lower.val_2000 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                    format(round((severe.waste.table$upper.val_2000 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                  
                                  sev.waste.2010 = paste(paste0("   ",format(round((severe.waste.table$mean.val_2010 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((severe.waste.table$lower.val_2010 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                    format(round((severe.waste.table$upper.val_2010 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                  
                                  sev.waste.2020 = paste(paste0("   ",format(round((severe.waste.table$mean.val_2020 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((severe.waste.table$lower.val_2020 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                    format(round((severe.waste.table$upper.val_2020 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                  
                                  ex.waste.1990 = paste(paste0("   ",format(round((extreme.waste.table$mean.val_1990 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((extreme.waste.table$lower.val_1990 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                    format(round((extreme.waste.table$upper.val_1990 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                  
                                  ex.waste.2000 = paste(paste0("   ",format(round((extreme.waste.table$mean.val_2000 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((extreme.waste.table$lower.val_2000 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                    format(round((extreme.waste.table$upper.val_2000 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                  
                                  ex.waste.2010 = paste(paste0("   ",format(round((extreme.waste.table$mean.val_2010 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((extreme.waste.table$lower.val_2010 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                    format(round((extreme.waste.table$upper.val_2010 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                  
                                  ex.waste.2020 = paste(paste0("   ",format(round((extreme.waste.table$mean.val_2020 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((extreme.waste.table$lower.val_2020 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                    format(round((extreme.waste.table$upper.val_2020 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n")
                                  
)



#so we can know how to indent location names
sr.ids <- loc.met[level == 1]$location_id
r.ids <- loc.met[level == 2]$location_id
country.ids <- loc.met[level == 3]$location_id
uk.ids <- c(433, 434, 4749, 4636)
subnat.ids <- loc.met[level == 4 & location_id %ni% c(433, 434, 4749, 4636)]$location_id
uk.subnat.ids <- loc.met[level == 5]$location_id


#doing some custom edits on places with particularly long names
wasting.final.table[location_id == 31, location_name := "Central Europe, Eastern Europe,\n  and Central Asia"]
wasting.final.table[location_id == 4, location_name := "Southeast Asia, East Asia,\n  and Oceania"]

#indenting names either 2, 4, or 6 spaces based on their level in the location heirarchy
wasting.final.table[location_id %in% sr.ids, location_name := paste0("  ", location_name)]
wasting.final.table[location_id %in% r.ids, location_name := paste0("    ", location_name)]
wasting.final.table[location_id %in% country.ids, location_name := paste0("      ", location_name)]
wasting.final.table[location_id %in% uk.ids, location_name := paste0("        ", location_name)]
wasting.final.table[location_id %in% subnat.ids, location_name := paste0("         ", location_name)]
wasting.final.table[location_id %in% uk.subnat.ids, location_name := paste0("           ", location_name)]


#now removing location_id from the table
wasting.final.table$location_id <- NULL


#telling the table to left align location names, and then center all other columns. Adjust ncol and nrow to the number of cols and rows in your table
hj <- matrix(c(0.01, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5), ncol=13, nrow=nrow(wasting.final.table),byrow=TRUE)
x <- matrix(c(0.01, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5), ncol=13, nrow=nrow(wasting.final.table), byrow=TRUE)


# this applies those left aligns and center aligns to the theme of the table, as well as sets the column title adjustments and general base font size for the entire table
tt1 <- ttheme_default(core=list(fg_params=list(hjust = as.vector(hj), 
                                               x = as.vector(x))),
                      colhead=list(fg_params=list(hjust=.5, x=.5)), base_size = 8)


#this actually creates the table, using previously made theme, using new column names
wasting.final.table <- tableGrob(wasting.final.table, rows = NULL, theme = tt1,
                                 cols = c("Location",
                                          "Overall Wasting\nPrevalence 1990",
                                          "Overall Wasting\nPrevalence 2000", 
                                          "Overall Wasting\nPrevalence 2010",
                                          "Overall Wasting\nPrevalence 2020",
                                          "Severe Wasting\nPrevalence 1990",
                                          "Severe Wasting\nPrevalence 2000",
                                          "Severe Wasting\nPrevalence 2010",
                                          "Severe Wasting\nPrevalence 2020",
                                          "Extreme Wasting\nPrevalence 1990",
                                          "Extreme Wasting\nPrevalence 2000",
                                          "Extreme Wasting\nPrevalence 2010",
                                          "Extreme Wasting\nPrevalence 2020"))



#here is where we're preparing to loop through and adjust the aesthetics for all cells that are higher in the location heirarchy to make them bold or bold.italic and also bigger
param.setup.table <- loc.met[location_id %in% table.locs, c("location_id", "location_name", "level", "super_region_name", "region_name", "path_to_top_parent")]
param.setup.table <- param.setup.table[order(super_region_name, region_name, path_to_top_parent )]
param.setup.table <- param.setup.table[, c("location_id", "location_name", "level")]


#making a param map of how I will want to adjust those aesthetics
param.setup.table[level == 0, fsize := 11]
param.setup.table[level == 1, fsize := 10]
param.setup.table[level == 2, fsize := 9]

param.setup.table[is.na(fsize), fsize := 8]

param.setup.table[level == 0, fface := "bold.italic"]
param.setup.table[level == 1, fface := "bold.italic"]
param.setup.table[level == 2, fface := "bold"]
param.setup.table[level > 3 & location_id %ni% uk.ids, fface := "italic"]

param.setup.table[is.na(fface), fface := "plain"]


param.setup.table[, index := (1:.N) + 1]

#this is how i'll identify which cell in the table I'm actually adjusting
find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}

for (i in unique(param.setup.table$location_id)) {
  
  loc <- param.setup.table[location_id == i]$location_name
  
  print(loc)
  
  cell.row <- param.setup.table[location_id == i]$index
  
  #the 1 here refers to the first column (which has location names)
  cell.of.interest <- find_cell(stunting.final.table, cell.row, 1, "core-fg")
  
  #assigning font size and face from param map
  ftsize <- param.setup.table[location_id == i]$fsize
  ftface <- param.setup.table[location_id == i]$fface
  
  
  wasting.final.table$grobs[cell.of.interest][[1]][["gp"]] <- gpar(fontsize=ftsize, fontface=ftface)
  
}

# 2, 6, 10

s.rows <- seq(from = 2, to  = 620, by = 18)
e.rows <- seq(from = 19, to  = 620, by = 18)
e.rows <- append(e.rows[1:length(e.rows)], 620)

row.param <- data.table(starts = s.rows, ends = e.rows)
row.param[, index := 1:.N]


for (i in unique(row.param$index)) {
  
  start.val <- row.param[index == i]$starts
  
  end.val <- row.param[index == i]$ends
  
  wasting.final.table <- gtable_add_grob(wasting.final.table,
                                         grobs = segmentsGrob(x0 = unit(0,"npc"),y0 = unit(0,"npc"),x1 = unit(0,"npc"),y1 = unit(1,"npc"),gp = gpar(lwd = 1)),
                                         t = start.val, b = end.val, l = 2)
  
  wasting.final.table <- gtable_add_grob(wasting.final.table,
                                         grobs = segmentsGrob(x0 = unit(0,"npc"),y0 = unit(0,"npc"),x1 = unit(0,"npc"),y1 = unit(1,"npc"),gp = gpar(lwd = 1)),
                                         t = start.val, b = end.val, l = 6)
  
  wasting.final.table <- gtable_add_grob(wasting.final.table,
                                         grobs = segmentsGrob(x0 = unit(0,"npc"),y0 = unit(0,"npc"),x1 = unit(0,"npc"),y1 = unit(1,"npc"),gp = gpar(lwd = 1)),
                                         t = start.val, b = end.val, l = 10)
  
  
}








cairo_pdf("filepath", height = 9, width = 17.5, onefile = TRUE)
for (i in unique(row.param$index)) {
  
  start.val <- row.param[index == i]$starts
  
  end.val <- row.param[index == i]$ends
  
  grid.draw(wasting.final.table[c(1,start.val:end.val)])
  grid.newpage()
  
  
}
dev.off()




##########################################
# UNDERWEIGHT 
##########################################





# prepping overall (moderate) underweight
underweight.rate.compare.overall <- get_draws("modelable_entity_id", 10560, year_id=c(1990,2000,2010,2020),
                                         source="epi", gbd_round_id=7, decomp_step="iterative",
                                         age_group_id = c(1), sex_id = c(3), location_id = figure.locs)

underweight.rate.compare.overall <- melt(underweight.rate.compare.overall, id.vars = id.vars)

write.csv(underweight.rate.compare.overall, "filepath", row.names = F)
underweight.rate.compare.overall <- fread("filepath")




underweight.rate.compare.overall[, mean.val := mean(value), by = c("location_id", "sex_id", "age_group_id", "year_id")]
underweight.rate.compare.overall[, lower.val := quantile(value, probs = .025), by = c("location_id", "sex_id", "age_group_id", "year_id")]
underweight.rate.compare.overall[, upper.val := quantile(value, probs = .975), by = c("location_id", "sex_id", "age_group_id", "year_id")]
underweight.rate.compare.overall[,loc.year := paste0(location_id, "_", year_id)]
underweight.rate.compare.overall.collapsed <- underweight.rate.compare.overall[!duplicated(underweight.rate.compare.overall$loc.year)]
underweight.rate.compare.overall.collapsed$value <- NULL
underweight.rate.compare.overall.collapsed$variable <- NULL
underweight.rate.compare.overall.collapsed$loc.year <- NULL


underweight.rate.compare.overall.format <- dcast(underweight.rate.compare.overall.collapsed, metric_id + age_group_id + location_id + measure_id + modelable_entity_id + sex_id + model_version_id ~ year_id, value.var = c("mean.val", "lower.val", "upper.val"))







# prepping severe underweight
underweight.rate.compare.severe <- get_draws("modelable_entity_id", 2540, year_id=c(1990,2000,2010,2020),
                                        source="epi", gbd_round_id=7, decomp_step="iterative",
                                        age_group_id = c(1), sex_id = c(3), location_id = figure.locs)

underweight.rate.compare.severe <- melt(underweight.rate.compare.severe, id.vars = id.vars)

write.csv(underweight.rate.compare.severe, "filepath", row.names = F)
underweight.rate.compare.severe <- fread("filepath")




underweight.rate.compare.severe[, mean.val := mean(value), by = c("location_id", "sex_id", "age_group_id", "year_id")]
underweight.rate.compare.severe[, lower.val := quantile(value, probs = .025), by = c("location_id", "sex_id", "age_group_id", "year_id")]
underweight.rate.compare.severe[, upper.val := quantile(value, probs = .975), by = c("location_id", "sex_id", "age_group_id", "year_id")]
underweight.rate.compare.severe[,loc.year := paste0(location_id, "_", year_id)]
underweight.rate.compare.severe.collapsed <- underweight.rate.compare.severe[!duplicated(underweight.rate.compare.severe$loc.year)]
underweight.rate.compare.severe.collapsed$value <- NULL
underweight.rate.compare.severe.collapsed$variable <- NULL
underweight.rate.compare.severe.collapsed$loc.year <- NULL


underweight.rate.compare.severe.format <- dcast(underweight.rate.compare.severe.collapsed, metric_id + age_group_id + location_id + measure_id + modelable_entity_id + sex_id + model_version_id ~ year_id, value.var = c("mean.val", "lower.val", "upper.val"))







# prepping extreme underweight
underweight.rate.compare.extreme <- get_draws("modelable_entity_id", 26942, year_id=c(1990,2000,2010,2020),
                                         source="epi", gbd_round_id=7, decomp_step="iterative",
                                         age_group_id = c(1), sex_id = c(3), location_id = figure.locs)

underweight.rate.compare.extreme <- melt(underweight.rate.compare.extreme, id.vars = id.vars)

write.csv(underweight.rate.compare.extreme, "filepath", row.names = F)
underweight.rate.compare.extreme <- fread("filepath")





underweight.rate.compare.extreme[, mean.val := mean(value), by = c("location_id", "sex_id", "age_group_id", "year_id")]
underweight.rate.compare.extreme[, lower.val := quantile(value, probs = .025), by = c("location_id", "sex_id", "age_group_id", "year_id")]
underweight.rate.compare.extreme[, upper.val := quantile(value, probs = .975), by = c("location_id", "sex_id", "age_group_id", "year_id")]
underweight.rate.compare.extreme[,loc.year := paste0(location_id, "_", year_id)]
underweight.rate.compare.extreme.collapsed <- underweight.rate.compare.extreme[!duplicated(underweight.rate.compare.extreme$loc.year)]
underweight.rate.compare.extreme.collapsed$value <- NULL
underweight.rate.compare.extreme.collapsed$variable <- NULL
underweight.rate.compare.extreme.collapsed$loc.year <- NULL


underweight.rate.compare.extreme.format <- dcast(underweight.rate.compare.extreme.collapsed, metric_id + age_group_id + location_id + measure_id + modelable_entity_id + sex_id + model_version_id ~ year_id, value.var = c("mean.val", "lower.val", "upper.val"))









# subsetting to necessary locations for tables, will use all locations for maps
overall.underweight.table <- underweight.rate.compare.overall.format[location_id %in% table.locs]
severe.underweight.table <- underweight.rate.compare.severe.format[location_id %in% table.locs]
extreme.underweight.table <- underweight.rate.compare.extreme.format[location_id %in% table.locs]

#merging on location names for the table
loc.met.names <- loc.met[, c("location_id", "location_name")]
overall.underweight.table <- merge(overall.underweight.table, loc.met.names, by = "location_id")
severe.underweight.table <- merge(severe.underweight.table, loc.met.names, by = "location_id")
extreme.underweight.table <- merge(extreme.underweight.table, loc.met.names, by = "location_id")


#bringing in location heirarchy just so I can order locations correctly
loc.info <- loc.met[, c("location_name", "region_name", "super_region_name", "path_to_top_parent", "location_id")]
overall.underweight.table <- merge(loc.info, overall.underweight.table, by = c("location_id", "location_name"))
severe.underweight.table <- merge(loc.info, severe.underweight.table, by = c("location_id", "location_name"))
extreme.underweight.table <- merge(loc.info, extreme.underweight.table, by = c("location_id", "location_name"))

#ordering locations now
overall.underweight.table <- overall.underweight.table[order(super_region_name, region_name, path_to_top_parent )]
severe.underweight.table <- severe.underweight.table[order(super_region_name, region_name, path_to_top_parent )]
extreme.underweight.table <- extreme.underweight.table[order(super_region_name, region_name, path_to_top_parent )]



#creating the dataframe that actually becomes the table
#formatting here is very important because it rounds ALL decimals to 2 places after decimal and places the uncertainty interval below the mean estimate
underweight.final.table <- data.table(location_id = overall.underweight.table$location_id,
                                      location_name = overall.underweight.table$location_name,
                                      mod.under.1990 = paste(paste0("   ",format(round((overall.underweight.table$mean.val_1990 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((overall.underweight.table$lower.val_1990 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                               format(round((overall.underweight.table$upper.val_1990 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                      
                                      mod.under.2000 = paste(paste0("   ",format(round((overall.underweight.table$mean.val_2000 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((overall.underweight.table$lower.val_2000 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                               format(round((overall.underweight.table$upper.val_2000 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                      
                                      mod.under.2010 = paste(paste0("   ",format(round((overall.underweight.table$mean.val_2010 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((overall.underweight.table$lower.val_2010 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                               format(round((overall.underweight.table$upper.val_2010 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                      
                                      mod.under.2020 = paste(paste0("   ",format(round((overall.underweight.table$mean.val_2020 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((overall.underweight.table$lower.val_2020 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                               format(round((overall.underweight.table$upper.val_2020 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                      
                                      sev.under.1990 = paste(paste0("   ",format(round((severe.underweight.table$mean.val_1990 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((severe.underweight.table$lower.val_1990 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                              format(round((severe.underweight.table$upper.val_1990 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                      
                                      sev.under.2000 = paste(paste0("   ",format(round((severe.underweight.table$mean.val_2000 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((severe.underweight.table$lower.val_2000 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                              format(round((severe.underweight.table$upper.val_2000 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                      
                                      sev.under.2010 = paste(paste0("   ",format(round((severe.underweight.table$mean.val_2010 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((severe.underweight.table$lower.val_2010 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                              format(round((severe.underweight.table$upper.val_2010 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                      
                                      sev.under.2020 = paste(paste0("   ",format(round((severe.underweight.table$mean.val_2020 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((severe.underweight.table$lower.val_2020 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                              format(round((severe.underweight.table$upper.val_2020 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                      
                                      ex.under.1990 = paste(paste0("   ",format(round((extreme.underweight.table$mean.val_1990 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((extreme.underweight.table$lower.val_1990 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                              format(round((extreme.underweight.table$upper.val_1990 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                      
                                      ex.under.2000 = paste(paste0("   ",format(round((extreme.underweight.table$mean.val_2000 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((extreme.underweight.table$lower.val_2000 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                              format(round((extreme.underweight.table$upper.val_2000 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                      
                                      ex.under.2010 = paste(paste0("   ",format(round((extreme.underweight.table$mean.val_2010 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((extreme.underweight.table$lower.val_2010 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                              format(round((extreme.underweight.table$upper.val_2010 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n"),
                                      
                                      ex.under.2020 = paste(paste0("   ",format(round((extreme.underweight.table$mean.val_2020 * 100), digits = 2), nsmall = 2), "%"), paste0("(",format(round((extreme.underweight.table$lower.val_2020 * 100), digits = 2), nsmall = 2), "% - ",
                                                                                                                                                                              format(round((extreme.underweight.table$upper.val_2020 * 100), digits = 2), nsmall = 2), "%)"), sep = "\n")
                                      
)





#so we can know how to indent location names
sr.ids <- loc.met[level == 1]$location_id
r.ids <- loc.met[level == 2]$location_id
country.ids <- loc.met[level == 3]$location_id
uk.ids <- c(433, 434, 4749, 4636)
subnat.ids <- loc.met[level == 4 & location_id %ni% c(433, 434, 4749, 4636)]$location_id
uk.subnat.ids <- loc.met[level == 5]$location_id


#doing some custom edits on places with particularly long names
underweight.final.table[location_id == 31, location_name := "Central Europe, Eastern Europe,\n  and Central Asia"]
underweight.final.table[location_id == 4, location_name := "Southeast Asia, East Asia,\n  and Oceania"]

#indenting names either 2, 4, or 6 spaces based on their level in the location heirarchy
underweight.final.table[location_id %in% sr.ids, location_name := paste0("  ", location_name)]
underweight.final.table[location_id %in% r.ids, location_name := paste0("    ", location_name)]
underweight.final.table[location_id %in% country.ids, location_name := paste0("      ", location_name)]
underweight.final.table[location_id %in% uk.ids, location_name := paste0("        ", location_name)]
underweight.final.table[location_id %in% subnat.ids, location_name := paste0("         ", location_name)]
underweight.final.table[location_id %in% uk.subnat.ids, location_name := paste0("           ", location_name)]


#now removing location_id from the table
underweight.final.table$location_id <- NULL


#telling the table to left align location names, and then center all other columns. Adjust ncol and nrow to the number of cols and rows in your table
hj <- matrix(c(0.01, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5), ncol=13, nrow=nrow(underweight.final.table),byrow=TRUE)
x <- matrix(c(0.01, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5), ncol=13, nrow=nrow(underweight.final.table), byrow=TRUE)


# this applies those left aligns and center aligns to the theme of the table, as well as sets the column title adjustments and general base font size for the entire table
tt1 <- ttheme_default(core=list(fg_params=list(hjust = as.vector(hj), 
                                               x = as.vector(x))),
                      colhead=list(fg_params=list(hjust=.5, x=.5)), base_size = 8)


#this actually creates the table, using previously made theme, using new column names
underweight.final.table <- tableGrob(underweight.final.table, rows = NULL, theme = tt1,
                                     cols = c("Location",
                                              "Overall Underweight\nPrevalence 1990",
                                              "Overall Underweight\nPrevalence 2000", 
                                              "Overall Underweight\nPrevalence 2010",
                                              "Overall Underweight\nPrevalence 2020",
                                              "Severe Underweight\nPrevalence 1990",
                                              "Severe Underweight\nPrevalence 2000",
                                              "Severe Underweight\nPrevalence 2010",
                                              "Severe Underweight\nPrevalence 2020",
                                              "Extreme Underweight\nPrevalence 1990",
                                              "Extreme Underweight\nPrevalence 2000",
                                              "Extreme Underweight\nPrevalence 2010",
                                              "Extreme Underweight\nPrevalence 2020"))



#here is where we're preparing to loop through and adjust the aesthetics for all cells that are higher in the location heirarchy to make them bold or bold.italic and also bigger
param.setup.table <- loc.met[location_id %in% table.locs, c("location_id", "location_name", "level", "super_region_name", "region_name", "path_to_top_parent")]
param.setup.table <- param.setup.table[order(super_region_name, region_name, path_to_top_parent )]
param.setup.table <- param.setup.table[, c("location_id", "location_name", "level")]


#making a param map of how I will want to adjust those aesthetics
param.setup.table[level == 0, fsize := 11]
param.setup.table[level == 1, fsize := 10]
param.setup.table[level == 2, fsize := 9]

param.setup.table[is.na(fsize), fsize := 8]

param.setup.table[level == 0, fface := "bold.italic"]
param.setup.table[level == 1, fface := "bold.italic"]
param.setup.table[level == 2, fface := "bold"]
param.setup.table[level > 3 & location_id %ni% uk.ids, fface := "italic"]

param.setup.table[is.na(fface), fface := "plain"]


param.setup.table[, index := (1:.N) + 1]

#this is how i'll identify which cell in the table I'm actually adjusting
find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}

for (i in unique(param.setup.table$location_id)) {
  
  loc <- param.setup.table[location_id == i]$location_name
  
  print(loc)
  
  cell.row <- param.setup.table[location_id == i]$index
  
  #the 1 here refers to the first column (which has location names)
  cell.of.interest <- find_cell(stunting.final.table, cell.row, 1, "core-fg")
  
  #assigning font size and face from param map
  ftsize <- param.setup.table[location_id == i]$fsize
  ftface <- param.setup.table[location_id == i]$fface
  
  
  underweight.final.table$grobs[cell.of.interest][[1]][["gp"]] <- gpar(fontsize=ftsize, fontface=ftface)
  
}

# 2, 6, 10

s.rows <- seq(from = 2, to  = 620, by = 18)
e.rows <- seq(from = 19, to  = 620, by = 18)
e.rows <- append(e.rows[1:length(e.rows)], 620)

row.param <- data.table(starts = s.rows, ends = e.rows)
row.param[, index := 1:.N]


for (i in unique(row.param$index)) {
  
  start.val <- row.param[index == i]$starts
  
  end.val <- row.param[index == i]$ends
  
  underweight.final.table <- gtable_add_grob(underweight.final.table,
                                             grobs = segmentsGrob(x0 = unit(0,"npc"),y0 = unit(0,"npc"),x1 = unit(0,"npc"),y1 = unit(1,"npc"),gp = gpar(lwd = 1)),
                                             t = start.val, b = end.val, l = 2)
  
  underweight.final.table <- gtable_add_grob(underweight.final.table,
                                             grobs = segmentsGrob(x0 = unit(0,"npc"),y0 = unit(0,"npc"),x1 = unit(0,"npc"),y1 = unit(1,"npc"),gp = gpar(lwd = 1)),
                                             t = start.val, b = end.val, l = 6)
  
  underweight.final.table <- gtable_add_grob(underweight.final.table,
                                             grobs = segmentsGrob(x0 = unit(0,"npc"),y0 = unit(0,"npc"),x1 = unit(0,"npc"),y1 = unit(1,"npc"),gp = gpar(lwd = 1)),
                                             t = start.val, b = end.val, l = 10)
  
  
}








cairo_pdf("filepath", height = 9, width = 17.5, onefile = TRUE)
for (i in unique(row.param$index)) {
  
  start.val <- row.param[index == i]$starts
  
  end.val <- row.param[index == i]$ends
  
  grid.draw(underweight.final.table[c(1,start.val:end.val)])
  grid.newpage()
  
  
}
dev.off()













##########################################
##########################################
##########################################
# MAPPING 
##########################################
##########################################
##########################################


ihme.loc.met <- loc.met[, c("location_id", "ihme_loc_id")]

source("filepath")



##########################################
#OVERALL STUNTING
##########################################



stunting.rate.compare.overall.format <- merge(stunting.rate.compare.overall.format, ihme.loc.met, by = "location_id")




map.a.1990 <- map_national_and_subnational(data = stunting.rate.compare.overall.format, map.var = "mean.val_1990", scale = "cont", plot.title = "A",
                                           legend.title = "Overall Stunting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .65, by = .05), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



map.a.2000 <- map_national_and_subnational(data = stunting.rate.compare.overall.format, map.var = "mean.val_2000", scale = "cont", plot.title = "A",
                                           legend.title = "Overall Stunting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .60, by = .05), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



map.a.2010 <- map_national_and_subnational(data = stunting.rate.compare.overall.format, map.var = "mean.val_2010", scale = "cont", plot.title = "A",
                                           legend.title = "Overall Stunting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .5, by = .05), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



##########################################
#SEVERE STUNTING
##########################################


stunting.rate.compare.severe.format <- merge(stunting.rate.compare.severe.format, ihme.loc.met, by = "location_id")




map.b.1990 <- map_national_and_subnational(data = stunting.rate.compare.severe.format, map.var = "mean.val_1990", scale = "cont", plot.title = "B",
                                           legend.title = "Severe Stunting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .4, by = .05), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



map.b.2000 <- map_national_and_subnational(data = stunting.rate.compare.severe.format, map.var = "mean.val_2000", scale = "cont", plot.title = "B",
                                           legend.title = "Severe Stunting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .35, by = .05), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



map.b.2010 <- map_national_and_subnational(data = stunting.rate.compare.severe.format, map.var = "mean.val_2010", scale = "cont", plot.title = "B",
                                           legend.title = "Severe Stunting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .3, by = .05), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



##########################################
#EXTREME STUNTING
##########################################


stunting.rate.compare.extreme.format <- merge(stunting.rate.compare.extreme.format, ihme.loc.met, by = "location_id")




map.c.1990 <- map_national_and_subnational(data = stunting.rate.compare.extreme.format, map.var = "mean.val_1990", scale = "cont", plot.title = "C",
                                           legend.title = "Extreme Stunting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .25, by = .05), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



map.c.2000 <- map_national_and_subnational(data = stunting.rate.compare.extreme.format, map.var = "mean.val_2000", scale = "cont", plot.title = "C",
                                           legend.title = "Extreme Stunting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .21, by = .03), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



map.c.2010 <- map_national_and_subnational(data = stunting.rate.compare.extreme.format, map.var = "mean.val_2010", scale = "cont", plot.title = "C",
                                           legend.title = "Extreme Stunting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .15, by = .03), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")







##########################################
#OVERALL WASTING
##########################################


wasting.rate.compare.overall.format <- merge(wasting.rate.compare.overall.format, ihme.loc.met, by = "location_id")




map.d.1990 <- map_national_and_subnational(data = wasting.rate.compare.overall.format, map.var = "mean.val_1990", scale = "cont", plot.title = "D",
                                           legend.title = "Overall Wasting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .35, by = .05), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



map.d.2000 <- map_national_and_subnational(data = wasting.rate.compare.overall.format, map.var = "mean.val_2000", scale = "cont", plot.title = "D",
                                           legend.title = "Overall Wasting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .35, by = .05), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



map.d.2010 <- map_national_and_subnational(data = wasting.rate.compare.overall.format, map.var = "mean.val_2010", scale = "cont", plot.title = "D",
                                           legend.title = "Overall Wasting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .28, by = .04), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")




##########################################
#SEVERE WASTING
##########################################


wasting.rate.compare.severe.format <- merge(wasting.rate.compare.severe.format, ihme.loc.met, by = "location_id")




map.e.1990 <- map_national_and_subnational(data = wasting.rate.compare.severe.format, map.var = "mean.val_1990", scale = "cont", plot.title = "E",
                                           legend.title = "Severe Wasting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .2, by = .04), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



map.e.2000 <- map_national_and_subnational(data = wasting.rate.compare.severe.format, map.var = "mean.val_2000", scale = "cont", plot.title = "E",
                                           legend.title = "Severe Wasting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .18, by = .03), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



map.e.2010 <- map_national_and_subnational(data = wasting.rate.compare.severe.format, map.var = "mean.val_2010", scale = "cont", plot.title = "E",
                                           legend.title = "Severe Wasting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .12, by = .02), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")




##########################################
#EXTREME WASTING
##########################################


wasting.rate.compare.extreme.format <- merge(wasting.rate.compare.extreme.format, ihme.loc.met, by = "location_id")




map.f.1990 <- map_national_and_subnational(data = wasting.rate.compare.extreme.format, map.var = "mean.val_1990", scale = "cont", plot.title = "F",
                                           legend.title = "Extreme Wasting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .08, by = .01), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



map.f.2000 <- map_national_and_subnational(data = wasting.rate.compare.extreme.format, map.var = "mean.val_2000", scale = "cont", plot.title = "F",
                                           legend.title = "Extreme Wasting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .07, by = .01), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



map.f.2010 <- map_national_and_subnational(data = wasting.rate.compare.extreme.format, map.var = "mean.val_2010", scale = "cont", plot.title = "F",
                                           legend.title = "Extreme Wasting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .04, by = .005), percent.round = .5,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")





##########################################
#OVERALL UNDERWEIGHT
##########################################


underweight.rate.compare.overall.format <- merge(underweight.rate.compare.overall.format, ihme.loc.met, by = "location_id")




map.g.1990 <- map_national_and_subnational(data = underweight.rate.compare.overall.format, map.var = "mean.val_1990", scale = "cont", plot.title = "G",
                                           legend.title = "Overall Underweight Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .45, by = .05), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



map.g.2000 <- map_national_and_subnational(data = underweight.rate.compare.overall.format, map.var = "mean.val_2000", scale = "cont", plot.title = "G",
                                           legend.title = "Overall Underweight Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .45, by = .05), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



map.g.2010 <- map_national_and_subnational(data = underweight.rate.compare.overall.format, map.var = "mean.val_2010", scale = "cont", plot.title = "G",
                                           legend.title = "Overall Underweight Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .4, by = .05), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")




##########################################
#SEVERE UNDERWEIGHT
##########################################


underweight.rate.compare.severe.format <- merge(underweight.rate.compare.severe.format, ihme.loc.met, by = "location_id")




map.h.1990 <- map_national_and_subnational(data = underweight.rate.compare.severe.format, map.var = "mean.val_1990", scale = "cont", plot.title = "H",
                                           legend.title = "Severe Underweight Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .28, by = .04), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



map.h.2000 <- map_national_and_subnational(data = underweight.rate.compare.severe.format, map.var = "mean.val_2000", scale = "cont", plot.title = "H",
                                           legend.title = "Severe Underweight Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .28, by = .04), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



map.h.2010 <- map_national_and_subnational(data = underweight.rate.compare.severe.format, map.var = "mean.val_2010", scale = "cont", plot.title = "H",
                                           legend.title = "Severe Underweight Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .21, by = .03), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")





##########################################
#EXTREME UNDERWEIGHT
##########################################


underweight.rate.compare.extreme.format <- merge(underweight.rate.compare.extreme.format, ihme.loc.met, by = "location_id")




map.i.1990 <- map_national_and_subnational(data = underweight.rate.compare.extreme.format, map.var = "mean.val_1990", scale = "cont", plot.title = "I",
                                           legend.title = "Extreme Underweight Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .14, by = .02), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



map.i.2000 <- map_national_and_subnational(data = underweight.rate.compare.extreme.format, map.var = "mean.val_2000", scale = "cont", plot.title = "I",
                                           legend.title = "Extreme Underweight Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .16, by = .02), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")



map.i.2010 <- map_national_and_subnational(data = underweight.rate.compare.extreme.format, map.var = "mean.val_2010", scale = "cont", plot.title = "I",
                                           legend.title = "Extreme Underweight Prevalence", subnat.style = "in.map", subnat.level = "public",
                                           show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                           col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .09, by = .01), percent.round = 1,
                                           plot.title.hjust = .14, plot.title.vjust = -7, legend.title.size = 10, outline.width = .06,
                                           unit.type = "percent", 
                                           outline.col = "black")














##########################################
#FORMAT PREP
##########################################








temp.df <- data.table(cgf.type = c("", "", ""),
                      val = c(1, 2, 3))


stunting.legend.plot <- ggplot() +
  geom_histogram(data = temp.df, aes(x = val, fill = cgf.type)) +
  scale_fill_manual(values = c("grey90", "grey90", "grey90"), 
                    guide=guide_legend(title="                    Stunting (HAZ)                    ", title.position = "top", title.hjust = .5, ncol = 3)) +
  theme(legend.title = element_text(size = 22, vjust = -2),
        legend.text = element_text(size = 20),
        legend.position = "bottom",
        legend.justification = 'center',
        legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
        legend.key = element_blank(),
        legend.key.size = unit(0, "cm"))

stunting.label <- get_legend(stunting.legend.plot)


wasting.legend.plot <- ggplot() +
  geom_histogram(data = temp.df, aes(x = val, fill = cgf.type)) +
  scale_fill_manual(values = c("grey90", "grey90", "grey90"), 
                    guide=guide_legend(title="                    Wasting (WHZ)                    ", title.position = "top", title.hjust = .5, ncol = 3)) +
  theme(legend.title = element_text(size = 22, vjust = -2),
        legend.text = element_text(size = 20),
        legend.position = "bottom",
        legend.justification = 'center',
        legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
        legend.key = element_blank(),
        legend.key.size = unit(0, "cm"))

wasting.label <- get_legend(wasting.legend.plot)


underweight.legend.plot <- ggplot() +
  geom_histogram(data = temp.df, aes(x = val, fill = cgf.type)) +
  scale_fill_manual(values = c("grey90", "grey90", "grey90"), 
                    guide=guide_legend(title="                Underweight (WAZ)                ", title.position = "top", title.hjust = .5, ncol = 3)) +
  theme(legend.title = element_text(size = 22, vjust = -2),
        legend.text = element_text(size = 20),
        legend.position = "bottom",
        legend.justification = 'center',
        legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
        legend.key = element_blank(),
        legend.key.size = unit(0, "cm"))

underweight.label <- get_legend(underweight.legend.plot)




overall.legend.plot <- ggplot() +
  geom_histogram(data = temp.df, aes(x = val, fill = cgf.type)) +
  scale_fill_manual(values = c("grey90", "grey90", "grey90"), 
                    guide=guide_legend(title="Overall (<-2SD)    ", title.position = "top", title.hjust = 0, ncol = 3)) +
  theme(legend.title = element_text(size = 22, angle = 90),
        legend.text = element_text(size = 20),
        legend.position = "bottom",
        legend.justification = 'center',
        legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
        legend.key = element_blank(),
        legend.key.size = unit(0, "cm"))


overall.label <- get_legend(overall.legend.plot)





severe.legend.plot <- ggplot() +
  geom_histogram(data = temp.df, aes(x = val, fill = cgf.type)) +
  scale_fill_manual(values = c("grey90", "grey90", "grey90"), 
                    guide=guide_legend(title="Severe (<-3SD)    ", title.position = "top", title.hjust = 0, ncol = 3)) +
  theme(legend.title = element_text(size = 22, angle = 90),
        legend.text = element_text(size = 20),
        legend.position = "bottom",
        legend.justification = 'center',
        legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
        legend.key = element_blank(),
        legend.key.size = unit(0, "cm"))

severe.label <- get_legend(severe.legend.plot)



extreme.legend.plot <- ggplot() +
  geom_histogram(data = temp.df, aes(x = val, fill = cgf.type)) +
  scale_fill_manual(values = c("grey90", "grey90", "grey90"), 
                    guide=guide_legend(title="Extreme (<-4SD)    ", title.position = "top", title.hjust = 0, ncol = 3)) +
  theme(legend.title = element_text(size = 22, angle = 90),
        legend.text = element_text(size = 20),
        legend.position = "bottom",
        legend.justification = 'center',
        legend.background = element_rect(fill="grey90", size=0.5, linetype="solid", colour ="grey30"),
        legend.key = element_blank(),
        legend.key.size = unit(0, "cm"))

extreme.label <- get_legend(extreme.legend.plot)




library("ggpubr", lib.loc = "filepath")






empty <- ggplot() + theme_void()


caption.1990 <- ggplot() +
  labs(caption = expression(paste(bold("   Appendix D Figure 1:"), " Estimated overall, severe, and extreme stunting, wasting, and underweight prevalences among children under 5, both sexes, in 1990."))) +
  theme_void() +
  theme(plot.caption = element_text(hjust = 0, size = 16))



combined.1990 <- ggarrange(caption.1990,empty, empty,empty,empty,
                           empty,empty,empty,empty,empty,
                           empty, stunting.label, wasting.label, underweight.label, empty,
                           overall.label, map.a.1990, map.d.1990, map.g.1990, empty,
                           severe.label, map.b.1990, map.e.1990, map.h.1990, empty,
                           extreme.label, map.c.1990, map.f.1990, map.i.1990,empty,
                           empty,empty, empty,empty,empty,
                           nrow = 7, ncol = 5, heights = c(.18,.07,.25, 1, 1, 1, .08), widths = c(.192, 1,1,1, .01))

pdf("filepath", height = 12, width = 22)
print(combined.1990)
dev.off()

caption.2000 <- ggplot() +
  labs(caption = expression(paste(bold("   Appendix D Figure 2:"), " Estimated overall, severe, and extreme stunting, wasting, and underweight prevalences among children under 5, both sexes, in 2000."))) +
  theme_void() +
  theme(plot.caption = element_text(hjust = 0, size = 16))



combined.2000 <- ggarrange(caption.2000,empty, empty,empty,empty,
                           empty,empty,empty,empty,empty,
                           empty, stunting.label, wasting.label, underweight.label, empty,
                           overall.label, map.a.2000, map.d.2000, map.g.2000, empty,
                           severe.label, map.b.2000, map.e.2000, map.h.2000, empty,
                           extreme.label, map.c.2000, map.f.2000, map.i.2000,empty,
                           empty,empty, empty,empty,empty,
                           nrow = 7, ncol = 5, heights = c(.18,.07,.25, 1, 1, 1, .08), widths = c(.192, 1,1,1, .01))

pdf("filepath", height = 12, width = 22)
print(combined.2000)
dev.off()



caption.2010 <- ggplot() +
  labs(caption = expression(paste(bold("   Appendix D Figure 3:"), " Estimated overall, severe, and extreme stunting, wasting, and underweight prevalences among children under 5, both sexes, in 2010."))) +
  theme_void() +
  theme(plot.caption = element_text(hjust = 0, size = 16))




combined.2010 <- ggarrange(caption.2010,empty, empty,empty,empty,
                           empty,empty,empty,empty,empty,
                           empty, stunting.label, wasting.label, underweight.label, empty,
                           overall.label, map.a.2010, map.d.2010, map.g.2010, empty,
                           severe.label, map.b.2010, map.e.2010, map.h.2010, empty,
                           extreme.label, map.c.2010, map.f.2010, map.i.2010,empty,
                           empty,empty, empty,empty,empty,
                           nrow = 7, ncol = 5, heights = c(.18, .07,.25, 1, 1, 1, .08), widths = c(.192, 1,1,1, .01))

pdf("filepath", height = 12, width = 22)
print(combined.2010)
dev.off()








#output the combined file with landscape, 12, 22 dimensions






































