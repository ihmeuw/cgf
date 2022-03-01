

# load functions and libraries


library(data.table)
library(ggplot2)
library(dplyr)
library(scales)
library('cowplot', lib.loc = "filepath")
library(gridExtra)
library("ggridges", lib.loc = "filepath")
library("ggplotify", lib.loc = "filepath")
"%ni%" <- Negate("%in%")

invisible(sapply(list.files("filepath", full.names = T), source))


# loading populations

populations <- get_population(gbd_round_id = 7, decomp_step = 'iterative', location_id = 'all', location_set_id = 35, 
                              year_id = c(1990:2020), age_group_id = c(1, 2, 3, 388, 389, 238, 34), sex_id = c(1, 2, 3))
populations$run_id <- NULL


# setting id vars to use in melts for this section

id.vars <- c("metric_id", "age_group_id", "location_id", "measure_id", "modelable_entity_id", "sex_id", "year_id", "model_version_id")


source("filepath")




loc.met <- get_location_metadata(location_set_id = 35, release_id = 9)

all.locs <- unique(loc.met$location_id)

mapping.loc.met <- get_location_metadata(location_set_id = 91, release_id = 9)
mapping.locs <- unique(mapping.loc.met$location_id)
mapping.ihme.locs <- data.table(location_id = mapping.loc.met$location_id, ihme_loc_id = mapping.loc.met$ihme_loc_id)



# overall (moderate) stunting

overall.stunting.map.df <- get_draws("modelable_entity_id", 10556, year_id=c(2020),
                                     source="epi", gbd_round_id=7, decomp_step="iterative",
                                     age_group_id = 1, sex_id = c(3), location_id = all.locs)


overall.stunting.map.df$value <- rowMeans(overall.stunting.map.df[, 3:1002])
overall.stunting.map.df <- overall.stunting.map.df[, -c(3:1002) ]

overall.stunting.map.df <- merge(overall.stunting.map.df, mapping.ihme.locs)

overall.stunting.map.df[, mapvar := value]

i.locs <- loc.met[,c("ihme_loc_id", "location_id")]

overall.stunting.map.df <- merge(overall.stunting.map.df, i.locs, by = "location_id")






map.a <- map_national_and_subnational(data = overall.stunting.map.df, map.var = "mapvar", scale = "cont", plot.title = "A",
                                      legend.title = "Overall Stunting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                      show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                      col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .45, by = .05), percent.round = 1,
                                      plot.title.hjust = .14, plot.title.vjust = -11, legend.title.size = 10, outline.width = .06,
                                      unit.type = "percent", 
                                      outline.col = "black")














# severe stunting

severe.stunting.map.df <- get_draws("modelable_entity_id", 8949, year_id=c(2020),
                                    source="epi", gbd_round_id=7, decomp_step="iterative",
                                    age_group_id = 1, sex_id = c(3), location_id = mapping.locs)


severe.stunting.map.df$value <- rowMeans(severe.stunting.map.df[, 3:1002])
severe.stunting.map.df <- severe.stunting.map.df[, -c(3:1002) ]

severe.stunting.map.df <- merge(severe.stunting.map.df, mapping.ihme.locs)


severe.stunting.map.df[, mapvar := value]



map.b <- map_national_and_subnational(data = severe.stunting.map.df, map.var = "mapvar", scale = "cont", plot.title = "B",
                                      legend.title = "Severe Stunting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                      show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                      col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .3, by = .05), percent.round = 1,
                                      plot.title.hjust = .14, plot.title.vjust = -11,legend.title.size = 10,outline.width = .06,
                                      unit.type = "percent", 
                                      outline.col = "black")












# extreme stunting

extreme.stunting.map.df <- get_draws("modelable_entity_id", 26941, year_id=c(2020),
                                     source="epi", gbd_round_id=7, decomp_step="iterative",
                                     age_group_id = 1, sex_id = c(3), location_id = mapping.locs)


extreme.stunting.map.df$value <- rowMeans(extreme.stunting.map.df[, 3:1002])
extreme.stunting.map.df <- extreme.stunting.map.df[, -c(3:1002) ]

extreme.stunting.map.df <- merge(extreme.stunting.map.df, mapping.ihme.locs)


extreme.stunting.map.df[, mapvar := value]

map.c <- map_national_and_subnational(data = extreme.stunting.map.df, map.var = "mapvar", scale = "cont", plot.title = "C",
                                      legend.title = "Extreme Stunting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                      show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                      col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .15, by = .03), percent.round = 1,
                                      plot.title.hjust = .14, plot.title.vjust = -11,legend.title.size = 10,outline.width = .06,
                                      unit.type = "percent", 
                                      outline.col = "black")










# overall (moderate) wasting

overall.wasting.map.df <- get_draws("modelable_entity_id", 10558, year_id=c(2020),
                                    source="epi", gbd_round_id=7, decomp_step="iterative",
                                    age_group_id = 1, sex_id = c(3), location_id = mapping.locs)


overall.wasting.map.df$value <- rowMeans(overall.wasting.map.df[, 3:1002])
overall.wasting.map.df <- overall.wasting.map.df[, -c(3:1002) ]

overall.wasting.map.df <- merge(overall.wasting.map.df, mapping.ihme.locs)

overall.wasting.map.df[, mapvar := value]


map.d <- map_national_and_subnational(data = overall.wasting.map.df, map.var = "mapvar", scale = "cont", plot.title = "D",
                                      legend.title = "Overall Wasting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                      show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                      col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .2, by = .04), percent.round = 1,
                                      plot.title.hjust = .14, plot.title.vjust = -11,legend.title.size = 10,outline.width = .06,
                                      unit.type = "percent", 
                                      outline.col = "black")











# severe wasting

severe.wasting.map.df <- get_draws("modelable_entity_id", 8945, year_id=c(2020),
                                   source="epi", gbd_round_id=7, decomp_step="iterative",
                                   age_group_id = 1, sex_id = c(3), location_id = mapping.locs)


severe.wasting.map.df$value <- rowMeans(severe.wasting.map.df[, 3:1002])
severe.wasting.map.df <- severe.wasting.map.df[, -c(3:1002) ]

severe.wasting.map.df <- merge(severe.wasting.map.df, mapping.ihme.locs)


severe.wasting.map.df[, mapvar := value]


map.e <- map_national_and_subnational(data = severe.wasting.map.df, map.var = "mapvar", scale = "cont", plot.title = "E",
                                      legend.title = "Severe Wasting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                      show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                      col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .08, by = .01), percent.round = 1,
                                      plot.title.hjust = .14, plot.title.vjust = -11,legend.title.size = 10,outline.width = .06,
                                      unit.type = "percent", 
                                      outline.col = "black")









# extreme wasting

extreme.wasting.map.df <- get_draws("modelable_entity_id", 26943, year_id=c(2020),
                                    source="epi", gbd_round_id=7, decomp_step="iterative",
                                    age_group_id = 1, sex_id = c(3), location_id = mapping.locs)

extreme.wasting.map.df$value <- rowMeans(extreme.wasting.map.df[, 3:1002])
extreme.wasting.map.df <- extreme.wasting.map.df[, -c(3:1002) ]

extreme.wasting.map.df <- merge(extreme.wasting.map.df, mapping.ihme.locs)


extreme.wasting.map.df[, mapvar := value]



map.f <- map_national_and_subnational(data = extreme.wasting.map.df, map.var = "mapvar", scale = "cont", plot.title = "F",
                                      legend.title = "Extreme Wasting Prevalence", subnat.style = "in.map", subnat.level = "public",
                                      show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                      col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .02, by = .005), percent.round = .5,
                                      plot.title.hjust = .14, plot.title.vjust = -11,legend.title.size = 10,outline.width = .06,
                                      unit.type = "percent", 
                                      outline.col = "black")







# overall (moderate) underweight

overall.underweight.map.df <- get_draws("modelable_entity_id", 10560, year_id=c(2020),
                                        source="epi", gbd_round_id=7, decomp_step="iterative",
                                        age_group_id = 1, sex_id = c(3), location_id = mapping.locs)


overall.underweight.map.df$value <- rowMeans(overall.underweight.map.df[, 3:1002])
overall.underweight.map.df <- overall.underweight.map.df[, -c(3:1002) ]

overall.underweight.map.df <- merge(overall.underweight.map.df, mapping.ihme.locs)

overall.underweight.map.df[, mapvar := value]


map.g <- map_national_and_subnational(data = overall.underweight.map.df, map.var = "mapvar", scale = "cont", plot.title = "G",
                                      legend.title = "Overall Underweight Prevalence", subnat.style = "in.map", subnat.level = "public",
                                      show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                      col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .35, by = .05), percent.round = 1,
                                      plot.title.hjust = .14, plot.title.vjust = -11,legend.title.size = 10,outline.width = .06,
                                      unit.type = "percent", 
                                      outline.col = "black")







# severe underweight

severe.underweight.map.df <- get_draws("modelable_entity_id", 2540, year_id=c(2020),
                                       source="epi", gbd_round_id=7, decomp_step="iterative",
                                       age_group_id = 1, sex_id = c(3), location_id = mapping.locs)


severe.underweight.map.df$value <- rowMeans(severe.underweight.map.df[, 3:1002])
severe.underweight.map.df <- severe.underweight.map.df[, -c(3:1002) ]

severe.underweight.map.df <- merge(severe.underweight.map.df, mapping.ihme.locs)


severe.underweight.map.df[, mapvar := value]


map.h <- map_national_and_subnational(data = severe.underweight.map.df, map.var = "mapvar", scale = "cont", plot.title = "H",
                                      legend.title = "Severe Underweight Prevalence", subnat.style = "in.map", subnat.level = "public",
                                      show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                      col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .18, by = .03), percent.round = 1,
                                      plot.title.hjust = .14, plot.title.vjust = -11,legend.title.size = 10,outline.width = .06,
                                      unit.type = "percent", 
                                      outline.col = "black")





# extreme underweight

extreme.underweight.map.df <- get_draws("modelable_entity_id", 26942, year_id=c(2020),
                                        source="epi", gbd_round_id=7, decomp_step="iterative",
                                        age_group_id = 1, sex_id = c(3), location_id = mapping.locs)

extreme.underweight.map.df$value <- rowMeans(extreme.underweight.map.df[, 3:1002])
extreme.underweight.map.df <- extreme.underweight.map.df[, -c(3:1002) ]

extreme.underweight.map.df <- merge(extreme.underweight.map.df, mapping.ihme.locs)


extreme.underweight.map.df[, mapvar := value]




map.i <- map_national_and_subnational(data = extreme.underweight.map.df, map.var = "mapvar", scale = "cont", plot.title = "I",
                                      legend.title = "Extreme Underweight Prevalence", subnat.style = "in.map", subnat.level = "public",
                                      show.india.urban.rural = FALSE, show.england.utlas = FALSE,
                                      col.rev = TRUE, na.col = 'grey', breaks = seq(from = 0, to = .06, by = .01), percent.round = 1,
                                      plot.title.hjust = .14, plot.title.vjust = -11,legend.title.size = 10,outline.width = .06,
                                      unit.type = "percent", 
                                      outline.col = "black")















#print with 12.5, 21 landscape




temp.df <- data.table(cgf.type = c("", "", ""),
                      val = c(1, 2, 3))


stunting.legend.plot <- ggplot() +
  geom_histogram(data = temp.df, aes(x = val, fill = cgf.type)) +
  scale_fill_manual(values = c("grey90", "grey90", "grey90"), 
                    guide=guide_legend(title="                    Stunting (HAZ)                    ", title.position = "top", title.hjust = .5, ncol = 3)) +
  theme(legend.title = element_text(size = 22, vjust = -4),
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
  theme(legend.title = element_text(size = 22, vjust = -4),
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
  theme(legend.title = element_text(size = 22, vjust = -4),
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


empty <- ggplot()

caption <- ggplot() +
  labs(caption = expression(paste(bold("   Figure 2:"), " Maps of overall, severe, and extreme stunting (Figures 2a, 2b, and 2c), wasting (Figures 2d, 2e, and 2f), and underweight (Figures 2g, 2h, and 2i) prevalences in 2020 for children of both sexes under age 5."))) +
  theme(plot.caption = element_text(hjust = 0, size = 16))



combined <- ggarrange(empty,empty,empty,empty,empty,
                      empty, stunting.label, wasting.label, underweight.label, empty,
                      overall.label, map.a, map.d, map.g, empty,
                      severe.label, map.b, map.e, map.h, empty,
                      extreme.label, map.c, map.f, map.i,empty,
                      caption,empty, empty,empty,empty,
                      nrow = 6, ncol = 5, heights = c(.05,.25, 1, 1, 1, .18), widths = c(.192, 1,1,1, .01))

#output the combined file with landscape, 12, 22 dimensions




