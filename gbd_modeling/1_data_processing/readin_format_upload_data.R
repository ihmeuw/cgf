
'%ni%' <- Negate('%in%')

library(data.table)
library('openxlsx')
library(readxl)

invisible(sapply(list.files("filepath", full.names = T), source))

#### WHO DATA ####

WHO.data <- fread("filepath")
WHO.data <- WHO.data[case_name != "moderate_overweight"]


WHO.data[, orig_year_start := year_start]
WHO.data[, orig_year_end := year_end]
WHO.data[year_start == year_end, year_id := year_start ]
WHO.data[year_start != year_end, year_id := ceiling((year_start + year_end)/2)]
WHO.data[year_start != year_end, `:=` (year_start = year_id, year_end = year_id)]

WHO.data[age_start == 0.5, age_start := 0.50136986]
WHO.data[age_end == .49, age_end := 0.50136986]
WHO.data[age_end == .99, age_end := 1]

WHO.data[age_start == 0 & age_end == 0.01917808, age_group_id := 2]
WHO.data[age_start == 0.01917808 & age_end == 0.07671233, age_group_id := 3]
WHO.data[age_start == 0.07671233 & age_end == 0.50136986, age_group_id := 388]
WHO.data[age_start == 0.50136986 & age_end == 1 & age_demographer ==0, age_group_id := 389]
WHO.data[age_start == 1 & age_end == 1 & age_demographer == 1, age_group_id := 238]
WHO.data[age_start == 2 & age_end == 4, age_group_id := 34]
WHO.data[is.na(age_group_id), age_group_id := 999]
WHO.data[age_group_id == 999, age_sex_specific := 0]
WHO.data[is.na(age_sex_specific), age_sex_specific := 1]
WHO.data[, orig_age_start := age_start]
WHO.data[, orig_age_end:= age_end]
WHO.data[age_group_id == 999, age_start := 999]
WHO.data[age_group_id == 999, age_end := 999]

WHO.data[case_name %like% "mean", measure := "continuous"]
WHO.data[!(case_name %like% "mean"), measure := "proportion"]

WHO.data[is.na(val), val := cases / sample_size]
WHO.data[, variance := ((val * (1 - val))/sample_size) + ((1.96^2)/(4 * (sample_size^2)))]
WHO.data <- WHO.data[variance != 0 & variance < 1]

WHO.data[, is_outlier := 0]
WHO.data[sample_size < 10, is_outlier := 1]

WHO.data[, seq := NA]
WHO.data[, sex:= Sex]
WHO.data$Sex <- NULL
WHO.data$nid <- 136038
WHO.data$underlying_nid <- NA
WHO.data[sex == "Both sexes", sex := "Both"]
WHO.data <- WHO.data[is.finite(variance)]
WHO.data$data.type <- "gbd2020.who.data"

# WHO DATA IS CLEANED UP 


#### EXTRACTION SET 1 #### 
extraction.set.1 <- read_excel("filepath", sheet = "extraction")

extraction.set.1 <- as.data.table(extraction.set.1)
extraction.set.1 <- extraction.set.1[!is.na(case_name)]
extraction.set.1 <- extraction.set.1[!is.na(age_start)]


extraction.set.1[ case_name == "moderate_undreweight", case_name := "moderate_underweight"]



extraction.set.1[age_start == .5, age_start := 0.50136986]
extraction.set.1[age_end == .5, age_end := 0.50136986]
extraction.set.1[age_end == .49, age_end := 0.50136986]
extraction.set.1[age_end == .99, age_end:= 1]


extraction.set.1[age_start == 0 & age_end == 0.01917808, age_group_id := 2]
extraction.set.1[age_start == 0.01917808 & age_end == 0.07671233, age_group_id := 3]
extraction.set.1[age_start == 0.07671233 & age_end == 0.50136986, age_group_id := 388]
extraction.set.1[age_start == 0.50136986 & age_end == 1 & age_demographer ==0, age_group_id := 389]
extraction.set.1[age_start == 1 & age_end == 1 & age_demographer == 1, age_group_id := 238]
extraction.set.1[age_start == 2 & age_end == 4, age_group_id := 34]
extraction.set.1[is.na(age_group_id), age_group_id := 999]
extraction.set.1[age_group_id == 999, age_sex_specific := 0]
extraction.set.1[is.na(age_sex_specific), age_sex_specific := 1]

extraction.set.1[, orig_age_start := age_start]
extraction.set.1[, orig_age_end:= age_end]
extraction.set.1[age_group_id == 999, age_start := 999]
extraction.set.1[age_group_id == 999, age_end := 999]


extraction.set.1[, orig_year_start := year_start]
extraction.set.1[, orig_year_end := year_end]
extraction.set.1[, year_id := ceiling((year_start + year_end)/2)]
extraction.set.1[, year_start := year_id]
extraction.set.1[, year_end := year_id]

extraction.set.1[case_name %like% "mean", measure := "continuous"]
extraction.set.1[!(case_name %like% "mean"), measure := "proportion"]

extraction.set.1$is_outlier <- NULL
extraction.set.1$is_outlier <- 0
extraction.set.1[sample_size < 10, is_outlier := 1]
extraction.set.1$val <- extraction.set.1$mean
extraction.set.1$mean <- NULL
extraction.set.1$seq <- NA
extraction.set.1$underlying_nid <- NA


extraction.set.1[, cases := sample_size*val]
extraction.set.1[, variance := ((val * (1 - val))/sample_size) + ((1.96^2)/(4 * (sample_size^2)))]
extraction.set.1 <- extraction.set.1[variance != 0]
extraction.set.1$data.type <- "extraction_set_1"

#Extraction Set 1 is ready to upload
























#### EXTRACTION SET 2 ####

extraction.set.2 <- read_xlsx("filepath", sheet = "extraction")

extraction.set.2 <- extraction.set.2[ -c(31) ]

extraction.set.2 <- as.data.table(extraction.set.2)
extraction.set.2 <- extraction.set.2[!is.na(case_name)]
extraction.set.2 <- extraction.set.2[!is.na(age_start)]
extraction.set.2[case_name == "moderare_wasting", case_name := "moderate_wasting"]

extraction.set.2[age_end == .99, age_end := 1]
extraction.set.2[age_start == .5, age_start := 0.50136986]
extraction.set.2[age_end == .5, age_end := 0.50136986]
extraction.set.2[age_end == .49, age_end := 0.50136986]


extraction.set.2[age_start == 0 & age_end == 0.01917808, age_group_id := 2]
extraction.set.2[age_start == 0.01917808 & age_end == 0.07671233, age_group_id := 3]
extraction.set.2[age_start == 0.07671233 & age_end == 0.50136986, age_group_id := 388]
extraction.set.2[age_start == 0.50136986 & age_end == 1 & age_demographer ==0, age_group_id := 389]
extraction.set.2[age_start == 1 & age_end == 1 & age_demographer == 1, age_group_id := 238]
extraction.set.2[age_start == 2 & age_end == 4, age_group_id := 34]
extraction.set.2[is.na(age_group_id), age_group_id := 999]
extraction.set.2[age_group_id == 999, age_sex_specific := 0]
extraction.set.2[is.na(age_sex_specific), age_sex_specific := 1]

extraction.set.2[, orig_age_start := age_start]
extraction.set.2[, orig_age_end:= age_end]
extraction.set.2[age_group_id == 999, age_start := 999]
extraction.set.2[age_group_id == 999, age_end := 999]


extraction.set.2[, orig_year_start := year_start]
extraction.set.2[, orig_year_end := year_end]
extraction.set.2[, year_id := ceiling((year_start + year_end)/2)]
extraction.set.2[, year_start := year_id]
extraction.set.2[, year_end := year_id]


extraction.set.2[case_name %like% "mean", measure := "continuous"]
extraction.set.2[!(case_name %like% "mean"), measure := "proportion"]

extraction.set.2$is_outlier <- NULL
extraction.set.2[, is_outlier := 0]
extraction.set.2[sample_size < 10, is_outlier := 1]
extraction.set.2$val <- extraction.set.2$mean
extraction.set.2$mean <- NULL
extraction.set.2$seq <- NA
extraction.set.2$underlying_nid <- NA


extraction.set.2[nid == 425208 & age_start == 0 & case_name %like% "wast", sample_size := 126]
extraction.set.2[nid == 425208 & age_start == 0.50136986 & case_name %like% "wast", sample_size := 150]
extraction.set.2[nid == 425208 & age_start == 1 & case_name %like% "wast", sample_size := 297]

extraction.set.2[is.na(cases), cases := sample_size*val]
extraction.set.2[, variance := ((val * (1 - val))/sample_size) + ((1.96^2)/(4 * (sample_size^2)))]
extraction.set.2 <- extraction.set.2[variance != 0]
extraction.set.2$data.type <- "extraction_set_2"

# Extraction set 2 is ready to upload





#### NEW MICRODATA ####

library(plyr)
library(dplyr)


microdata <- fread("filepath")



#fixing ages
microdata[age_start == .5, age_start := 0.50136986]
microdata[age_end == .5, age_end := 0.50136986]

microdata[age_start == 0 & age_end == 0.01917808, age_group_id := 2]
microdata[age_start == 0 & age_end == 0.01917808, age_demographer := 0]

microdata[age_start == 0.01917808 & age_end == 0.07671233, age_group_id := 3]
microdata[age_start == 0.01917808 & age_end == 0.07671233, age_demographer := 0]

microdata[age_start == 0.07671233 & age_end == 0.50136986, age_group_id := 388]
microdata[age_start == 0.07671233 & age_end == 0.50136986, age_demographer := 0]

microdata[age_start == 0.50136986 & age_end == .999, age_end := 1]
microdata[age_start == 0.50136986 & age_end == 1, age_demographer := 0]
microdata[age_start == 0.50136986 & age_end == 1 & age_demographer == 0, age_group_id := 389]

microdata[age_start == 1 & age_end == 1, age_group_id := 238]
microdata[age_start == 1 & age_end == 1, age_demographer := 1]

microdata[age_start == 2 & age_end == 4, age_group_id := 34]
microdata[age_start == 2 & age_end == 4, age_demographer := 1]


microdata$age_sex_specific <- 1

microdata[, orig_age_start := age_start]
microdata[, orig_age_end := age_end]




#fixing years
microdata[, orig_year_start := year_start]
microdata[,orig_year_end := year_end]
microdata[year_start == year_end, year_id := year_start]
microdata[year_start != year_end, year_id := ceiling((year_end + year_start)/2)]
microdata[, year_start := year_id]
microdata[, year_end := year_id]


#adding location_id
loc.met <- get_location_metadata(location_set_id = 35, gbd_round_id = 7, decomp_step = 'iterative')
subset.loc.met <- data.table(location_id = loc.met$location_id, ihme_loc_id = loc.met$ihme_loc_id)

microdata <- merge(microdata, subset.loc.met, by = "ihme_loc_id")


#adding sex column
microdata[sex_id == 1, sex := "Male"]
microdata[sex_id == 2, sex := "Female"]


#adding case_name column
microdata[var == "HAZ", case_name := "mean_stunting"]
microdata[var == "WAZ", case_name := "mean_underweight"]
microdata[var == "WHZ", case_name := "mean_wasting"]

microdata[var == "HAZ_b1_1", case_name := "mild_stunting"]
microdata[var == "WAZ_b1_1", case_name := "mild_underweight"]
microdata[var == "WHZ_b1_1", case_name := "mild_wasting"]

microdata[var == "HAZ_b2_1", case_name := "moderate_stunting"]
microdata[var == "WAZ_b2_1", case_name := "moderate_underweight"]
microdata[var == "WHZ_b2_1", case_name := "moderate_wasting"]

microdata[var == "HAZ_b3_1", case_name := "severe_stunting"]
microdata[var == "WAZ_b3_1", case_name := "severe_underweight"]
microdata[var == "WHZ_b3_1", case_name := "severe_wasting"]

microdata[var == "HAZ_b1", case_name := "mild_stunting"]
microdata[var == "WAZ_b1", case_name := "mild_underweight"]
microdata[var == "WHZ_b1", case_name := "mild_wasting"]

microdata[var == "HAZ_b2", case_name := "moderate_stunting"]
microdata[var == "WAZ_b2", case_name := "moderate_underweight"]
microdata[var == "WHZ_b2", case_name := "moderate_wasting"]

microdata[var == "HAZ_b3", case_name := "severe_stunting"]
microdata[var == "WAZ_b3", case_name := "severe_underweight"]
microdata[var == "WHZ_b3", case_name := "severe_wasting"]

microdata$is_outlier <- 0
microdata[sample_size < 10, is_outlier := 1]
microdata$val <- microdata$mean
microdata$mean <- NULL
microdata$seq <- NA
microdata$underlying_nid <- NA
microdata[, variance := (standard_error^2)]
microdata[is.na(standard_error), variance := ((val * (1 - val))/sample_size) + ((1.96^2)/(4 * (sample_size^2)))]

microdata[case_name %like% "mean", measure := "continuous"]
microdata[!(case_name %like% "mean"), measure := "proportion"]


microdata <- microdata[!is.na(variance)]
microdata <- microdata[variance < 1]
microdata <- microdata[variance > 0]

microdata$data.type <- "new.microdata"

#microdata is ready to be uploaded



#### RE-EXTRACTED MICRODATA ####



re.micro <- fread("filepath")


#adding case_name column
re.micro[var == "HAZ", case_name := "mean_stunting"]
re.micro[var == "WAZ", case_name := "mean_underweight"]
re.micro[var == "WHZ", case_name := "mean_wasting"]

re.micro[var == "HAZ_b1_1", case_name := "mild_stunting"]
re.micro[var == "WAZ_b1_1", case_name := "mild_underweight"]
re.micro[var == "WHZ_b1_1", case_name := "mild_wasting"]

re.micro[var == "HAZ_b2_1", case_name := "moderate_stunting"]
re.micro[var == "WAZ_b2_1", case_name := "moderate_underweight"]
re.micro[var == "WHZ_b2_1", case_name := "moderate_wasting"]

re.micro[var == "HAZ_b3_1", case_name := "severe_stunting"]
re.micro[var == "WAZ_b3_1", case_name := "severe_underweight"]
re.micro[var == "WHZ_b3_1", case_name := "severe_wasting"]

re.micro[var == "HAZ_b1", case_name := "mild_stunting"]
re.micro[var == "WAZ_b1", case_name := "mild_underweight"]
re.micro[var == "WHZ_b1", case_name := "mild_wasting"]

re.micro[var == "HAZ_b2", case_name := "moderate_stunting"]
re.micro[var == "WAZ_b2", case_name := "moderate_underweight"]
re.micro[var == "WHZ_b2", case_name := "moderate_wasting"]

re.micro[var == "HAZ_b3", case_name := "severe_stunting"]
re.micro[var == "WAZ_b3", case_name := "severe_underweight"]
re.micro[var == "WHZ_b3", case_name := "severe_wasting"]




#fixing ages

re.micro <- re.micro[age_start < 4]

re.micro[age_end == .5, age_end := 0.50136986]
re.micro[age_start == .5, age_start := 0.50136986]
re.micro[age_end == .999, age_end := 1]


re.micro[age_start == 0 & age_end == 0.01917808, age_group_id := 2]
re.micro[age_start == 0 & age_end == 0.01917808, age_demographer := 0]

re.micro[age_start == 0.01917808 & age_end == 0.07671233 , age_demographer := 0]
re.micro[age_start == 0.01917808 & age_end == 0.07671233 , age_group_id := 3]

re.micro[age_start == 0.07671233 & age_end == 0.50136986, age_group_id := 388]
re.micro[age_start == 0.07671233 & age_end == 0.50136986, age_demographer := 0]

re.micro[age_start == 0.50136986 & age_end == 1, age_demographer := 0]
re.micro[age_start == 0.50136986 & age_end == 1 & age_demographer == 0, age_group_id := 389]

re.micro[age_start == 1 & age_end == 1, age_group_id := 238]
re.micro[age_start == 1 & age_end == 1, age_demographer := 1]

re.micro[age_start == 2 & age_end == 4, age_group_id := 34]
re.micro[age_start == 2 & age_end == 4, age_demographer := 1]


re.micro$age_sex_specific <- 1

re.micro[, orig_age_start := age_start]
re.micro[, orig_age_end := age_end]




#fixing years
re.micro[, orig_year_start := year_start]
re.micro[,orig_year_end := year_end]
re.micro[year_start == year_end, year_id := year_start]
re.micro[year_start != year_end, year_id := ceiling((year_end + year_start)/2)]
re.micro[, year_start := year_id]
re.micro[, year_end := year_id]


#adding location_id
loc.met <- get_location_metadata(location_set_id = 35, gbd_round_id = 7, decomp_step = 'iterative')
subset.loc.met <- data.table(location_id = loc.met$location_id, ihme_loc_id = loc.met$ihme_loc_id)

re.micro <- merge(re.micro, subset.loc.met, by = "ihme_loc_id")


#adding sex column
re.micro[sex_id == 1, sex := "Male"]
re.micro[sex_id == 2, sex := "Female"]


re.micro$is_outlier <- 0
re.micro[sample_size < 10, is_outlier := 1]
re.micro$val <- re.micro$mean
re.micro$mean <- NULL
re.micro$seq <- NA
re.micro$underlying_nid <- NA
re.micro[, variance := (standard_error^2)]
re.micro[is.na(standard_error), variance:= ((val * (1 - val))/sample_size) + ((1.96^2)/(4 * (sample_size^2)))]

re.micro[case_name %like% "mean", measure := "continuous"]
re.micro[!(case_name %like% "mean"), measure := "proportion"]


re.micro <- re.micro[!is.na(variance)]
re.micro <- re.micro[variance < 1]
re.micro <- re.micro[variance > 0]

re.micro$data.type <- "reextracted.microdata"

# new microdata is ready to be uploaded









#### COMBINING ALL DATA SETS THAT WILL BE EVENTUALLY BE UPLOADED TOGETHER TO HAVE ONE COMPLETE DT ####



all.data.going.up <- rbind(WHO.data, extraction.set.1, extraction.set.2, microdata, re.micro, fill = T)


#### moderate stunting ####

#getting 2019 data
mod.stunt.2019 <- get_bundle_version(bundle_version_id = 13928, fetch = 'all')


#gettin a vector of all nids in 2019 data
mod.stunt.2019.nids <- unique(mod.stunt.2019$nid)


#getting a vector of all nids going up for mod stunting
mod.stunt.going.up <- all.data.going.up[case_name == "moderate_stunting"]
mod.stunt.going.up.nids <- unique(mod.stunt.going.up$nid)


#find out how many of those nids are in common
mod.stunt.nids.in.common <- intersect(mod.stunt.2019.nids, mod.stunt.going.up.nids)


#entire set of data coming down
mod.stunt.2019.that.should.come.down <- mod.stunt.2019[nid %in% mod.stunt.nids.in.common | nid == 136039]



mod.stunt.seqs.to.delete <- data.table(seq = mod.stunt.2019.that.should.come.down$seq)


write.xlsx(mod.stunt.seqs.to.delete, 
           file = "filepath",
           sheetName = "extraction")

#pause to get the bundle empty and populated with the gbd2019 data

whats.there.now <- get_bundle_data(bundle_id = 4877, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = whats.there.now$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

mod.delete.upload <- upload_bundle_data(bundle_id = 4877, decomp_step = 'iterative', gbd_round_id = 7,
                                        filepath =  "filepath")



mod.stunt.2019 <- get_bundle_version(bundle_version_id = 13928, fetch = 'all')


write.xlsx(mod.stunt.2019, 
           file = "filepath",
           sheetName = "extraction")

mod.populate.upload <- upload_bundle_data(bundle_id = 4877, decomp_step = 'iterative', gbd_round_id = 7,
                                          filepath =  "filepath")


# now that that's done, time to remove the ones that are going to be replaced

mod.take.down.upload <- upload_bundle_data(bundle_id = 4877, decomp_step = 'iterative', gbd_round_id = 7,
                                           filepath =  "filepath")


#now time to upload my replacement and new data

write.xlsx(mod.stunt.going.up, 
           file = "filepath",
           sheetName = "extraction")

mod.replacement.new.data <- upload_bundle_data(bundle_id = 4877, decomp_step = 'iterative', gbd_round_id = 7,
                                               filepath =  "filepath")

mod.stunting.bundle.version <- save_bundle_version(bundle_id = 4877, decomp_step = "iterative", gbd_round_id = 7)
















#### moderate underweight ####



#getting 2019 data
mod.under.2019 <- get_bundle_version(bundle_version_id = 13919, fetch = 'all')


#gettin a vector of all nids in 2019 data
mod.under.2019.nids <- unique(mod.under.2019$nid)


#getting a vector of all nids going up for mod stunting
mod.under.going.up <- all.data.going.up[case_name == "moderate_underweight"]
mod.under.going.up.nids <- unique(mod.under.going.up$nid)


#find out how many of those nids are in common
mod.under.nids.in.common <- intersect(mod.under.2019.nids, mod.under.going.up.nids)


#entire set of data coming down
mod.under.2019.that.should.come.down <- mod.under.2019[nid %in% mod.under.nids.in.common | nid == 136039]



mod.under.seqs.to.delete <- data.table(seq = mod.under.2019.that.should.come.down$seq)


write.xlsx(mod.under.seqs.to.delete, 
           file = "filepath",
           sheetName = "extraction")

#pause to get the bundle empty and populated with the gbd2019 data

whats.there.now <- get_bundle_data(bundle_id = 4745, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = whats.there.now$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

mod.delete.upload <- upload_bundle_data(bundle_id = 4745, decomp_step = 'iterative', gbd_round_id = 7,
                                        filepath =  "filepath")



mod.under.2019 <- get_bundle_version(bundle_version_id = 13919, fetch = 'all')


write.xlsx(mod.under.2019, 
           file = "filepath",
           sheetName = "extraction")

mod.populate.upload <- upload_bundle_data(bundle_id = 4745, decomp_step = 'iterative', gbd_round_id = 7,
                                          filepath =  "filepath")


# now that that's done, time to remove the ones that are going to be replaced

mod.under.take.down.upload <- upload_bundle_data(bundle_id = 4745, decomp_step = 'iterative', gbd_round_id = 7,
                                                 filepath =  "filepath")


#now time to upload my replacement and new data

write.xlsx(mod.under.going.up, 
           file = "filepath",
           sheetName = "extraction")

mod.replacement.new.data <- upload_bundle_data(bundle_id = 4745, decomp_step = 'iterative', gbd_round_id = 7,
                                               filepath =  "filepath")

mod.under.bundle.version <- save_bundle_version(bundle_id = 4745, decomp_step = "iterative", gbd_round_id = 7)































#### moderate wasting ####




#getting 2019 data
mod.waste.2019 <- get_bundle_version(bundle_version_id = 13904, fetch = 'all')


#gettin a vector of all nids in 2019 data
mod.waste.2019.nids <- unique(mod.waste.2019$nid)


#getting a vector of all nids going up for mod stunting
mod.waste.going.up <- all.data.going.up[case_name == "moderate_wasting"]
mod.waste.going.up.nids <- unique(mod.waste.going.up$nid)


#find out how many of those nids are in common
mod.waste.nids.in.common <- intersect(mod.waste.2019.nids, mod.waste.going.up.nids)


#entire set of data coming down
mod.waste.2019.that.should.come.down <- mod.waste.2019[nid %in% mod.waste.nids.in.common | nid == 136039]


mod.waste.seqs.to.delete <- data.table(seq = mod.waste.2019.that.should.come.down$seq)


write.xlsx(mod.waste.seqs.to.delete, 
           file = "filepath",
           sheetName = "extraction")

#pause to get the bundle empty and populated with the gbd2019 data

whats.there.now <- get_bundle_data(bundle_id = 4865, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = whats.there.now$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

mod.delete.upload <- upload_bundle_data(bundle_id = 4865, decomp_step = 'iterative', gbd_round_id = 7,
                                        filepath =  "filepath")



mod.waste.2019 <- get_bundle_version(bundle_version_id = 13904, fetch = 'all')


write.xlsx(mod.waste.2019, 
           file = "filepath",
           sheetName = "extraction")

mod.populate.upload <- upload_bundle_data(bundle_id = 4865, decomp_step = 'iterative', gbd_round_id = 7,
                                          filepath =  "filepath")


# now that that's done, time to remove the ones that are going to be replaced

mod.waste.take.down.upload <- upload_bundle_data(bundle_id = 4865, decomp_step = 'iterative', gbd_round_id = 7,
                                                 filepath =  "filepath")


#now time to upload my replacement and new data

write.xlsx(mod.waste.going.up, 
           file = "filepath",
           sheetName = "extraction")

mod.replacement.new.data <- upload_bundle_data(bundle_id = 4865, decomp_step = 'iterative', gbd_round_id = 7,
                                               filepath =  "filepath")

mod.waste.bundle.version <- save_bundle_version(bundle_id = 4865, decomp_step = "iterative", gbd_round_id = 7)






















#### severe wasting ####






#getting 2019 data
sev.waste.2019 <- get_bundle_version(bundle_version_id = 13898, fetch = 'all')


#gettin a vector of all nids in 2019 data
sev.waste.2019.nids <- unique(sev.waste.2019$nid)


#getting a vector of all nids going up for mod stunting
sev.waste.going.up <- all.data.going.up[case_name == "severe_wasting"]
sev.waste.going.up.nids <- unique(sev.waste.going.up$nid)


#find out how many of those nids are in common
sev.waste.nids.in.common <- intersect(sev.waste.2019.nids, sev.waste.going.up.nids)


#entire set of data coming down
sev.waste.2019.that.should.come.down <- sev.waste.2019[nid %in% sev.waste.nids.in.common | nid == 136039]

sev.waste.seqs.to.delete <- data.table(seq = sev.waste.2019.that.should.come.down$seq)


write.xlsx(sev.waste.seqs.to.delete, 
           file = "filepath",
           sheetName = "extraction")

#pause to get the bundle empty and populated with the gbd2019 data

whats.there.now <- get_bundle_data(bundle_id = 4868, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = whats.there.now$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

sev.delete.upload <- upload_bundle_data(bundle_id = 4868, decomp_step = 'iterative', gbd_round_id = 7,
                                        filepath =  "filepath")



sev.waste.2019 <- get_bundle_version(bundle_version_id = 13898, fetch = 'all')


write.xlsx(sev.waste.2019, 
           file = "filepath",
           sheetName = "extraction")

sev.populate.upload <- upload_bundle_data(bundle_id = 4868, decomp_step = 'iterative', gbd_round_id = 7,
                                          filepath =  "filepath")


# now that that's done, time to remove the ones that are going to be replaced

sev.waste.take.down.upload <- upload_bundle_data(bundle_id = 4868, decomp_step = 'iterative', gbd_round_id = 7,
                                                 filepath =  "filepath")


#now time to upload my replacement and new data

write.xlsx(sev.waste.going.up, 
           file = "filepath",
           sheetName = "extraction")

sev.replacement.new.data <- upload_bundle_data(bundle_id = 4868, decomp_step = 'iterative', gbd_round_id = 7,
                                               filepath = "filepath")

sev.waste.bundle.version <- save_bundle_version(bundle_id = 4868, decomp_step = "iterative", gbd_round_id = 7)






















#### mild wasting ####








#getting 2019 data
mild.waste.2019 <- get_bundle_version(bundle_version_id = 13895, fetch = 'all')


#gettin a vector of all nids in 2019 data
mild.waste.2019.nids <- unique(mild.waste.2019$nid)


#getting a vector of all nids going up for mod stunting
mild.waste.going.up <- all.data.going.up[case_name == "mild_wasting"]
mild.waste.going.up.nids <- unique(mild.waste.going.up$nid)


#find out how many of those nids are in common
mild.waste.nids.in.common <- intersect(mild.waste.2019.nids, mild.waste.going.up.nids)


#entire set of data coming down
mild.waste.2019.that.should.come.down <- mild.waste.2019[nid %in% mild.waste.nids.in.common | nid == 136039]



mild.waste.seqs.to.delete <- data.table(seq = mild.waste.2019.that.should.come.down$seq)


write.xlsx(mild.waste.seqs.to.delete, 
           file = "filepath",
           sheetName = "extraction")

#pause to get the bundle empty and populated with the gbd2019 data

whats.there.now <- get_bundle_data(bundle_id = 4862, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = whats.there.now$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

mild.delete.upload <- upload_bundle_data(bundle_id = 4862, decomp_step = 'iterative', gbd_round_id = 7,
                                         filepath =  "filepath")



mild.waste.2019 <- get_bundle_version(bundle_version_id = 13895, fetch = 'all')


write.xlsx(mild.waste.2019, 
           file = "filepath",
           sheetName = "extraction")

mild.populate.upload <- upload_bundle_data(bundle_id = 4862, decomp_step = 'iterative', gbd_round_id = 7,
                                           filepath =  "filepath")


# now that that's done, time to remove the ones that are going to be replaced

mild.waste.take.down.upload <- upload_bundle_data(bundle_id = 4862, decomp_step = 'iterative', gbd_round_id = 7,
                                                  filepath = "filepath")


#now time to upload my replacement and new data

write.xlsx(mild.waste.going.up, 
           file = "filepath",
           sheetName = "extraction")

mild.replacement.new.data <- upload_bundle_data(bundle_id = 4862, decomp_step = 'iterative', gbd_round_id = 7,
                                                filepath = "filepath")

mild.waste.bundle.version <- save_bundle_version(bundle_id = 4862, decomp_step = "iterative", gbd_round_id = 7)





#### mild underweight ####




#getting 2019 data
mild.under.2019 <- get_bundle_version(bundle_version_id = 13913, fetch = 'all')


#gettin a vector of all nids in 2019 data
mild.under.2019.nids <- unique(mild.under.2019$nid)


#getting a vector of all nids going up for mod stunting
mild.under.going.up <- all.data.going.up[case_name == "mild_underweight"]
mild.under.going.up.nids <- unique(mild.under.going.up$nid)


#find out how many of those nids are in common
mild.under.nids.in.common <- intersect(mild.under.2019.nids, mild.under.going.up.nids)


#entire set of data coming down
mild.under.2019.that.should.come.down <- mild.under.2019[nid %in% mild.under.nids.in.common | nid == 136039]


mild.under.seqs.to.delete <- data.table(seq = mild.under.2019.that.should.come.down$seq)


write.xlsx(mild.under.seqs.to.delete, 
           file = "filepath",
           sheetName = "extraction")

#pause to get the bundle empty and populated with the gbd2019 data

whats.there.now <- get_bundle_data(bundle_id = 4742, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = whats.there.now$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

mild.delete.upload <- upload_bundle_data(bundle_id = 4742, decomp_step = 'iterative', gbd_round_id = 7,
                                         filepath =  "filepath")



mild.under.2019 <- get_bundle_version(bundle_version_id = 13913, fetch = 'all')


write.xlsx(mild.under.2019, 
           file = "filepath",
           sheetName = "extraction")

mild.populate.upload <- upload_bundle_data(bundle_id = 4742, decomp_step = 'iterative', gbd_round_id = 7,
                                           filepath =  "filepath")


# now that that's done, time to remove the ones that are going to be replaced

mild.under.take.down.upload <- upload_bundle_data(bundle_id = 4742, decomp_step = 'iterative', gbd_round_id = 7,
                                                  filepath = "filepath")


#now time to upload my replacement and new data

write.xlsx(mild.under.going.up, 
           file = "filepath",
           sheetName = "extraction")

mild.replacement.new.data <- upload_bundle_data(bundle_id = 4742, decomp_step = 'iterative', gbd_round_id = 7,
                                                filepath = "filepath")

mild.under.bundle.version <- save_bundle_version(bundle_id = 4742, decomp_step = "iterative", gbd_round_id = 7)
















#### severe underweight ####




#getting 2019 data
severe.under.2019 <- get_bundle_version(bundle_version_id = 13916, fetch = 'all')


#gettin a vector of all nids in 2019 data
sev.under.2019.nids <- unique(severe.under.2019$nid)


#getting a vector of all nids going up for mod stunting
sev.under.going.up <- all.data.going.up[case_name == "severe_underweight"]
sev.under.going.up.nids <- unique(sev.under.going.up$nid)


#find out how many of those nids are in common
sev.under.nids.in.common <- intersect(sev.under.2019.nids, sev.under.going.up.nids)


#entire set of data coming down
sev.under.2019.that.should.come.down <- severe.under.2019[nid %in% sev.under.nids.in.common | nid == 136039]



sev.under.seqs.to.delete <- data.table(seq = sev.under.2019.that.should.come.down$seq)


write.xlsx(sev.under.seqs.to.delete, 
           file = "filepath",
           sheetName = "extraction")

#pause to get the bundle empty and populated with the gbd2019 data

whats.there.now <- get_bundle_data(bundle_id = 4748, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = whats.there.now$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

sev.delete.upload <- upload_bundle_data(bundle_id = 4748, decomp_step = 'iterative', gbd_round_id = 7,
                                        filepath =  "filepath")



sev.under.2019 <- get_bundle_version(bundle_version_id = 13916, fetch = 'all')


write.xlsx(sev.under.2019, 
           file = "filepath",
           sheetName = "extraction")

sev.populate.upload <- upload_bundle_data(bundle_id = 4748, decomp_step = 'iterative', gbd_round_id = 7,
                                          filepath =  "filepath")


# now that that's done, time to remove the ones that are going to be replaced

sev.under.take.down.upload <- upload_bundle_data(bundle_id = 4748, decomp_step = 'iterative', gbd_round_id = 7,
                                                 filepath = "filepath")


#now time to upload my replacement and new data

write.xlsx(sev.under.going.up, 
           file = "filepath",
           sheetName = "extraction")

sev.replacement.new.data <- upload_bundle_data(bundle_id = 4748, decomp_step = 'iterative', gbd_round_id = 7,
                                               filepath = "filepath")

sev.under.bundle.version <- save_bundle_version(bundle_id = 4748, decomp_step = "iterative", gbd_round_id = 7)
















#### mild stunting ####






#getting 2019 data
mild.stunt.2019 <- get_bundle_version(bundle_version_id = 13907, fetch = 'all')


#gettin a vector of all nids in 2019 data
mild.stunt.2019.nids <- unique(mild.stunt.2019$nid)


#getting a vector of all nids going up for mod stunting
mild.stunt.going.up <- all.data.going.up[case_name == "mild_stunting"]
mild.stunt.going.up.nids <- unique(mild.stunt.going.up$nid)


#find out how many of those nids are in common
mild.stunt.nids.in.common <- intersect(mild.stunt.2019.nids, mild.stunt.going.up.nids)


#entire set of data coming down
mild.stunt.2019.that.should.come.down <- mild.stunt.2019[nid %in% mild.stunt.nids.in.common | nid == 136039]


mild.stunt.seqs.to.delete <- data.table(seq = mild.stunt.2019.that.should.come.down$seq)


write.xlsx(mild.stunt.seqs.to.delete, 
           file = "filepath",
           sheetName = "extraction")

#pause to get the bundle empty and populated with the gbd2019 data

whats.there.now <- get_bundle_data(bundle_id = 4874, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = whats.there.now$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

mild.delete.upload <- upload_bundle_data(bundle_id = 4874, decomp_step = 'iterative', gbd_round_id = 7,
                                         filepath =  "filepath")



mild.stunt.2019 <- get_bundle_version(bundle_version_id = 13907, fetch = 'all')


write.xlsx(mild.stunt.2019, 
           file = "filepath",
           sheetName = "extraction")

mild.populate.upload <- upload_bundle_data(bundle_id = 4874, decomp_step = 'iterative', gbd_round_id = 7,
                                           filepath =  "filepath")


# now that that's done, time to remove the ones that are going to be replaced

mild.stunt.take.down.upload <- upload_bundle_data(bundle_id = 4874, decomp_step = 'iterative', gbd_round_id = 7,
                                                  filepath ="filepath")


#now time to upload my replacement and new data

write.xlsx(mild.stunt.going.up, 
           file = "filepath",
           sheetName = "extraction")

mild.replacement.new.data <- upload_bundle_data(bundle_id = 4874, decomp_step = 'iterative', gbd_round_id = 7,
                                                filepath = "filepath")

mild.stunt.bundle.version <- save_bundle_version(bundle_id = 4874, decomp_step = "iterative", gbd_round_id = 7)








#### severe stunting ####




#getting 2019 data
sev.stunt.2019 <- get_bundle_version(bundle_version_id = 13910, fetch = 'all')


#gettin a vector of all nids in 2019 data
sev.stunt.2019.nids <- unique(sev.stunt.2019$nid)


#getting a vector of all nids going up for mod stunting
sev.stunt.going.up <- all.data.going.up[case_name == "severe_stunting"]
sev.stunt.going.up.nids <- unique(sev.stunt.going.up$nid)


#find out how many of those nids are in common
sev.stunt.nids.in.common <- intersect(sev.stunt.2019.nids, sev.stunt.going.up.nids)


#entire set of data coming down
sev.stunt.2019.that.should.come.down <- sev.stunt.2019[nid %in% sev.stunt.nids.in.common | nid == 136039]



sev.stunt.seqs.to.delete <- data.table(seq = sev.stunt.2019.that.should.come.down$seq)


write.xlsx(sev.stunt.seqs.to.delete, 
           file = "filepath",
           sheetName = "extraction")

#pause to get the bundle empty and populated with the gbd2019 data

whats.there.now <- get_bundle_data(bundle_id = 4880, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = whats.there.now$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

sev.delete.upload <- upload_bundle_data(bundle_id = 4880, decomp_step = 'iterative', gbd_round_id = 7,
                                        filepath = "filepath")



sev.stunt.2019 <- get_bundle_version(bundle_version_id = 13910, fetch = 'all')


write.xlsx(sev.stunt.2019, 
           file = "filepath",
           sheetName = "extraction")

sev.populate.upload <- upload_bundle_data(bundle_id = 4880, decomp_step = 'iterative', gbd_round_id = 7,
                                          filepath =  "filepath")


# now that that's done, time to remove the ones that are going to be replaced

sev.stunt.take.down.upload <- upload_bundle_data(bundle_id = 4880, decomp_step = 'iterative', gbd_round_id = 7,
                                                 filepath ="filepath")


#now time to upload my replacement and new data

write.xlsx(sev.stunt.going.up, 
           file = "filepath",
           sheetName = "extraction")

sev.replacement.new.data <- upload_bundle_data(bundle_id = 4880, decomp_step = 'iterative', gbd_round_id = 7,
                                               filepath = "filepath")

sev.stunt.bundle.version <- save_bundle_version(bundle_id = 4880, decomp_step = "iterative", gbd_round_id = 7)













#### mean stunting ####





#getting 2019 data
mean.stunt.2019 <- get_bundle_version(bundle_version_id = 13922, fetch = 'all')


#gettin a vector of all nids in 2019 data
mean.stunt.2019.nids <- unique(mean.stunt.2019$nid)


#getting a vector of all nids going up for mod stunting
mean.stunt.going.up <- all.data.going.up[case_name == "mean_stunting"]
mean.stunt.going.up.nids <- unique(mean.stunt.going.up$nid)


#find out how many of those nids are in common
mean.stunt.nids.in.common <- intersect(mean.stunt.2019.nids, mean.stunt.going.up.nids)


#entire set of data coming down
mean.stunt.2019.that.should.come.down <- mean.stunt.2019[nid %in% mean.stunt.nids.in.common | nid == 136039]


mean.stunt.seqs.to.delete <- data.table(seq = mean.stunt.2019.that.should.come.down$seq)


write.xlsx(mean.stunt.seqs.to.delete, 
           file = "filepath",
           sheetName = "extraction")

#pause to get the bundle empty and populated with the gbd2019 data

whats.there.now <- get_bundle_data(bundle_id = 4883, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = whats.there.now$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

mean.delete.upload <- upload_bundle_data(bundle_id = 4883, decomp_step = 'iterative', gbd_round_id = 7,
                                         filepath = "filepath")



mean.stunt.2019 <- get_bundle_version(bundle_version_id = 13922, fetch = 'all')


write.xlsx(mean.stunt.2019, 
           file = "filepath",
           sheetName = "extraction")

mean.populate.upload <- upload_bundle_data(bundle_id = 4883, decomp_step = 'iterative', gbd_round_id = 7,
                                           filepath =  "filepath")


# now that that's done, time to remove the ones that are going to be replaced

mean.stunt.take.down.upload <- upload_bundle_data(bundle_id = 4883, decomp_step = 'iterative', gbd_round_id = 7,
                                                  filepath ="filepath")


#now time to upload my replacement and new data

write.xlsx(mean.stunt.going.up, 
           file = "filepath",
           sheetName = "extraction")

mean.replacement.new.data <- upload_bundle_data(bundle_id = 4883, decomp_step = 'iterative', gbd_round_id = 7,
                                                filepath = "filepath")

mean.stunt.bundle.version <- save_bundle_version(bundle_id = 4883, decomp_step = "iterative", gbd_round_id = 7)


















#### mean underweight ####



#getting 2019 data
mean.under.2019 <- get_bundle_version(bundle_version_id = 13925, fetch = 'all')


#gettin a vector of all nids in 2019 data
mean.under.2019.nids <- unique(mean.under.2019$nid)


#getting a vector of all nids going up for mod stunting
mean.under.going.up <- all.data.going.up[case_name == "mean_underweight"]
mean.under.going.up.nids <- unique(mean.under.going.up$nid)


#find out how many of those nids are in common
mean.under.nids.in.common <- intersect(mean.under.2019.nids, mean.under.going.up.nids)


#entire set of data coming down
mean.under.2019.that.should.come.down <- mean.under.2019[nid %in% mean.under.nids.in.common | nid == 136039]


mean.under.seqs.to.delete <- data.table(seq = mean.under.2019.that.should.come.down$seq)


write.xlsx(mean.under.seqs.to.delete, 
           file = "filepath",
           sheetName = "extraction")

#pause to get the bundle empty and populated with the gbd2019 data

whats.there.now <- get_bundle_data(bundle_id = 4751, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = whats.there.now$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

mean.delete.upload <- upload_bundle_data(bundle_id = 4751, decomp_step = 'iterative', gbd_round_id = 7,
                                         filepath = "filepath")



mean.under.2019 <- get_bundle_version(bundle_version_id = 13925, fetch = 'all')


write.xlsx(mean.under.2019, 
           file = "filepath",
           sheetName = "extraction")

mean.populate.upload <- upload_bundle_data(bundle_id = 4751, decomp_step = 'iterative', gbd_round_id = 7,
                                           filepath =  "filepath")


# now that that's done, time to remove the ones that are going to be replaced

mean.under.take.down.upload <- upload_bundle_data(bundle_id = 4751, decomp_step = 'iterative', gbd_round_id = 7,
                                                  filepath ="filepath")


#now time to upload my replacement and new data

write.xlsx(mean.under.going.up, 
           file = "filepath",
           sheetName = "extraction")

mean.replacement.new.data <- upload_bundle_data(bundle_id = 4751, decomp_step = 'iterative', gbd_round_id = 7,
                                                filepath ="filepath")

mean.under.bundle.version <- save_bundle_version(bundle_id = 4751, decomp_step = "iterative", gbd_round_id = 7)





#### mean wasting ####

#getting 2019 data
mean.waste.2019 <- get_bundle_version(bundle_version_id = 13901, fetch = 'all')


#gettin a vector of all nids in 2019 data
mean.waste.2019.nids <- unique(mean.waste.2019$nid)


#getting a vector of all nids going up for mod stunting
mean.waste.going.up <- all.data.going.up[case_name == "mean_wasting"]
mean.waste.going.up.nids <- unique(mean.waste.going.up$nid)


#find out how many of those nids are in common
mean.waste.nids.in.common <- intersect(mean.waste.2019.nids, mean.waste.going.up.nids)


#entire set of data coming down
mean.waste.2019.that.should.come.down <- mean.waste.2019[nid %in% mean.waste.nids.in.common | nid == 136039]


mean.waste.seqs.to.delete <- data.table(seq = mean.waste.2019.that.should.come.down$seq)


write.xlsx(mean.waste.seqs.to.delete, 
           file = "filepath",
           sheetName = "extraction")

#pause to get the bundle empty and populated with the gbd2019 data

whats.there.now <- get_bundle_data(bundle_id = 4871, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = whats.there.now$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

mean.delete.upload <- upload_bundle_data(bundle_id = 4871, decomp_step = 'iterative', gbd_round_id = 7,
                                         filepath = "filepath")



mean.waste.2019 <- get_bundle_version(bundle_version_id = 13901, fetch = 'all')


write.xlsx(mean.waste.2019, 
           file = "filepath",
           sheetName = "extraction")

mean.populate.upload <- upload_bundle_data(bundle_id = 4871, decomp_step = 'iterative', gbd_round_id = 7,
                                           filepath =  "filepath")


# now that that's done, time to remove the ones that are going to be replaced

mean.waste.take.down.upload <- upload_bundle_data(bundle_id = 4871, decomp_step = 'iterative', gbd_round_id = 7,
                                                  filepath ="filepath")


#now time to upload my replacement and new data

write.xlsx(mean.waste.going.up, 
           file = "filepath",
           sheetName = "extraction")

mean.replacement.new.data <- upload_bundle_data(bundle_id = 4871, decomp_step = 'iterative', gbd_round_id = 7,
                                                filepath ="filepath")

mean.waste.bundle.version <- save_bundle_version(bundle_id = 4871, decomp_step = "iterative", gbd_round_id = 7)



#### GETTING AGE-SEX SPECIFIC DATA ONCE IT'S ALL BEEN UPLOADED TO THE TOTAL BUNDLE ####


#### mild_stunting age sex specific ####





mild.stunting <- get_bundle_version(bundle_version_id = 30167, fetch = 'all')


mild.stunting.as <- mild.stunting[sex == "Male" | sex == "Female"]
mild.stunting.as <- mild.stunting.as[age_group_id == 2 |
                                       age_group_id == 3 |
                                       age_group_id == 388 |
                                       age_group_id == 389 |
                                       age_group_id == 238 |
                                       age_group_id == 34]

current.as.mild.stunting <- get_bundle_data(bundle_id = 7802, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = current.as.mild.stunting$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

mild.stunting.delete.upload <- upload_bundle_data(bundle_id = 7802, decomp_step = 'iterative', gbd_round_id = 7,
                                                  filepath =  "filepath")


write.xlsx(mild.stunting.as, 
           file = "filepath",
           sheetName = "extraction")

mild.stunting.upload <- upload_bundle_data(bundle_id = 7802, decomp_step = 'iterative', gbd_round_id = 7,
                                           filepath =  "filepath")


mild.stunting.bundle.version <- save_bundle_version(bundle_id = 7802, decomp_step = "iterative", gbd_round_id = 7)



mild.stunting.as$crosswalk_parent_seq <- NA
mild.stunting.as$uncertainty_type_value <- 95
mild.stunting.as$unit_value_as_published <- NULL
mild.stunting.as$unit_value_as_published <- 1


write.xlsx(mild.stunting.as, 
           file = "filepath",
           sheetName = "extraction")

mild.stunt.xw <- save_crosswalk_version(bundle_version_id = 30194, 
                                        data_filepath ="filepath", 
                                        description = "Age Sex Split Mild Stunting Crosswalk")









#### moderate_stunting age sex specific ####

mod.stunting <- get_bundle_version(bundle_version_id = 30146, fetch = 'all')


mod.stunting.as <- mod.stunting[sex == "Male" | sex == "Female"]
mod.stunting.as <- mod.stunting.as[age_group_id == 2 |
                                     age_group_id == 3 |
                                     age_group_id == 388 |
                                     age_group_id == 389 |
                                     age_group_id == 238 |
                                     age_group_id == 34]

current.as.mod.stunting <- get_bundle_data(bundle_id = 7805, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = current.as.mod.stunting$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

mod.stunting.delete.upload <- upload_bundle_data(bundle_id = 7805, decomp_step = 'iterative', gbd_round_id = 7,
                                                 filepath =  "filepath")


write.xlsx(mod.stunting.as, 
           file = "filepath",
           sheetName = "extraction")

mod.stunting.upload <- upload_bundle_data(bundle_id = 7805, decomp_step = 'iterative', gbd_round_id = 7,
                                          filepath =  "filepath")


mod.stunting.bundle.version <- save_bundle_version(bundle_id = 7805, decomp_step = "iterative", gbd_round_id = 7)


mod.stunting.as$crosswalk_parent_seq <- NA
mod.stunting.as$uncertainty_type_value <- 95
mod.stunting.as$unit_value_as_published <- NULL
mod.stunting.as$unit_value_as_published <- 1


write.xlsx(mod.stunting.as, 
           file = "/snfs1/WORK/12_bundle/nutrition_stunting/7805/01_input_data/01_nonlit/xw.xlsx",
           sheetName = "extraction")

mod.stunt.xw <- save_crosswalk_version(bundle_version_id = 30197, 
                                       data_filepath ="/snfs1/WORK/12_bundle/nutrition_stunting/7805/01_input_data/01_nonlit/xw.xlsx", 
                                       description = "Age Sex Split Moderate Stunting Crosswalk")




















#### severe_stunting age sex specific ####




sev.stunting <- get_bundle_version(bundle_version_id = 30170, fetch = 'all')


sev.stunting.as <- sev.stunting[sex == "Male" | sex == "Female"]
sev.stunting.as <- sev.stunting.as[age_group_id == 2 |
                                     age_group_id == 3 |
                                     age_group_id == 388 |
                                     age_group_id == 389 |
                                     age_group_id == 238 |
                                     age_group_id == 34]

current.as.sev.stunting <- get_bundle_data(bundle_id = 7808, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = current.as.sev.stunting$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

sev.stunting.delete.upload <- upload_bundle_data(bundle_id = 7808, decomp_step = 'iterative', gbd_round_id = 7,
                                                 filepath =  "filepath")


write.xlsx(sev.stunting.as, 
           file = "filepath",
           sheetName = "extraction")

sev.stunting.upload <- upload_bundle_data(bundle_id = 7808, decomp_step = 'iterative', gbd_round_id = 7,
                                          filepath =  "filepath")


sev.stunting.bundle.version <- save_bundle_version(bundle_id = 7808, decomp_step = "iterative", gbd_round_id = 7)



sev.stunting.as$crosswalk_parent_seq <- NA
sev.stunting.as$uncertainty_type_value <- 95
sev.stunting.as$unit_value_as_published <- NULL
sev.stunting.as$unit_value_as_published <- 1


write.xlsx(sev.stunting.as, 
           file = "filepath",
           sheetName = "extraction")

sev.stunt.xw <- save_crosswalk_version(bundle_version_id = 30200, 
                                       data_filepath ="filepath", 
                                       description = "Age Sex Split Severe Stunting Crosswalk")





#### mean_stunting age sex specific ####







mean.stunting <- get_bundle_version(bundle_version_id = 30185, fetch = 'all')


mean.stunting.as <- mean.stunting[sex == "Male" | sex == "Female"]
mean.stunting.as <- mean.stunting.as[age_group_id == 2 |
                                       age_group_id == 3 |
                                       age_group_id == 388 |
                                       age_group_id == 389 |
                                       age_group_id == 238 |
                                       age_group_id == 34]

current.as.mean.stunting <- get_bundle_data(bundle_id = 7811, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = current.as.mean.stunting$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

mean.stunting.delete.upload <- upload_bundle_data(bundle_id = 7811, decomp_step = 'iterative', gbd_round_id = 7,
                                                  filepath =  "filepath")


write.xlsx(mean.stunting.as, 
           file = "filepath",
           sheetName = "extraction")

mean.stunting.upload <- upload_bundle_data(bundle_id = 7811, decomp_step = 'iterative', gbd_round_id = 7,
                                           filepath =  "filepath")


mean.stunting.bundle.version <- save_bundle_version(bundle_id = 7811, decomp_step = "iterative", gbd_round_id = 7)

mean.stunting.as$crosswalk_parent_seq <- NA
mean.stunting.as$uncertainty_type_value <- 95
mean.stunting.as$unit_value_as_published <- NULL
mean.stunting.as$unit_value_as_published <- 1


write.xlsx(mean.stunting.as, 
           file = "filepath",
           sheetName = "extraction")

mean.stunt.xw <- save_crosswalk_version(bundle_version_id = 30203, 
                                        data_filepath ="filepath", 
                                        description = "Age Sex Split Mean Stunting Crosswalk")




#### mild underweight age sex specific ####





mild.underweight <- get_bundle_version(bundle_version_id = 30149, fetch = 'all')


mild.underweight.as <- mild.underweight[sex == "Male" | sex == "Female"]
mild.underweight.as <- mild.underweight.as[age_group_id == 2 |
                                             age_group_id == 3 |
                                             age_group_id == 388 |
                                             age_group_id == 389 |
                                             age_group_id == 238 |
                                             age_group_id == 34]

current.as.mild.underweight <- get_bundle_data(bundle_id = 7814, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = current.as.mild.underweight$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

mild.underweight.delete.upload <- upload_bundle_data(bundle_id = 7814, decomp_step = 'iterative', gbd_round_id = 7,
                                                     filepath =  "filepath")


write.xlsx(mild.underweight.as, 
           file = "filepath",
           sheetName = "extraction")

mild.underweight.upload <- upload_bundle_data(bundle_id = 7814, decomp_step = 'iterative', gbd_round_id = 7,
                                              filepath =  "filepath")


mild.underweight.bundle.version <- save_bundle_version(bundle_id = 7814, decomp_step = "iterative", gbd_round_id = 7)




mild.underweight.as$crosswalk_parent_seq <- NA
mild.underweight.as$uncertainty_type_value <- 95
mild.underweight.as$unit_value_as_published <- NULL
mild.underweight.as$unit_value_as_published <- 1


write.xlsx(mild.underweight.as, 
           file ="filepath",
           sheetName = "extraction")

mild.underweight.xw <- save_crosswalk_version(bundle_version_id = 30206, 
                                              data_filepath ="filepath", 
                                              description = "Age Sex Split Mild Underweight Crosswalk")








#### moderate underweight age sex specific ####



moderate.underweight <- get_bundle_version(bundle_version_id = 30164, fetch = 'all')


moderate.underweight.as <- moderate.underweight[sex == "Male" | sex == "Female"]
moderate.underweight.as <- moderate.underweight.as[age_group_id == 2 |
                                                     age_group_id == 3 |
                                                     age_group_id == 388 |
                                                     age_group_id == 389 |
                                                     age_group_id == 238 |
                                                     age_group_id == 34]

current.as.moderate.underweight <- get_bundle_data(bundle_id = 7817, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = current.as.moderate.underweight$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

moderate.underweight.delete.upload <- upload_bundle_data(bundle_id = 7817, decomp_step = 'iterative', gbd_round_id = 7,
                                                         filepath =  "filepath")


write.xlsx(moderate.underweight.as, 
           file = "filepath",
           sheetName = "extraction")

moderate.underweight.upload <- upload_bundle_data(bundle_id = 7817, decomp_step = 'iterative', gbd_round_id = 7,
                                                  filepath =  "filepath")


moderate.underweight.bundle.version <- save_bundle_version(bundle_id = 7817, decomp_step = "iterative", gbd_round_id = 7)




moderate.underweight.as$crosswalk_parent_seq <- NA
moderate.underweight.as$uncertainty_type_value <- 95
moderate.underweight.as$unit_value_as_published <- NULL
moderate.underweight.as$unit_value_as_published <- 1


write.xlsx(moderate.underweight.as, 
           file = "filepath",
           sheetName = "extraction")

mod.underweight.xw <- save_crosswalk_version(bundle_version_id = 30209, 
                                             data_filepath ="filepath", 
                                             description = "Age Sex Split Moderate Underweight Crosswalk")








#### severe underweight age sex specific ####


severe.underweight <- get_bundle_version(bundle_version_id = 30188, fetch = 'all')


severe.underweight.as <- severe.underweight[sex == "Male" | sex == "Female"]
severe.underweight.as <- severe.underweight.as[age_group_id == 2 |
                                                 age_group_id == 3 |
                                                 age_group_id == 388 |
                                                 age_group_id == 389 |
                                                 age_group_id == 238 |
                                                 age_group_id == 34]

current.as.severe.underweight <- get_bundle_data(bundle_id = 7820, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = current.as.severe.underweight$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

severe.underweight.delete.upload <- upload_bundle_data(bundle_id = 7820, decomp_step = 'iterative', gbd_round_id = 7,
                                                       filepath =  "filepath")


write.xlsx(severe.underweight.as, 
           file = "filepath",
           sheetName = "extraction")

severe.underweight.upload <- upload_bundle_data(bundle_id = 7820, decomp_step = 'iterative', gbd_round_id = 7,
                                                filepath =  "filepath")


severe.underweight.bundle.version <- save_bundle_version(bundle_id = 7820, decomp_step = "iterative", gbd_round_id = 7)


severe.underweight.as$crosswalk_parent_seq <- NA
severe.underweight.as$uncertainty_type_value <- 95
severe.underweight.as$unit_value_as_published <- NULL
severe.underweight.as$unit_value_as_published <- 1


write.xlsx(severe.underweight.as, 
           file = "filepath",
           sheetName = "extraction")

sev.underweight.xw <- save_crosswalk_version(bundle_version_id = 30212, 
                                             data_filepath ="filepath", 
                                             description = "Age Sex Split Severe Underweight Crosswalk")





#### mean underweight age sex specific ####



mean.underweight <- get_bundle_version(bundle_version_id = 30188, fetch = 'all')


mean.underweight.as <- mean.underweight[sex == "Male" | sex == "Female"]
mean.underweight.as <- mean.underweight.as[age_group_id == 2 |
                                             age_group_id == 3 |
                                             age_group_id == 388 |
                                             age_group_id == 389 |
                                             age_group_id == 238 |
                                             age_group_id == 34]

current.as.mean.underweight <- get_bundle_data(bundle_id = 7823, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = current.as.mean.underweight$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

mean.underweight.delete.upload <- upload_bundle_data(bundle_id = 7823, decomp_step = 'iterative', gbd_round_id = 7,
                                                     filepath =  "filepath")


write.xlsx(mean.underweight.as, 
           file = "filepath",
           sheetName = "extraction")

mean.underweight.upload <- upload_bundle_data(bundle_id = 7823, decomp_step = 'iterative', gbd_round_id = 7,
                                              filepath =  "filepath")


mean.underweight.bundle.version <- save_bundle_version(bundle_id = 7823, decomp_step = "iterative", gbd_round_id = 7)



mean.underweight.as$crosswalk_parent_seq <- NA
mean.underweight.as$uncertainty_type_value <- 95
mean.underweight.as$unit_value_as_published <- NULL
mean.underweight.as$unit_value_as_published <- 1


write.xlsx(mean.underweight.as, 
           file = "filepath",
           sheetName = "extraction")

mean.underweight.xw <- save_crosswalk_version(bundle_version_id = 30215, 
                                              data_filepath ="filepath", 
                                              description = "Age Sex Split Mean Underweight Crosswalk")





#### mild wasting age sex specific ####

mild.wasting <- get_bundle_version(bundle_version_id = 30158, fetch = 'all')


mild.wasting.as <- mild.wasting[sex == "Male" | sex == "Female"]
mild.wasting.as <- mild.wasting.as[age_group_id == 2 |
                                     age_group_id == 3 |
                                     age_group_id == 388 |
                                     age_group_id == 389 |
                                     age_group_id == 238 |
                                     age_group_id == 34]

current.as.mild.wasting <- get_bundle_data(bundle_id = 7826, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = current.as.mild.wasting$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

mild.wasting.delete.upload <- upload_bundle_data(bundle_id = 7826, decomp_step = 'iterative', gbd_round_id = 7,
                                                 filepath =  "filepath")


write.xlsx(mild.wasting.as, 
           file = "filepath",
           sheetName = "extraction")

mild.wasting.upload <- upload_bundle_data(bundle_id = 7826, decomp_step = 'iterative', gbd_round_id = 7,
                                          filepath =  "filepath")


mild.wasting.bundle.version <- save_bundle_version(bundle_id = 7826, decomp_step = "iterative", gbd_round_id = 7)



mild.wasting.as$crosswalk_parent_seq <- NA
mild.wasting.as$uncertainty_type_value <- 95
mild.wasting.as$unit_value_as_published <- NULL
mild.wasting.as$unit_value_as_published <- 1


write.xlsx(mild.wasting.as, 
           file = "filepath",
           sheetName = "extraction")

mild.wasting.xw <- save_crosswalk_version(bundle_version_id = 30218, 
                                          data_filepath ="filepath", 
                                          description = "Age Sex Split Mild Wasting Crosswalk")





#### moderate wasting age sex specific ####




moderate.wasting <- get_bundle_version(bundle_version_id = 30152, fetch = 'all')


moderate.wasting.as <- moderate.wasting[sex == "Male" | sex == "Female"]
moderate.wasting.as <- moderate.wasting.as[age_group_id == 2 |
                                             age_group_id == 3 |
                                             age_group_id == 388 |
                                             age_group_id == 389 |
                                             age_group_id == 238 |
                                             age_group_id == 34]

current.as.moderate.wasting <- get_bundle_data(bundle_id = 7829, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = current.as.moderate.wasting$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

moderate.wasting.delete.upload <- upload_bundle_data(bundle_id = 7829, decomp_step = 'iterative', gbd_round_id = 7,
                                                     filepath =  "filepath")


write.xlsx(moderate.wasting.as, 
           file = "filepath",
           sheetName = "extraction")

moderate.wasting.upload <- upload_bundle_data(bundle_id = 7829, decomp_step = 'iterative', gbd_round_id = 7,
                                              filepath =  "filepath")


moderate.wasting.bundle.version <- save_bundle_version(bundle_id = 7829, decomp_step = "iterative", gbd_round_id = 7)



moderate.wasting.as$crosswalk_parent_seq <- NA
moderate.wasting.as$uncertainty_type_value <- 95
moderate.wasting.as$unit_value_as_published <- NULL
moderate.wasting.as$unit_value_as_published <- 1


write.xlsx(moderate.wasting.as, 
           file =  "filepath",
           sheetName = "extraction")

mod.wasting.xw <- save_crosswalk_version(bundle_version_id = 30221, 
                                         data_filepath ="filepath", 
                                         description = "Age Sex Split Moderate Wasting Crosswalk")









#### severe wasting age sex specific ####





severe.wasting <- get_bundle_version(bundle_version_id = 30155, fetch = 'all')


severe.wasting.as <- severe.wasting[sex == "Male" | sex == "Female"]
severe.wasting.as <- severe.wasting.as[age_group_id == 2 |
                                         age_group_id == 3 |
                                         age_group_id == 388 |
                                         age_group_id == 389 |
                                         age_group_id == 238 |
                                         age_group_id == 34]

current.as.severe.wasting <- get_bundle_data(bundle_id = 7832, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = current.as.severe.wasting$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

severe.wasting.delete.upload <- upload_bundle_data(bundle_id = 7832, decomp_step = 'iterative', gbd_round_id = 7,
                                                   filepath =  "filepath")


write.xlsx(severe.wasting.as, 
           file = "filepath",
           sheetName = "extraction")

severe.wasting.upload <- upload_bundle_data(bundle_id = 7832, decomp_step = 'iterative', gbd_round_id = 7,
                                            filepath =  "filepath")


severe.wasting.bundle.version <- save_bundle_version(bundle_id = 7832, decomp_step = "iterative", gbd_round_id = 7)

severe.wasting.as$crosswalk_parent_seq <- NA
severe.wasting.as$uncertainty_type_value <- 95
severe.wasting.as$unit_value_as_published <- NULL
severe.wasting.as$unit_value_as_published <- 1


write.xlsx(severe.wasting.as, 
           file = "filepath",
           sheetName = "extraction")

sev.wasting.xw <- save_crosswalk_version(bundle_version_id = 30224, 
                                         data_filepath ="filepath", 
                                         description = "Age Sex Split Severe Wasting Crosswalk")



#### mean wasting age sex specific  ####


mean.wasting <- get_bundle_version(bundle_version_id = 30191, fetch = 'all')


mean.wasting.as <- mean.wasting[sex == "Male" | sex == "Female"]
mean.wasting.as <- mean.wasting.as[age_group_id == 2 |
                                     age_group_id == 3 |
                                     age_group_id == 388 |
                                     age_group_id == 389 |
                                     age_group_id == 238 |
                                     age_group_id == 34]

current.as.mean.wasting <- get_bundle_data(bundle_id = 7835, decomp_step = 'iterative', gbd_round_id = 7)


to.delete <- data.table(seq = current.as.mean.wasting$seq)

write.xlsx(to.delete, 
           file = "filepath",
           sheetName = "extraction")

mean.wasting.delete.upload <- upload_bundle_data(bundle_id = 7835, decomp_step = 'iterative', gbd_round_id = 7,
                                                 filepath =  "filepath")


write.xlsx(mean.wasting.as, 
           file = "filepath",
           sheetName = "extraction")

mean.wasting.upload <- upload_bundle_data(bundle_id = 7835, decomp_step = 'iterative', gbd_round_id = 7,
                                          filepath =  "filepath")


mean.wasting.bundle.version <- save_bundle_version(bundle_id = 7835, decomp_step = "iterative", gbd_round_id = 7)



mean.wasting.as$crosswalk_parent_seq <- NA
mean.wasting.as$uncertainty_type_value <- 95
mean.wasting.as$unit_value_as_published <- NULL
mean.wasting.as$unit_value_as_published <- 1


write.xlsx(mean.wasting.as, 
           file = "filepath",
           sheetName = "extraction")

mean.wasting.xw <- save_crosswalk_version(bundle_version_id = 30227, 
                                          data_filepath ="filepath", 
                                          description = "Age Sex Split Mean Wasting Crosswalk")




































