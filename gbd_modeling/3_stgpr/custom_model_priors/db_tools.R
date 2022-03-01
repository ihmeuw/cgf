
### Project: ST-GPR
### Purpose: Database utility functions
###########################################################

###################
### Setting up ####
###################
library(data.table)
library(stringr)
library(plyr)
#library(dplyr)
library(RMySQL)
library(parallel)

####################################################################################################################################################
# 															   Table of Contents
####################################################################################################################################################

## Base
	## query

## Pulls
	## get_ages
	## get_sexes
	## get_location_hierarchy
	## get_demographics
	## get_covariate_metadata
	## get_covariates
	## get_citations
  ## get_gbd_round_id
  ## get_location_set_id
  ##get_best_lvid 

## Cooper
	## get_nid_meta

## Utilities
	## detect_demographics
	## age_sex_spec
	## age_sex_merge
	## agg_vars
	## get_names
	## make_square


####################################################################################################################################################
# 																	 Base
####################################################################################################################################################

query <- function(query,conn_def) {
  odbc <- read.ini("filepath")
  conn <- dbConnect(RMySQL::MySQL(), 
                    host = odbc[[conn_def]]$server, 
                    username = odbc[[conn_def]]$user, 
                    password = odbc[[conn_def]]$password)
  dt <- dbGetQuery(conn,query) %>% data.table
  dbDisconnect(conn)
  return(dt)
}

####################################################################################################################################################
# 																	 Pulls
####################################################################################################################################################

get_ages <- function(age_group_set_id=19) {
	dbname <- "shared"
	host   <- "cod"
	q  <- paste0("SELECT age_group_id FROM shared.age_group_set_list WHERE age_group_set_id=", age_group_set_id)
	age.list <- query(q, host)$age_group_id

	q <- paste0("SELECT * FROM shared.age_group")
	ages <- query(q, host)
	ages <- ages[age_group_id %in% age.list]
}

#####################################################################################################################################################

get_sexes <- function() {
  host   <- "cod"
  q  <- "SELECT * FROM shared.sex"
  query(q, host)
}

#####################################################################################################################################################

get_location_hierarchy <- function(location_set_id = 22, gbd_round_id = 7, standard_location_set_id = 101) {
  
  #pull best location_set_version_id from location_set_version_active
	host   <- "cod"
	q  <- sprintf("SELECT * FROM shared.location_set_version_active WHERE location_set_id=%i AND gbd_round_id=%i",
	              location_set_id, gbd_round_id)
	lvid <- query(q, host)[, location_set_version_id]
	
	if(gbd_round_id > 5){
	  q <- sprintf("SELECT * FROM shared.location_set_version_active WHERE location_set_id=%i AND gbd_round_id=%i",
	               standard_location_set_id, gbd_round_id)
	  standard_locs_lvid <- query(q, host)[, location_set_version_id]
	}

	#pull location hierarchy based on best location_set_version_id for given hierarchy and GBD round
	q <- sprintf('SELECT * FROM shared.location_hierarchy_history WHERE location_set_version_id=%i', lvid)
	df <- query(q, host)
	
	if(gbd_round_id > 5){
	  q <- sprintf('SELECT * FROM shared.location_hierarchy_history WHERE location_set_version_id=%i', standard_locs_lvid)
	  std_locs <- query(q, host)$location_id
	}
	
	#assert existing location_set
	if(nrow(df) == 0){
	  stop("Locations dataframe is empty! Make sure your location_set_id and gbd_round_id are legit.")
	}

	## Create hierarchy
	hierarchy <- str_split_fixed(df$path_to_top_parent, ",", max(df$level) + 1) %>% data.table
	hierarchy <- hierarchy[, lapply(.SD, as.numeric)]
	setnames(hierarchy, names(hierarchy), paste0("level_", seq(0, max(df$level))))
	df <- cbind(df, hierarchy)
	
	#Create indicator column for standard locations, using the janky setup from GBD2017 for all rounds before GBD2019
	if(gbd_round_id > 5){
	  df[, standard_location:=ifelse(location_id %in% std_locs, 1, 0)]
	}else{
	  df[, standard_location:=ifelse(level == 3 | location_id %in% c(4749, 4636, 434, 433), 1, 0)]
	}
	
	return(df)
}

#####################################################################################################################################################

get_gbd_round_id <- function(location_set_version_id){
  
  #Databaser stuff
  host   <- "cod"
  
  ## Find active version id if not specified
  q <- paste0("SELECT gbd_round FROM shared.location_set_version WHERE location_set_version_id = ", location_set_version_id)
  gbd_round <- query(q, host)
  
  #Re-query to match gbd_round (only available thing) to gbd_round_id
  q <- paste0("SELECT gbd_round_id FROM shared.gbd_round WHERE gbd_round = ", gbd_round)
  gbd_round_id <- query(q, host) %>% as.integer()
  
  return(gbd_round_id)
  
}

#####################################################################################################################################################

get_location_set_id <- function(location_set_version_id){

  host   <- "cod"
  q <- paste0("SELECT location_set_id from shared.location_set_version WHERE location_set_version_id = ", location_set_version_id)
  location_set_id <- query(q, host) %>% unlist %>% unname
  
  return(location_set_id)
}

#####################################################################################################################################################

get_best_lvid <- function(location_set_id, gbd_round_id){
  host   <- "cod"
  q <- paste0("SELECT shared.active_location_set_version(", location_set_id, ",", gbd_round_id, ") as location_set_version_id")
  location_set_best <- query(q, host) %>% unlist %>% unname
  
  return(location_set_best)
}

#####################################################################################################################################################

get_demographics <- function(location_set_id,
                             gbd_round_id=7,
              							 year_start, year_end, 
              							 by_sex=1, by_age=1, 
              							 custom_sex_id=NULL, custom_age_group_id=NULL) {
	## Locations
	locs <- get_location_hierarchy(location_set_id, gbd_round_id)[level >= 3]$location_id
	## Years
	years <- seq(year_start, year_end,1)
	## Sexes
	if (is.blank(custom_sex_id)) { 	
		if (by_sex == 1) {
			sexes <- c(1,2)
		} else if (by_sex == 0) {
			sexes <- 3
		}
	} else {
		sexes <- custom_sex_id
	}
	## Ages
	if (is.blank(custom_age_group_id)) {
		if (by_age==1) {
			ages <- get_ages()$age_group_id
		} else if (by_age==0) {
			ages <- 22
		}
	} else {
		ages <- custom_age_group_id
	}

	## Expand
	df <- data.table(expand.grid(location_id = locs, year_id=years, sex_id=sexes, age_group_id=ages))
	## Force integer
	df <- df[, lapply(.SD, as.character)]
	df <- df[, lapply(.SD, as.integer)]

	return(df)
}

#####################################################################################################################################################

get_covariate_metadata <- function(list=NULL) {
	## Where clause
	if (is.blank(list)) {
		where <- ""
	} else {
		where <- paste0("WHERE LOWER(covariate_name_short) in (", tolower(toString(shQuote(list))), ")")
	}
	
	## Pull
	host <- "cod"
	q <- paste0("SELECT * FROM shared.covariate ", where)
	df <- query(q, host)

	## Convert names to lowercase
	df$covariate_name_short <- tolower(df$covariate_name_short)

	## Throw an error check if not int output
	if (!is.blank(list)) {
		return <- df$covariate_name_short
		if (length(list[which(!list %in% return)]) > 0) {
			stop(paste0("The following covariates do not exist in the db: ", toString(list[which(!list %in% return)])))
		}
	}
	return(df)
}

#####################################################################################################################################################

pull_covariate <- function(cov, ci=FALSE, covariate_name_short, gbd_round_id) {

#  host <- 'covariates'
  
  #  q = sprintf("
  #            SELECT
  #            model.model_version_id,
  #            covariate.covariate_id,
  #            covariate.covariate_name_short,
  #            model.location_id,
  #            location.location_name,
  #            model.year_id,
  ##           model.age_group_id,
  #            age_group.age_group_name,
  #            model.sex_id,
  #            model.mean_value,
  #            model.lower_value,
  #            model.upper_value
  #            FROM covariate.model
  #            JOIN covariate.model_version
  #            on model.model_version_id=model_version.model_version_id
  #            JOIN covariate.data_version
  #            on model_version.data_version_id=data_version.data_version_id
  #            JOIN shared.covariate
  #            on data_version.covariate_id=covariate.covariate_id
  #            JOIN shared.location
  #            on model.location_id=location.location_id
  #            JOIN shared.age_group
  #            on model.age_group_id=age_group.age_group_id
  #            WHERE
  #            is_best = 1
  #            AND
  #            gbd_round_id = %i
  #            AND
  #            covariate.last_updated_action!='delete' AND
  #            covariate.covariate_id=%i
  #            ", gbd_round_id, cov)
  
  # data <- query(q, host)

	## HT: fixed covariate SQL pull and don't trust central functions to stay working
  if (gbd_round_id == 6){
    source('/home/j/temp/central_comp/libraries/2019_gbd_env/r/get_covariate_estimates.R')
  }else{
    source('/ihme/cc_resources/libraries/current/r/get_covariate_estimates.R')
  }
	data<-get_covariate_estimates(covariate_id = cov, location_set_id = location_set_id, gbd_round_id = gbd_round_id)
	data<-data[,.(location_id, year_id, age_group_id, sex_id, mean_value)]
	setnames(data, "mean_value", paste0(covariate_name_short))

#	locs <- unique(data$location_id)
#	if (!(4749 %in% locs)|!(44533 %in% locs)) data <- janky.covariate.fix(data)

	return(data)

}

#####################################################################################################################################################

get_covariates <- function(list, location_set_id, prediction_years, ci=FALSE, gbd_round_id = 7) {

	## Get metadata
	meta <- get_covariate_metadata(list)

	## Parse into age/sex specific, age specific, sex specific
	meta <- data.table(meta)
	meta <- meta[, .(covariate_id, covariate_name_short, by_sex, by_age)]
	age_sex <- meta[by_sex == 1 & by_age == 1, .(covariate_id, covariate_name_short)]
	age <- meta[by_sex == 0 & by_age==1, .(covariate_id, covariate_name_short)]
	sex <- meta[by_sex == 1 & by_age==0, .(covariate_id, covariate_name_short)]
	all <- meta[by_sex == 0 & by_age==0, .(covariate_id, covariate_name_short)]

	## Reorder from the most specific to most general
	frame <- unique(rbind(age_sex, age, sex, all)[, .(covariate_id, covariate_name_short)])
	#list <- unique(rbind(age_sex, age, sex, all)$covariate_name_short)
	
	# Pull and Merge Covariates
	flag <- 0
	output <- NULL
	for (cov in frame$covariate_id) {

	  cov_name_short <- frame[cov == covariate_id, covariate_name_short]
		data <- pull_covariate(cov, ci, covariate_name_short = cov_name_short, gbd_round_id = gbd_round_id)
		
		#make sure covariate has estimates for all prediction years
	  if(!all(prediction_years %in% unique(data$year_id))){
	    stop(sprintf('Covariate %s does not have estimates for all years between %i and %i!',
	                 cov_name_short, prediction_years[1], prediction_years[length(prediction_years)]))
	  }

		if (flag == 0) {
			output <- data
			flag <- 1
		} else {
			output <- age_sex_merge(output, data)
		}
	}
		
	return(output)
}

#####################################################################################################################################################

get_covariate_version <- function(list, gbd_round_id = 7) {
  host <- "covariates"
  
  q <- sprintf("SELECT
					model_version.covariate_id,
					covariate_name_short,
					model_version.model_version_id,
                    model_version.last_updated,
					model_version.description 			
					FROM covariate.model_version 
                    JOIN shared.covariate USING (covariate_id)
					WHERE covariate.last_updated_action!='DELETE' 
					AND is_best=1
          AND gbd_round_id in (%i)
          AND LOWER(covariate_name_short) in (%s);", gbd_round_id, tolower(toString(shQuote(list))))
  
  query(q, host)
}

####################################################################################################################################################
# 																	 cooper
####################################################################################################################################################

get_nid_meta <- function(nids, meta=NULL) {

	## Settings
	meta_allow <- c("citation", "path", "iso3", "year")
	meta_bad <- setdiff(meta, meta_allow)
	if (length(meta_bad) > 0) {
		stop(paste0("Bad meta: ", meta_bad))
	}

	## Pulls
	if ("citation" %in% meta) {
		host <- "cod"
		q <- paste0("SELECT 
						nid, field_citation_value, series_title
						FROM
						shared.mv_citation
						WHERE nid in (", toString(nids), ")")
		citation <- query(q, host) %>% unique
	}
	if ("path" %in% meta) {
		host <- "cod"
		q <- paste0("SELECT 
					nid, file_location
					FROM
					shared.mv_nid_file
					WHERE nid in (", toString(nids), ")")
		path <- query(q, host) %>% unique
	}
	if ("iso3" %in% meta) {
		host <- "ghdx"
		q <- paste0("SELECT 
					field_data_field_geography.entity_id as nid, 
					field_iso3_value as iso3,
					field_location_id_value as location_id
					FROM ghdx.field_data_field_geography 
					JOIN ghdx.field_data_field_iso3 ON field_data_field_geography.field_geography_tid = field_data_field_iso3.entity_id
					JOIN ghdx.field_data_field_location_id ON field_data_field_geography.field_geography_tid = field_data_field_location_id.entity_id
					WHERE field_data_field_geography.entity_id in (", toString(nids), ")")
		iso3 <- query(q, host) %>% unique
	}
	if ("year" %in% meta) {
		host <- "ghdx"
		q <- paste0("SELECT 
					entity_id as nid, field_time_value as year_start, field_time_value2 as year_end
					FROM
					ghdx.field_data_field_time
					WHERE entity_id in (", toString(nids), ")")
		year <- query(q, host) %>% unique
		year <- year[, c("year_start", "year_end") := lapply(.SD, function(x) gsub("[-].*", "", x)), .SDcols = c("year_start", "year_end")]
	}

	## Merge
	out <- get(meta[1])
	if (length(meta) > 1)for (i in 2:length(meta)) out <- merge(out, get(meta[i]), by='nid', all=TRUE)

	return(out)
}

#####################################################################################################################################################

get_path_nid <- function(paths) {
	host <- "cod"
	q <- paste0("SELECT
	         nid, file_location, file_name
	         FROM
	         shared.mv_nid_file")
	df <- query(q, host) %>% unique
	df <- df[, file_path := paste0(gsub("[\\]", "/", file_location), "/", file_name)]
	df <- df[, c("file_location", "file_name") := NULL]
	df <- df[file_path %in% paths]
	return(df)
}

####################################################################################################################################################
# 																	 Utility
####################################################################################################################################################

is.blank <- function(x) {
	 any(is.null(x))  || any(is.na(x))  || any(is.nan(x)) 
}

#####################################################################################################################################################


detect_demographics <- function(df) {
	vars <- c("location_id", "year_id", "age_group_id", "sex_id")
	## Check if data frame has id's and only return on those that exist
	vars <- names(df)[names(df) %in% vars]
	demos <- lapply(vars, function(x) unique(df[[x]]))
	names(demos) <- vars
	return(demos)
}

#####################################################################################################################################################


age_sex_spec <- function(df) {	
	demos <- detect_demographics(df)
	by_age <- ifelse(((22 %in% demos$age_group_id) | (27 %in% demos$age_group_id)), 0, 1)
	by_sex <- ifelse(3 %in% demos$sex_id, 0 , 1)
	spec <- cbind(by_age, by_sex) %>% data.table
	return(spec)
}

#####################################################################################################################################################

age_sex_merge <- function(df1, df2) {

	## Find specificity
	spec1 <- age_sex_spec(df1)
	spec2 <- age_sex_spec(df2)
	test <- data.table(spec1 == spec2)

	## Merge
	cols <- c("location_id", "year_id", "age_group_id", "sex_id")
	drop_cols <- NULL
	merge_cols <- col

	## If age and sex match
	if (test$by_age & test$by_sex) {
		df <- merge(df1, df2, by=cols)
	} else {
		## If age matches but sex doesn't match
		if (test$by_age & !test$by_sex) {
			drop_cols <- "sex_id"
		}	
		## If age doesnt match and sex matches
		else if (!test$by_age & test$by_sex) {
			drop_cols <- "age_group_id"
		}
		## If neither match
		else {
			drop_cols <- c("sex_id", "age_group_id")
		}
		## Merge
		merge_cols <- cols[!cols %in% drop_cols]
		df <- merge(df1, df2[, drop_cols := NULL, with=F], by=merge_cols)
	}
	return(df)
}

#####################################################################################################################################################

agg_vars <- function(df, location_set_id, gbd_round_id, vars, parent_ids) {

	## Merge on populations if need be
	if (!("pop_scaled" %in% names(df))) {
		demos <- detect_demographics(df)
		year_start <- min(demos$year_id)
		year_end <- max(demos$year_id)
		custom_sex_id <- demos$sex_id
		custom_age_group_id <- demos$age_group_id
		pops <- get_populations(location_set_id, gbd_round_id, year_start, year_end, custom_sex_id=custom_sex_id, custom_age_group_id=custom_age_group_id)
		df <- merge(df, pops, by=c("location_id", "year_id", "age_group_id", "sex_id")) 
	}	

	## Merge on parent_ids if need be
	if (!("parent_id" %in% names(df))) {
		locs <- get_location_hierarchy(location_set_id, gbd_round_id)[, .(location_id, parent_id)]
		df <- merge(df, locs, by="location_id")
	}

	## Subset to requested parent_ids
	df <- df[parent_id %in% parent_ids]

	## Aggregate estimates to the parent_id [ sum(var * pop) /sum(pop) ]
	df[, (vars) := lapply(.SD, function(x) sum(x * df[['pop_scaled']])/sum(df[['pop_scaled']])), .SDcols=vars, by=c("parent_id", "year_id", "age_group_id", "sex_id")]
	## De-duplicate so get one set of estimates
	df <- unique(df[, c("parent_id", "year_id", "age_group_id", "sex_id", vars), with=F])
	## Rename parent_id -> location_id
	setnames(df, "parent_id", "location_id")

	return(df)        
}

#####################################################################################################################################################

get_names <- function(df) {

	## Given a data.table with location, age_group, sex, merge on namescu
	required <- c("location_id", "age_group_id", "sex_id") 
	missing <- required[!required %in% names(df)]
	if (length(missing) > 0) stop(paste0("Missing required columns: ", toString(required)))

	## Detect what needs names
	cols <- c("ihme_loc_id", "location_name", "age_group_name", "sex")
	need <- cols[!cols %in% names(df)]

	## Names
	if ("ihme_loc_id" %in% need) df <- merge(df, get_location_hierarchy(41)[,.(location_id, ihme_loc_id)], by="location_id", all.x=T)
	if ("location_name" %in% need) df <- merge(df, get_location_hierarchy(41)[,.(location_id, location_name)], by="location_id", all.x=T)
	if ("age_group_name" %in% need) df <- merge(df, get_ages()[,.(age_group_id, age_group_name)], by="age_group_id", all.x=T)
	if ("sex" %in% need) df <- merge(df, get_sexes()[,.(sex_id, sex)], by="sex_id", all.x=T)
		
	return(df)
}

#####################################################################################################################################################

make_square <- function(location_set_id, gbd_round_id,
						year_start, year_end,
						by_sex=1, by_age=1,
						custom_sex_id=NULL, custom_age_group_id=NULL,
						covariates=NULL, population=FALSE) {	
  
	## Skeleton
	df <- get_demographics(location_set_id, gbd_round_id, 
							 year_start, year_end, 
							 by_sex, by_age, 
							 custom_sex_id, custom_age_group_id)

	## Covariates
	prediction_years <- seq(year_start, year_end)
	
	if (!is.null(covariates) & !is.na(covariates)) {
		covs <- get_covariates(list = covariates, location_set_id = location_set_id, gbd_round_id = gbd_round_id, prediction_years = prediction_years)
		df <- age_sex_merge(df, covs)
	}

	return(df)

}


####################################################################################################################################################
# 															     Janky Shit
####################################################################################################################################################

# ## 4749, 44533 fix to create if missing
janky.covariate.fix <- function(df) {

	locs <-unique(df$location_id)

	childs 	<- c(4749, 44533) 
	parents	<- c(95, 6)
	
	for (i in 1:length(childs)) {
		child <- childs[[i]]
		parent <- parents[[i]]
		if (!(child %in% locs)) {
			df.parent <- df[location_id==parent]
			df.parent <- df.parent[, location_id := child]
			df <- rbind(df, df.parent)
		}
	}

	return(df)
}

china.hierarchy.fix <- function(df) {

	## Reroute China subnationals to level 4, make CHN w/o HKG and MAC, HKG, MAC level 3
	## Remove China
	df <- df[location_id != 6]
		## Remove from path to top parent
		df <- df[, path_to_top_parent := gsub(",6,", ",", path_to_top_parent)]
	## Reroute 44533, 354, 361 to level 3
	df <- df[location_id %in% c(44533, 354, 361), level := 3]
	## Reroute the other subnationals to level 4
	df <- df[grepl("CHN", df$ihme_loc_id) & level == 5, level := 4]

	return(df)
	
}

get.subnat.locs.and.lvls <- function(location_set_id, gbd_round_id, get.subnats = F, national_level = 3){
  
  ids <- c("all_subnat_locs","lvl6_subnat_locs", "lvl5_subnat_locs", "lvl4_subnat_locs")
  
  df <- get_location_hierarchy(location_set_id, gbd_round_id = gbd_round_id)

  #set max level observed
  max_level <- df[, level] %>% max
  df <- df[level >= national_level]
  df[, c("level_0"):=NULL]
  
  #get max level by grabbing level-3 location_id of locs at subnat levels (>lvl4)
  grps <- c()
  all_subnat_locs <- c()
  if(max_level == national_level){
    for (x in ids) assign(x, NA)
    grps <- ids
  } else{
    for(n in seq(max_level,4, -1)){
      assign("lvl", paste0("level_", n))
      out <- paste0("lvl", n, "_subnat_locs")
      
      assign(out, df[!is.na(get(lvl)), str_split_fixed(path_to_top_parent, ",", max_level+1)[, 4]] %>% unique %>% as.numeric)
      
      #make sure that sets are disjoint (ie only loc_ids with MAX lvl6 in lvl6_subnat_locs)
      if(n < max_level){
        for (j in c(1:(max_level - n))){
          assign(out, get(out)[!(get(out) %in% get(paste0("lvl", n+j, "_subnat_locs")))])
        } #this is an excellent moment for a jojo reference...
      }
  
      #add lists to a full vector so we can iterate through + grab subnats
      grps <- append(out, grps)
      
    }
    
    #append all disjoint subnat levels together for a full subnat grouping
    all_subnat_locs <- c()
    for(grp in grps){
      all_subnat_locs <- dplyr::union(all_subnat_locs, get(grp))
    }
    
    #make sure all outputs exist, even if those levels not in location hierarchy
    grps <- append(grps, "all_subnat_locs")
    for(item in ids){
      #if(exception_locs == item){
      if(!(item %in% grps)) assign(item, NA)
    }
  }

  #finally, return subnats (either to global environment or to a named list)
  if(get.subnats==F){
    for(item in ids){
      #if(exception_locs == item){
      if(!(item %in% grps)) assign(item, NA)
      assign(item, get(item), envir = globalenv())
    }
  } else{
    sublocs <- list(all_subnat_locs = all_subnat_locs, lvl4_subnat_locs = lvl4_subnat_locs, lvl5_subnat_locs = lvl5_subnat_locs, lvl6_subnat_locs = lvl6_subnat_locs)
    return(sublocs)
  }
  
  
}


