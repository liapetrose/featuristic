#----------------------------------------------------------------------------#

#' @title Compile features (Stage II).
#'
#' @description \
#'
#' @export
#' @param cohort_path 
#' @param control_path 
#' @param data_path    
#' @param feature_path  
#' @param feature_set_id     
#' @param feature_set_prefix 
#' @return
#' @examples

feature_compilation <- function(cohort_path, control_path, data_path, feature_path, 
	feature_set_id, feature_set_prefix) {

	
	#----------------------------------------------------------------------------#
	#                		             SETUP                                   #
	#----------------------------------------------------------------------------#
	current_date <- as.character(format(Sys.time(), "%d_%m_%Y")) 
	
	# source the control / data scripts
	print(control_path)
	source(control_path)

	print(data_path)
	source(data_path)

	# initialise
	#-------------------------------------------------#	
	feature_initialisation()

	#----------------------------------------------------------------------------#
	#                        COMPILE THE FEATURES                               #
	#----------------------------------------------------------------------------#

	# load the feature files  & set key to outcome_id 
    #----------------------------------------------------------------------------#
	invisible(lapply (compile_list, function(x) assign(paste0(x, "_feature"), 
	  setkey(as.data.table(readRDS(paste0(modified_folder, x, "_feature_", 
	  feature_set_name, ".Rds"))), outcome_id), envir = sys.frame(sys.parent(n=2)))))

	# merge the feature files with the cohort - merge on outcome_id & merge in 
	# cohort extra columns
    #----------------------------------------------------------------------------#
 	pred_set <- Reduce(mymerge, mget(paste0(compile_list, "_feature")))

    if (nrow(pred_set)!=nrow(cohort)) {
    
    	## in case subsetting post assembly
		print(sprintf("subsetting prediction set ---- number of observations in pred_set: %d vs. number of observations 
			in cohort: %d", nrow(pred_set), nrow(cohort)))

    	pred_set <- pred_set[outcome_id %in% cohort$outcome_id]

    }

	if (nrow(cohort_extra_col)>0) {
	  pred_set <- cohort_extra_col[pred_set, on=c("outcome_id")]
	}


	print(sprintf("number of observations in pred_set: %d vs. number of observations 
		in cohort: %d", nrow(pred_set), nrow(cohort)))

	gc()

	inv_lapply(paste0(compile_list,"_feature"), function(x) rm(x))

	gc()

	## ensure that no special characters in names
	setnames(pred_set, gsub("[^a-zA-Z_\\.0-9]", "_", names(pred_set)))

	# ensure that time_min, time_max variables are included in cohort_extra_col
	#----------------------------------------------------------------------------#
	cohort_extra_col <- cbind(cohort_extra_col, pred_set[, 
		mget(grep("_time_min|_time_max", names(pred_set), value=T))])

	raw_feature_coll <- feature_coll(pred_set)

	# save (temp)
	saveRDS(pred_set, paste0(temp_folder, "pred_set_temp_", feature_set_name, ".Rds"))
	saveRDS(cohort_extra_col, paste0(temp_folder, "cohort_extra_col_temp_", 
		feature_set_name, ".Rds"))

	write.csv(raw_feature_coll, paste0(temp_folder, "raw_feature_coll_", feature_set_name, "_", 
		current_date, ".csv"), row.names=F)
   
	# subset features - missingess/completeness
    #----------------------------------------------------------------------------#
	obs_check(pred_set)


	# subset features - feature selection (manual)
	#---------------------------------------------#
	var_list         <- paste(paste0(variable_list[include==1]$var_cat, "xx"), collapse="|")
	col_omit_select  <- which(!(gsub("\\.\\.", "xx", colnames(pred_set)) %like% var_list))
	col_omit_select  <- names(pred_set[, col_omit_select , with=F])
	col_omit_select  <- setdiff(col_omit_select , union(cohort_key_var, names(cohort_extra_col)))

	print(sprintf("columns that are deselected (%d)", length(unique(col_omit_select))))
	deselect_col <- length(unique(col_omit_select))

	pred_set[, col_omit_select:=NULL, with=F]

	obs_check(pred_set)

	# identify quant/qual var
    #---------------------------------------------#
    variable_type <- data.table(var_type=unlist(sapply(pred_set, class)))
	variable_type[, var_type_count:=.N, by=c("var_type")]
	print(unique(variable_type, by=c("var_type")))

	num_factor_var   <- setdiff(names(pred_set[, (c(which(sapply(pred_set, function(x) 
		class(x)[1]) %in% c("numeric", "factor")))), with=F]), union(cohort_key_var, 
		names(cohort_extra_col)))
	write.csv(num_factor_var, paste0(temp_folder, "_num_var.csv"), row.names=F)

	indic_var <- setdiff(names(pred_set[, (c(which(sapply(pred_set, function(x) 
		class(x)[1]) %in% c("integer")))), with=F]), union(cohort_key_var, 
		names(cohort_extra_col)))
	write.csv(indic_var, paste0(temp_folder, "_indic_var.csv"), row.names=F)
	
	if (miss_imp==FALSE) {

		num_factor_var_mod <- setdiff(unique(c(num_factor_var, grep(non_impute_var_cat, 
			names(pred_set), value=T))),  c(cohort_key_var, names(cohort_extra_col), 
			grep("_days_to_last", names(pred_set),value=T)))
	    write.csv(num_factor_var_mod, paste0(temp_folder, "_num_factor_var_mod_raw.csv"), row.names=F)

		indic_var_mod      <- setdiff(indic_var, num_factor_var_mod)
		write.csv(indic_var_mod, paste0(temp_folder, "_indic_var_mod.csv"), row.names=F)

		indic_var <- indic_var_mod
		num_factor_var <- num_factor_var_mod

	} 
	


	# impose thresholds
	#---------------------------------------------#
	if (miss_imp==TRUE) {

		## no steps

	} else if (miss_imp==FALSE) {

		# indic vars mod - -0 -> NA
		#---------------------------------------------#
		pred_set_non_num <- pred_set[,mget(setdiff(names(pred_set), num_factor_var))]
		pred_set_num     <- pred_set[,mget(num_factor_var)]
		set_zero_na(pred_set_non_num, 0)

		pred_set <- cbind(pred_set_non_num, pred_set_num)
		
		rm(pred_set_num)
		rm(pred_set_non_num)
		gc()

	}


	# deal with missing (numeric data)
   	#---------------------------------------------#
   	if (length(num_factor_var)>0) {
		pred_set_missing  <- sapply(pred_set[, mget(c(num_factor_var))], function(y) sum(is.na(y)))
	} else {
		pred_set_missing  <- 0
	}
	pred_set_missing_perc <- perc(pred_set_missing, (nrow(pred_set)), digit=0)


	# deal with 0,1 data (imputed dummy data)
	#---------------------------------------------#
	pred_set_zero      <- sapply(pred_set[, mget(indic_var)], function(y) sum(y==0, na.rm=T))
	pred_set_zero_perc <- perc(pred_set_zero, (nrow(pred_set)), digit=0)	


	# identify 100% missing/0 observations
	#---------------------------------------------#
	col_omit_missing <- pred_set_missing_perc[pred_set_missing_perc==100]
	col_omit_zero    <- pred_set_zero_perc[pred_set_zero_perc == 100]	
	

	# deal with  missing  - ext (numeric data)
   	#---------------------------------------------#	
	for (i in 1:length(name_ext_extended)) {
		temp <- pred_set_missing_perc[names(pred_set_missing_perc) %like% paste0(name_ext_extended[i], "$") & 
			pred_set_missing_perc>quant_missing_threshold[[i]]]
			col_omit_missing <- c(col_omit_missing, temp)
	}


	# deal with 0,1 data  - ext (imputed dummy data)
	#---------------------------------------------#
	for (i in 1:length(name_ext_extended)) {
		temp <- pred_set_zero_perc[names(pred_set_zero_perc) %like% paste0(name_ext_extended[i], "$") & 
			pred_set_zero_perc>indic_missing_threshold[[i]]]
			col_omit_zero <- c(col_omit_zero, temp)
	}


	col_omit_missing_name <- names(col_omit_missing)
	col_omit_zero_name    <- names(col_omit_zero)

	col_omit_missing 	  <- col_omit_missing[names(col_omit_missing) %in% col_omit_missing_name]
	col_omit_zero         <- col_omit_zero[names(col_omit_zero) %in% col_omit_zero_name]

	col_omit <- unique(union(col_omit_missing_name, col_omit_zero_name))


	cat(sprintf("columns that are ommitted - all / more than %s percent of data points missing (%d) [numeric/factor] (%s) \n// all / more than %s percent of data points 0 (%d) [indic] (%s) \n// total omissions (%d) \n", 
		paste0(paste0(quant_missing_threshold, "%"), collapse=" / "), 
		length(unique(col_omit_missing_name)), paste0(name_ext_name_extended, collapse= " / "), 
		paste0(paste0(indic_missing_threshold, "%"), collapse=" / "), 
		length(unique(col_omit_zero_name)), paste0(name_ext_name_extended, collapse= " / "), 
		length(unique(col_omit))))


	zero_col <- length(unique(col_omit_zero_name))
	na_col   <- length(unique(col_omit_missing_name))
    	
    if (length(col_omit>0)) {
		pred_set[, c(col_omit):=NULL]
	}

	obs_check(pred_set)

    # Missingness output - documentation (2)
	# ----------------------------------------------------------------------------#
    variable_type <- data.table(var_type=unlist(sapply(pred_set, class)))
	variable_type[, var_type_count:=.N, by=c("var_type")]
	print(unique(variable_type, by=c("var_type")))

	num_factor_var   <- setdiff(names(pred_set[, (c(which(sapply(pred_set, function(x) 
		class(x)[1]) %in% c("numeric","factor")))), with=F]), union(cohort_key_var, 
		names(cohort_extra_col)))
	indic_var <- setdiff(names(pred_set[, (c(which(sapply(pred_set, function(x) 
		class(x)[1]) %in% c("integer")))), with=F]), union(cohort_key_var, 
		names(cohort_extra_col)))

	if (miss_imp==FALSE) {

		num_factor_var_mod <- setdiff(unique(c(num_factor_var, grep(non_impute_var_cat, 
			names(pred_set), value=T))),  c(cohort_key_var, names(cohort_extra_col), 
			grep("_days_to_last", names(pred_set),value=T)))
	    write.csv(num_factor_var_mod, paste0(temp_folder, "_num_factor_var_mod_mod.csv"), row.names=F)

		indic_var_mod      <- setdiff(indic_var, num_factor_var_mod)
		write.csv(indic_var_mod, paste0(temp_folder, "_indic_var_mod.csv"), row.names=F)

		indic_var <- indic_var_mod
		num_factor_var <- num_factor_var_mod

	} 

    # Median Imputation
	# ----------------------------------------------------------------------------#
	if (fill_na==TRUE) {

		missing <- sapply(pred_set[,mget(setdiff(names(pred_set),c(cohort_key_var,
			names(cohort_extra_col))))], function(y) sum(is.na(y)))

		number_of_obs <- ncol(pred_set[,mget(setdiff(names(pred_set),c(cohort_key_var,
			names(cohort_extra_col))))]) * 
			nrow(pred_set[,mget(setdiff(names(pred_set),c(cohort_key_var,
			names(cohort_extra_col))))])
		impute_value_perc <- perc(sum(missing), number_of_obs)

	    missing_var <- setdiff(names(missing[missing>0]), c(cohort_key_var, 
	    	names(cohort_extra_col)))

		pred_set_missing     <- pred_set[, mget(c(missing_var))]
		pred_set_non_missing <- pred_set[, mget(c(setdiff(names(pred_set), c(missing_var))))]
	
		pred_set_missing <- imputeMissings::impute(pred_set_missing)

		pred_set <- cbind(pred_set_non_missing, pred_set_missing)

	    obs_check(pred_set)

	}

	# SAVE
	#---------------------------------------------#
	saveRDS(pred_set, file = paste0(modified_folder, "pred_set_raw_", 
		feature_set_name, ".Rds"))

	write.csv(as.data.table(names(pred_set)), file=paste0(output_folder, 
		"pred_set_var_name_raw_", feature_set_name, ".csv"),row.names=F)

	write.csv(unique(data.table(var_name=c(col_omit_missing_name, "x"), perc=c(col_omit_missing, "_")))[order(-perc)],
	 file=paste0(output_folder, "pred_set_col_omit_missing_", feature_set_name, ".csv"),row.names=F)
	write.csv(unique(data.table(var_name=c(col_omit_zero_name,"x"), perc=c(col_omit_zero, "_")))[order(-perc)], 
		file=paste0(output_folder, "pred_set_col_omit_zero_", feature_set_name, ".csv"),row.names=F)
	write.csv(as.data.table(c(col_omit_select, "x")), file=paste0(output_folder, 
		"pred_set_col_omit_select_", feature_set_name, ".csv"),row.names=F)

	#----------------------------------------------------------------------------#
	#                        FORMAT THE FEATURES                                 #
	#----------------------------------------------------------------------------#

	# Store the unformatted column names & dates
	# ----------------------------------------------------------------------------#
	var_name_raw   <- copy(names(pred_set))
	var_name_final <- copy(names(pred_set))
	
	date_col  <- pred_set[, c(which(sapply(pred_set, function(x) 
		class(x)[1]) %in% c("Date"))), with=F]
    date_col_table <- data.table(date=t(date_col[1]), 
    	var_name=names(date_col))

	setnames(date_col_table, "date.V1", "date")
	date_col_table[, date:=as.IDate(date, "%Y-%m-%d")]

	date_col_table <- rbindlist(list(date_col_table[var_name!="t0_date"], 
		data.table(var_name=c("t0_date_min", "t0_date_max"),
		date=c(as.IDate(min(pred_set$t0_date), "%Y-%m-%d"), 
		as.IDate(max(pred_set$t0_date), "%Y-%m-%d")))), use.names=T)

	print(date_col_table)

    pred_set <- pred_set[, !(c(which(sapply(pred_set, function(x) 
		class(x)[1]) %in% c("Date")))), with=F]
	
	var_name_final <- copy(names(pred_set))

	# Format the column names
	# ----------------------------------------------------------------------------#

	# main timeframes
	for (i in name_ext) {
	   var_name_final <- gsub(paste0(gsub("_", "", i), "$"), get(paste0("timeframe", i, "_name")),
	   	var_name_final)
	}

	# max timeframe
	var_name_final <- gsub("(?<!time)_max$", "_timeframe_max", var_name_final, perl=T)

	# difference timeframes
	for (i in 1:length(name_ext)) {
	   var_name_final <- gsub(paste0(name_ext[i], name_ext[i+1], "_diff", "$"), 
	   	paste0("_timeframe_diff",gsub("timeframe", "", name_ext_name[i]), 
	   		gsub("timeframe", "", name_ext_name[i+1])), var_name_final)
	}

	var_name_final <- gsub("max_diff$", "timeframe_diff_max", var_name_final)
    
    # ensure no duplicates
    dupl <- which(var_name_final %in% var_name_final[duplicated(var_name_final)])
    for (i in dupl) {
			var_name_final[i] <-  gsub("_timeframe", 
				paste0( i, "_timeframe"), var_name_final[i])
    }
	
    # rename
    setnames(pred_set, var_name_final)

 
    # add 'var' to identify features
	setnames(pred_set, setdiff(names(pred_set), c(cohort_key_var, names(cohort_extra_col))),
	 paste0("var_", setdiff(names(pred_set), c(cohort_key_var, names(cohort_extra_col)))))
	var_name_final <- copy(names(pred_set))

    # remove final _
    var_name_final <- gsub("_$", "", var_name_final)
	setnames(pred_set,var_name_final)
	
	# Reformat - integer / numeric
	# ----------------------------------------------------------------------------#

	# cast the factor columns such that factors are converted to independent indicator variables
	factor_var <- names(pred_set)[which(sapply(pred_set, function(x) class(x)[1]) %in% c("factor"))]
	pred_set <- one_hot_encoding(pred_set, factor_var)

    # format names
	# ----------------------------------------------------------------------------#
	setnames(pred_set, make.names(names(pred_set)))

    # Subset - omit date var & omit max var
	# ----------------------------------------------------------------------------#
	pred_set_final <- pred_set[, mget(setdiff(names(pred_set), 
		grep("_time_min|_time_max|_timeframe_max|timeframe_diff_max", 
		names(pred_set), value=T)))]

	final_feature_coll <- feature_coll(pred_set_final)

	# rename
    var_name_final <- gsub("var_([^_]*_)(.*)$", "var_\\2", names(pred_set_final))
    var_name_final <- gsub("_$", "", var_name_final)
    setnames(pred_set_final, var_name_final)

	write.csv(final_feature_coll, paste0(output_folder, "final_feature_coll_", 
		feature_set_name, "_", current_date, ".csv"), row.names=F)

	# save
	# ----------------------------------------------------------------------------#
	saveRDS(pred_set, file = paste0(temp_folder, "pred_set_final_temp_", feature_set_name,".Rds"))
	saveRDS(pred_set_final, file = paste0(modified_folder, "pred_set_final_", feature_set_name,".Rds"))
	
	# final variable overview
	# ----------------------------------------------------------------------------#

	pred_set_zero             <- sapply(pred_set_final, function(y) sum(y==0, na.rm=T))
	pred_set_zero_perc   	  <- perc(pred_set_zero, (nrow(pred_set)), digit=1)	

	pred_set_missing          <- sapply(pred_set_final, function(y) sum(is.na(y)))
	pred_set_missing_perc     <- perc(pred_set_missing, (nrow(pred_set)), digit=1)

	var_type                  <- sapply(pred_set_final, function(y) class(y)[[1]])

	final_var_dt <- data.table(var_name=names(pred_set_final))
	final_var_dt[, zero_perc:=pred_set_zero_perc]
	final_var_dt[, missing_perc:=pred_set_missing_perc]
	final_var_dt[, variable_type:=var_type]
	
	# save
	write.csv(final_var_dt, file = paste0(output_folder, 
		"pred_set_final_var_name_", feature_set_name, ".csv"),row.names=F)


	# ----------------------------------------------------------------------------#
	#                          Code V - STATS & OUTPUT                           #
	# ----------------------------------------------------------------------------#
	feature_overview()
}

#----------------------------------------------------------------------------#

	