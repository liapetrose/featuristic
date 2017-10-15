#----------------------------------------------------------------------------#

#' @title Compile features modified in stage 2 [this is stage 3].
#'
#' @description Read in the curated and cleaned features of each type ("dem" ... etc.) from the output of stage 2 and compile them to one large prediction set. Then create summary statistics and save the final results as the final feature set output, ready to be used in a supervised learning model.
#'
#' @export
#' @param cohort_path path to .Rds cohort file which must have the following column names - "empi", "outcome", "t0_date", "outcome_id"; can also contain a "test_set" column; other columns are non-essential but not a problem (character)
#' @param control_path path to control.R file which contains the user defined parameters for feature construction (character)
#' @param data_def_path path to data_def_path.R file which contains lists of relevant file paths where master data sets that are to be used in the FC process live (character)
#' @param feature_path path to feature_selection.csv file which provides options to select variables for FC process (character)
#' @param feature_set_id suffix indicating an id number or index number for this cohort's feature set (if multiple attempts were made with the same cohort, for example) (character)
#' @param feature_set_prefix prefix identifying the cohort / feature set that is used in the output file names for all datasets / output created as part of the FC process (character)
#' @return
#' @examples

feature_compilation <- function(cohort_path, control_path, 
	data_def_path, feature_path, feature_set_id, feature_set_prefix) {

	
	#----------------------------------------------------------------------------#
	#                		             SETUP                                   #
	#----------------------------------------------------------------------------#

	# source the control / data scripts
	print(data_def_path)
	source(data_def_path)
	
	print(control_path)
	source(control_path)

	# initialise
	#-------------------------------------------------#	
	feature_initialisation()

	#----------------------------------------------------------------------------#
	#                        COMPILE THE FEATURES                               #
	#----------------------------------------------------------------------------#

	# load the feature files  & set key to outcome_id 
    #----------------------------------------------------------------------------#
	invisible(lapply (compile_list, function(x) {
		print(paste0("Reading in features for ", x))
		assign(paste0(x, "_feature"), 
	  		   setkey(
	  		   		as.data.table(readRDS(paste0(mod_feature_folder, x, "_feature_set_mod_", feature_set_name, ".Rds"))),
	  		   		outcome_id), 
	  		   envir = sys.frame(sys.parent(n=2)))
	}))

	# merge the feature files with the cohort - merge on outcome_id & merge in 
	# cohort extra columns
    #----------------------------------------------------------------------------#
 	pred_set <- Reduce(mymerge, mget(paste0(compile_list, "_feature")))

    if (nrow(pred_set)!=nrow(cohort)) {
    
    	## in case subsetting post assembly
		print(sprintf(paste0("subsetting prediction set ---- number of observations in pred_set: ", 
			"%d vs. number of observations in cohort: %d"), nrow(pred_set), nrow(cohort)))

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

	# ## ensure that no special characters in names
	# setnames(pred_set, gsub("[^a-zA-Z_\\.0-9]", "_", names(pred_set)))

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
	pred_set   <- one_hot_encoding(pred_set, factor_var)

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

	write.csv(final_feature_coll, paste0(metadata_folder, "final_feature_coll_", 
		feature_set_name, "_", current_date, ".csv"), row.names=F)

	# save
	# ----------------------------------------------------------------------------#
	saveRDS(pred_set, file = paste0(temp_folder, "pred_set_final_temp_", feature_set_name,".Rds"))
	saveRDS(pred_set_final, file = paste0(final_feature_folder, "pred_set_final_", feature_set_name,".Rds"))
	
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
	write.csv(final_var_dt, file = paste0(metadata_folder, 
		"pred_set_final_var_name_", feature_set_name, ".csv"),row.names=F)


	# ----------------------------------------------------------------------------#
	#                              STATS & OUTPUT                                 #
	# ----------------------------------------------------------------------------#

	feature_overview(pred_set_final = pred_set_final, pred_set = pred_set, deselect_col = deselect_col, na_col = na_col, zero_col = zero_col, date_col_table = date_col_table)

}

#----------------------------------------------------------------------------#

	