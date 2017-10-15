#----------------------------------------------------------------------------#

#' @title Modify features constructed in stage 1 [this is stage 2].
#'
#' @description Taking the feature sets constructed in stage 1 and saving each individual set in a .Rds file after modifying them by (a) selection - subsetting to only variables specified by the user in the feature_selection.csv (b) thresholding - dropping feature sets that do not meet the thresholds for missingness / zeroness as specified in the control.R file (c) missing imputation - reversing the automatic imputation (set missing instead of 0) from stage 1 for variables as specified in control.R (d) median imputation - imputing missing observations to the median value for the the features specified in control.R
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

feature_modification <- function(cohort_path, control_path, data_def_path, feature_path, 
	feature_set_id, feature_set_prefix) {

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
	#                         MODIFY THE FEATURES                                #
	#----------------------------------------------------------------------------#

	# feature_set takes the form "dem", "dia", "prc",
	feature_mod <- function(feature_set){
		print("beginning stage 2 (modification) of ", feature_set, " features:")

		# load the raw features corresp to the feature_set, set key to outcome_id 
		#------------------------------------------------------------------------#
		print(paste0("Reading in features for ", feature_set))
		assign(paste0(x, "_feature"), 
			   setkey(as.data.table(readRDS(paste0(raw_feature_folder, x, "_feature_", feature_set_name, ".Rds"))), 
			   	outcome_id), 
			   envir = sys.frame(sys.parent(n=2)))

		# merge the feature files with the cohort - merge on outcome_id & merge in 
		# cohort extra columns
    	#----------------------------------------------------------------------------#
    	pred_set <- raw_feature <- get(paste0(feature_set, "_feature")) # raw set of features of "feature_set" type [also called pred_set for legacy code reasons]

    	# ensure all cohort obs are accounted for in the raw feature set
	    if(nrow(pred_set)!=nrow(cohort)) {
	    
	    	## in case subsetting post construction
			print(sprintf(paste0("subsetting prediction set ---- number of observations in pred_set: ", 
				"%d vs. number of observations in cohort: %d"), nrow(pred_set), nrow(cohort)))

	    	pred_set <- pred_set[outcome_id %in% cohort$outcome_id]

	    }
		if(nrow(cohort_extra_col)>0) {
		  	pred_set <- cohort_extra_col[pred_set, on=c("outcome_id")]
		}
		print(sprintf("number of observations in pred_set: %d vs. number of observations 
			in cohort: %d", nrow(pred_set), nrow(cohort)))

		## ensure that no special characters in names
		setnames(pred_set, gsub("[^a-zA-Z_\\.0-9]", "_", names(pred_set)))

		# ensure that time_min, time_max variables are included in cohort_extra_col
		#----------------------------------------------------------------------------#
		cohort_extra_col <- cbind(cohort_extra_col, pred_set[, 
			mget(grep("_time_min|_time_max", names(pred_set), value=T))])

		raw_feature_coll <- feature_coll(pred_set)

		# save (temp)
		saveRDS(pred_set, paste0(temp_folder, feature_set, "_feature_set_temp_", feature_set_name, ".Rds"))
		saveRDS(cohort_extra_col, paste0(temp_folder, feature_set, "_cohort_extra_col_temp_", 
			feature_set_name, ".Rds"))
		write.csv(raw_feature_coll, paste0(temp_folder, feature_set, "_raw_feature_coll_", feature_set_name, "_", 
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

		# manually handle lab vars (aggregated categories > by definition deselected)
		if(feature_set == "lab"){
			if ("lab_lab.non.numeric" %in% var_list) col_omit_select <- setdiff(col_omit_select, 
				grep("lab_lab.non.numeric", col_omit_select, value=T))
			if ("lab.dfci_dfci.lab.non.numeric" %in% var_list) col_omit_select <- setdiff(col_omit_select, 
				grep("lab.dfci_dfci.lab.non.numeric", col_omit_select, value=T))
		}
		
		print(sprintf("columns that are deselected (%d)", length(unique(col_omit_select))))
		deselect_col <- length(unique(col_omit_select))

		pred_set[, as.character(col_omit_select) := NULL, ]

		obs_check(pred_set)

		# identify quant/qual var
	    #---------------------------------------------#
	    variable_type <- data.table(var_type=unlist(sapply(pred_set, class)))
		variable_type[, var_type_count:=.N, by=c("var_type")]
		print(unique(variable_type, by=c("var_type")))

		num_factor_var   <- setdiff(names(pred_set[, (c(which(sapply(pred_set, function(x) 
			class(x)[1]) %in% c("numeric", "factor")))), with=F]), union(cohort_key_var, 
			names(cohort_extra_col))) # vector of numeric and factor variables (column names)
		write.csv(num_factor_var, paste0(temp_folder, feature_set, "_num_var.csv"), row.names=F)

		indic_var <- setdiff(names(pred_set[, (c(which(sapply(pred_set, function(x) 
			class(x)[1]) %in% c("integer")))), with=F]), union(cohort_key_var, 
			names(cohort_extra_col))) # vector of indicator variables (column names)
		write.csv(indic_var, paste0(temp_folder, feature_set, "_indic_var.csv"), row.names=F)
		
		# missingness imputation setup [column identification]
	    #-----------------------------------------------------#

	    # keep in mind that stage 1 automatically performs missing => 0 imputation, so
	    # it needs to be reversed in this stage if required by control parameters / user settings
		if (miss_imp==FALSE) {

			if(is.na(impute_var_cat)) {
			
				## no steps
				num_factor_var_mod <- num_factor_var # if no exceptions to imputation rule, retain all numeric and factor variables for the undoing of the imputation

			} else {

				num_factor_var_mod <- setdiff(unique(c(num_factor_var, grep(impute_var_cat, 
					names(pred_set), value=T))),  c(cohort_key_var, names(cohort_extra_col), 
					grep("_day_to_last", names(pred_set),value=T))) # what subset should we be imputing for?

			}

			print("indicator variables -> Non-Imputed")
			print(setdiff(num_factor_var_mod, num_factor_var))

		    write.csv(num_factor_var_mod, paste0(temp_folder, feature_set, "_num_factor_var_mod_raw.csv"), row.names=F)

			indic_var_mod      <- setdiff(indic_var, num_factor_var_mod)
			write.csv(indic_var_mod, paste0(temp_folder, feature_set, "_indic_var_mod.csv"), row.names=F)

			indic_var          <- indic_var_mod
			num_factor_var     <- num_factor_var_mod

		} 

		# deal with 'missing data' (indicator variables)
		#----------------------------------------------#
		if (miss_imp==TRUE) {

			## no steps - imputed (i.e. missing treated as 0) by default in stage 1

		} else if (miss_imp==FALSE) {

			# indic vars mod - -0 -> NA
			#---------------------------------------------#
			pred_set_non_num <- pred_set[,mget(setdiff(names(pred_set), num_factor_var))]
			pred_set_num     <- pred_set[,mget(num_factor_var)]
			set_zero_na(pred_set_non_num)

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

		print("summary of feature subsetting for ", feature_set, " features:")
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
		saveRDS(pred_set, file = paste0(mod_feature_folder, feature_set, "_feature_set_mod_", 
			feature_set_name, ".Rds"))

		write.csv(as.data.table(names(pred_set)), file=paste0(metadata_folder, feature_set,
			"_feature_set_mod_var_name_", feature_set_name, ".csv"),row.names=F)

		write.csv(unique(data.table(var_name=c(col_omit_missing_name, "x"), perc=c(col_omit_missing, 
			"_")))[order(-perc)],file=paste0(metadata_folder,  feature_set, "_feature_set_mod_col_omit_missing_", 
			feature_set_name, ".csv"),row.names=F)
		write.csv(unique(data.table(var_name=c(col_omit_zero_name,"x"), 
			perc=c(col_omit_zero, "_")))[order(-perc)], file=paste0(metadata_folder, feature_set, "_feature_set_mod_col_omit_zero_", feature_set_name, ".csv"),row.names=F)
		write.csv(as.data.table(c(col_omit_select, "x")), file=paste0(metadata_folder, 
			feature_set, "_feature_set_mod_col_omit_select_", feature_set_name, ".csv"),row.names=F)

		# clear up memory
		gc()
		rm(pred_set, raw_feature, paste0(feature_set, "_feature"))
	}

	# perform modification
	lapply(compile_list, feature_mod)
}

#----------------------------------------------------------------------------#