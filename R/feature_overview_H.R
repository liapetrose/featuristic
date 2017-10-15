#----------------------------------------------------------------------------#

#' @title Initialise the feature construction machinery. 
#'
#' @description \
#'
#' @export
#' @param pred_set_final Final feature set generated for which to generate an extensive set of summary stats
#' @param pred_set "Penultimate" feature set from which the final feature set is generated after some variable name changes and column subsetting
#' @param deselect_col Columns chosen to be deselected from the feature set 
#' @param na_col Columns/features dropped based on the na threshold (quant missing threshold)
#' @param zero_col Columns/features dropped based on the zero threshold (indic_missing_threshold)
#' @param date_col_table data.table with earliest/latest observation dates for each of the feature sets
#' @return
#' @examples

feature_overview <- function(pred_set_final, pred_set, deselect_col, na_col, zero_col, date_col_table) {	

	# overview stats
	# ----------------------------------------------------------------------------#

	sink(paste0(metadata_folder, "feature_stat_temp_", feature_set_name, "_", current_date), 
		split=TRUE)

	# number of features / complete cases
	feature_count        <- ncol(pred_set_final[, mget(grep("^var", names(pred_set_final), 
								value=T))])
	feature_unique_count <- length(unique(gsub("(_day_to_last){0,}_timeframe.*", "", 
								names(pred_set_final[, mget(grep("^var", names(pred_set_final), 
								value=T))]))))
	obs_count            <- nrow(pred_set_final)
	obs_count_complete   <- nrow(pred_set_final[complete.cases(pred_set_final[, 
								mget(grep("^var", names(pred_set_final), value=T))])])

	if ("test_set" %in% names(pred_set)) {

		obs_count_train 		 <- nrow(pred_set_final[get("test_set")==1])
		obs_count_complete_train <- nrow(pred_set_final[complete.cases(pred_set_final[, 
									mget(grep("^var", names(pred_set_final), value=T))])][
									get("test_set")==1])

		obs_count_test           <- nrow(pred_set_final[get("test_set")==0])
		obs_count_complete_test  <- nrow(pred_set_final[complete.cases(pred_set_final[, 
									mget(grep("^var", names(pred_set_final), value=T))])][
									get("test_set")==0])

	}

	cat("\n\n")

	cat(sprintf("final number of features: %d, final number of unique features: %d, final number of obs: %d, 
		final number of complete obs: %d (perc: %f)", 
		feature_count, feature_unique_count, 
		obs_count, obs_count_complete, perc(obs_count_complete, obs_count)))

	cat("\n\n")

	# variable types
	variable_type <- data.table(var_type=unlist(sapply(pred_set_final, class)))
	variable_type[, var_type_count:=.N, by=c("var_type")]

	variable_type_var <- data.table(var_type=unlist(sapply(pred_set_final[, mget(grep("^var_", 
		names(pred_set_final), value=T))], class)))
	variable_type_var[, var_type_count:=.N, by=c("var_type")]
	
	cat("var type all - pred set final")
	cat("\n")
	print(unique(variable_type, by=c("var_type")))

	cat("\n\n")

	cat("var type var only - pred set final")
	cat("\n")
	print(unique(variable_type_var, by=c("var_type")))

	cat("\n\n")

	# other cols 
	cat("cohort_extra_col")
	cat("\n")
	print(names(cohort_extra_col)[names(cohort_extra_col) %in% 
		names(pred_set_final)])
	
	cat("\n\n")

	cat("other extra_col")
	cat("\n")
	print(names(pred_set_final)[!(names(pred_set_final) %like% "^var_" |  
		names(pred_set_final) %in% names(cohort_extra_col))])

	cat("\n\n")

	sink()

 	# output stats
	# ----------------------------------------------------------------------------#

	# preparation
    leak_table <- data.table(var_name=names(leak_list), leak_day=c(unlist(leak_list)))

	# set-up
	feature_vital_sign <- list()
	space              <- 1
 
	# vars
	feature_vital_sign$feature_set_name <- feature_set_name
	feature_vital_sign$feature_set_id   <- feature_set_id

	list_space("feature_vital_sign")

    feature_vital_sign$id_var_name      <- "outcome_id"	
    feature_vital_sign$date_var_name    <- "t0_date"	
    feature_vital_sign$outcome_var_name <- "outcome"	

	list_space("feature_vital_sign")
    
    feature_vital_sign$timeperiod_name       <- name_ext_name	
    feature_vital_sign$timeperiod_count      <- length(name_ext_name)	

	list_space("feature_vital_sign")
 
    feature_vital_sign$feature_type             <- compile_list	

    list_space("feature_vital_sign")

    feature_vital_sign$feature_count            <- feature_count
    feature_vital_sign$feature_unique_count     <- feature_unique_count	

	list_space("feature_vital_sign")

    feature_vital_sign$obs_count                <- obs_count	
    feature_vital_sign$obs_count_complete       <- obs_count_complete

    if ("test_set" %in% names(pred_set)) {

    	feature_vital_sign$obs_count_train               <- obs_count_train	
    	feature_vital_sign$obs_count_complete_train      <- obs_count_complete_train

    	feature_vital_sign$obs_count_test                <- obs_count_test
    	feature_vital_sign$obs_count_complete_test       <- obs_count_complete_test

    }

	list_space("feature_vital_sign")

    feature_vital_sign$indic_missing_zero_imputation      <- miss_imp	
    feature_vital_sign$var_selection                      <- sprintf("%s (omit: %d)", variable_list_file_selection, 
    															deselect_col)	

    feature_vital_sign$numeric_factor_missing_threshold   <- sprintf("%s (omit: %d)", paste0(unlist(quant_missing_threshold),
    															collapse=" - "), na_col)	
    feature_vital_sign$indic_missing_threshold            <- sprintf("%s (omit: %d)", paste0(unlist(indic_missing_threshold),
    															collapse=" - "), zero_col)
    feature_vital_sign$missing_imputation                 <- ifelse(fill_na, sprintf("%s (method: %s / perc.values imputed: %f)", 
    															as.character(fill_na),
     															as.character(fill_na_method), as.numeric(impute_value_perc)), 
    															sprintf("Missing imputation disabled\n."))	

  
    list_space("feature_vital_sign")

	feature_vital_sign$leak_day 	     <-  dt_paste(leak_table, "", line_sep=TRUE, length=nrow(leak_table))
 

    list_space("feature_vital_sign")

	feature_vital_sign$outcome_earliest  <- as.character(as.Date(min(date_col_table[var_name=="t0_date_min", date]),
												"%Y-%m-%d"))
	feature_vital_sign$outcome_latest 	 <- as.character(as.Date(max(date_col_table[var_name=="t0_date_max", date]), 
												"%Y-%m-%d"))
	feature_vital_sign$feature_earliest  <- as.character(as.Date(min(date_col_table[var_name %like% "min" & 
												!(var_name %like% "t0_date"), date]), "%Y-%m-%d"))
	feature_vital_sign$feature_latest    <- as.character(as.Date(max(date_col_table[var_name %like% "max" & 
												!(var_name %like% "t0_date"), date]), "%Y-%m-%d"))

    list_space("feature_vital_sign")

    feature_vital_sign$feature_earliest_specific <- dt_paste(date_col_table[var_name %like% "min" & !(var_name %like% "t0_date")], "", line_sep=TRUE, 
    													length=nrow(date_col_table[var_name %like% "min" & !(var_name %like% "t0_date")])) 
    feature_vital_sign$feature_latest_specific   <- dt_paste(date_col_table[var_name %like% "max" & !(var_name %like% "t0_date")], "", line_sep=TRUE, 
    													length=nrow(date_col_table[var_name %like% "max" & !(var_name %like% "t0_date")])) 

    list_space("feature_vital_sign")

	# read in vital signs and merge & save
	#----------------------------------------------------------------------------#

	ps("sucessfully generated feature vital signs")

	# unnest
	feature_vital_sign <- as.list(unlist(feature_vital_sign))
	
	print(head(feature_vital_sign))

	if (file.exists(paste0(metadata_folder, "feature_vital_sign.csv"))) {
			
		feature_vital_sign_raw <- fread(paste0(metadata_folder, "feature_vital_sign.csv"))

		feature_vital_sign_table <- list_table(feature_vital_sign, current_date,
 			vital_signs_merge=T, vital_signs_old=feature_vital_sign_raw)

		ps("sucessfully generated feature vital signs _table_ (appended)")

		print(head(feature_vital_sign_table))

	} else {
			
		feature_vital_sign_table <- list_table(feature_vital_sign, current_date,
 			vital_signs_merge=F)

		ps("sucessfully generated feature vital signs _table_ (new)")

		print(head(feature_vital_sign_table))

	}


	# save
	write.csv(feature_vital_sign_table, paste0(metadata_folder, "feature_vital_sign.csv"), 
		row.names=F, na="")
}
