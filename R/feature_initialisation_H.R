#----------------------------------------------------------------------------#

#' @title Initialise the feature construction machinery. 
#'
#' @description \
#'
#' @export
#' @return
#' @examples


# NOTE: ALL VARIBALES NEED TO BE DEFINED AS GLOBAL VARIABLES ('<<-')

feature_initialisation <- function() {	

	# ----------------------------------------------- #
	# date
	# ----------------------------------------------- #
	current_date <<- as.character(format(Sys.time(), "%d_%m_%Y")) 

	# ----------------------------------------------- #
	# feature set name
	# ----------------------------------------------- #
	feature_set_name <<- paste0(feature_set_prefix, "_", feature_set_id)

	# ----------------------------------------------- #
	# timeframes
	# ----------------------------------------------- #

	# load the settings
	name_ext_name        			 <<- rev(c(unlist(lapply(timeframe_list, function(x) x$name))))
	name_ext   	         			 <<- rev(paste0("_", c(unlist(lapply(timeframe_list, function(x) x$name_abb)))))	
	timeframe_duration   			 <<- rev(as.integer(c(unlist(lapply(timeframe_list, function(x) x$length)))))

	name_ext_name_extended           <<- c("max", name_ext_name)
	name_ext_extended                <<- c("_max", name_ext)
	timeframe_duration_extended      <<- c(max(timeframe_duration), timeframe_duration)


	# generate timeframe variables
	for (i in 1:length(timeframe_duration_extended)) {

		assign(paste0("timeframe", name_ext_extended[i]), timeframe_duration_extended[i],
			envir = sys.frame(sys.parent(n=3)))

		assign(paste0("timeframe", name_ext_extended[i], "_name"), name_ext_name_extended[i],
			envir = sys.frame(sys.parent(n=3)))

	}

	# ----------------------------------------------- #
	# leakage control
	# ----------------------------------------------- #

	# main
	for (i in 1:length(leak_list)) {

		assign(names(leak_list)[i], as.integer(leak_list[i][[1]]),
			envir = sys.frame(sys.parent(n=3)))

	}

	# oncdrs
	leak_oncdrs_dia_day   <<- leak_dia_day
	leak_oncdrs_chemo_day <<- leak_med_day
	leak_oncdrs_med_day   <<- leak_med_day
	leak_oncdrs_enc_day   <<- leak_enc_day
	leak_oncdrs_lab_day   <<- leak_lab_day

	# ----------------------------------------------- #
	# feature type selection
	# ----------------------------------------------- #

	# ensure that no 'duplication', i.e. inclusion of standard and 
	# combined features (i.e. BWH + DFCI datasets)
	# ----------------------------------
	modify_selection <- function(selection_raw) {

		if ("dia_oncdrs_rpdr" %in% selection_raw)       {
			selection_raw <- setdiff(selection_raw, c("dia_oncdrs", "dia"))
		}

		if ("med_chemo_oncdrs_rpdr" %in% selection_raw) {
			selection_raw <- setdiff(selection_raw, c("med", "med_oncdrs", "chemo_oncdrs"))
		}

		if ("lab_oncdrs_rpdr" %in% selection_raw)  {
			selection_raw <- setdiff(selection_raw, c("lab", "lab_oncdrs"))
		}

		return(selection_raw)	

	} 

	assemble_list <<- modify_selection(assemble_list)
	compile_list  <<- modify_selection(compile_list)

	# ----------------------------------------------- #
	# cohort
	# ----------------------------------------------- #

	# load 
	# ----------------------------------
	cohort <<- readRDS(cohort_path)	
	print(sprintf("number of observations in cohort: %d", nrow(cohort)))

	# verify column names
	# ----------------------------------
	ifelse(all(c("outcome_id", "outcome", "t0_date", "empi") %in% names(cohort)), 
	  "cohort columns names correct", stop("cohort column names incorrect"))

	# verify data types of key columns
	# ----------------------------------
	if(typeof(cohort$empi) == "character"){
		print("empi column type [character] is correct")
		if(typeof(cohort$outcome_id) == "integer"){
			print("outcome_id column type [integer] is correct")
			if(is.Date(cohort$t0_date)){
				print("t0_date column type [Date] is correct")
				print("all key columns are of correct data type")
			} else{
				stop("t0_date column must be of type Date")
			}
		} else{
			stop("outcome_id column must be of type integer")
		}
	} else{
		stop("empi column must be of type character")
	}


	# store key variables
	# ----------------------------------
	cohort_key_var <<- c("outcome_id", "t0_date", "empi")
	
	# store 'extra' variables (for later merging) 
	# ----------------------------------------------- #
	cohort_extra_col <<- cohort[, c("outcome_id", setdiff(names(cohort), cohort_key_var)), with = FALSE]
	
	cat(sprintf("extra cohort columns: %s", 
		paste0(c("\n", names(cohort_extra_col),"\n"),collapse='\n')))

	# format  dates & generate timeframe-specific dates 
	# ----------------------------------------------- #	
	invisible(format_date(list(cohort), list("t0_date")))

	for (i in name_ext) {
		cohort[,c(paste0("t0_date_beg",i)):=t0_date-get(paste0("timeframe", i))]
	}
	
	setkeyv(cohort, c("empi", paste0("t0_date_beg", name_ext[length(name_ext)]), 
		"t0_date"))
	
	# store variables to be used to merge
	# ----------------------------------------------- #
	cohort_key_var_merge <<- c("empi", "outcome_id", "t0_date", 
		grep("t0_date_", names(cohort), value=T))
	
	# ----------------------------------------------- #
	# feature category selection
	# ----------------------------------------------- #
	
	# load the variable list  & check version
	# ----------------------------------------------- #
	variable_list <- fread(feature_path)
		
	# specify variable selection 
	# ----------------------------------------------- #
	if (!is.na(variable_list_file_selection)) {
		setnames(variable_list, variable_list_file_selection, "include")
	}

	# ensure that no 'duplication', i.e. inclusion of standard and 
	# combined features (i.e. BWH + DFCI datasets)
	# ---------------------------------

	modify_feature_selection <- function(feature_selection_raw) {

		if ("dia_oncdrs_rpdr" %in% compile_list)       {
			feature_selection_raw[filename=="dia.dfci", include:=0]
		}

		if ("med_chemo_oncdrs_rpdr" %in% compile_list) {
			feature_selection_raw[filename=="chemo.dfci"| filename=="med.dfci", include:=0]
		}

		if ("lab_oncdrs_rpdr" %in% compile_list)  {
			feature_selection_raw[filename=="lab.dfci", include:=0]
		}
		
		return(feature_selection_raw)	

	} 

	variable_list <<- modify_feature_selection(variable_list)
}

#----------------------------------------------------------------------------#
