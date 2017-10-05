#----------------------------------------------------------------------------#

#' @title Initialise the feature construction machinery. 
#'
#' @description \
#'
#' @export
#' @return
#' @examples

feature_initialisation <- function() {	

	# ----------------------------------------------- #
	# feature set name
	# ----------------------------------------------- #
	feature_set_name <- paste0(feature_set_prefix, "_", feature_set_id)

	# ----------------------------------------------- #
	# timeframes
	# ----------------------------------------------- #

	# load the settings
	name_ext_name   <<- rev(c(unlist(lapply(timeframe_list, function(x) x$name))))
	name_ext   	    <<- rev(paste0("_", c(unlist(lapply(timeframe_list, function(x) x$name_abb)))))	
	length     	    <<- rev(as.integer(c(unlist(lapply(timeframe_list, function(x) x$length)))))

	name_ext_name_extemded       <<- c("max", name_ext_name)
	name_ext_extended            <<- c("_max", name_ext)
	length_extended              <<- c(max(length), length)


	# generate timeframe variables
	for (i in 1:length(length_extended)) {

		assign(paste0("timeframe", name_ext_extended[i]), length_extended[i],
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

		if ("enc_oncdrs_rpdr" %in% selection_raw)     {
			selection_raw <- setdiff(selection_raw, c("enc", "enc_oncdrs"))
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
	cohort <- readRDS(cohort_path)	
	print(sprintf("number of observations in cohort: %d", nrow(cohort)))

	# verify column names
	# ----------------------------------
	ifelse(all(c("outcome_id", "outcome", "t0_date", "empi") %in% names(cohort)), 
	  "cohort columns names correct", stop("cohort column names incorrect"))

	# store key variables
	# ----------------------------------
	cohort_key_var <- c("outcome_id", "t0_date", "empi")
	
	# store 'extra' variables (for later merging) 
	# ----------------------------------------------- #
	cohort_extra_col <- cohort[, mget(c("outcome_id", setdiff(names(cohort), 
		cohort_key_var)))]
	
	cat(sprintf("extra cohort columns: %s", 
		paste0(c("\n", names(cohort_extra_col),"\n"),collapse='\n')))

	# format  dates & generate timeframe-specific dates 
	# ----------------------------------------------- #	
	invisible(parse_date(cohort, c("t0_date")))
	
	for (i in name_ext) {
		cohort[,c(paste0("t0_date_beg",i)):=t0_date-get(paste0("timeframe", i))]
	}
	
	setkeyv(cohort, c("empi", paste0("t0_date_beg", name_ext[length(name_ext)]), 
		"t0_date"))
	
	# store variables to be used to merge
	# ----------------------------------------------- #
	cohort_key_var_merge <- c("empi", "outcome_id", "t0_date", 
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

	modify_feature_selection(feature_selection_raw) <- function() {

		if ("dia_oncdrs_rpdr" %in% compile_list)       {
			feature_selection_raw[file_name=="dia.dfci", include:=0]
		}

		if ("med_chemo_oncdrs_rpdr" %in% compile_list) {
			feature_selection_raw[file_name=="chemo.dfci"| file_name=="med.dfci", include:=0]
		}

		if ("lab_oncdrs_rpdr" %in% selection_raw)  {
			feature_selection_raw[file_name=="lab.dfci", include:=0]
		}

		if ("enc_oncdrs_rpdr" %in% selection_raw)     {
			feature_selection_raw[file_name=="enc.dfci", include:=0]
		}

		return(feature_selection_raw)	

	} 

	variable_list <- modify_feature_selection(variable_list)



	# ----------------------------------------------- #
	# global assignment
	# ----------------------------------------------- #
	cohort               <<- cohort
	cohort_key_var_merge <<- cohort_key_var_merge
	cohort_key_var       <<- cohort_key_var
	cohort_extra_col     <<- cohort_extra_col
	variable_list        <<- variable_list
}

#----------------------------------------------------------------------------#
