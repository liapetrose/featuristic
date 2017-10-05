#----------------------------------------------------------------------------#

#' @title Assemble features (Stage I).
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

feature_construction <- function(cohort_path, control_path, data_path, feature_path, 
	feature_set_id, feature_set_prefix) {

	#----------------------------------------------------------------------------#
	#                		             SETUP                                   #
	#----------------------------------------------------------------------------#
	current_date <- as.character(format(Sys.time(), "%d_%m_%Y")) 
	
	# source the control / data scripts
	print(data_path)
	source(data_path)
	
	print(control_path)
	source(control_path)

	# initialise
	#-------------------------------------------------#	
	feature_initialisation()

	#----------------------------------------------------------------------------#
	#                		       FEATURE GENERATION                            #
	#----------------------------------------------------------------------------#
		
	inv_lapply(assemble_list, function(feature_set) {

		tryCatch({

			# source feature generation
			temp_feature <- indiv_feature_gen(feature_set, cohort=cohort, 
				cohort_key_var_merge=cohort_key_var_merge, cohort_key_var=cohort_key_var)
	
			feature_dataset_check(temp_feature, paste0(feature_set, "_feature"), cohort_dt=cohort,
				cohort_key_var_list=cohort_key_var, extra_var_list=names(cohort_extra_col))

			# save 
			saveRDS(temp_feature, paste0(modified_folder, paste0(feature_set, "_feature_"), 
				feature_set_name, ".Rds"))
	
			rm("temp_feature")

		},error=function(e) print(e))

	})	

}	

#----------------------------------------------------------------------------#
