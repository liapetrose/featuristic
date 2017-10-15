#----------------------------------------------------------------------------#

#' @title Assemble features (Stage I).
#'
#' @description \
#'
#' @export
#' @param cohort_path 
#' @param control_path 
#' @param data_def_path    
#' @param feature_path  
#' @param feature_set_id     
#' @param feature_set_prefix 
#' @return
#' @examples

feature_construction <- function(cohort_path, control_path, data_def_path, feature_path, 
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

	# checks (dem check, stage_0 check...)
	#-------------------------------------------------#	
	cohort_dem_check() # check that all cohort observations are associated 
					   # with a record in the dem file

	#----------------------------------------------------------------------------#
	#                		       FEATURE GENERATION                            #
	#----------------------------------------------------------------------------#
	
	# Initialize
	error_list   <- c()
	success_list <- c()

	# Loop over files 
	#----------------------------------------------------------------------------#
	inv_lapply(assemble_list, function(feature_set) {

		print(paste0("start - ", feature_set))
		
		tryCatch({

			# source feature generation
			temp_feature <- indiv_feature_gen(feature_set, cohort=cohort, 
				cohort_key_var_merge=cohort_key_var_merge, cohort_key_var=cohort_key_var)
		
			sink(paste0(temp_folder, "feature_dataset_check_", feature_set,"_",feature_set_name,".txt"),
				split=TRUE)
			
			feature_dataset_check(temp_feature, paste0(feature_set, "_feature"), cohort_dt=cohort,
				cohort_key_var_list=cohort_key_var)

			sink()

			# save 
			saveRDS(temp_feature, paste0(raw_feature_folder, paste0(feature_set, "_feature_"), 
				feature_set_name, ".Rds"))
	
			rm("temp_feature")

			success_list <<- c(success_list, feature_set)

		}, error=function(e) {

			print(e)

			error_list <<- c(error_list, feature_set)

		}
			
	)})	

	# Status 
	#----------------------------------------------------------------------------#
	
	cat("\n #------------------------#\n")
	cat(sprintf("* Successfully Completed: %s\n", paste0(success_list, collapse=" / ")))
	cat(sprintf("* Error /Not Completed: %s\n", paste0(error_list, collapse=" / ")))
	cat("\n #------------------------#\n")

}	

#----------------------------------------------------------------------------#
