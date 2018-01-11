#----------------------------------------------------------------------------#

#' @title Combine, subset datafiles for a given feature set
#'
#' @description For a a given feature set and list of data files (of master data from several cohorts), create a combined master subset containing only observations relevant to the provided cohort patient_id_master list.
#'
#' @export
#' @param file_source_list list of paths to .Rds files which are expected to be "master datasets" for multiple patient populations (eg. list of dia files for icmp, bwh_ed_100k cohorts) (list of character)
#' @param feature_set abbreviation of feature set these files are relevant to (eg. "dia", "prc", "enc") (character)
#' @param cohort_patient_id_master vector of all unique patient_id_masters in cohort observation set (character vector)
#' @return
#' @examples

combine_subset_data <- function(file_source_list, feature_set, cohort_patient_id_master){

	# load and combine all datasets relevant to concerned feature_set
	#-------------------------------------------------------------------------------#
	
	list_of_list_feature_set <- c("enc", "ed") # for which feature sets are individual cohort files a list of files?

	# accounting for the edge cases such as "enc", outlined in object list_of_list_feature_set, where each dataset in the list consists of a list of datasets
	if(feature_set %in% list_of_list_feature_set){
		tryCatch(
			combined_dt <- readRDS_merge(file_source_list,nested=TRUE), 
			error=function(e) print(paste0("no source files found / provided for ", feature_set, " feature set"))
		)
	} else{
		tryCatch(
			combined_dt <- readRDS_merge(file_source_list), 
			error=function(e) print(paste0("no source files found / provided for ", feature_set, " feature set"))
		)
	}

	# subset combined datasets to only include observations from relevant patient_id_master / identifiers
	#-------------------------------------------------------------------------------#

	# accounting for the edge cases such as "enc", outlined in object list_of_list_feature_set, where each dataset in the list consists of a list of datasets
	if(feature_set %in% list_of_list_feature_set){
		subset_combined_dt <- mapply(function(dt_list, dt_name){
			print(sprintf(paste0("Number of rows in combined master set for ", feature_set, "[", dt_name, "]: %d"), nrow(dt_list)))
			dt_list <- dt_list[patient_id_master %in% cohort_patient_id_master, ]
			print(sprintf(paste0("Number of rows in combined master *subset* for ", feature_set, "[", dt_name, "]: %d"), nrow(dt_list)))
			return(dt_list)
		}, dt_list = combined_dt, dt_name = names(combined_dt))
	}else{
		print(sprintf(paste0("Number of rows in combined master set for ", feature_set, ": %d"), nrow(combined_dt)))
		subset_combined_dt <- combined_dt[patient_id_master %in% cohort_patient_id_master, ]
		print(sprintf(paste0("Number of rows in combined master *subset* for ", feature_set, ": %d"), nrow(subset_combined_dt)))
	}

	# check
	#-------------------------------------------------#
	# warn user if demographics records don't exist for all patients in cohort -- 
	if(feature_set == "dem"){
			no_dem <- nrow(cohort[!(patient_id_master %in% subset_combined_dt$patient_id_master), ])
			if(no_dem > 0){
				print(sprintf("WARNING - COMBINED DEMOGRAPHICS FILE MISSING DATA ON %d patient_id_masters", no_dem))
			} else{
				print("All cohort observations are associated with a record in the dem file")
			}
	}

	# save the subset files using the feature set as the output file name
	saveRDS(subset_combined_dt, paste0(source_folder, feature_set, ".Rds"))

	# free up memory
	rm(combined_dt)
	rm(subset_combined_dt)
}