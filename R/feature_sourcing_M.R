#----------------------------------------------------------------------------#

#' @title Source features (Stage 0).
#'
#' @description Create master subsets for each file type from which features are to be assembled. These master subsets are then used in Stage 1 to create features. The idea is to load in all master relevant master cohorts for each file type, subset them to include only records relevant to the cohort at hand, combine these subsetted datasets for each file type, then saving just one subsetted and merged master dataset for each file type, which acts as the source for all feature construction for that feature type. Doing this once allows rapid iteration in stages 1 and 2, since stage 0 is unlikely be repeated more than once for the feature construction process for a given cohort.
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

feature_sourcing <- function(cohort_path, control_path, data_def_path, feature_path, 
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

	# save a list of the unique empis used to subset the datasets in stage 0 - for downstream checking
	cohort_empi <- unique(cohort$empi)
	saveRDS(data.table(empi = cohort_empi), 
		paste0(modified_folder, "empi_list_", feature_set_prefix, feature_set_id, ".Rds"))

	#----------------------------------------------------------------------------#
	#                		       FEATURE SOURCING                              #
	#----------------------------------------------------------------------------#
	
	# Initialize
	error_list   <- c()
	success_list <- c()
	print("commence stage 0 - preparing master data subsets by feature set ")
	print(sprintf("Number of patients in cohort: %d", length(cohort_empi)))
	sink(paste0(temp_folder, "source_dataset_creation_check_", feature_set,"_",feature_set_name,".txt"), split=TRUE)

	# Loop over files to create merged and subset versions of each using the combine_subset_data helper function
	#----------------------------------------------------------------------------#
	inv_lapply(source_list, function(feature_set) {

		print(paste0("start - ", feature_set))
		
		# create a dictionary for which list of source filepaths correspond to which feature set
		file_source_list_dictionary <- list(
			dem=dem_file_source, 
			enc=enc_file_source, 
			dia=dia_file_source, 
			prc=prc_file_source, 
			lvs=lvs_file_source, 
			lab=lab_file_source, 
			med=med_file_source, 
			mic=mic_file_source,
			ed=ed_file_source, 
			dia_oncdrs=dia_oncdrs_file_source, 
			med_oncdrs=med_oncdrs_file_source, 
			chemo_oncdrs=chemo_oncdrs_file_source, 
			enc_oncdrs=enc_oncdrs_file_source, 
			lab_oncdrs=lab_oncdrs_file_source
		)

		# extract the list of file paths that are relevant to the concerned feature_set
		file_source_list <- file_source_list_dictionary[[feature_set]]

		# call helper to combine, subset and save results for current feature_set
		combine_subset_data(file_source_list = file_source_list, feature_set = feature_set, cohort_empi = cohort_empi)
	})
	sink()
}	

#----------------------------------------------------------------------------#
