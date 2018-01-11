# Generating Features - TEMPLATE
#----------------------------------------------------------------------------#

# Control parameters
#----------------------------------------------------------------------------#
stage <- as.character(commandArgs(trailingOnly = TRUE)[1])
print(sprintf("stage: %s", stage))
stage_vector <- unlist(strsplit(stage, split = '')) # break stage into a vector of all stages to run

# SETUP
#----------------------------------------------------------------------------#

# Set the working directory
#---------------------------------------
wd_path <- "/data/zolab/sl/featuristic_dev/"

# [0-A] Load the package
#---------------------------------------
# library(featuristic)

# [0-B] Load the package - locally 
#---------------------------------------
package_path <- paste0(wd_path,"codebase/")
source(paste0(package_path,"featuristic/package_management/load_package_locally.R"))

# [1] Specify the location of the cohort for which to generate features 
#---------------------------------------
# specify the path to the cohort file (.Rds) for which features are to be generated
## > the cohort file MUST contain (at minimum) the following columns (additional 
###  columns are preserved):

### "outcome_id" - a unique identifier for each observation
### "outcome"    - a (binary or continuous) outcome variable (e.g. deceased - yes (1) / no (0))
### "empi"       - patient identifier associated with each observation
### "t0_date"    - date associated with each observation and based on which 
###                features are constructed (~ 't0')

## Optionally the cohort file can contain any of the following variables which 
## are taken into account in the generation of any summary statistics:

### "test_set  " - a binary variable indicating whether a given observation belongs to the
###                'testing' (1) / 'training' (0) set

cohort_path           <- paste0(wd_path,"other/cohort.Rds")

# [2] Specify the paths to the key control inputs 
#---------------------------------------
## > These should be modified versions of the templates located in the
##   template folder

# control.R file   > settings
control_path         <- paste0(wd_path,"featuristic/template/control.R")


# data_def_path.R file > data paths
data_def_path        <- paste0(wd_path,"featuristic/template/data_def_path.R") 

# feature_selection.csv file > variable selection 
feature_path         <- paste0(wd_path,"featuristic/template/feature_selection.csv") 

# [3] Specify the ID and prefix which is to be associated with any outputs 
#---------------------------------------
# specify a unique cohort-specific prefix and ID - the prefix and ID will
# be associated with all (a) intermediate data files and (b) the final feature set.
## > the prefix and ID should be different for every set of settings specified in this file
## even if working with the same cohort data file

feature_set_prefix   <- "test_cohort"
feature_set_id       <- "1" 


# EXECUTION
#----------------------------------------------------------------------------#

# Execute Stage-0 > Create and subset data sources from which features can be constructed
#---------------------------------------
if ('0' %in% stage_vector) {

	print("starting stage-0 (feature sourcing)")

	feature_sourcing(cohort_path=cohort_path, control_path=control_path,
 		data_def_path=data_def_path, feature_path=feature_path,
 		feature_set_id=feature_set_id,
 		feature_set_prefix=feature_set_prefix)
}


# Execute Stage-1 > Construct the Features
#---------------------------------------
if ('1' %in% stage_vector) {

	print("starting stage-1 (feature construction)")

	feature_construction(cohort_path=cohort_path, control_path=control_path,
 		data_def_path=data_def_path, feature_path=feature_path,
 		feature_set_id=feature_set_id,
 		feature_set_prefix=feature_set_prefix)
}


# Execute Stage-2 > Modify (clean, subset) the Features
#---------------------------------------
if ('2' %in% stage_vector) {

	print("starting stage-2 (feature modification)")

	feature_modification(cohort_path=cohort_path, control_path=control_path,
 		data_def_path=data_def_path, feature_path=feature_path,
 		feature_set_id=feature_set_id,
 		feature_set_prefix=feature_set_prefix)

}

# Execute Stage-3 > Assemble the Features
#---------------------------------------
if ('3' %in% stage_vector) {

	print("starting stage-3 (feature compilation)")

	feature_compilation(cohort_path=cohort_path, control_path=control_path,
 		data_def_path=data_def_path, feature_path=feature_path,
 		feature_set_id=feature_set_id,
 		feature_set_prefix=feature_set_prefix, 
 		overview_stat=TRUE)

}

#----------------------------------------------------------------------------#
