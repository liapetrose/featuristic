# Generating Features - TEMPLATE
#----------------------------------------------------------------------------#

# SETUP
#----------------------------------------------------------------------------#

# [0] Load the package
#---------------------------------------
library(featuristic)

# [1] Specify the location of the cohort for which to generate features 
#---------------------------------------
# specify the path to the cohort file (.Rds) for which features are to be generated
## > the cohort file MUST contain (at minimum) the following columns (additional 
###  columns are preserved):

### "outcome_id" - a unique identifier for each observation
### "outcome"    - a (binary) outcome variable (e.g. deceased - yes (1) / no (0))
### "empi"       - patient identifier associated with each observation
### "t0_date"    - date associated with each observation and based on which 
###                features are constructed (~ 't0')

## Optionally the cohort file can contain any of the following variables which 
## are taken into account in the generation of any summary statistics:

### "test_set  " - a binary variable indicating whether a given observation belongs to the
###                'testing' (1) / 'training' (0) set

cohort_path           <- paste0(modified_folder, "sample_cohort.Rds")

# [2] Specify the paths to the key control inputs 
#---------------------------------------
## > These should be modified versions of the templates located in the
##   template folder

# control.R file   > settings
control_script_path    <- "/data/zolab/featuristic/template/control.R"

# data_path.R file > data paths
data_script_path       <- "/data/zolab/featuristic/template/data_path.R"

# feature_selection.csv file > variable selection 
feature_selection_path <- "/data/zolab/featuristic/template/feature_selection.csv"

# [3] Specify the ID and prefix which is to be associated with any outputs 
#---------------------------------------
# specify a unique cohort-specific prefix and ID - the prefix and ID will
# be associated with all (a) intermediate data files and (b) the final feature set.
## > the prefix and ID should be different for every set of settings specified in this file
## even if working with the same cohort data file

feature_set_id     <-  "test_cohort_main_model"
feature_set_prefix <- "3" 



# EXECUTION
#----------------------------------------------------------------------------#

# Execute Stage-1 > Construct the Features
#---------------------------------------
feature_construction(control=control_script_path, data=data_script_path, 
	feature=feature_selection_path, cohort_id=, cohort_prefix=, cohort=)

# Execute Stage-2 > Assemble the Features
#---------------------------------------
feature_compilation(control=control_script_path, data=data_script_path, 
	feature=feature_selection_path, cohort_id=, cohort_prefix=, cohort=)


#----------------------------------------------------------------------------#