# Global Settings --- MASTER TEMPLATE
#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
#                               Control Section                              #
#----------------------------------------------------------------------------#

# [A] (project-specific) directory structure 
#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#

# [1] specify the directories where the output is to be stored 
## > these folders should be located in the project-specific folder on 
# the zolab server 

temp_folder           <- "/data/zolab/featuristic_sample/temp/"              #[MODIFY]
modified_folder       <- "/data/zolab/featuristic_sample/modified_data/"     #[MODIFY]
output_folder         <- "/data/zolab/featuristic_sample/output/"            #[MODIFY]

# [B] cohort file
#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#

# [1] cohort file path
#----------------------------
# specify the path to the cohort file (.Rds) for which features are to be generated
## > the cohort file MUST contain (at minimum) the following columns (additional 
###  columns are preserved):

### "outcome_id" - a unique identifier for each observation
### "outcome"    - a (binary) outcome variable (e.g. deceased - yes (1) / no (0))
### "empi"       - patient identifier associated with each observation
### "pred_date"  - date associated with each observation and based on which 
###                features are constructed (~ 't0')

## Optionally the cohort file can contain any of the following variables which 
## are taken into account in the generation of any summary statistics:

### "test_set  " - a binary variable indicating whether a given observation belongs to the
###                'testing' (1) / 'training' (0) set

cohort_file       <- paste0(modified_folder, "sample_cohort.Rds") 			 #[MODIFY]

# [2] cohort sub setting
#----------------------------
# specify whether to generate the features for only a subset of the 
# cohort - if yes: specify and expression based on which to subset the 
# cohort // if no: set to NA

cohort_mod        <- "test_subset==1"                              			 #[MODIFY]

# [3] cohort prefix/ID 
#----------------------------
# specify a unique cohort-specific prefix and ID - the prefix and ID will
# be associated with all (a) intermediate data files and (b) the final feature set.
## > the prefix and ID should be different for every set of settings specified in this file
## even if working with the same cohort data file
 
cohort_name_prefix <- "test_cohort_main_model"                       		 #[MODIFY]
cohort_id          <- "3"                                          			 #[MODIFY]


# [4] cohort prefix/ID 
#----------------------------
# specify whether to (i) construct new features (stage-1) or 
# (ii) to merely load and re-compile existing feature sets (stage-2) (using 
# the settings in this script). To select option (ii) specify the 'cohort_name_prefix'_'cohort_id'
# of an existing cohort (located in the output folder). To select option (i) specify NA

cohort_name_assemble <- "prelimtest_cohort_main_model_2"            		 #[MODIFY]

# [C] data 
#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#

# [1] data files
#----------------------------
# specify the data files that are to be used to generate each type of feature
## > the data 'objects' listed here, e.g. 'rpdr_dem_master_bwh_ed_100k' have to refer
## to paths defined in the modified, project-specific 'data_path.R' script
 
# BWH - RPDR                                                                 #[MODIFY]
# --------------           

## > Generate demographic features
dem_file_mod <- list(rpdr_dem_master_bwh_ed_100k, rpdr_dem_master_pe, 
					rpdr_dem_master_icmp, rpdr_dem_master_hdlab)  

## > Generate microbiology features
mic_file_mod <- list(rpdr_mic_master_bwh_ed_100k, rpdr_mic_master_pe, 
					rpdr_mic_master_icmp, rpdr_mic_master_hdlab)  

## > Generate diagnosis features
dia_file_mod <- list(rpdr_dia_master_bwh_ed_100k, rpdr_dia_master_pe, 
					rpdr_dia_master_icmp, rpdr_dia_master_hdlab)           

## > Generate encounter features
enc_file_mod <- list(rpdr_enc_master_bwh_ed_100k, rpdr_enc_master_pe, 
					rpdr_enc_master_icmp, rpdr_enc_master_hdlab)

## > Generate vital sign features
lvs_file_mod <- list(rpdr_lvs_master_bwh_ed_100k, rpdr_lvs_master_pe, 
					rpdr_lvs_master_icmp, rpdr_lvs_master_hdlab) 

## > Generate procedure features
prc_file_mod <- list(rpdr_prc_master_bwh_ed_100k, rpdr_prc_master_pe,
					rpdr_prc_master_icmp, rpdr_prc_master_hdlab)

## > Generate medication features
med_file_mod <- list(rpdr_med_master_bwh_ed_100k, rpdr_med_master_pe, 
					rpdr_med_master_icmp, rpdr_med_master_hdlab)

## > Generate lab features
lab_file_mod <- list(rpdr_lab_master_bwh_ed_100k, rpdr_lab_master_pe, 
					rpdr_lab_master_icmp, rpdr_lab_master_hdlab)

# BWH - EDADMIN 															 #[MODIFY]
# --------------

## > Generate ed encounter/order features
ed_enc_file_mod       <- list(edadmin_ed_master_bwh_ed_100k)

# DFCI - ONCDRS    															 #[MODIFY]
# --------------

## > Generate oncdrs chemo features
chemo_oncdrs_file_mod <- list(oncdrs_chemo_master_dfci)

## > Generate oncdrs medication features
med_oncdrs_file_mod   <- list(oncdrs_med_master_dfci)

## > Generate oncdrs encounter features
enc_oncdrs_file_mod   <- list(oncdrs_enc_master_dfci)

## > Generate oncdrs lab features
lab_oncdrs_file_mod   <- list(oncdrs_lab_master_dfci)

## > Generate oncdrs diagnosis features
dia_oncdrs_file_mod   <- list(oncdrs_dia_master_dfci)


# [D] feature selection 
#----------------------------------------------------------------------------#

# [1] feature types to construct
#--------------------------------#
# specify the feature categories which are to be constructed (stage-1)

construct_list <- list(                                                  	 #[MODIFY]
	# basic
	"dem", "prc", "lvs", "mic", "ed", "enc", "lab", "med", "dia", 
	"dia_oncdrs", "chemo_oncdrs", "med_oncdrs","lab_oncdrs",
	# combined
	"dia_oncdrs_rpdr", "med_chemo_oncdrs_rpdr", "lab_oncdrs_rpdr", "enc_oncdrs"
)

# [2] feature types to compile                                               #[MODIFY]
#--------------------------------#
# specify the feature categories which are to be compiled (stage-2)

compile_list <- list(
	# basic
	"dem", "prc", "lvs", "mic", "ed", "enc", "lab", "med", "dia", 
	"dia_oncdrs", "chemo_oncdrs", "med_oncdrs","lab_oncdrs",
	# combined
	"dia_oncdrs_rpdr", "med_chemo_oncdrs_rpdr", "lab_oncdrs_rpdr", "enc_oncdrs"
)



# [E] feature construction settings 
#----------------------------------------------------------------------------#

# [1] feature category selection 
#--------------------------------#
# specify the feature category selection scheme that is to be used to 
# define the set of features represented in the final feature set by providing the 
# name of the indicator column in the modified, project-specific 'var_selection.csv' file 

variable_list_file_selection                    <- "var_basic"                #[MODIFY]

# [2] missingness constraints
#--------------------------------#
# specify the missingness thresholds which are used to decide whether a given variable
# should be dropped due to being too 'rare'
## > numeric/factor variables are dropped if missing for >  'quant_missing_threshold' % of observations
## > indicator variables are dropped if they do not occur (==0/NA depending on the imputation 
##   mode (see [5] below)) for >  'indic_missing_threshold' % of observations
## > for each thresholds ('quant_missing_threshold', 'indic_missing_threshold') a separate
##   value has to be specified for each timeframe (see [3] timeframes below), e.g. 
##   in the case of 3 timeframes the 1st value corresponds to the 'aggregate' timeframe, the second value
##   to the 'day' timeframe and the 3rd value to the 'short' timeframe and 
##   the 4th value to the 'end' timeframe (i.e. values are ordered
##   from the shortest to the longest timeframe starting with the aggregate timeframe)
## > setting a threshold value to 100 ensures that no variables are dropped (on the 
##   basis of this constraint)

quant_missing_threshold <- list(30, 50, 40, 30)									  #[MODIFY]
indic_missing_threshold <- list(30, 50, 40, 30)									  #[MODIFY]

# [3] timeframes
#--------------------------------#
# specify the timeframes over which to construct the features
## > one time frame MUST be specified - this timeframe gives
##   the overall number of days over which features are to be constructed, e.g.
##   with 'timeframe_end'=365L > features are constructed over the year prior to t0
## > the specification of additional timeframes results in a splitting of the 
##   longest timeframe into multiple, non-overlapping sub-timeframes, e.g.
##   the day of t0, the day before t0 - 30 days before, 30 days before - 1 year before
## > timeframes must be specified in the right order starting with the longest timeframe
##   and each timeframe must have a unique '_name' and '_name_abb' (which must start with a '_')
 
## initialize (DO NOT MODIFY)
timeframe_list                          <- list()                                  #[DO NOT MODIFY]

## 
timeframe_list$timeframe_end             <- 365L                                   #[MODIFY]
timeframe_list$timeframe_end_name        <- "timeframe_0m_12m"
timeframe_list$timeframe_end_name_abb    <- "_end"

timeframe_list$timeframe_short           <- 30L
timeframe_list$timeframe_short_name      <- "timeframe_1m"
timeframe_list$timeframe_short_name_abb  <- "_short"

timeframe_list$timeframe_day             <- 0L
timeframe_list$timeframe_day_name        <- "timeframe_outcome_day"
timeframe_list$timeframe_day_name_abb    <- "_day"

# [4] leakage
#----------------------------------------------------------------------------#
# specify whether or not leakage controls are to be implemented for each of the 
# feature types, i.e. whether to omit data over a certain time period directly
# preceding t0 to prevent information leakage from the future
## > NA: no data is omitted (all data is used up to and including data recorded
##   on t0) / 0: data recorded on the day of t0 is NOT used / >1: data recorded on t0 and 
##   in the x days before t0 is omitted

## initialize (DO NOT MODIFY)
leak_list <- list()                                                                 #[DO NOT MODIFY]

##
leak_list$leak_dem_day 					<- 0                                        #[MODIFY]
leak_list$leak_enc_day 					<- 0
leak_list$leak_dia_day 					<- 0
leak_list$leak_prc_day 					<- 0
leak_list$leak_lvs_day 					<- 0
leak_list$leak_lab_day 					<- 0
leak_list$leak_med_day 					<- 0
leak_list$leak_mic_day 					<- 0
leak_list$leak_ed_day  					<- 0

# [5] imputation
#----------------------------------------------------------------------------#


## imputation - indicator variables
##---------------
# specify whether to impute non-present indicator variables (e.g. a non-occurring diagnosis), i.e.
# whether to assume that such variables = 0 (mis_imp=TRUE) or are actually missing/unknown (mis_imp=FALSE)

miss_imp         <- TRUE                                                              #[MODIFY]

## imputation - numeric variables
# specify whether to impute numeric variables (e.g. a missing lab value) according to the
# specified median imputation procedure ("fill_na_method") (fill_na=TRUE)
## > supported imputation procedures: "median_imputation"

fill_na          <- FALSE                                                             #[MODIFY]
fill_na_method   <- "median_imputation"

# [F] additional feature construction settings
# * (!) NOTE - these settings should generally be left at their default value
#----------------------------------------------------------------------------#

# [1] test mode
#----------------------------------------------------------------------------#
# specify whether to run the codebase in test mode, i.e. whether to use
# a subset of all data files and the cohort to generate the features (test_raw_file=TRUE / 
#  test_row = [max number of rows which are to be used])

test_raw_file <- FALSE  
test_row 	  <- 20000  


#----------------------------------------------------------------------------#
#                                  End                                       #
#----------------------------------------------------------------------------#
