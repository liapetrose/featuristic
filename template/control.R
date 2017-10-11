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

project_path          <- "/data/zolab/featuristic/test_project/" 
temp_folder           <- paste0(project_path, "temp/")              #[MODIFY]
modified_folder       <- paste0(project_path, "modified/")          #[MODIFY]
output_folder         <- paste0(project_path, "output/")            #[MODIFY]

# [B] data 
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
dem_file_mod <- list(rpdr_dem_master_bwh_ed_100k, rpdr_dem_master_dfci)  

## > Generate microbiology features
mic_file_mod <- list(rpdr_mic_master_bwh_ed_100k)  

## > Generate diagnosis features
dia_file_mod <- list(rpdr_dia_master_bwh_ed_100k, rpdr_dia_master_dfci)           

## > Generate encounter features
enc_file_mod <- list(rpdr_enc_master_bwh_ed_100k, rpdr_enc_master_dfci)

## > Generate vital sign features
lvs_file_mod <- list(rpdr_lvs_master_bwh_ed_100k, rpdr_lvs_master_dfci) 

## > Generate procedure features
prc_file_mod <- list(rpdr_prc_master_bwh_ed_100k, rpdr_prc_master_dfci)

## > Generate medication features
med_file_mod <- list(rpdr_med_master_bwh_ed_100k, rpdr_med_master_dfci)

## > Generate lab features
lab_file_mod <- list(rpdr_lab_master_bwh_ed_100k)

# BWH - EDADMIN 															 #[MODIFY]
# --------------

## > Generate ed encounter/order features
ed_enc_file_mod     <- list(edadmin_ed_master_bwh_ed_100k)

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


# [C] feature selection 
#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#

# [1] feature types to assemble                                            #[MODIFY]
#--------------------------------#
# specify the feature categories which are to be assembled (stage-1)

assemble_list <- list(                                                  	
  # basic
  "dem", "prc","lvs","med","dia","mic","ed","enc","lab",
  "dia_oncdrs","chemo_oncdrs","med_oncdrs","enc_oncdrs","lab_oncdrs",
  # combined
  "dia_oncdrs_rpdr","med_chemo_oncdrs_rpdr","lab_oncdrs_rpdr"
)

# [2] feature types to compile                                               #[MODIFY]
#--------------------------------#
# specify the feature categories which are to be compiled (stage-2)

compile_list <- list(
  # basic
  "dem", "prc","lvs","med","dia","mic","ed","enc","lab",
  "dia_oncdrs","chemo_oncdrs","med_oncdrs","enc_oncdrs","lab_oncdrs",
  # combined
  "dia_oncdrs_rpdr","med_chemo_oncdrs_rpdr","lab_oncdrs_rpdr"
)



# [D] feature construction settings 
#----------------------------------------------------------------------------#
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
##   and each timeframe must have a unique '_name' and '_name_abb' (unit: days)
 
## main timeframes
##---------------

## initialize (DO NOT MODIFY)
timeframe_list                           <- list()                                 #[DO NOT MODIFY]

## specify the timeframes
timeframe_list$timeframe_end$length      <- 2190                                #[MODIFY]
timeframe_list$timeframe_end$name        <- "timeframe_1m_6y"
timeframe_list$timeframe_end$name_abb    <- "end"

timeframe_list$timeframe_short$length    <- 30
timeframe_list$timeframe_short$name      <- "timeframe_1m"
timeframe_list$timeframe_short$name_abb  <- "short"

timeframe_list$timeframe_day$length      <- 0
timeframe_list$timeframe_day$name        <- "timeframe_outcome_day"
timeframe_list$timeframe_day$name_abb    <- "day"

## additional timeframe settings
##---------------

## specify whether the final feature set is to include "day_to_last" (meta-) features
## for each feature (timeframe_day_to_last=TRUE) (e.g. zc_cough (count - number of coughs in a given
## timeperiod) -> zc_cough_day_to_last (the number of days between t0 and the last
## recorded zc_cough diagnosis in the given timeframe)

timeframe_day_to_last                     <- TRUE

# [4] leakage
#--------------------------------#
# specify whether or not leakage controls are to be implemented for each of the 
# feature types, i.e. whether to omit data over a certain time period directly
# preceding t0 to prevent information leakage from the future
## > NA: no data is omitted (all data is used up to and including data recorded
##   on t0) / 0: data recorded on the day of t0 is NOT used / >1: data recorded on t0 and 
##   in the x days before t0 is omitted (unit: days)

## initialize (DO NOT MODIFY)
leak_list <- list()                                                                 #[DO NOT MODIFY]

## specify the leakage timeperiods
leak_list$leak_dem_day 					<- 0                                        #[MODIFY]
leak_list$leak_enc_day 					<- 0
leak_list$leak_dia_day 					<- 0
leak_list$leak_prc_day 					<- NA
leak_list$leak_lvs_day 					<- NA
leak_list$leak_lab_day 					<- NA
leak_list$leak_med_day 					<- 0
leak_list$leak_mic_day 					<- 0
leak_list$leak_ed_day  					<- NA

# [5] imputation
#--------------------------------#

## imputation - indicator variables
##---------------
# specify whether to impute non-present indicator variables (e.g. a non-occurring diagnosis), i.e.
# whether to assume that such variables = 0 (mis_imp=TRUE) or are actually missing/unknown (mis_imp=FALSE)
## > If set to FALSE: Specify whether to NOT impute all indicator variables  (impute_var_cat=NA)
##   or to NOT impute only a subset of indicator variables (impute_var_cat="regx1|regx2" where
##   regx1, regx2 stand for regular expression patterns that identify specific variable categories, e.g. "dia_dia.count"
##   which ARE to be imputed)

miss_imp               <- TRUE                                                        #[MODIFY]
impute_var_cat         <- "dia_dia.count"

## imputation - numeric variables
# specify whether to impute numeric variables (e.g. a missing lab value) according to the
# specified median imputation procedure ("fill_na_method") (fill_na=TRUE)
## > supported imputation procedures: "median_imputation"

fill_na                <- FALSE                                                        #[MODIFY]
fill_na_method         <- "median_imputation"

# [E] additional feature construction settings
# * (!) NOTE - these settings should generally be left at their default value
#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#

# [1] test mode
#----------------------------------------------------------------------------#
# specify whether to run the codebase in test mode, i.e. whether to use
# a subset of all data files and the cohort to generate the features (test_mode=TRUE / 
#  test_row = [max number of rows which are to be used])

test_mode           <- FALSE  
test_row 	          <- 20000  


#----------------------------------------------------------------------------#
#                                  End                                       #
#----------------------------------------------------------------------------#
