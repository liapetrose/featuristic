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

project_path          <- "/data/zolab/sl/featuristic_dev/output/" 
source_folder         <- paste0(project_path, "data_source/")       #[MODIFY]
raw_feature_folder    <- paste0(project_path, "raw_feature/")       #[MODIFY]
mod_feature_folder    <- paste0(project_path, "mod_feature/")       #[MODIFY]
final_feature_folder  <- paste0(project_path, "final_feature/")     #[MODIFY]
metadata_folder       <- paste0(project_path, "metadata/")
temp_folder           <- paste0(project_path, "temp/")              #[MODIFY]


# [B] data 
#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#

# [0] data files used to generate master data subsets in stage 0
#----------------------------
# specify the data files that are to be used to generate the master data subsets for 
# each type of feature
## > the data 'objects' listed here, e.g. 'rpdr_dem_master_bwh_ed_100k' have to refer
## to paths defined in the project-specific 'data_path.R' script
 
# BWH - RPDR        #[MODIFY TO INDICATE MASTER COHORTS OF INTEREST (eg. bwh_ed_100k, icmp, dfci etc.)] 
#                   #[NA for those not of interest]
# --------------           

## > Generate demographic features
dem_file_source <- list(rpdr_dem_master_bwh_ed_100k, rpdr_dem_master_dfci)  

## > Generate microbiology features
mic_file_source <- list(rpdr_mic_master_bwh_ed_100k)  

## > Generate diagnosis features
dia_file_source <- list(rpdr_dia_master_bwh_ed_100k, rpdr_dia_master_dfci)           

## > Generate encounter features
enc_file_source <- list(rpdr_enc_master_bwh_ed_100k, rpdr_enc_master_dfci)

## > Generate vital sign features
lvs_file_source <- list(rpdr_lvs_master_bwh_ed_100k, rpdr_lvs_master_dfci) 

## > Generate procedure features
prc_file_source <- list(rpdr_prc_master_bwh_ed_100k, rpdr_prc_master_dfci)

## > Generate medication features
med_file_source <- list(rpdr_med_master_bwh_ed_100k, rpdr_med_master_dfci)

## > Generate lab features
lab_file_source <- list(rpdr_lab_master_bwh_ed_100k)

# BWH - EDADMIN   #[MODIFY TO INDICATE MASTER COHORTS OF INTEREST (eg. bwh_ed_100k, icmp, dfci etc.)]
#                 #[NA for those not of interest]
# --------------

## > Generate ed encounter/order features
ed_file_source     <- list(edadmin_ed_master_bwh_ed_100k)

# DFCI - ONCDRS   #[MODIFY TO INDICATE MASTER COHORTS OF INTEREST (eg. bwh_ed_100k, icmp, dfci etc.)]
#                 #[NA for those not of interest]
# --------------

## > Generate oncdrs chemo features
chemo_oncdrs_file_source <- list(oncdrs_chemo_master_dfci)

## > Generate oncdrs medication features
med_oncdrs_file_source   <- list(oncdrs_med_master_dfci)

## > Generate oncdrs encounter features
enc_oncdrs_file_source   <- list(oncdrs_enc_master_dfci)

## > Generate oncdrs lab features
lab_oncdrs_file_source   <- list(oncdrs_lab_master_dfci)

## > Generate oncdrs diagnosis features
dia_oncdrs_file_source   <- list(oncdrs_dia_master_dfci)

#[1] data files used in stage 1 -- by default assigned to output from stage 0
#----------------------------

#[LEAVE UNCHANGED UNLESS YOU WANT TO SKIP STAGE 0 (AT THE COST OF EXTRA COMPUTE TIME)]

## the data files here are to be used to generate each type of feature
## by default, these are assigned standardized datasets that stage 1 expects from the stage 2 output
 
# BWH - RPDR      #[LEAVE UNCHANGED UNLESS YOU WANT TO SKIP STAGE 0 (AT THE COST OF EXTRA COMPUTE TIME)]
# --------------           

## > Generate demographic features
dem_file_mod <- paste0(source_folder, "dem.Rds")

## > Generate microbiology features
mic_file_mod <- paste0(source_folder, "mic.Rds")

## > Generate diagnosis features
dia_file_mod <- paste0(source_folder, "dia.Rds")

## > Generate encounter features
enc_file_mod <- paste0(source_folder, "enc.Rds")

## > Generate vital sign features
lvs_file_mod <- paste0(source_folder, "lvs.Rds")

## > Generate procedure features
prc_file_mod <- paste0(source_folder, "prc.Rds")

## > Generate medication features
med_file_mod <- paste0(source_folder, "med.Rds")

## > Generate lab features
lab_file_mod <- paste0(source_folder, "lab.Rds")

# BWH - EDADMIN     #[LEAVE UNCHANGED UNLESS YOU WANT TO SKIP STAGE 0 (AT THE COST OF EXTRA COMPUTE TIME)]
# --------------

## > Generate ed encounter/order features
ed_enc_file_mod     <- paste0(source_folder, "ed.Rds")

# DFCI - ONCDRS    	#[LEAVE UNCHANGED UNLESS YOU WANT TO SKIP STAGE 0 (AT THE COST OF EXTRA COMPUTE TIME)]
# --------------

## > Generate oncdrs chemo features
chemo_oncdrs_file_mod <- paste0(source_folder, "chemo_oncdrs.Rds")

## > Generate oncdrs medication features
med_oncdrs_file_mod   <- paste0(source_folder, "med_oncdrs.Rds")

## > Generate oncdrs encounter features
enc_oncdrs_file_mod   <- paste0(source_folder, "enc_oncdrs.Rds")

## > Generate oncdrs lab features
lab_oncdrs_file_mod   <- paste0(source_folder, "lab_oncdrs.Rds")

## > Generate oncdrs diagnosis features
dia_oncdrs_file_mod   <- paste0(source_folder, "dia_oncdrs.Rds")


# [C] feature selection 
#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#

# [0] feature types to source from RPDR datasets      #[MODIFY]
#--------------------------------#
# specify the feature categories which for which master data subsets should be created (stage-0)

source_list <- list(
  "dia"
  # "dem", "prc","lvs","med","dia","mic","ed","enc","lab",
  # "dia_oncdrs","chemo_oncdrs","med_oncdrs","enc_oncdrs","lab_oncdrs"
)

# [1] feature types to assemble                                            #[MODIFY]
#--------------------------------#
# specify the feature categories which are to be assembled using the datasets from stage 0 (stage-1)

assemble_list <- list(   
"dia"                                                 
  # basic
  # "dem", "prc","lvs","med","dia","mic","ed","enc","lab",
  # "dia_oncdrs","chemo_oncdrs","med_oncdrs","enc_oncdrs","lab_oncdrs",
  # combined
  # "dia_oncdrs_rpdr","med_chemo_oncdrs_rpdr","lab_oncdrs_rpdr"
)

# [2] feature types to compile                                               #[MODIFY]
#--------------------------------#
# specify the feature categories which are to be compiled (stage-2)

compile_list <- list(
  "dia"
  # basic
  # "dem", "prc","lvs","med","dia","mic","ed","enc","lab",
  # "dia_oncdrs","chemo_oncdrs","med_oncdrs","enc_oncdrs","lab_oncdrs",
  # combined
  # "dia_oncdrs_rpdr","med_chemo_oncdrs_rpdr","lab_oncdrs_rpdr"
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
timeframe_list$timeframe_end$length      <- 365                               #[MODIFY]
timeframe_list$timeframe_end$name        <- "timeframe_1m_1y"
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

## imputation - numeric variables [only median imputation is possible here]
# specify whether to impute missing variables (e.g. a missing lab value) using median 
# imputation (fill_na=TRUE). By default (indic_fill_na=FALSE) only numeric variables 
# are imputed, setting indic_fill_na=TRUE ensures that missing indicator variables (i.e.
# indicator variables which have not been imputed with 0) are also imputed *WITH THE MEDIAN* 
# indic_fill_na <- TRUE **WILL DO NOTHING** UNLESS fill_na is ALSO TRUE

fill_na                <- FALSE                                                        #[MODIFY]
indic_fill_na          <- FALSE # if unimputed indicators need to be median imputed, set TRUE

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
