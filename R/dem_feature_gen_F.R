#----------------------------------------------------------------------------#

#' @title Generate demographics-related features (dem data).
#'
#' @description \
#'
#' @export
#' @param cohort cohort dt
#' @param cohort_key_var_merge list of key identifiers on which to merge (see: feature_initialisation)
#' @param cohort_key_var list of key identifiers on which to merge (see: feature_initialisation)
#' @return
#' @examples

dem_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var) {

  print("launching dem_feature_gen")
 
  #-------------------------------------------------------------------------------#
  # SETUP
  #-------------------------------------------------------------------------------#

  # Load the data & subset
  #-------------------------------------------------------------------------------#

  # load data
  dem <- readRDS_merge(dem_file_mod)

  print(sprintf("number of observations: %d", nrow(dem)))

  # subset - unique (one row per patient)
  dem <- unique(dem, by=c("empi"))

  # subset to cohort, i.e. merge with cohort
  dem <- dem[cohort, as.list(c(mget(setdiff(names(dem), "empi")),
    mget(cohort_key_var_merge))), on=c("empi"), nomatch=0]

  ## SAMPLE SIZE CHECK #1
  assert("observations for cohort patients", nrow(dem)>0)

  # subset to smaller sample for testing if so specified in control file
  if (test_mode==TRUE) {
    store_shorten_file("dem")
  }

  # Date Formatting 
  #-------------------------------------------------------------------------------#
  
  # date formatting
  invisible(format_date(list(dem), c("date_of_birth", "date_of_death")))
  setnames(dem, c("date_of_birth", "date_of_death"), c("dob", "dod"))

  #-------------------------------------------------------------------------------#


  #-------------------------------------------------------------------------------#
  # FEATURE GENERATION - STATIC
  #-------------------------------------------------------------------------------#

  # [1] Basic features 
  #-------------------------------------------------------------------------------#
  print("feature generation - dem.basic")

  # age
  dem[,age:=difftime(t0_date,dob, units="days")/365]
  
  # race
  race_clean <- group_race(dem, "race")
  dem[, race:=race_clean]

  # language
  language_clean <- group_language(dem, "language")
  dem[, language:=language_clean]

  # marital status
  marital_status_clean <- group_marital_status(dem, "marital_status")
  dem[, marital_status:=marital_status_clean]

  # religion
  religion_clean <- group_religion(dem, "religion")
  dem[, religion:=religion_clean]

  # misc - rename
  setnames_check(dem, old=setdiff(names(dem), cohort_key_var_merge),
    new=paste0("dem_dem.basic..", setdiff(names(dem), cohort_key_var_merge)))

  # subset to relevant columns
  remove_var <- grep("dob$|dod$|vital_status$", names(dem), value=T)
  suppressWarnings(dem[, c(remove_var):=NULL])

  #-------------------------------------------------------------------------------#
  # FORMAT VARIABLES
  #-------------------------------------------------------------------------------#

  print("cohort merging & feature formatting")

  # merge
  suppressWarnings(dem[, grep("dem_id$", names(dem), value=T):=NULL])

  # format variables
  dem <- feature_type_format(dt=dem, day_to_last=timeframe_day_to_last, 
    num_feature_pattern="dem_dem.basic..age", 
    factor_feature_pattern="...",
    int_feature_pattern=NA) 

  #-------------------------------------------------------------------------------#
  # CLEAR MEMORY & RETURN FEATURE SET
  #-------------------------------------------------------------------------------#
  return(dem)

}

#----------------------------------------------------------------------------#

