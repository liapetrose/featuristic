#----------------------------------------------------------------------------#

#' @title Generate demographics-related features (dem data).
#'
#' @description \
#'
#' @export
#' @param cohort
#' @param cohort_key_var_merge
#' @return
#' @examples

dem_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var) {

  print("launching dem_feature_gen")
 
  #-------------------------------------------------------------------------------#
  #load the raw file & load requisite helpers

  ## raw file
  dem <- readRDS_merge(dem_file_mod)
  dem <- unique(dem, by=c("empi"))

  # subset to smaller sample for testing if so specified in control file
  if (test_raw_file==TRUE) {
    store_shorten_file("dem")
  }

  #-------------------------------------------------------------------------------#
  # merge demographics file with cohort (cohort_key_var_merge) & format/rename 
  # dates & report number of patients with no complete dem information 
  ## NOTE: multiple rows per patient - to allow for the incorporation of time 
  ##       dependent data (age, flu data...)
  ## NOTE: remove missing observations, cohort observations with no dem record (empi)
  dem <- dem[cohort, as.list(c(mget(setdiff(names(dem), "empi")),mget(cohort_key_var_merge))), 
    on=c("empi"), nomatch=0]

  print(sprintf("number of cohort observations with no dem record: %d, 
  	number of patients with no dem record: %d, number of cohort observations with a dem_record -
    this should be the final number of rows:%d", nrow(cohort[!empi %in% dem$empi]),
  	nrow(unique(cohort[!empi %in% dem$empi], by=c("empi"))),
    nrow(cohort[empi %in% dem$empi])))

  # XXX NOTE: Always check date output
  invisible(parse_date(dem, c("date_of_birth", "date_of_death"), 
    c("dob", "dod")))

  #-------------------------------------------------------------------------------#
  # age
  dem[,age:=difftime(t0_date,dob, units="days")/365]
  
  # impose variable categories (dem_dem.basic..//dem_dem.basic_race..)
  setnames(dem, setdiff(names(dem), cohort_key_var_merge),
    paste0("dem_dem.basic..", setdiff(names(dem), cohort_key_var_merge)))
  setnames(dem, grep("race.", names(dem), value=T), gsub("\\.\\.", "_race..",
    grep("race.", names(dem), value=T)))

  #-------------------------------------------------------------------------------#
  # subset to the relevant columns
  remove_var <- grep("dob$|dod$|vital_status$", names(dem), value=T)
  dem[, c(remove_var):=NULL]

  dem[, grep("dem_id", names(dem), value=T):=NULL]
  
  #-------------------------------------------------------------------------------#
  # categorize variables to ensure proper treatment in models -- integer & factor
  dem_factor <- dem[, mget(setdiff(grep("dem.basic", names(dem), value=T), 
    c("dem_dem.basic..age")))]
  dem_factor[, names(dem_factor):=lapply(.SD, function(x) as.factor(x))]

  dem_numeric <- dem[, mget(c("dem_dem.basic..age"))]
  dem_numeric[, names(dem_numeric):=lapply(.SD, function(x) as.numeric(round(x, digits=2)))]

  dem <- cbind(dem[, mget(c("outcome_id", "t0_date", "empi"))], dem_factor, 
    dem_numeric)

  #-------------------------------------------------------------------------------#
  # return demographic file  & delete key files creted in function

  print(sprintf("final number of rows: %d", nrow(dem)))

  rm(dem_factor, dem_numeric)
  return(dem)



}

#----------------------------------------------------------------------------#

