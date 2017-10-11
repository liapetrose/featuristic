#----------------------------------------------------------------------------#

#' @title Generate DFCI medication-related features (med_oncdrs data).
#'
#' @description \
#'
#' @export
#' @param cohort
#' @param cohort_key_var_merge
#' @return
#' @examples

 med_oncdrs_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var) {

  print("launching med_oncdrs_feature_gen")
  
  #-------------------------------------------------------------------------------#
  # Source > med_feature_gen
  #-------------------------------------------------------------------------------#
  med_oncdrs <- med_feature_gen(cohort=cohort, cohort_key_var_merge=cohort_key_var_merge, 
    cohort_key_var=cohort_key_var, med_file_mod_arg=med_oncdrs_file_mod, 
    leak_med_day_arg=leak_oncdrs_med_day, file_date_var="med_date")

  #-------------------------------------------------------------------------------#
  # Format 
  #-------------------------------------------------------------------------------#

  # rename variables
  var_rename <- setdiff(names(med_oncdrs), cohort_key_var_merge)
  setnames_check(med_oncdrs, old=var_rename, new=gsub("^med", "med.dfci", var_rename))
 
  var_rename <- setdiff(names(med_oncdrs), cohort_key_var_merge)
  setnames_check(med_oncdrs,  old=var_rename , new=paste0(gsub("_", "_dfci.", 
    gsub("(.*)(\\.\\.)(.*)", "\\1", var_rename)), "..", gsub("(.*)(\\.\\.)(.*)", "\\3", 
    var_rename)))
  setnames_check(med_oncdrs, new=gsub("(.*)(med.dfci_time)", "med_oncdrs_time", 
    names(med_oncdrs)))

  #-------------------------------------------------------------------------------#
  # Return
  #-------------------------------------------------------------------------------#

  return(med_oncdrs)

}


#----------------------------------------------------------------------------#

