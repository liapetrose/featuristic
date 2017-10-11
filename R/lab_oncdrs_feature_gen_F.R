#----------------------------------------------------------------------------#

#' @title Generate DFCI lab-related features (lab_oncdrs data).
#'
#' @description \
#'
#' @export
#' @param cohort
#' @param cohort_key_var_merge
#' @param cohort_key_var
#' @return
#' @examples


lab_oncdrs_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var) {

  
  print("launching lab_oncdrs_feature_gen")
  
  #-------------------------------------------------------------------------------#
  # Source > lab_feature_gen
  #-------------------------------------------------------------------------------#
  lab_oncdrs <- lab_feature_gen(cohort=cohort, cohort_key_var_merge=cohort_key_var_merge, 
    cohort_key_var=cohort_key_var, lab_file_mod_arg=lab_oncdrs_file_mod, 
    leak_lab_day_arg=leak_oncdrs_lab_day, file_date_var="lab_date")

  #-------------------------------------------------------------------------------#
  # Format 
  #-------------------------------------------------------------------------------#

  # rename variables
  var_rename <- setdiff(names(lab_oncdrs), cohort_key_var_merge)
  setnames_check(lab_oncdrs, old=var_rename, new=gsub("^lab", "lab.dfci", var_rename))
 
  var_rename <- setdiff(names(lab_oncdrs), cohort_key_var_merge)
  setnames_check(lab_oncdrs,  old=var_rename , new=paste0(gsub("_", "_dfci.", 
    gsub("(.*)(\\.\\.)(.*)", "\\1", var_rename)), "..", gsub("(.*)(\\.\\.)(.*)", "\\3", 
    var_rename)))
  setnames_check(lab_oncdrs, new=gsub("(.*)(lab.dfci_time)", "lab_oncdrs_time", 
    names(lab_oncdrs)))

  #-------------------------------------------------------------------------------#
  # Return
  #-------------------------------------------------------------------------------#

  return(lab_oncdrs)


}

#----------------------------------------------------------------------------#

