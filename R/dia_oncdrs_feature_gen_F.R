#----------------------------------------------------------------------------#

#' @title Generate DFCI diagnosis-related features (dia_oncdrs data).
#'
#' @description \
#'
#' @export
#' @param cohort
#' @param cohort_key_var_merge 
#' @param cohort_key_var
#' @return
#' @examples

 dia_oncdrs_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var) {

  print("launching dia_oncdrs_feature_gen")
  
  #-------------------------------------------------------------------------------#
  # Source > dia_feature_gen
  #-------------------------------------------------------------------------------#
  dia_oncdrs <- dia_feature_gen(cohort=cohort, cohort_key_var_merge=cohort_key_var_merge, 
    cohort_key_var=cohort_key_var, dia_file_mod_arg=dia_oncdrs_file_mod, 
    leak_dia_day_arg=leak_oncdrs_dia_day, file_date_var="dia_date")

  #-------------------------------------------------------------------------------#
  # Format 
  #-------------------------------------------------------------------------------#

  # rename variables
  var_rename <- setdiff(names(dia_oncdrs), cohort_key_var_merge)
  setnames_check(dia_oncdrs, old=var_rename, new=gsub("^dia", "dia.dfci", var_rename))
  
  var_rename <- setdiff(names(dia_oncdrs), cohort_key_var_merge)
  setnames_check(dia_oncdrs,  old=var_rename , new=paste0(gsub("_", "_dfci.", 
    gsub("(.*)(\\.\\.)(.*)", "\\1", var_rename)), "..", gsub("(.*)(\\.\\.)(.*)", "\\3", 
    var_rename)))
  setnames_check(dia_oncdrs, new=gsub("(.*)(dia.dfci_time)", "dia_oncdrs_time", 
    names(dia_oncdrs)))

  #-------------------------------------------------------------------------------#
  # Return
  #-------------------------------------------------------------------------------#

  return(dia_oncdrs)

}


#----------------------------------------------------------------------------#
