#----------------------------------------------------------------------------#

#' @title Generate DFCI chemotherapy-related features (chemo_oncdrs data).
#'
#' @description \
#'
#' @export
#' @param cohort
#' @param cohort_key_var_merge
#' @param cohort_key_var
#' @param 
#' @return
#' @examples


chemo_oncdrs_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var) {

  print("launching chemo_oncdrs_feature_gen")

  #-------------------------------------------------------------------------------#
  # Source > med_feature_gen
  #-------------------------------------------------------------------------------#
  chemo_oncdrs <- med_feature_gen(cohort=cohort, cohort_key_var_merge=cohort_key_var_merge, 
    cohort_key_var=cohort_key_var, med_file_mod_arg=chemo_oncdrs_file_mod, 
    leak_med_day_arg=leak_oncdrs_chemo_day, file_date_var="med_date")

  #-------------------------------------------------------------------------------#
  # Format 
  #-------------------------------------------------------------------------------#

  # rename variables
  var_rename <- setdiff(names(chemo_oncdrs), cohort_key_var_merge)
  setnames_check(chemo_oncdrs, old=var_rename, new=gsub("^med", "chemo.dfci", var_rename))
 
  var_rename <- setdiff(names(chemo_oncdrs), cohort_key_var_merge)
  setnames_check(chemo_oncdrs,  old=var_rename , new=paste0(gsub("_", "_dfci.chemo.", 
    gsub("(.*)(\\.\\.)(.*)", "\\1", var_rename)), "..", gsub("(.*)(\\.\\.)(.*)", "\\3", 
    var_rename)))
  setnames_check(chemo_oncdrs, new=gsub("med\\.", "chemo\\.", names(chemo_oncdrs)))
  setnames_check(chemo_oncdrs, new=gsub("(.*)(chemo.dfci_time)", "chemo_oncdrs_time", 
    names(chemo_oncdrs)))


  #-------------------------------------------------------------------------------#
  # Return
  #-------------------------------------------------------------------------------#

  return(chemo_oncdrs)

}


#----------------------------------------------------------------------------#
