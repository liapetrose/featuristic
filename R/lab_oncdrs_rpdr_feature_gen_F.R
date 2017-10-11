#----------------------------------------------------------------------------#

#' @title Generate DFCI & RPDR (combined) lab-related features (lab_oncdrs & lab data).
#'
#' @description \
#'
#' @export
#' @param cohort
#' @param cohort_key_var_merge
#' @param cohort_key_var
#' @return
#' @examples


lab_oncdrs_rpdr_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var) {
    
  print("launching lab_oncdrs_rpdr_feature_gen")
  
  #-------------------------------------------------------------------------------#
  # Source > lab_feature_gen
  #-------------------------------------------------------------------------------#
  lab_oncdrs_rpdr <- lab_feature_gen(cohort=cohort, cohort_key_var_merge=cohort_key_var_merge, 
    cohort_key_var=cohort_key_var, lab_file_mod_arg=lab_oncdrs_file_mod, 
    leak_lab_day_arg=leak_lab_day, combine=TRUE, lab_file_mod_ext=lab_file_mod, 
    file_date_var="lab_date")

  #-------------------------------------------------------------------------------#
  # Return
  #-------------------------------------------------------------------------------#

  return(lab_oncdrs_rpdr)


}

#----------------------------------------------------------------------------#

