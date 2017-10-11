#----------------------------------------------------------------------------#

#' @title Generate DFCI & RPDR (combined) diagnosis-related features (dia_oncdrs & dia data).
#'
#' @description \
#'
#' @export
#' @param cohort
#' @param cohort_key_var_merge 
#' @param cohort_key_var
#' @return
#' @examples

 dia_oncdrs_rpdr_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var) {

  print("launching dia_oncdrs_rpdr_feature_gen")
  
  #-------------------------------------------------------------------------------#
  # Source > dia_feature_gen
  #-------------------------------------------------------------------------------#
  dia_oncdrs_rpdr <- dia_feature_gen(cohort=cohort, cohort_key_var_merge=cohort_key_var_merge, 
    cohort_key_var=cohort_key_var, dia_file_mod_arg=dia_oncdrs_file_mod, 
    leak_dia_day_arg=leak_dia_day, combine=TRUE, dia_file_mod_ext=dia_file_mod, 
    file_date_var="dia_date")

  #-------------------------------------------------------------------------------#
  # Return
  #-------------------------------------------------------------------------------#
  return(dia_oncdrs_rpdr)

}


#----------------------------------------------------------------------------#



