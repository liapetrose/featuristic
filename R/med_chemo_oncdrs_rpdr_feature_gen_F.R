#----------------------------------------------------------------------------#

#' @title Generate DFCI & RPDR (combined) medication and chemotherapy-related 
#' features (med_oncdrs & med & chemo_oncdrs data).
#'
#' @description \
#'
#' @export
#' @param cohort
#' @param cohort_key_var_merge
#' @param cohort_key_var
#' @return
#' @examples


 med_chemo_oncdrs_rpdr_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var) {

  print("launching med_chemo_oncdrs_rpdr_feature_gen")
  
  #-------------------------------------------------------------------------------#
  # Source > med_feature_gen
  #-------------------------------------------------------------------------------#
  med_chemo_oncdrs_rpdr <- med_feature_gen(cohort=cohort, cohort_key_var_merge=cohort_key_var_merge, 
    cohort_key_var=cohort_key_var, med_file_mod_arg=med_oncdrs_file_mod, 
    leak_med_day_arg=leak_med_day, combine=TRUE, med_file_mod_ext=med_file_mod, 
    med_file_mod_ext_ext=chemo_oncdrs_file_mod, file_date_var="med_date")

  #-------------------------------------------------------------------------------#
  # Return
  #-------------------------------------------------------------------------------#
  return(med_chemo_oncdrs_rpdr)

}


#----------------------------------------------------------------------------#

