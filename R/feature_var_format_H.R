#----------------------------------------------------------------------------#

#' @title Format features. 
#'
#' @description \
#'
#' @export
#' @param dt_list 
#' @param function_ext_1
#' @param function_ext_2
#' @param drop
#' @return
#' @examples

feature_var_format <- function(dt_list, function_ext_1="_length_", 
  function_ext_2="_function_", drop=FALSE) {
  
  # rename
  lapply(dt_list, function(x)  {
    if(nrow(x)>0) {
      setnames_check(x, old=grep(paste0("time_diff", function_ext_2), names(x), value=T), 
        new=paste0(grep(paste0("time_diff", function_ext_2), names(x), value=T), 
          "_day_to_last"))
    }
  })

  lapply(dt_list, function(x)  {
    if(nrow(x)>0) {
      setnames_check(x, new=gsub(paste0("time_diff", "(", function_ext_1, "|", 
        function_ext_2,")"), "", names(x)))
    }
  })

  # drop NA variables
  suppressWarnings(inv_lapply(dt_list, function(x) x[, grep("^0|NA|NA_day_to_last|0_day_to_last", 
      names(x), value=T):=NULL]))

  # drop irrelevant variables
  if (drop==TRUE) {
 
    inv_lapply(dt_list, function(x) x[, c(setdiff(setdiff(names(x), 
      grep("day_to_last",names(x), value=T )), cohort_key_var_merge)):=NULL])
  }

  return(dt_list)
  
}

#----------------------------------------------------------------------------#
