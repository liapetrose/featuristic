#----------------------------------------------------------------------------#

#' @title Combine a dataframe split into different timeframes. 
#'
#' @description \
#'
#' @export
#' @param DT_list 
#' @param var_list
#' @return
#' @examples


timeframe_combine <- function(DT_list, DT_list_name, var_list=cohort_key_var) {

  merge_data <- lapply(DT_list, function(x) {

  	x_tmp <- Reduce(function(...) merge(..., all = TRUE, 
    	by=var_list), x)

    return(x_tmp)
  })     

  names(merge_data) <- DT_list_name

  return(merge_data)

}

#----------------------------------------------------------------------------#

