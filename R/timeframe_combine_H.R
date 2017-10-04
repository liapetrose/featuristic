#----------------------------------------------------------------------------#

#' @title Combine a dataframe split into different timeframes. 
#'
#' @description \
#'
#' @export
#' @param DT_list 
#' @return
#' @examples


timeframe_combine <- function(DT_list, var_list=cohort_key_var) {

  lapply(DT_list, function(x) {
   assign(x, (Reduce(function(...) merge(..., all = TRUE, 
    by=var_list), get(x, sys.frame(sys.parent(n=3))))), sys.frame(sys.parent(n=3)))
  })     
}

#----------------------------------------------------------------------------#
