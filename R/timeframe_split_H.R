#----------------------------------------------------------------------------#

#' @title Split a dataframe into timeframes. 
#'
#' @description \
#'
#' @export
#' @param DT_list 
#' @param colname 
#' @return
#' @examples

timeframe_split <- function(DT_list, colname) {

  split_data <- lapply(DT_list, function(x) {
    timeframe_comb_list <- list()
    timeframe_comb_list[[1]] <- get(x,sys.frame(
        sys.parent(n=3)))[get(colname) >= get(paste0("t0_date_beg", 
          name_ext[length(name_ext)])) & get(colname) <=t0_date]
    timeframe_comb_list[[2]] <- get(x,sys.frame(
        sys.parent(n=3)))[get(colname) >= get(paste0("t0_date_beg", 
          name_ext[1])) & get(colname) <=t0_date]

    if(length(name_ext)>1) {
      for (i in 2:length(name_ext)) {
        timeframe_comb_list[[i+1]] <- get(x,sys.frame(
          sys.parent(n=3)))[get(colname) >= get(paste0("t0_date_beg", name_ext[i])) & 
          get(colname) < get(paste0("t0_date_beg", name_ext[i-1]))]
      }
    }

    # format
    names(timeframe_comb_list) <- c("timeframe_comb_max",paste0("timeframe_comb", name_ext))

    # subset
    timeframe_comb_list <- timeframe_comb_list[sapply(timeframe_comb_list, nrow)!=0]

    # generate time min/max
    time_min <- min(do.call("c", lapply(timeframe_comb_list, function(x) as.Date(min(x[, 
      get(colname)]), "%Y-%m-%d"))))
    time_max <- max(do.call("c", lapply(timeframe_comb_list, function(x) as.Date(max(x[, 
      get(colname)]), "%Y-%m-%d"))))

    # combine 
    return(list(timeframe_comb_list,time_min, time_max))

  })

  return(split_data)
}

#----------------------------------------------------------------------------#



