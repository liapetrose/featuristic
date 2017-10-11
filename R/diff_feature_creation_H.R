#----------------------------------------------------------------------------#

#' @title Collapse features into a category-level tabulation. 
#'
#' @description \
#'
#' @export
#' @param dt 
#' @param dt_feature 
#' @param feature_list 
#' @param feature_name 
#' @return
#' @examples

diff_feature_creation <- function(dt, dt_feature, feature_list, feature_name, feature_name_ext, name_ext_tmp) { 

    # differences between timeframes (e.g. day-short, short-end)
    suppressWarnings(inv_mapply(function(name_ext_1, name_ext_2) {

        dt[, paste0(feature_name,"_",feature_name_ext, feature_name,".mean.diff..", feature_list, 
          name_ext_1, name_ext_2, "_diff"):=lapply(feature_list, function(x) {

          if (length(grep(paste0("mean..", x, name_ext_1), names(dt), value=T))>0 & length(grep(paste0("mean..", x, name_ext_2), 
            names(dt), value=T))>0) {
        
              get(grep(paste0("mean..", x, name_ext_1), 
              names(dt_feature), value=T)) - get(grep(paste0("mean..",x, name_ext_2), 
              names(dt_feature), value=T))

          } else {
            NULL
          }

        })]}, name_ext_1=name_ext_tmp[1:(length(name_ext_tmp)-1)], name_ext_2=name_ext_tmp[2:length(name_ext_tmp)]))
    

    # max difference between timeframes (e.g. day-max)

      suppressWarnings(dt[, paste0(feature_name,"_",feature_name_ext, feature_name,".mean.diff..", feature_list, "_max", 
        "_diff"):=lapply(feature_list, function(x) {

          if (length(grep(paste0("mean..", x, name_ext_tmp[1]), names(dt), value=T))>0 & length(grep(paste0("mean..", x, name_ext_tmp[length(name_ext_tmp)]), 
            names(dt), value=T))>0) {

              get(grep(paste0("mean..", x, name_ext_tmp[1]), names(dt_feature), value=T)) - get(grep(paste0("mean..",x, name_ext_tmp[length(name_ext_tmp)]), 
              names(dt_feature), value=T))

          } else {
            NULL
          }

      })])

     return(dt)

}

#----------------------------------------------------------------------------#
