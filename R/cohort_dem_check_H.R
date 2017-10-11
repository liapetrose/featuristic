#----------------------------------------------------------------------------#

#' @title Check whether all cohort observations are associated with a demographic record. 
#'
#' @description \
#'
#' @export
#' @return
#' @examples

cohort_dem_check <- function() {

	# load the dem file
  	dem <- readRDS_merge(dem_file_mod)

	# check the cohort against the dem file 
  	no_dem <- nrow(cohort[!empi %in% dem$empi])

  	assert("All cohort observations are associated with a record in the dem file", 
  		no_dem==0)

}


#----------------------------------------------------------------------------#
