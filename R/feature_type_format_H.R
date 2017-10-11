#----------------------------------------------------------------------------#

#' @title Format features based on the variable type.
#'
#' @description \
#'
#' @export
#' @param dt 
#' @param day_to_last 
#' @param num_feature_pattern
#' @param int_feature_pattern
#' @param factor_feature_pattern
#' @return
#' @examples

feature_type_format <- function(dt, num_feature_pattern=NA, int_feature_pattern=NA, 
	factor_feature_pattern=NA, day_to_last) {

	# ----------
	# LOGIC

	## * cohort_key_var_merge           >> No formatting
	## * Date var (time_min, time_max)  >> No formatting

	## Day-to-last var         >> NA/Inf/-Inf -> NA // numeric
	## Numeric var             >> NA/Inf/-Inf -> NA // numeric // round
	## Integer var             >> NA/Inf/-Inf -> 0 // integer 
	## Factor var              >> NA/Inf/-Inf -> 0 // factor 

	# ----------

	# features vs. day-to-last features vs. date features
	# ----------------------------
	day_to_last_var     <- setdiff(grep("day_to_last", names(dt),value=T), c(cohort_key_var_merge))
	date_var             <- grep("_time_min$|_time_max$", names(dt), value=T)
	non_day_to_last_var <- setdiff(names(dt), c(grep("day_to_last", names(dt),value=T), cohort_key_var_merge, date_var))

  	# format day to last features
  	# ----------------------------	
	if (day_to_last==TRUE && length(day_to_last_var)>0) {

		set_na_zero(dt, subset_col=day_to_last_var, replace=NA)
		
		dt[, c(day_to_last_var):=lapply(.SD, function(x) as.numeric(x)), 
			.SDcols=c(day_to_last_var)]

	} else if (day_to_last==FALSE && length(day_to_last_var)>0) {

		dt[, day_to_last_var:=NULL]
	}

  	# format features
  	# ---------------------------	

  	# (!) at most one catch all ("...") feature category
  	assert("at most one catch-all (...) feature category [feature_type_format']", 
  		sum(sapply(c(num_feature_pattern, int_feature_pattern, factor_feature_pattern), 
  		function(x) x=="..."), na.rm=TRUE)<=1)

	# split into categories based on type
	num_feature    <- ifelse((is.na(num_feature_pattern) | num_feature_pattern=="..."), NA, grep(num_feature_pattern, non_day_to_last_var, value=T))
	int_feature    <- ifelse((is.na(int_feature_pattern) | int_feature_pattern=="..."), NA, grep(int_feature_pattern, non_day_to_last_var, value=T))
	factor_feature <- ifelse((is.na(factor_feature_pattern) | factor_feature_pattern=="..."), NA, grep(factor_feature_pattern, non_day_to_last_var, value=T))
  
  	# deal with catch all categories ("...")
	if (!(is.na(num_feature_pattern)) & num_feature_pattern=="...")       num_feature    <- setdiff(non_day_to_last_var, c(int_feature,factor_feature))
	if (!(is.na(int_feature_pattern)) & int_feature_pattern=="...")       int_feature    <- setdiff(non_day_to_last_var, c(num_feature,factor_feature))
	if (!(is.na(factor_feature_pattern)) & factor_feature_pattern=="...") factor_feature <- setdiff(non_day_to_last_var, c(int_feature,num_feature))

	# [1] format numeric features
	if (length(num_feature)>0 && !is.na(num_feature[1])) {

		set_na_zero(dt, subset_col=num_feature, replace=NA)
		
		dt[, c(num_feature):=lapply(.SD, function(x) as.numeric(x)), .SDcols=c(num_feature)]
  		
  		dt[, c(num_feature):=lapply(.SD, function(x) round(x, digits=2)), .SDcols=c(num_feature)]

	}

	# [2] format integer features
	if (length(int_feature)>0 && !is.na(int_feature[1])) {

		set_na_zero(dt, subset_col=int_feature, replace=0)
		
		dt[, c(int_feature):=lapply(.SD, function(x) as.integer(x)), .SDcols=c(int_feature)]
  		
	}

	# [3] format factor features
	if (length(factor_feature)>0 && !is.na(factor_feature[1])) {

		set_na_zero(dt, subset_col=factor_feature, replace=NA)
		
		dt[, c(factor_feature):=lapply(.SD, function(x) as.factor(x)), .SDcols=c(factor_feature)]

	}

  	# return
  	# ----------------------------

	return(dt)

}

#----------------------------------------------------------------------------#


