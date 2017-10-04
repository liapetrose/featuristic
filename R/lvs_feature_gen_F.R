#----------------------------------------------------------------------------#

#' @title Generate vital sign-related features (lvs data).
#'
#' @description \
#'
#' @export
#' @param cohort
#' @param cohort_key_var_merge
#' @param lvs_file_mod_arg
#' @param leak_lvs_day_arg
#' @param combine
#' @param lvs_file_mod_ext
#' @param lvs_file_mod_ext_ext
#' @param load
#' @param file_date_var
#' @return
#' @examples


lvs_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var, lvs_file_mod_arg=lvs_file_mod, 
  leak_lvs_day_arg=leak_lvs_day, combine=FALSE, 
  lvs_file_mod_ext=NA, lvs_file_mod_ext_ext=NA, load=TRUE, file_date_var="lvs_date") {
    
  print("launching lvs_feature_gen")
  
  #-------------------------------------------------------------------------------#
  # Load the  modified/pre-processed lvs file for the specified data sample -- 
    
  # load data
  tryCatch(lvs <- readRDS_merge(lvs_file_mod_arg), warning=function(w)
    print("no classified lvs file available for the data sample"))

  if (combine==TRUE) {

    if (load==TRUE) {
      tryCatch(lvs_ext <- readRDS_merge(lvs_file_mod_ext), warning=function(w)
        print("no classified lvs file available for the data sample"))
    } else {
      lvs_ext <- lvs_file_mod_ext

    }
    
    lvs <- rbindlist(list(lvs, lvs_ext), fill=T, use.names=T)
    lvs[, lvs_id:=1:nrow(lvs)]
 
  }

  # subset to smaller sample for testing if so specified in control file
  if (test_raw_file==TRUE) {
    store_shorten_file("lvs")
  }

  #-------------------------------------------------------------------------------#
  # merge vital signs file with cohort (cohort_key_variables) & format dates
  lvs <- lvs[empi %in% cohort$empi]

  invisible(parse_date(lvs, c("lvs_date")))

  lvs[, c("lvs_date_1","lvs_date_2"):=.(lvs_date)]

  lvs <-foverlaps(lvs, cohort[, mget(cohort_key_var_merge)], by.x=c("empi",
    "lvs_date_1", "lvs_date_2"), nomatch=0)

  lvs[, time_diff:=as.numeric(difftime(t0_date, get(file_date_var), 
    units="days"))]

  # implement leakage control 
  if (!is.na(leak_lvs_day_arg)) {
    lvs <- lvs[!(t0_date-lvs_date_1<=leak_lvs_day_arg)]
  }
  
  #-------------------------------------------------------------------------------#
  # sub setting & dividing into smaller DT based on timeframe - 
  # return as list (...timeframe_comb)
  invisible(timeframe_split(list("lvs"), "lvs_date"))

  name_ext_extended <- name_ext_extended[sapply(lvs_timeframe_comb, nrow)!=0]
  name_ext <- name_ext_extended[2:length(name_ext_extended)]
  lvs_timeframe_comb <- lvs_timeframe_comb[sapply(lvs_timeframe_comb, nrow)!=0]

  time_min <- min(do.call("c", lapply(lvs_timeframe_comb, function(x) as.Date(min(x[, 
    lvs_date]), "%Y-%m-%d"))))
  time_max <- max(do.call("c", lapply(lvs_timeframe_comb, function(x) as.Date(max(x[, 
    lvs_date]), "%Y-%m-%d"))))


  #-------------------------------------------------------------------------------#
  # reshaping & generating features - mean, sd, max, min of numeric lvs  impose 
  # feature  categorization ("lvs_value_max/min.."..."_short/_long")
  lvs_timeframe_comb_value <- lapply(lvs_timeframe_comb, function(x)
    dcast.data.table(x, outcome_id + empi + t0_date  ~ 
    paste0("..", lvs_cat),list(min, max, mean, sd), value.var="lvs_value"))

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("lvs_value", 
    names(DT), value=T), paste0(gsub("_value_", "_lvs.value_lvs.", gsub("_\\.\\.", "\\.\\.", 
    grep("lvs_value", names(DT), value=T))), name_ext)), DT=lvs_timeframe_comb_value, 
    name_ext_extended))

  lvs_timeframe_comb_time <- lapply(lvs_timeframe_comb, function(x)
    dcast.data.table(x, outcome_id + empi + t0_date  ~ 
    paste0("..", lvs_cat),fun.aggregate=list(length, function(x) min(x, na.rm=T)), 
    value.var = "time_diff"))

  lvs_timeframe_comb_time <- feature_var_format(lvs_timeframe_comb_time)
  invisible(lapply(lvs_timeframe_comb_time, function(x) x[, c(setdiff(setdiff(names(x), 
    grep("days_to_last",names(x), value=T )), cohort_key_var_merge)):=NULL]))

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("days_to_last", 
    names(DT), value=T), paste0("lvs_lvs.value_lvs.mean_temp", grep("days_to_last", 
    names(DT), value=T), name_ext)), DT=lvs_timeframe_comb_time, 
    name_ext_extended))

  #-------------------------------------------------------------------------------#
  # merge lvs feature files
  lvs_feature_list <- list("lvs_timeframe_comb_time", "lvs_timeframe_comb_value")

  timeframe_combine(lvs_feature_list)

  lvs <- Reduce(mymerge, mget(unlist(lvs_feature_list)))


  #-------------------------------------------------------------------------------#
  # generate time.diff features - difference in means between each timeframe &
  # between shortest and longest timeframe & 
  # impose feature hierarchy ("lvs_value_mean.diff.."..."_diff")
  lvs_list <- c("bps", "bpd", "bp_difference", "pulse","rr",
    "temperature", "weight", "height")
  
  if (length(name_ext)>1) {
    mapply(function(name_ext_1, name_ext_2) lvs[, paste0("lvs_lvs.value_lvs.mean.diff..", lvs_list, 
      name_ext_1, name_ext_2, "_diff"):=lapply(lvs_list, function(x) if (length(grep(paste0("mean..", x, name_ext_1), 
      names(lvs), value=T))>0 & length(grep(paste0("mean..", x, name_ext_2), 
      names(lvs), value=T))>0) {get(grep(paste0("mean..", x, name_ext_1), 
      names(lvs_timeframe_comb_value), value=T)) - get(grep(paste0("mean..",x, name_ext_2), 
      names(lvs_timeframe_comb_value), value=T))})], name_ext_1=name_ext[1:(length(name_ext)-1)],
      name_ext_2=name_ext[2:length(name_ext)])
  }

  lvs[, paste0("lvs_lvs.value_lvs.mean.diff..", lvs_list, "_max", 
    "_diff"):=lapply(lvs_list, function(x) if (length(grep(paste0("mean..", x, name_ext[1]), 
    names(lvs), value=T))>0 & length(grep(paste0("mean..", x, name_ext[length(name_ext)]), 
    names(lvs), value=T))>0) {get(grep(paste0("mean..", x, name_ext[1]), 
    names(lvs_timeframe_comb_value), value=T)) - get(grep(paste0("mean..",x, name_ext[length(name_ext)]), 
    names(lvs_timeframe_comb_value), value=T))})]
  
  #-------------------------------------------------------------------------------#
  # merge with cohort file - empty records -> NA & round to two decimals
  lvs <- lvs[cohort, mget(names(lvs)), on=c("outcome_id", "empi", "t0_date")]
  
  non_days_to_last_var <- setdiff(names(lvs),grep("days_to_last", names(lvs),value=T))
  set_na_zero(lvs, replace=NA, subset_col=non_days_to_last_var)

  lvs[, grep("lvs_lvs.value", names(lvs), value=T):=lapply(.SD, function(x) 
    round(x, digits=2)), .SDcols=grep("lvs_lvs.value", names(lvs))]
  setnames(lvs, gsub("mean_temp", "mean", names(lvs)))
  
  #-------------------------------------------------------------------------------#
  # categorize variables to ensure proper treatment in models -- integer 
  lvs_numeric <- lvs[, mget(setdiff(names(lvs), c("outcome_id", "t0_date", "empi")))]
  lvs_numeric[, names(lvs_numeric):=lapply(.SD, function(x) as.numeric(x))]

  lvs <- cbind(lvs[, mget(c("outcome_id", "t0_date", "empi"))], lvs_numeric)

  lvs[, ':='(lvs_time_min=time_min, lvs_time_max=time_max)]
  
  lvs[, grep("lvs_id$", names(lvs), value=T):=NULL]

  ## deal with date variables
  feature_var_format_2(lvs)

  #-------------------------------------------------------------------------------#
  # return lvs & delete key files 
  rm(lvs_numeric)
  rm(lvs_timeframe_comb)
 
  return (lvs)

}

   
#----------------------------------------------------------------------------#
