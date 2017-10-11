#----------------------------------------------------------------------------#

#' @title Generate vital sign-related features (lvs data).
#'
#' @description \
#'
#' @export
#' @param cohort cohort dt
#' @param cohort_key_var_merge list of key identifiers on which to merge (see: feature_initialisation)
#' @param cohort_key_var list of key identifiers on which to merge (see: feature_initialisation)
#' @param lvs_file_mod_arg list of master files which are to be loaded (see: control.R) 
#' @param leak_lvs_day_arg name of variable specifying the relevant number of leakage days (see control.R)
#' @param file_date_var 
#' @return
#' @examples


lvs_feature_gen <- function(cohort, cohort_key_var_merge,cohort_key_var, lvs_file_mod_arg=lvs_file_mod, 
  leak_lvs_day_arg=leak_lvs_day, file_date_var="lvs_date") {
    
  print("launching lvs_feature_gen")
  
  #-------------------------------------------------------------------------------#
  # SETUP
  #-------------------------------------------------------------------------------#

  # Load the data & subset
  #-------------------------------------------------------------------------------#
    
  # load data
  tryCatch(lvs <- readRDS_merge(lvs_file_mod_arg), error=function(e)
    print("no classified lvs file available for the data sample"))

  # ensure that date variables exist
  if (!(paste0(file_date_var, "_1") %in% names(lvs))) lvs[, 
    c(paste0(file_date_var, "_1")):=get(file_date_var)]

  print(sprintf("number of observations: %d", nrow(lvs)))

  # subset to cohort
  lvs <- lvs[empi %in% cohort$empi]
  
  print(sprintf("number of observations - cohort subset: %d", nrow(lvs)))
  
  ## SAMPLE SIZE CHECK #1
  assert("observations for cohort patients", nrow(lvs)>0)

  # subset to smaller sample (if test_mode==TRUE)
  if (test_mode==TRUE) {
    
    lvs <- lvs[1:min(test_row, nrow(lvs))]

  }

  #-------------------------------------------------------------------------------#
  # MERGING & TIMEFRAMES
  #-------------------------------------------------------------------------------#

  # Date Formatting & Merge data file with cohort & Implement leakage control
  #-------------------------------------------------------------------------------#
  
  # date formatting
  invisible(format_date(list(lvs), c(file_date_var,paste0(file_date_var, "_1"))))

  # foverlaps merge
  lvs[, c(paste0(file_date_var, "_1"),paste0(file_date_var, "_2")):=.(get(file_date_var))]

  lvs <-foverlaps(lvs, cohort[, mget(cohort_key_var_merge)], by.x=c("empi",
    paste0(file_date_var, "_1"), paste0(file_date_var, "_2")), nomatch=0)

  print(sprintf("number of observations - cohort subset within the relevant timeframe(s): %d", nrow(lvs)))

  ## SAMPLE SIZE CHECK #2
  assert("observations - cohort subset within the relevant timeframe(s)", nrow(lvs)>0)

  # time difference calculation (event date vs. t0 date)
  lvs[, time_diff:=as.numeric(difftime(t0_date, get(file_date_var), 
    units="days"))]

  # implement leakage control 
  if (!is.na(leak_lvs_day_arg)) {
    
    lvs <- lvs[!(t0_date-get(paste0(file_date_var, "_1"))<=leak_lvs_day_arg)]

  }
  
  # split data into feature timeframes
  #-------------------------------------------------------------------------------#
  return_mult[lvs_timeframe_comb, time_min, time_max] <- timeframe_split(list("lvs"), file_date_var)[[1]]
  name_ext_extended_tmp                               <- gsub("timeframe_comb", "", names(lvs_timeframe_comb))
  name_ext_tmp                                        <- name_ext_extended_tmp[2:length(name_ext_extended_tmp)]

  ## SAMPLE SIZE CHECK #3
  assert("observations for cohort patients within relevant timeframes", 
    all(sapply(lvs_timeframe_comb, function(x) nrow(x)>0)))

  #-------------------------------------------------------------------------------#
  # FEATURE GENERATION - STATIC
  #-------------------------------------------------------------------------------#

  # [1] Basic features 
  #-------------------------------------------------------------------------------#
  print("feature generation - lvs_lvs.value_lvs.max/min/mean/sd")
  
  lvs_timeframe_comb_value <- lapply(lvs_timeframe_comb, function(x)
    dcast.data.table_check(data=x, 
      formula=as.formula("outcome_id + empi + t0_date  ~ paste0('..', lvs_cat)"), 
      value.var="lvs_value", mode="numeric"))

  inv_mapply(function(DT, name_ext_tmp) setnames_check(DT, old=grep("lvs_value", 
    names(DT), value=T), new=paste0(gsub("_value_", "_lvs.value_lvs.", gsub("_\\.\\.", "\\.\\.", 
    grep("lvs_value", names(DT), value=T))), name_ext_tmp)), DT=lvs_timeframe_comb_value, 
    name_ext_extended_tmp)

  # [2] 'Day to last' features 
  #-------------------------------------------------------------------------------#
  print("feature generation - day-to-last")

  lvs_timeframe_comb_time <- lapply(lvs_timeframe_comb, function(x)
    dcast.data.table_check(data=x, formula=as.formula("outcome_id + empi + t0_date  ~ paste0('..', lvs_cat)"), 
    value.var = "time_diff", mode="basic"))

  lvs_timeframe_comb_time <- feature_var_format(lvs_timeframe_comb_time, drop=TRUE)

  inv_mapply(function(DT, name_ext_tmp) setnames_check(DT, old=grep("day_to_last", 
    names(DT), value=T), new=paste0("lvs_lvs.value_lvs.mean_temp", grep("day_to_last", 
    names(DT), value=T), name_ext_tmp)), DT=lvs_timeframe_comb_time, 
    name_ext_extended_tmp)

  #-------------------------------------------------------------------------------#
  # FEATURE MERGING
  #-------------------------------------------------------------------------------#
  
  print("feature merging")

  lvs_feature_list <- list("lvs_timeframe_comb_time", "lvs_timeframe_comb_value")
  lvs_feature_list <- lvs_feature_list[which(lvs_feature_list %in% ls())]

  # combine features across feature timeframes
  lvs_list_tmp <- timeframe_combine(mget(unlist(lvs_feature_list)), lvs_feature_list)

  # combine features across feature types
  lvs <- Reduce(mymerge, lvs_list_tmp)

  #-------------------------------------------------------------------------------#
  # FEATURE GENERATION - DIFFERENCE ACROSS TIMEFRAMES
  #-------------------------------------------------------------------------------#
  
  if (length(name_ext_tmp)>1) {
  
    print("feature generation - lvs_lvs.value_lvs.mean.diff")

    lvs_list <- c("bps", "bpd", "bp_difference", "pulse", "rr",
      "temperature", "weight", "height")

    lvs <- diff_feature_creation(dt=lvs,dt_feature=lvs_list_tmp[[2]],lvs_list, 
      "lvs","lvs.value", name_ext_tmp=name_ext_tmp)

  }

  #-------------------------------------------------------------------------------#
  # MERGE FEATURES WITH COHORT & FORMAT VARIABLES
  #-------------------------------------------------------------------------------#

  print("cohort merging & feature formatting")

  # merge
  lvs <- lvs[cohort, mget(names(lvs)), on=c("outcome_id", "empi", "t0_date")]
  suppressWarnings(lvs[, grep("lvs_id$", names(lvs), value=T):=NULL])

  # format variables
  lvs <- feature_type_format(dt=lvs, day_to_last=timeframe_day_to_last, 
    num_feature_pattern="...", int_feature_pattern=NA, factor_feature_pattern=NA)

  # rename variables
  setnames_check(lvs, new=gsub("mean_temp", "mean", names(lvs)))

  # add in additional var / drop unnecessary variables
  lvs[, ':='(lvs_time_min=time_min, lvs_time_max=time_max)]
  
  
  #-------------------------------------------------------------------------------#
  # CLEAR MEMORY & RETURN FEATURE SET
  #-------------------------------------------------------------------------------#
  rm(lvs_timeframe_comb)
  rm(list=unlist(lvs_feature_list))

  return(lvs)

}

   
#----------------------------------------------------------------------------#
