#----------------------------------------------------------------------------#

#' @title Generate lab-related features (lab data).
#'
#' @description \
#'
#' @export
#' @param cohort
#' @param cohort_key_var_merge
#' @param cohort_key_var
#' @param lab_file_mod_arg
#' @param leak_lab_day_arg
#' @param combine
#' @param lab_file_mod_ext
#' @param file_date_var
#' @return
#' @examples


lab_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var, lab_file_mod_arg=lab_file_mod, 
  leak_lab_day_arg=leak_lab_day, combine=FALSE, lab_file_mod_ext=NA, file_date_var="lab_date") {

  print("launching lab_feature_gen")
  
  #-------------------------------------------------------------------------------#
  # SETUP
  #-------------------------------------------------------------------------------#

  # Load the data & subset
  #-------------------------------------------------------------------------------#
    
  # load data
  tryCatch(lab <- readRDS_merge(lab_file_mod_arg), error=function(e)
    print("no classified lab file available for the data sample"))

  # ensure that date variables exist
  if (!(paste0(file_date_var, "_1") %in% names(lab))) lab[, 
    c(paste0(file_date_var, "_1")):=get(file_date_var)]

  # combine 
  if (combine==TRUE) {
    
    tryCatch(lab_ext <- readRDS_merge(lab_file_mod_ext), error=function(e)
      print("no classified lab file available for the data sample"))
    if (!(paste0(file_date_var, "_1") %in% names(lab_ext))) lab_ext[, 
      c(paste0(file_date_var, "_1")):=get(file_date_var)]
 
    # ensure that dates are standarised & aligned
    invisible(format_date(list(lab, lab_ext), c(file_date_var,paste0(file_date_var, "_1"))))
    
    # merge
    lab <- rbindlist(list(lab_ext, lab), fill=T, use.names=T)
    
    lab[, lab_id:=1:nrow(lab)]
 
  }

  print(sprintf("number of observations: %d", nrow(lab)))

  # subset to cohort
  lab <- lab[!is.na(empi)]
  lab <- lab[empi %in% cohort$empi]

  print(sprintf("number of observations - cohort subset: %d", nrow(lab)))
  
  ## SAMPLE SIZE CHECK #1
  assert("observations for cohort patients", nrow(lab)>0)

  # subset to smaller sample (if test_mode==TRUE)
  if (test_mode==TRUE) {
    
    lab <- lab[1:min(test_row, nrow(lab))]

  }

  #-------------------------------------------------------------------------------#
  # ADDITIONAL SUBSETTING / FORMATTING
  #-------------------------------------------------------------------------------#

  # select only valid results 
  lab <- lab[lab_result_type %in% c("num", "cat")]

  # drop irrelevant variables
  lab[, c("lab_time"):=NULL]

  print(sprintf("number of observations - cohort subset and result subset: %d", nrow(lab)))

  #-------------------------------------------------------------------------------#
  # MERGING & TIMEFRAMES
  #-------------------------------------------------------------------------------#

  # Date Formatting & Merge data file with cohort & Implement leakage control
  #-------------------------------------------------------------------------------#
 
  # date formatting
  invisible(format_date(list(lab), c(file_date_var,paste0(file_date_var, "_1"))))

  # foverlaps merge
  lab[, c(paste0(file_date_var, "_1"),paste0(file_date_var, "_2")):=.(get(file_date_var))]

  lab <-  foverlaps(lab, cohort[, mget(cohort_key_var_merge)], by.x=c("empi",
    paste0(file_date_var, "_1"), paste0(file_date_var, "_2")), nomatch=0)

  print(sprintf("number of observations - cohort subset within the relevant timeframe(s): %d", nrow(lab)))

  ## SAMPLE SIZE CHECK #2
  assert("observations - cohort subset within the relevant timeframe(s)", nrow(lab)>0)

  # time difference calculation (event date vs. t0 date)
  lab[, time_diff:=as.numeric(difftime(t0_date, get(file_date_var), 
    units="days"))]

  # implement leakage control 
  if (!is.na(leak_lab_day_arg)) {
    
    lab <- lab[!(t0_date-get(paste0(file_date_var, "_1"))<=leak_lab_day_arg)]

  }

  # split data into feature timeframes
  #-------------------------------------------------------------------------------#
  return_mult[lab_timeframe_comb, time_min, time_max] <- timeframe_split(list("lab"), file_date_var)[[1]]
  name_ext_extended_tmp                               <- gsub("timeframe_comb", "", names(lab_timeframe_comb))
  name_ext_tmp                                        <- name_ext_extended_tmp[2:length(name_ext_extended_tmp)]

  ## SAMPLE SIZE CHECK #3
  assert("observations for cohort patients within relevant timeframes", 
    all(sapply(lab_timeframe_comb, function(x) nrow(x)>0)))

  #-------------------------------------------------------------------------------#
  # FEATURE GENERATION
  #-------------------------------------------------------------------------------#

  # [1] Numeric Lab Features
  #-------------------------------------------------------------------------------#
  print("feature generation - lab_lab.numeric_lab.mean_temp")

  lab_num_timeframe_comb <- lapply(lab_timeframe_comb, function(x)
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date  ~ paste0('..', lab_cat)"),
    value.var="lab_value", subset=equal_to("lab_result_type", "num"), 
    mode="numeric"))

  inv_mapply(function(DT, name_ext_tmp) setnames_check(DT, old=grep("lab_value", 
    names(DT), value=T), new=paste0(gsub("_value_", "_lab.numeric_lab.", 
    gsub("_\\.\\.", "\\.\\.", grep("lab_value", 
    names(DT), value=T))), name_ext_tmp)), DT=lab_num_timeframe_comb, 
    name_ext_extended_tmp)

  lab_num_timeframe_comb_time <- lapply(lab_timeframe_comb, function(x)
    dcast.data.table_check(data=x, 
    formula=as.formula(outcome_id + empi + t0_date  ~ paste0('..', lab_cat)),
    value.var = "time_diff", subset=equal_to("lab_result_type", "num"), 
    mode="basic"))

  lab_num_timeframe_comb_time  <- feature_var_format(lab_num_timeframe_comb_time, 
    drop=TRUE)

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("day_to_last", 
    names(DT), value=T), new=paste0("lab_lab.numeric_lab.mean_temp", 
    grep("day_to_last", names(DT), value=T), name_ext_tmp)), 
    DT=lab_num_timeframe_comb_time, name_ext_extended_tmp)

  # [2] Categorical Lab Features
  #-------------------------------------------------------------------------------#
  print("feature generation - lab_lab.non.numeric")

  lab_cat_timeframe_comb <- lapply(lab_timeframe_comb, function(x)
    dcast.data.table_check(data=x, 
    formula=as.formula(outcome_id + empi + t0_date  ~ paste0('..', lab_cat)), 
    value.var=grep("indic", names(x), value=T),
    subset=equal_to("lab_result_type", "cat"), mode="sum"))

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("indic", 
    names(DT), value=T), new=gsub("_function_\\.\\.", "..", paste0("lab_lab.non.numeric_lab.",
    gsub("indic\\.", "", grep("indic", names(DT), value=T)),name_ext_tmp))), 
    DT=lab_cat_timeframe_comb, name_ext_extended_tmp)

  lab_cat_timeframe_comb_time <- lapply(lab_timeframe_comb, function(x)
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date  ~ paste0('..', lab_cat)"),
    value.var = "time_diff", subset=equal_to("lab_result_type", "cat"), 
    mode="basic"))

  lab_cat_timeframe_comb_time <- feature_var_format(lab_cat_timeframe_comb_time, 
    drop=TRUE)
 
  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("day_to_last", 
    names(DT), value=T), new=paste0("lab_lab.non.numeric", 
    grep("day_to_last", names(DT), value=T), name_ext_tmp)), 
    DT=lab_cat_timeframe_comb_time, name_ext_extended_tmp)

  # drop non-sensible variables
  non_sensible_cat_feature <- "lab.non.numeric_lab.wbc|lab.non.numeric_lab.rbc|lab.non.numeric_lab.uric.acid|lab.non.numeric_lab.yeast"

  suppressWarnings(inv_lapply(lab_cat_timeframe_comb, function(x) x[, 
    c(grep(non_sensible_cat_feature, names(x), value=T)):=NULL]))
  suppressWarnings(inv_lapply(lab_cat_timeframe_comb_time, function(x) x[, 
    c(grep(non_sensible_cat_feature, names(x), value=T)):=NULL]))

  #-------------------------------------------------------------------------------#
  # FEATURE MERGING
  #-------------------------------------------------------------------------------#

  print("feature merging")

  lab_feature_list <- list("lab_cat_timeframe_comb", "lab_num_timeframe_comb", 
    "lab_num_timeframe_comb_time", "lab_cat_timeframe_comb_time")
  lab_feature_list <- lab_feature_list[which(lab_feature_list %in% ls())]

  # combine features across feature timeframes
  lab_list_tmp <- timeframe_combine(mget(unlist(lab_feature_list)), lab_feature_list)

  # combine features across feature types
  lab <- Reduce(mymerge, lab_list_tmp)


  #-------------------------------------------------------------------------------#
  # FEATURE GENERATION - DIFFERENCE ACROSS TIMEFRAMES
  #-------------------------------------------------------------------------------#
  
  if (length(name_ext_tmp)>1) {
  
    print("feature generation - lab_lab.numeric_lab.mean.diff")

    lab_list <- unique(gsub(paste0(name_ext_extended_tmp, collapse="|"), "", gsub(".*\\.\\.", "", 
      names(lab_list_tmp[[2]][, mget(grep("lab.mean", names(lab_list_tmp[[2]]), value=T))]))))

    lab <- diff_feature_creation(dt=lab,dt_feature=lab_list_tmp[[2]],lab_list, "lab", "lab.numeric",
      name_ext_tmp=name_ext_tmp)

  }

  #-------------------------------------------------------------------------------#
  # MERGE FEATURES WITH COHORT & FORMAT VARIABLES
  #-------------------------------------------------------------------------------#

  print("cohort merging & feature formatting")

  # merge
  lab <- lab[cohort, mget(names(lab)), on=c("outcome_id", "empi", "t0_date")]
  suppressWarnings(lab[, grep("lab_id$", names(lab), value=T):=NULL])

  # format variables
  lab <- feature_type_format(dt=lab, day_to_last=timeframe_day_to_last, 
    num_feature_pattern="lab_lab.numeric", int_feature_pattern="lab.non.numeric", 
    factor_feature_pattern=NA)

  # rename variables
  setnames_check(lab, new=gsub("mean_temp", "mean", names(lab)))
 
  # add in additional var / drop unnecessary variables
  lab[, ':='(lab_time_min=time_min, lab_time_max=time_max)]

  #-------------------------------------------------------------------------------#
  # CLEAR MEMORY & RETURN FEATURE SET
  #-------------------------------------------------------------------------------#
  rm(lab_timeframe_comb)
  rm(list=unlist(lab_feature_list))

  return(lab)

}

#-------------------------------------------------------------------------------#







