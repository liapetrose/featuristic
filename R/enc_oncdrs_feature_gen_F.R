#----------------------------------------------------------------------------#

#' @title Generate DFCI enc-related features (enc_oncdrs data).
#'
#' @description \
#'
#' @export
#' @param cohort
#' @param cohort_key_var_merge
#' @param cohort_key_var
#' @param file_date_var
#' @param leak_enc_oncdrs_day_arg
#' @param enc_oncdrs_file_mod_arg
#' @return
#' @examples

 enc_oncdrs_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var, 
  file_date_var="adm_date", leak_enc_oncdrs_day_arg=leak_enc_day, 
  enc_oncdrs_file_mod_arg=enc_oncdrs_file_mod) {
  
  print("launching enc_oncdrs_feature_gen")
  
 
  #-------------------------------------------------------------------------------#
  # SETUP
  #-------------------------------------------------------------------------------#

  # Load the data & subset
  #-------------------------------------------------------------------------------#
    
  # load data
  tryCatch(enc_oncdrs <- readRDS_merge(enc_oncdrs_file_mod_arg), error=function(e)
    print("no classified enc_oncdrs file available for the data sample"))

  # ensure that date variables exist
  if (!(paste0(file_date_var, "_1") %in% names(enc_oncdrs))) enc_oncdrs[, 
    c(paste0(file_date_var, "_1")):=get(file_date_var)]

  print(sprintf("number of observations: %d", nrow(enc_oncdrs)))

  # subset to cohort
  enc_oncdrs <- enc_oncdrs[!is.na(empi)]
  enc_oncdrs <- enc_oncdrs[empi %in% cohort$empi]

  print(sprintf("number of observations - cohort subset: %d", nrow(enc_oncdrs)))
  
  ## SAMPLE SIZE CHECK #1
  assert("observations for cohort patients", nrow(enc_oncdrs)>0)

  # subset to smaller sample (if test_mode==TRUE)
  if (test_mode==TRUE) {
    
    enc_oncdrs <- enc_oncdrs[1:min(test_row, nrow(enc_oncdrs))]

  }

  #-------------------------------------------------------------------------------#
  # ADDITIONAL SUBSETTING / FORMATTING
  #-------------------------------------------------------------------------------#

  # drop irrelevant columns
  suppressWarnings(enc_oncdrs <- enc_oncdrs[, setdiff(names(enc_oncdrs), 
    c(grep("enc_type|enc_dept",names(enc_oncdrs), value=T), "empi", "patient_id", 
    "adm_date", "adm_date_1")):=NULL])

  #-------------------------------------------------------------------------------#
  # MERGING & TIMEFRAMES
  #-------------------------------------------------------------------------------#

  # Date Formatting & Merge data file with cohort & Implement leakage control
  #-------------------------------------------------------------------------------#
 
  # date formatting
  invisible(format_date(list(enc_oncdrs), c(file_date_var,paste0(file_date_var, "_1"))))

  # foverlaps merge
  enc_oncdrs[, c(paste0(file_date_var, "_1"),paste0(file_date_var, "_2")):=.(get(file_date_var))]

  enc_oncdrs <-  foverlaps(enc_oncdrs, cohort[, mget(cohort_key_var_merge)], by.x=c("empi",
    paste0(file_date_var, "_1"), paste0(file_date_var, "_2")), nomatch=0)

  print(sprintf("number of observations - cohort subset within the relevant timeframe(s): %d", nrow(enc_oncdrs)))

  ## SAMPLE SIZE CHECK #2
  assert("observations - cohort subset within the relevant timeframe(s)", nrow(enc_oncdrs)>0)

  # time difference calculation (event date vs. t0 date)
  enc_oncdrs[, time_diff:=as.numeric(difftime(t0_date, get(file_date_var), 
    units="days"))]

  # implement leakage control 
  if (!is.na(leak_enc_oncdrs_day_arg)) {
    
    enc_oncdrs <- enc_oncdrs[!(t0_date-get(paste0(file_date_var, "_1"))<=leak_enc_oncdrs_day_arg)]

  }

  # split data into feature timeframes
  #-------------------------------------------------------------------------------#
  return_mult[enc_oncdrs_timeframe_comb, time_min, time_max] <- timeframe_split(list("enc_oncdrs"), file_date_var)[[1]]
  name_ext_extended_tmp                                      <- gsub("timeframe_comb", "", names(enc_oncdrs_timeframe_comb))
  name_ext_tmp                                               <- name_ext_extended_tmp[2:length(name_ext_extended_tmp)]

  ## SAMPLE SIZE CHECK #3
  assert("observations for cohort patients within relevant timeframes", 
    all(sapply(enc_oncdrs_timeframe_comb, function(x) nrow(x)>0)))

  #-------------------------------------------------------------------------------#
  # FEATURE GENERATION
  #-------------------------------------------------------------------------------#

  # [1] Snomed Features 
  #-------------------------------------------------------------------------------#
  print("feature generation - med_med.count_med.snomed_med.")

  enc_type_timeframe_comb <- lapply(enc_oncdrs_timeframe_comb, function(x) 
    dcast.data.table_check(data=x, 
      formula=as.formula("outcome_id + empi + t0_date ~  paste0('enc.dfci_enc.enc_enc.count_enc.type.count..', enc_type)"), 
      mode="length",subset=non_missing("enc_type"), value.var="time_diff"))

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("enc.dfci_enc.enc_enc.count_enc.type.count", 
    names(DT), value=T), new=gsub("\\*", "",paste0(grep("enc.dfci_enc.enc_enc.count_enc.type.count", 
    names(DT), value=T),name_ext_tmp))), DT=enc_type_timeframe_comb ,  name_ext_extended_tmp)

  # [1] Snomed Features 
  #-------------------------------------------------------------------------------#
  print("feature generation - med_med.count_med.snomed_med.")

  enc_dept_timeframe_comb <- lapply(enc_oncdrs_timeframe_comb, function(x) 
    dcast.data.table_check(data=x, 
      formula=as.formula("outcome_id + empi + t0_date ~  paste0('enc.dfci_enc.enc_enc.count_enc.department.count..', enc_dept)"), 
      mode="length", subset=non_missing("enc_dept"), value.var="time_diff"))

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, 
    old=grep("enc.dfci_enc.enc_enc.count_enc.department.count", 
    names(DT), value=T), new=paste0(grep("enc.dfci_enc.enc_enc.count_enc.department.count", 
    names(DT), value=T),name_ext_tmp)), DT=enc_dept_timeframe_comb ,  name_ext_extended_tmp)

  #-------------------------------------------------------------------------------#
  # FEATURE MERGING
  #-------------------------------------------------------------------------------#

  print("feature merging")

  enc_oncdrs_feature_list <- list("enc_type_timeframe_comb", "enc_dept_timeframe_comb")
  enc_oncdrs_feature_list <- enc_oncdrs_feature_list[which(enc_oncdrs_feature_list %in% ls())]

  # combine features across feature timeframes
  enc_oncdrs_list_tmp <- timeframe_combine(mget(unlist(enc_oncdrs_feature_list)), enc_oncdrs_feature_list)

  # combine features across feature types
  enc_oncdrs <- Reduce(mymerge, enc_oncdrs_list_tmp)

  #-------------------------------------------------------------------------------#
  # MERGE FEATURES WITH COHORT & FORMAT VARIABLES
  #-------------------------------------------------------------------------------#

  print("cohort merging & feature formatting")

  # merge
  enc_oncdrs <- enc_oncdrs[cohort, mget(names(enc_oncdrs)), on=c("outcome_id", "empi", "t0_date")]
  suppressWarnings(enc_oncdrs[, grep("enc_id$", names(enc_oncdrs), value=T):=NULL])

  # format variables
  enc_oncdrs <- feature_type_format(dt=enc_oncdrs, day_to_last=timeframe_day_to_last, 
    num_feature_pattern=NA, int_feature_pattern="...", factor_feature_pattern=NA)
  
  # rename
  var_rename <- setdiff(names(enc_oncdrs), cohort_key_var_merge)
  setnames_check(enc_oncdrs,  var_rename , 
    new=paste0(gsub("_", "_dfci.", gsub("(.*)(\\.\\.)(.*)", "\\1", 
    var_rename)), "..", gsub("(.*)(\\.\\.)(.*)", "\\3",var_rename)))

  # add in additional var / drop unnecessary variables
  enc_oncdrs[, ':='(enc_oncdrs_time_min=time_min, enc_oncdrs_time_max=time_max)]

  #-------------------------------------------------------------------------------#
  # CLEAR MEMORY & RETURN FEATURE SET
  #-------------------------------------------------------------------------------#
  rm(enc_oncdrs_timeframe_comb)
  rm(list=unlist(enc_oncdrs_feature_list))

  return(enc_oncdrs)

}


#----------------------------------------------------------------------------#




