#----------------------------------------------------------------------------#

#' @title Generate medication-related features (med data).
#'
#' @description \
#'
#' @export
#' @param cohort
#' @param cohort_key_var_merge
#' @param cohort_key_var
#' @param med_file_mod_arg
#' @param leak_med_day_arg
#' @param combine
#' @param med_file_mod_ext
#' @param med_file_mod_ext_ext
#' @param file_date_var
#' @return
#' @examples

med_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var, med_file_mod_arg=med_file_mod, 
  leak_med_day_arg=leak_med_day, combine=FALSE,  med_file_mod_ext=NA, med_file_mod_ext_ext=NA, 
  file_date_var="med_date") {

  print("launching med_feature_gen")
  
  #-------------------------------------------------------------------------------#
  # SETUP
  #-------------------------------------------------------------------------------#

  # Load the data & subset
  #-------------------------------------------------------------------------------#
    
  # load data
  tryCatch(med <- readRDS_merge(med_file_mod_arg), error=function(e)
    print("no classified med file available for the data sample"))

  # ensure that date variables exist
  if (!(paste0(file_date_var, "_1") %in% names(med))) med[, 
    c(paste0(file_date_var, "_1")):=get(file_date_var)]

  # combine 
  if (combine==TRUE) {
    
    tryCatch(med_ext <- readRDS_merge(med_file_mod_ext), error=function(e)
      print("no classified med file available for the data sample"))
    if (!(paste0(file_date_var, "_1") %in% names(med_ext))) med_ext[, 
      c(paste0(file_date_var, "_1")):=get(file_date_var)]

    tryCatch(med_ext_ext <- readRDS_merge(med_file_mod_ext_ext), error=function(e)
      print("no classified med file available for the data sample"))
    if (!(paste0(file_date_var, "_1") %in% names(med_ext_ext))) med_ext_ext[, 
      c(paste0(file_date_var, "_1")):=get(file_date_var)]

    # ensure that dates are standarised & aligned
    invisible(format_date(list(med, med_ext, med_ext_ext), c(file_date_var, paste0(file_date_var, "_1"))))

    med_ext_ext[, activation_dt:=NULL]
    med_ext_ext[, indication_dt:=NULL]
    
    # merge
    med <- rbindlist(list(med_ext, med), fill=T, use.names=T)
    med <- rbindlist(list(med, med_ext_ext), fill=T, use.names=T)
    
    med[, med_id:=1:nrow(med)]
 
  }

  print(sprintf("number of observations: %d", nrow(med)))

  # subset to cohort
  med <- med[!is.na(empi)]
  med <- med[empi %in% cohort$empi]

  print(sprintf("number of observations - cohort subset: %d", nrow(med)))
  
  ## SAMPLE SIZE CHECK #1
  assert("observations for cohort patients", nrow(med)>0)

  # subset to smaller sample (if test_mode==TRUE)
  if (test_mode==TRUE) {
    
    med <- med[1:min(test_row, nrow(med))]

  }

  #-------------------------------------------------------------------------------#
  # ADDITIONAL SUBSETTING / FORMATTING
  #-------------------------------------------------------------------------------#

  # if no med_form > assume administered
  if(!("med_form" %in% names(med))) med[,med_form:=""]
  med[is.na(med_form)| med_form=="", med_form:="adm"]
   
  # if no chemo cat nci > ""
  if(!("chemo_cat_nci" %in% names(med))) med[,chemo_cat_nci:=""]

  #-------------------------------------------------------------------------------#
  # MERGING & TIMEFRAMES
  #-------------------------------------------------------------------------------#

  # Date Formatting & Merge data file with cohort & Implement leakage control
  #-------------------------------------------------------------------------------#
 
  # date formatting
  invisible(format_date(list(med), c(file_date_var,paste0(file_date_var, "_1"))))

  # foverlaps merge
  med[, c(paste0(file_date_var, "_1"),paste0(file_date_var, "_2")):=.(get(file_date_var))]

  med <-  foverlaps(med, cohort[, mget(cohort_key_var_merge)], by.x=c("empi",
    paste0(file_date_var, "_1"), paste0(file_date_var, "_2")), nomatch=0)

  print(sprintf("number of observations - cohort subset within the relevant timeframe(s): %d", nrow(med)))

  ## SAMPLE SIZE CHECK #2
  assert("observations - cohort subset within the relevant timeframe(s)", nrow(med)>0)

  # time difference calculation (event date vs. t0 date)
  med[, time_diff:=as.numeric(difftime(t0_date, get(file_date_var), 
    units="days"))]

  # implement leakage control 
  if (!is.na(leak_med_day_arg)) {
    
    med <- med[!(t0_date-get(paste0(file_date_var, "_1"))<=leak_med_day_arg)]

  }

  # split data into feature timeframes
  #-------------------------------------------------------------------------------#
  return_mult[med_timeframe_comb, time_min, time_max] <- timeframe_split(list("med"), file_date_var)[[1]]
  name_ext_extended_tmp                               <- gsub("timeframe_comb", "", names(med_timeframe_comb))
  name_ext_tmp                                        <- name_ext_extended_tmp[2:length(name_ext_extended_tmp)]

  ## SAMPLE SIZE CHECK #3
  assert("observations for cohort patients within relevant timeframes", 
    all(sapply(med_timeframe_comb, function(x) nrow(x)>0)))

  #-------------------------------------------------------------------------------#
  # FEATURE GENERATION
  #-------------------------------------------------------------------------------#

  # [1] Snomed Features 
  #-------------------------------------------------------------------------------#
  print("feature generation - med_med.count_med.snomed_med.")

  # [*] melt
  med_snomed_timeframe_comb <- lapply(med_timeframe_comb, function(x)
    melt_check(data=x, id.vars =c("outcome_id", "empi", "t0_date", "med_form", "time_diff"),
    measure=patterns("^snomed_destin"), variable.name="med_destin_name", 
    value.name="med_destin"))

  med_snomed_timeframe_comb <- lapply(med_snomed_timeframe_comb, function(x) 
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date ~  med_form + med_destin"), 
    value.var = "time_diff", subset=non_missing("med_destin"), 
    mode="basic"))

  med_snomed_timeframe_comb <- feature_var_format(med_snomed_timeframe_comb)

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("pres|adm", 
    names(DT), value=T), new=paste0("med_med.count_med.snomed_med.", gsub("(.*?)_(.*)", "\\1..\\2",
    grep("pres|adm", names(DT), value=T)),name_ext_tmp)), 
    DT = med_snomed_timeframe_comb, name_ext_extended_tmp)


  # [2] ATC Features 
  #-------------------------------------------------------------------------------#
  print("feature generation - med_med.count_med.atc_med")

  # [*] melt
  med_atc_timeframe_comb <- lapply(med_timeframe_comb, function(x)
    melt_check(data=x, id.vars =c("outcome_id", "empi", "t0_date", "med_form", "time_diff"),
    measure=patterns("^atc_cat_"), variable.name="med_destin_name", 
    value.name="med_destin"))

  med_atc_timeframe_comb <- lapply(med_atc_timeframe_comb, function(x) 
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date ~  med_form + med_destin"), 
    value.var = "time_diff", subset=non_missing("med_destin"),
    mode="basic"))

  med_atc_timeframe_comb <- feature_var_format(med_atc_timeframe_comb)

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("pres|adm", 
    names(DT), value=T), new=paste0("med_med.count_med.atc_med.", gsub("(.*?)_(.*)", "\\1..\\2",
    grep("pres|adm", names(DT), value=T)),name_ext_tmp)), DT = med_atc_timeframe_comb, 
    name_ext_extended_tmp)


  # [3] Snomed Antibiotic Features 
  #-------------------------------------------------------------------------------#
  print("feature generation - med_med.count_med.anti")
  
  med_anti_timeframe_comb <- lapply(med_timeframe_comb, function(x)
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date ~  paste0('..anti_', snomed_anti_cat)"), 
    value.var = "time_diff", subset=non_missing("snomed_anti_cat"), mode="basic"))

  med_anti_timeframe_comb <- feature_var_format(med_anti_timeframe_comb)

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("..anti_", 
    names(DT), value=T), new=paste0("med_med.count_med.anti", grep("..anti_", names(DT), 
    value=T),name_ext_tmp)), DT = med_anti_timeframe_comb, name_ext_tmp=name_ext_extended_tmp)


  # [4] Snomed Chemo Features 
  #-------------------------------------------------------------------------------#
  print("feature generation - med_med.count_med.chemo")

  med_chemo_timeframe_comb <- lapply(med_timeframe_comb, function(x)
     dcast.data.table_check(data=x, 
     formula=as.formula("outcome_id + empi + t0_date ~  paste0('..chemo_', snomed_chemo_cat)"), 
     value.var = "time_diff",subset=non_missing("snomed_chemo_cat"),mode="basic"))

  med_chemo_timeframe_comb <- feature_var_format(med_chemo_timeframe_comb)

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("..chemo_", 
    names(DT), value=T), new=paste0("med_med.count_med.chemo", grep("..chemo_", names(DT), 
    value=T),name_ext_tmp)), DT = med_chemo_timeframe_comb, name_ext_tmp=name_ext_extended_tmp)


  # [5] NCI Chemo Features 
  #-------------------------------------------------------------------------------#
  print("feature generation - med_med.count_med.chemo.nci")

  ## [*] melt
  med_nci_chemo_timeframe_comb <- lapply(med_timeframe_comb, function(x)
    melt_check(x, id.vars =c("outcome_id", "empi", "t0_date", "time_diff"),
    measure=patterns("^chemo_cat_nci"), variable.name="chemo_cat_nci_name", 
    value.name="chemo_cat_nci"))

  med_nci_chemo_timeframe_comb  <- lapply(med_nci_chemo_timeframe_comb, function(x) 
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date ~  paste0('..chemo_nci_', chemo_cat_nci)"), 
    value.var = "time_diff", subset=non_missing("chemo_cat_nci"), 
    mode="basic"))

  med_nci_chemo_timeframe_comb <- feature_var_format(med_nci_chemo_timeframe_comb)

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("..chemo_nci", 
    names(DT), value=T), new=paste0("med_med.count_med.chemo.nci", grep("..chemo_nci", names(DT), 
    value=T),name_ext_tmp)), DT = med_nci_chemo_timeframe_comb , 
    name_ext_tmp=name_ext_extended_tmp)

  #-------------------------------------------------------------------------------#
  # FEATURE MERGING
  #-------------------------------------------------------------------------------#

  print("feature merging")

  med_feature_list <- list("med_snomed_timeframe_comb", "med_anti_timeframe_comb",
    "med_chemo_timeframe_comb","med_nci_chemo_timeframe_comb", "med_atc_timeframe_comb")
  med_feature_list <- med_feature_list[which(med_feature_list %in% ls())]

  # combine features across feature timeframes
  med_list_tmp <- timeframe_combine(mget(unlist(med_feature_list)), med_feature_list)

  # combine features across feature types
  med <- Reduce(mymerge, med_list_tmp)

  #-------------------------------------------------------------------------------#
  # MERGE FEATURES WITH COHORT & FORMAT VARIABLES
  #-------------------------------------------------------------------------------#

  print("cohort merging & feature formatting")

  # merge
  med <- med[cohort, mget(names(med)), on=c("outcome_id", "empi", "t0_date")]
  suppressWarnings(med[, grep("med_id$", names(med), value=T):=NULL])

  # format variables
  med <- feature_type_format(dt=med, day_to_last=timeframe_day_to_last, 
    num_feature_pattern=NA, int_feature_pattern="...", factor_feature_pattern=NA)
  
  # add in additional var / drop unnecessary variables
  med[, ':='(med_time_min=time_min, med_time_max=time_max)]

  #-------------------------------------------------------------------------------#
  # CLEAR MEMORY & RETURN FEATURE SET
  #-------------------------------------------------------------------------------#
  rm(med_timeframe_comb)
  rm(list=unlist(med_feature_list))

  return(med)

}

#----------------------------------------------------------------------------#
