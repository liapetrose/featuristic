#----------------------------------------------------------------------------#

#' @title Generate procedure-related features (prc data).
#'
#' @description /
#'
#' @export
#' @param file_date_var 
#' @param cohort
#' @param cohort_key_var_merge
#' @param cohort_key_var
#' @param leak_prc_day_arg
#' @return
#' @examples


prc_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var, 
  file_date_var="prc_date", leak_prc_day_arg=leak_prc_day) {
  
  print("launching prc_feature_gen")
  
  #-------------------------------------------------------------------------------#
  # SETUP
  #-------------------------------------------------------------------------------#

  # Load the data & subset
  #-------------------------------------------------------------------------------#
    
  # load data
  tryCatch(prc <- readRDS_merge(prc_file_mod), error=function(e)
    print("no classified prc file available for the data sample"))
  
  # ensure that date variables exist
  if (!(paste0(file_date_var, "_1") %in% names(prc))) prc[, 
    c(paste0(file_date_var, "_1")):=get(file_date_var)]

  print(sprintf("number of observations: %d", nrow(prc)))

  # subset to cohort
  prc <- prc[!is.na(empi)]
  prc <- prc[empi %in% cohort$empi]

  print(sprintf("number of observations - cohort subset: %d", nrow(prc)))
  
  ## SAMPLE SIZE CHECK #1
  assert("observations for cohort patients", nrow(prc)>0)

  # subset to smaller sample (if test_mode==TRUE)
  if (test_mode==TRUE) {
    
    prc <- prc[1:min(test_row, nrow(prc))]

  }

  #-------------------------------------------------------------------------------#
  # ADDITIONAL SUBSETTING / FORMATTING
  #-------------------------------------------------------------------------------#

  # select only prognosis code related variables
  prc[, c("prc_code_type","clinic_name", "hospital", "inpatient_outpatient", "provider"):=NULL]

  # select ccs (single/multi & zc) - category names rather than numbers
  prc[, grep("num", names(prc)):=NULL]

  #-------------------------------------------------------------------------------#
  # MERGING & TIMEFRAMES
  #-------------------------------------------------------------------------------#

  # Date Formatting & Merge data file with cohort & Implement leakage control
  #-------------------------------------------------------------------------------#
 
  # date formatting
  invisible(format_date(list(prc), c(file_date_var,paste0(file_date_var, "_1"))))

  # foverlaps merge
  prc[, c(paste0(file_date_var, "_1"),paste0(file_date_var, "_2")):=.(get(file_date_var))]

  prc <-  foverlaps(prc, cohort[, mget(cohort_key_var_merge)], by.x=c("empi",
    paste0(file_date_var, "_1"), paste0(file_date_var, "_2")), nomatch=0)

  print(sprintf("number of observations - cohort subset within the relevant timeframe(s): %d", nrow(prc)))

  ## SAMPLE SIZE CHECK #2
  assert("observations - cohort subset within the relevant timeframe(s)", nrow(prc)>0)

  # time difference calculation (event date vs. t0 date)
  prc[, time_diff:=as.numeric(difftime(t0_date, get(file_date_var), 
    units="days"))]

  # implement leakage control 
  if (!is.na(leak_prc_day_arg)) {
    
    prc <- prc[!(t0_date-get(paste0(file_date_var, "_1"))<=leak_prc_day_arg)]

  }

  # split data into feature timeframes
  #-------------------------------------------------------------------------------#
  return_mult[prc_timeframe_comb, time_min, time_max] <- timeframe_split(list("prc"), file_date_var)[[1]]
  name_ext_extended_tmp                               <- gsub("timeframe_comb", "", names(prc_timeframe_comb))
  name_ext_tmp                                        <- name_ext_extended_tmp[2:length(name_ext_extended_tmp)]

  ## SAMPLE SIZE CHECK #3
  assert("observations for cohort patients within relevant timeframes", 
    all(sapply(prc_timeframe_comb, function(x) nrow(x)>0)))


  #-------------------------------------------------------------------------------#
  # FEATURE GENERATION
  #-------------------------------------------------------------------------------#

  # [1] Single CCS Features
  #-------------------------------------------------------------------------------#
  print("feature generation - prc_prc.count_prc.single.ccs")

  ccs_single_timeframe_comb <- lapply(prc_timeframe_comb, function(x) 
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date ~  paste0('prc_prc.count_prc.single.ccs..', ccs_single_cat_name)"), 
    value.var = "time_diff", subset=non_missing("ccs_single_cat_name"), mode="basic"))

  ccs_single_timeframe_comb <- feature_var_format(ccs_single_timeframe_comb)

  inv_mapply(function(DT, name_ext_tmp) setnames_check(DT, old=grep("prc_prc.count_prc.single.ccs", 
    names(DT), value=T), new=paste0(grep("prc_prc.count_prc.single.ccs", names(DT), value=T),
    name_ext_tmp)), DT=ccs_single_timeframe_comb, name_ext_extended_tmp)

  # [2] Multi CCS Features
  #-------------------------------------------------------------------------------#
  print("feature generation - prc_prc.count_prc.multi.ccs")
  
  # [*] combine
  ccs_multi_timeframe_comb <- lapply(prc_timeframe_comb, function(x)
    rbindlist(list(x[, .(outcome_id, empi, t0_date, ccs_multi_cat_name=ccs_multi_cat_name_1, time_diff)],
    x[, .(outcome_id, empi, t0_date, ccs_multi_cat_name=ccs_multi_cat_name_2, time_diff)], 
    x[, .(outcome_id, empi, t0_date, ccs_multi_cat_name=ccs_multi_cat_name_3, time_diff)]),
    use.names=T))

  ccs_multi_timeframe_comb <- lapply(ccs_multi_timeframe_comb, function(x)
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date ~  paste0('prc_prc.count_prc.multi.ccs..',ccs_multi_cat_name)"), 
    value.var = "time_diff", subset=non_missing("ccs_multi_cat_name"), mode="basic"))

  ccs_multi_timeframe_comb <- feature_var_format(ccs_multi_timeframe_comb)

  inv_mapply(function(DT, name_ext_tmp) setnames_check(DT, old=grep("prc_prc.count_prc.multi.ccs", 
    names(DT), value=T), new=paste0(grep("prc_prc.count_prc.multi.ccs", names(DT), value=T),
    name_ext_tmp)), DT=ccs_multi_timeframe_comb, name_ext_extended_tmp)

  #-------------------------------------------------------------------------------#
  # FEATURE MERGING
  #-------------------------------------------------------------------------------#
  
  print("feature merging")

  prc_feature_list <- list("ccs_single_timeframe_comb","ccs_multi_timeframe_comb")

  prc_feature_list <- prc_feature_list[which(prc_feature_list %in% ls())]

  # combine features across feature timeframes
  prc_list_tmp <- timeframe_combine(mget(unlist(prc_feature_list)), prc_feature_list)
  
  # combine features across feature types
  prc <- Reduce(mymerge, prc_list_tmp)
  
  #-------------------------------------------------------------------------------#
  # MERGE FEATURES WITH COHORT & FORMAT VARIABLES
  #-------------------------------------------------------------------------------#

  print("cohort merging & feature formatting")
  
  # merge
  prc <- prc[cohort, mget(names(prc)), on=c("outcome_id", "empi", "t0_date")]
  suppressWarnings(prc[, grep("prc_id$", names(prc), value=T):=NULL])

  # format variables
  prc <- feature_type_format(dt=prc, day_to_last=timeframe_day_to_last, 
    num_feature_pattern=NA, int_feature_pattern="...", factor_feature_pattern=NA)

  # add in additional var / drop unnecessary variables
  prc[, ':='(prc_time_min=time_min, prc_time_max=time_max)]

  #-------------------------------------------------------------------------------#
  # CLEAR MEMORY & RETURN FEATURE SET
  #-------------------------------------------------------------------------------#
  rm(prc_timeframe_comb)
  rm(list=unlist(prc_feature_list))

  return(prc)

}

#----------------------------------------------------------------------------#

