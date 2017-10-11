#----------------------------------------------------------------------------#

#' @title Generate ED-related features (ed_admin data).
#'
#' @description \
#'
#' @export
#' @param order_file_date_var
#' @param enc_file_date_var
#' @param cohort
#' @param cohort_key_var
#' @param cohort_key_var_merge
#' @param lead_ed_day_arg
#' @return
#' @examples

ed_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var, 
  order_file_date_var="order_enter_date", enc_file_date_var="adm_date",
  lead_ed_day_arg=leak_ed_day) {
  
  print("launching ed_feature_gen")
  
  #-------------------------------------------------------------------------------#
  # SETUP
  #-------------------------------------------------------------------------------#

  # Load the data & subset
  #-------------------------------------------------------------------------------#
    
  # load data
  tryCatch(ed <- readRDS_merge(ed_enc_file_mod), error=function(e)
    print("no classified ed file available for the data sample"))

  # split into encounter/order data
  ed_enc   <- ed$ed_enc
  ed_order <- ed$ed_order

  print(sprintf("number of observations (ed_enc): %d", nrow(ed_enc)))
  print(sprintf("number of observations (ed_order): %d", nrow(ed_order)))

  # ensure that date variables exist
  if (!(paste0(enc_file_date_var, "_1") %in% names(ed_enc))) ed_enc[, 
    c(paste0(enc_file_date_var, "_1")):=get(enc_file_date_var)]
  if (!(paste0(order_file_date_var, "_1") %in% names(ed_order))) ed_order[, 
    c(paste0(order_file_date_var, "_1")):=get(order_file_date_var)]

  # subset to cohort
  ed_enc   <- ed_enc[!is.na(empi)]
  ed_enc   <- ed_enc[empi %in% cohort$empi]
  ed_order <- ed_order[!is.na(empi)]
  ed_order <- ed_order[empi %in% cohort$empi]

  print(sprintf("number of observations - cohort subset (ed_enc): %d", nrow(ed_enc)))
  print(sprintf("number of observations - cohort subset (ed_order): %d", nrow(ed_order)))

  ## SAMPLE SIZE CHECK #1
  assert("observations for cohort patients (ed_enc)", nrow(ed_enc)>0)
  assert("observations for cohort patients (ed_order)", nrow(ed_order)>0)

  # subset to smaller sample (if test_mode==TRUE)
  if (test_mode==TRUE) {
    
    ed_enc   <- ed_enc[1:min(test_row, nrow(ed_enc))]
    ed_order <- ed_order[1:min(test_row, nrow(ed_order))]

  }

  #-------------------------------------------------------------------------------#
  # ADDITIONAL SUBSETTING / FORMATTING
  #-------------------------------------------------------------------------------#

  # subset to encounters/orders with a non-missing date
  ed_enc   <- ed_enc[!is.na(get(enc_file_date_var))]
  ed_order <- ed_order[!is.na(get(order_file_date_var))] 

  #-------------------------------------------------------------------------------#
  # MERGING & TIMEFRAMES
  #-------------------------------------------------------------------------------#

  # Date Formatting & Merge data file with cohort & Implement leakage control
  #-------------------------------------------------------------------------------#
 
  # date formatting
  invisible(format_date(list(ed_enc), c(enc_file_date_var,paste0(enc_file_date_var, "_1"))))
  invisible(format_date(list(ed_order), c(order_file_date_var,paste0(order_file_date_var, "_1"))))

  # foverlaps merge
  ed_enc[, c(paste0(enc_file_date_var, "_1"),paste0(enc_file_date_var, "_2")):=.(get(enc_file_date_var))]
  ed_order[, c(paste0(order_file_date_var, "_1"),paste0(order_file_date_var, "_2")):=.(get(order_file_date_var))]

  ed_enc <-  foverlaps(ed_enc, cohort[, mget(cohort_key_var_merge)], by.x=c("empi",
    paste0(enc_file_date_var, "_1"), paste0(enc_file_date_var, "_2")), nomatch=0)
  ed_order <-  foverlaps(ed_order, cohort[, mget(cohort_key_var_merge)], by.x=c("empi",
    paste0(order_file_date_var, "_1"), paste0(order_file_date_var, "_2")), nomatch=0)

  print(sprintf("number of observations - cohort subset within the relevant timeframe(s) (ed_enc): %d", nrow(ed_enc)))
  print(sprintf("number of observations - cohort subset within the relevant timeframe(s) (ed_order): %d", nrow(ed_order)))

  ## SAMPLE SIZE CHECK #2
  assert("observations - cohort subset within the relevant timeframe(s) (ed_enc)", 
    nrow(ed_enc)>0)
  assert("observations - cohort subset within the relevant timeframe(s) (ed_order)", 
    nrow(ed_order)>0)

  # time difference calculation (event date vs. t0 date)
  ed_enc[, time_diff:=as.numeric(difftime(t0_date, get(enc_file_date_var), 
    units="days"))]
  ed_order[, time_diff:=as.numeric(difftime(t0_date, get(order_file_date_var), 
    units="days"))]

  # implement leakage control 
  if (!is.na(lead_ed_day_arg)) {
    
    ed_enc <- ed_enc[!(t0_date-get(paste0(enc_file_date_var, "_1"))<=lead_ed_day_arg)]
    ed_order <- ed_order[!(t0_date-get(paste0(order_file_date_var, "_1"))<=lead_ed_day_arg)]

  }

  # split data into feature timeframes
  #-------------------------------------------------------------------------------#
  return_mult[ed_enc_timeframe_comb, time_min_ed_enc, time_max_ed_enc]       <- timeframe_split(list("ed_enc"), enc_file_date_var)[[1]]
  return_mult[ed_order_timeframe_comb, time_min_ed_order, time_max_ed_order] <- timeframe_split(list("ed_order"), order_file_date_var)[[1]]

  name_ext_extended_tmp                                                      <- gsub("timeframe_comb", "", names(ed_enc_timeframe_comb))
  name_ext_tmp                                                               <- name_ext_extended_tmp[2:length(name_ext_extended_tmp)]

  ## SAMPLE SIZE CHECK #3
  assert("observations for cohort patients within relevant timeframes (ed_enc)", 
    all(sapply(ed_enc_timeframe_comb, function(x) nrow(x)>0)))
  assert("observations for cohort patients within relevant timeframes (ed_order)", 
    all(sapply(ed_order_timeframe_comb, function(x) nrow(x)>0)))

  time_min <- min(time_min_ed_enc, time_min_ed_order)
  time_max <- max(time_max_ed_enc, time_max_ed_order)


  #-------------------------------------------------------------------------------#
  # FEATURE GENERATION
  #-------------------------------------------------------------------------------#

  # [1] ED chief complaint features 
  #-------------------------------------------------------------------------------#
  print("feature generation - ed_ed.chief.complaint.count_ed.zc")

  # [*] melt
  ed_cc_timeframe_comb <- lapply(ed_enc_timeframe_comb, function(x)
    melt_check(x, id.vars =c("outcome_id", "empi", "t0_date","time_diff"),
    measure=patterns("^ed_cc_[0-9]"), variable.name="ed_cc_name", 
    value.name="ed_cc"))

  ed_cc_timeframe_comb <- lapply(ed_cc_timeframe_comb, function(x) 
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date ~  paste0('ed_ed.chief.complaint.count_ed.zc..', ed_cc)"), 
    value.var = "time_diff",subset=non_missing("ed_cc"),mode="basic"))

  ed_cc_timeframe_comb <- feature_var_format(ed_cc_timeframe_comb)

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, 
    old=grep("ed_ed.chief.complaint.count", 
    names(DT), value=T), new=paste0(grep("ed_ed.chief.complaint.count_ed.zc", 
    names(DT), value=T),name_ext_tmp)), DT=ed_cc_timeframe_comb,  name_ext_extended_tmp)

  # [2] ED medication order features 
  #-------------------------------------------------------------------------------#
  print("feature generation - ed_ed.med.order.count_ed.snomed")

  # [*] melt
  ed_med_order_timeframe_comb <- lapply(ed_order_timeframe_comb, function(x)
    melt_check(x, id.vars =c("outcome_id", "empi", "t0_date", "time_diff"),
    measure=patterns("^snomed_destin"), variable.name="snomed_destin_name", 
    value.name="snomed_destin"))

  ed_med_order_timeframe_comb <- lapply(ed_med_order_timeframe_comb, function(x) 
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date ~  paste0('med_', snomed_destin)"), 
    value.var = "time_diff", mode="basic",
    subset=non_missing("snomed_destin")))

  ed_med_order_timeframe_comb <- feature_var_format(ed_med_order_timeframe_comb)

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("med_", 
    names(DT), value=T), new=paste0("ed_ed.med.order.count_ed.snomed", 
    gsub("(.*?)med_(.*)", "\\1..med_\\2",grep("med_", names(DT), value=T)),name_ext_tmp)), 
    DT = ed_med_order_timeframe_comb, name_ext_extended_tmp)

  #-------------------------------------------------------------------------------#
  # FEATURE MERGING
  #-------------------------------------------------------------------------------#

  print("feature merging")

  ed_feature_list <- list("ed_cc_timeframe_comb", "ed_med_order_timeframe_comb")
  ed_feature_list <- ed_feature_list[which(ed_feature_list %in% ls())]


  # combine features across feature timeframes
  ed_list_tmp <- timeframe_combine(mget(unlist(ed_feature_list)), ed_feature_list)

  # combine features across feature types
  ed <- Reduce(mymerge, ed_list_tmp)

  #-------------------------------------------------------------------------------#
  # MERGE FEATURES WITH COHORT & FORMAT VARIABLES
  #-------------------------------------------------------------------------------#

  print("cohort merging & feature formatting")

  # merge
  ed <- ed[cohort, mget(names(ed)), on=c("outcome_id", "empi", "t0_date")]
  suppressWarnings(ed[, grep("ed_(enc|order)_id$", names(ed), value=T):=NULL])

  # format variables
  ed <- feature_type_format(dt=ed, day_to_last=timeframe_day_to_last, 
    num_feature_pattern=NA, int_feature_pattern="...", factor_feature_pattern=NA)
  
  # add in additional var / drop unnecessary variables
  ed[, ':='(ed_time_min=time_min, ed_time_max=time_max)]

  #-------------------------------------------------------------------------------#
  # CLEAR MEMORY & RETURN FEATURE SET
  #-------------------------------------------------------------------------------#
  rm(ed_enc_timeframe_comb)
  rm(ed_order_timeframe_comb)
  rm(list=unlist(ed_feature_list))

  return(ed)

}

#----------------------------------------------------------------------------#











