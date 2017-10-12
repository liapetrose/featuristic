#----------------------------------------------------------------------------#

#' @title Generate enc-related features (enc data).
#'
#' @description \
#'
#' @export
#' @param file_date_var
#' @param cohort
#' @param cohort_key_var
#' @param cohort_key_var_merge
#' @param enc_file_mod_arg
#' @return
#' @examples

enc_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var, file_date_var="adm_date", 
  leak_enc_day_arg=leak_enc_day,enc_file_mod_arg=enc_file_mod) {
  
  print("launching enc_feature_gen")
  
  #-------------------------------------------------------------------------------#
  # SETUP
  #-------------------------------------------------------------------------------#

  # Load the data & subset
  #-------------------------------------------------------------------------------#
    
  # load data
  tryCatch(enc <- readRDS_merge(enc_file_mod_arg,nested=TRUE), error=function(e)
    print("no classified enc file available for the data sample"))

  # split into encounter types
  enc_class <- setkey(as.data.table(enc$enc_class), empi)
  ed        <- setkey(as.data.table(enc$ed_visit), empi)
  op        <- setkey(as.data.table(enc$op_visit), empi)
  ip        <- setkey(as.data.table(enc$ip_visit), empi)

  print(sprintf("number of observations (enc_class): %d", nrow(enc_class)))
  print(sprintf("number of observations (ed): %d", nrow(ed)))
  print(sprintf("number of observations (op): %d", nrow(op)))
  print(sprintf("number of observations (ip): %d", nrow(ip)))

  # ensure that date variables exist
  if (!(paste0(file_date_var, "_1") %in% names(enc_class))) enc_class[, 
    c(paste0(file_date_var, "_1")):=get(file_date_var)]
  if (!(paste0(file_date_var, "_1") %in% names(ed))) ed[, 
    c(paste0(file_date_var, "_1")):=get(file_date_var)]
  if (!(paste0(file_date_var, "_1") %in% names(op))) op[, 
    c(paste0(file_date_var, "_1")):=get(file_date_var)]
  if (!(paste0(file_date_var, "_1") %in% names(ip))) ip[, 
    c(paste0(file_date_var, "_1")):=get(file_date_var)]

  # subset to cohort
  enc_class <- enc_class[!is.na(empi)]
  enc_class <- enc_class[empi %in% cohort$empi]
  ed        <- ed[!is.na(empi)]
  ed        <- ed[empi %in% cohort$empi]
  op        <- op[!is.na(empi)]
  op        <- op[empi %in% cohort$empi]
  ip        <- ip[!is.na(empi)]
  ip        <- ip[empi %in% cohort$empi]

  print(sprintf("number of observations - cohort subset (enc_class): %d", nrow(enc_class)))
  print(sprintf("number of observations - cohort subset (ed): %d", nrow(ed)))
  print(sprintf("number of observations - cohort subset (op): %d", nrow(op)))
  print(sprintf("number of observations - cohort subset (ip): %d", nrow(ip)))

  ## SAMPLE SIZE CHECK #1
  assert("observations for cohort patients (enc_class)", nrow(enc_class)>0)
  assert("observations for cohort patients (ed)", nrow(ed)>0)
  assert("observations for cohort patients (op)", nrow(op)>0)
  assert("observations for cohort patients (ip)", nrow(ip)>0)

  # subset to smaller sample (if test_mode==TRUE)
  if (test_mode==TRUE) {
    
    enc_class <- enc_class[1:min(test_row, nrow(enc_class))]
    ed        <- ed[1:min(test_row, nrow(ed))]
    op        <- op[1:min(test_row, nrow(op))]
    ip        <- ip[1:min(test_row, nrow(ip))]

  }


  #-------------------------------------------------------------------------------#
  # ADDITIONAL SUBSETTING / FORMATTING
  #-------------------------------------------------------------------------------#

  # standardize column (names)
  setnames_check(enc_class, new=gsub("enc_clinic","enc_op", names(enc_class)))
  
  suppressWarnings(op[, grep("enc_op", names(op), value=T):=NULL])
  suppressWarnings(op[, grep("enc_clinic", names(op), value=T):=NULL])
  suppressWarnings(op[, enc_op:=1])


  #-------------------------------------------------------------------------------#
  # MERGING & TIMEFRAMES
  #-------------------------------------------------------------------------------#

  # Date Formatting & Merge data file with cohort & Implement leakage control
  #-------------------------------------------------------------------------------#
 
  for (DT in c("enc_class", "ed", "op", "ip")) {

     # date formatting
     invisible(format_date(list(get(DT)), c(file_date_var,paste0(file_date_var, "_1"))))
    
     # foverlaps merge
     get(DT)[, c(paste0(file_date_var, "_1"),paste0(file_date_var, "_2")):=.(get(file_date_var))]
    
     assign(DT, foverlaps(get(DT), cohort[, mget(cohort_key_var_merge)], by.x=c("empi",
       paste0(file_date_var, "_1"), paste0(file_date_var, "_2")), nomatch=0))

     print(sprintf("number of observations - cohort subset within the relevant timeframe(s) (%s): %d", 
      DT,nrow(get(DT))))
    
     ## SAMPLE SIZE CHECK #2
     assert(sprintf("observations - cohort subset within the relevant timeframe(s) (%s)", DT), 
       nrow(get(DT))>0)
    
     # time difference calculation (event date vs. t0 date)
     get(DT)[, time_diff:=as.numeric(difftime(t0_date, get(file_date_var), 
       units="days"))]

  }

  # implement leakage control 
  for (DT in c("enc_class", "ed", "op", "ip")) { 
    if (!is.na(leak_enc_day)) {
      assign(DT,  get(DT)[!(t0_date-get(paste0(file_date_var, "_1"))<=leak_enc_day)])
    }
  }

  # split data into feature timeframes
  #-------------------------------------------------------------------------------#
  return_mult[enc_class_timeframe_comb, time_min_enc, time_max_enc]          <- timeframe_split(list("enc_class"), file_date_var)[[1]]
  return_mult[ed_timeframe_comb, time_min_ed, time_max_ed]                   <- timeframe_split(list("ed"), file_date_var)[[1]]
  return_mult[op_timeframe_comb, time_min_op, time_max_op]                   <- timeframe_split(list("op"), file_date_var)[[1]]
  return_mult[ip_timeframe_comb, time_min_ip, time_max_ip]                   <- timeframe_split(list("ip"), file_date_var)[[1]]

  name_ext_extended_tmp                                                      <- gsub("timeframe_comb", "", names(enc_class_timeframe_comb))
  name_ext_tmp                                                               <- name_ext_extended_tmp[2:length(name_ext_extended_tmp)]

  ## SAMPLE SIZE CHECK #3
  assert("observations for cohort patients within relevant timeframes (enc_class)", 
    all(sapply(enc_class_timeframe_comb, function(x) nrow(x)>0)))
  assert("observations for cohort patients within relevant timeframes (ed)", 
    all(sapply(ed_timeframe_comb, function(x) nrow(x)>0)))
  assert("observations for cohort patients within relevant timeframes (op)", 
    all(sapply(op_timeframe_comb, function(x) nrow(x)>0)))
  assert("observations for cohort patients within relevant timeframes (ip)", 
    all(sapply(ip_timeframe_comb, function(x) nrow(x)>0)))

  time_min <- min(time_min_enc, time_min_ed, time_min_ip, time_min_op)
  time_max <- max(time_max_enc, time_max_ed, time_max_ip, time_max_op)

  #-------------------------------------------------------------------------------#
  # FEATURE GENERATION
  #-------------------------------------------------------------------------------#

  # [1] Encounter count features
  #-------------------------------------------------------------------------------#
  print("feature generation - enc_enc.enc_enc.count")

  for (DT in c("ed", "op", "ip")) {
  	
    DT_enc <- paste0("enc_",DT)

    assign(paste0(DT_enc, "_timeframe_comb"), lapply(enc_class_timeframe_comb, 
      function(x) dcast.data.table_check(data=x, 
      formula=as.formula(paste0("outcome_id +  empi + t0_date ~ ", DT_enc)),
      mode="basic", value.var = "time_diff")))

    assign(paste0(DT_enc, "_timeframe_comb"), 
      feature_var_format(get(paste0(DT_enc, "_timeframe_comb"))))
    
    inv_mapply(function(x, name_ext_tmp) setnames_check(x, old=grep("1$", names(x), value=T), 
      new=paste0("enc_enc.enc_enc.count..", DT, name_ext_tmp)), 
      x=get(paste0(DT_enc, "_timeframe_comb")), name_ext_extended_tmp)
  
    inv_mapply(function(x, name_ext_tmp) setnames_check(x, old=grep("1_day_to_last", names(x), value=T), 
      new=paste0("enc_enc.enc_enc.count..", DT, "_day_to_last", name_ext_tmp)), 
      x=get(paste0(DT_enc, "_timeframe_comb")), name_ext_extended_tmp)

  }
  
  # [2] Encounter count features - by clinic name
  #-------------------------------------------------------------------------------#
  print("feature generation - enc_enc.enc_enc.count_clinic.count_clinic.name")

  clinic_name_timeframe_comb <- lapply(enc_class_timeframe_comb, function(x) 
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date ~ clinic_name"), 
    mode="basic", value.var = "time_diff"))

  clinic_name_timeframe_comb <- feature_var_format(clinic_name_timeframe_comb)
 
  inv_mapply(function(x, name_ext_tmp) setnames_check(x, old=setdiff(names(x), c("empi", "outcome_id", 
    "t0_date")), new=paste0("enc_enc.enc_enc.count_clinic.count_clinic.name..", 
    setdiff(names(x), c("empi", "outcome_id", "t0_date")), name_ext_tmp)), 
    x=clinic_name_timeframe_comb, name_ext_extended_tmp)

  # [3] Encounter count features - by clinic category
  #-------------------------------------------------------------------------------#
  print("feature generation - enc_enc.enc_enc.count_clinic.count_clinic.cat")

  clinic_cat_timeframe_comb <- lapply(enc_class_timeframe_comb, function(x) 
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date ~ clinic_cat"), 
    mode="basic", value.var = "time_diff"))
 
  clinic_cat_timeframe_comb <- feature_var_format(clinic_cat_timeframe_comb)

  inv_mapply(function(x, name_ext_tmp) setnames_check(x, old=setdiff(names(x), 
    c("empi", "outcome_id", "t0_date")), new=paste0("enc_enc.enc_enc.count_clinic.count_clinic.cat..", 
    setdiff(names(x), c("empi", "outcome_id", "t0_date")), name_ext_tmp)), 
    x=clinic_cat_timeframe_comb, name_ext_extended_tmp)

  # [4] Visit count features 
  #-------------------------------------------------------------------------------#
  print("feature generation - enc_enc.visit_visit.count")

  for (DT in c("ed", "op", "ip")) {
  	
    visit_DT <- paste0("visit_",DT)
    enc_DT   <- paste0("enc_",DT)

    assign(paste0(visit_DT, "_timeframe_comb"), lapply(get(paste0(
      DT,"_timeframe_comb")), function(x) dcast.data.table_check(data=x, 
      formula=as.formula(paste0("outcome_id + empi + t0_date ~ ", enc_DT)), 
      value.var = "time_diff", mode="basic")))

    assign(paste0(visit_DT, "_timeframe_comb"), feature_var_format(get(paste0(visit_DT, 
      "_timeframe_comb"))))
  
    inv_mapply(function(x, name_ext_tmp) setnames_check(x, old=grep("1$", names(x), value=T), 
      new=paste0("enc_enc.visit_visit.count..", DT, name_ext_tmp)), x=get(paste0(visit_DT, 
      "_timeframe_comb")), name_ext_extended_tmp)

    inv_mapply(function(x, name_ext_tmp) setnames_check(x, old=grep("1_day_to_last", 
      names(x), value=T), new=paste0("enc_enc.visit_visit.count..", DT, "_day_to_last", name_ext_tmp)), 
      x=get(paste0(visit_DT, "_timeframe_comb")), name_ext_extended_tmp)
  
  }

  # [5] IP LOS features 
  #-------------------------------------------------------------------------------#
  print("feature generation - enc_enc.los")

	ip_days_timeframe_comb <- lapply(ip_timeframe_comb, function(x) 
    x[, .(ip_days=sum(los, na.rm=TRUE)), by = c("outcome_id", "empi", "t0_date")])
  
  inv_mapply(function(x, name_ext_tmp) setnames_check(x, old=c("ip_days"), 
    new=paste0("enc_enc.los..ip", name_ext_tmp)), x=ip_days_timeframe_comb,
    name_ext_extended_tmp)

  #-------------------------------------------------------------------------------#
  # FEATURE MERGING
  #-------------------------------------------------------------------------------#

  print("feature merging")

  enc_feature_list <- list("enc_ed_timeframe_comb", "enc_ip_timeframe_comb", 
    "enc_op_timeframe_comb", "clinic_name_timeframe_comb", 
    "clinic_cat_timeframe_comb", "visit_ed_timeframe_comb", 
    "visit_ip_timeframe_comb", "visit_op_timeframe_comb", 
    "ip_days_timeframe_comb")
  enc_feature_list <- enc_feature_list[which(enc_feature_list %in% ls())]

  # combine features across feature timeframes
  enc_list_tmp <- timeframe_combine(mget(unlist(enc_feature_list)), enc_feature_list)

  # combine features across feature types
  enc <- Reduce(mymerge, enc_list_tmp)

  #-------------------------------------------------------------------------------#
  # MERGE FEATURES WITH COHORT & FORMAT VARIABLES
  #-------------------------------------------------------------------------------#

  print("cohort merging & feature formatting")

  # merge
  enc <- enc[cohort, mget(names(enc)), on=c("outcome_id", "empi", "t0_date")]
  suppressWarnings(enc[, grep("enc_id$|visit_id$", names(enc), value=T):=NULL])

  # format variables
  enc <- feature_type_format(dt=enc, day_to_last=timeframe_day_to_last, 
    num_feature_pattern=NA, int_feature_pattern="...", factor_feature_pattern=NA)
  
  # add in additional var / drop unnecessary variables
  enc[, ':='(enc_time_min=time_min, enc_time_max=time_max)]

  #-------------------------------------------------------------------------------#
  # CLEAR MEMORY & RETURN FEATURE SET
  #-------------------------------------------------------------------------------#
  rm(enc_class_timeframe_comb)
  rm(list=unlist(enc_feature_list))

  return(enc)

}

#----------------------------------------------------------------------------#
