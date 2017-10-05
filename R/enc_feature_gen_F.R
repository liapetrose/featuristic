#----------------------------------------------------------------------------#

#' @title Generate enc-related features (enc data).
#'
#' @description \
#'
#' @export
#' @param file_date_var
#' @param cohort
#' @param cohort_key_var_merge
#' @return
#' @examples

enc_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var, file_date_var="adm_date") {
  
  print("launching enc_feature_gen")
  
  #-------------------------------------------------------------------------------#
  # Load the  modified/pre-processed enc file for the specified data sample -- 
  
  # load the data
   tryCatch(enc <- readRDS_merge(enc_file_mod, nested=TRUE), warning=function(w)
    print("no classified enc file available for the data sample"))

  # subset to smaller sample for testing if so specified in control file
  if (test_raw_file==TRUE) {
    store_shorten_file("enc")
  }

  # standardize column names 
  setnames(enc$enc_class, gsub("enc_clinic","enc_op", names(enc$enc_class)))
  enc$op_visit[, grep("enc_op", names(enc$op_visit), value=T):=NULL]
  enc$op_visit[, grep("enc_clinic", names(enc$op_visit), value=T):=NULL]
  enc$op_visit[, enc_op:=1]

  #-------------------------------------------------------------------------------#
  # split list into 4 DT
  enc_class <- setkey(as.data.table(enc$enc_class), empi)
  ed <- setkey(as.data.table(enc$ed_visit), empi)
  op <- setkey(as.data.table(enc$op_visit), empi)
  ip <- setkey(as.data.table(enc$ip_visit), empi)

  #-------------------------------------------------------------------------------#
  # merge encounter file with cohort (cohort_key_variables) & format dates

  for (DT in c("enc_class", "ed", "op", "ip")) {

    assign(DT, get(DT)[empi %in% cohort$empi])

    invisible(parse_date(get(DT), c("adm_date", "disch_date")))

    get(DT)[, c("adm_date_1", "adm_date_2"):=.(adm_date)]

    assign(DT,  foverlaps(get(DT), cohort[, mget(cohort_key_var_merge)],
      by.x=c("empi", "adm_date_1", "adm_date_2"), nomatch=0))

    get(DT)[, time_diff:=as.numeric(difftime(t0_date, get(file_date_var), 
      units="days"))]

  }

  # implement leakage control 
  for (DT in c("enc_class", "ed", "op", "ip")) { 
    if (!is.na(leak_enc_day)) {
      assign(DT,  get(DT)[!(t0_date-adm_date_1<=leak_enc_day)])
    }
  }

  #-------------------------------------------------------------------------------#
  # sub setting & dividing into smaller DT based on timeframe 
  timeframe_split(list("enc_class", "ed", "op", "ip"), "adm_date")

  name_ext_extended <- name_ext_extended[sapply(enc_class_timeframe_comb, nrow)!=0]
  name_ext <- name_ext_extended[2:length(name_ext_extended)]

  ed_timeframe_comb <- ed_timeframe_comb[sapply(enc_class_timeframe_comb, nrow)!=0]
  op_timeframe_comb <- op_timeframe_comb[sapply(enc_class_timeframe_comb, nrow)!=0]
  ip_timeframe_comb <- ip_timeframe_comb[sapply(enc_class_timeframe_comb, nrow)!=0]
  enc_class_timeframe_comb <- enc_class_timeframe_comb[sapply(enc_class_timeframe_comb, nrow)!=0]

  time_min_enc <- min(do.call("c", lapply(enc_class_timeframe_comb, function(x) as.Date(min(x[, 
    adm_date]), "%Y-%m-%d"))))
  time_max_enc <- max(do.call("c", lapply(enc_class_timeframe_comb, function(x) as.Date(max(x[, 
    adm_date]), "%Y-%m-%d"))))
  
  time_min_ed <- min(do.call("c", lapply(ed_timeframe_comb, function(x) as.Date(min(x[, 
    adm_date]), "%Y-%m-%d"))))
  time_max_ed <- max(do.call("c", lapply(ed_timeframe_comb, function(x) as.Date(max(x[, 
    adm_date]), "%Y-%m-%d"))))

  time_min_op <- min(do.call("c", lapply(op_timeframe_comb, function(x) as.Date(min(x[, 
    adm_date]), "%Y-%m-%d"))))
  time_max_op <- max(do.call("c", lapply(op_timeframe_comb, function(x) as.Date(max(x[, 
    adm_date]), "%Y-%m-%d"))))
  
  time_min_ip <- min(do.call("c", lapply(ip_timeframe_comb, function(x) as.Date(min(x[, 
    adm_date]), "%Y-%m-%d"))))
  time_max_ip <- max(do.call("c", lapply(ip_timeframe_comb, function(x) as.Date(max(x[, 
    adm_date]), "%Y-%m-%d"))))

  time_min <- min(time_min_enc, time_min_ed, time_min_ip, time_min_op)
  time_max <- max(time_max_enc, time_max_ed, time_max_ip, time_max_op)

  #-------------------------------------------------------------------------------#
  # reshaping - create encounter dummies (ed/ip/op)
  for (DT in c("ed", "op", "ip")) {
  	assign(paste0("enc_",DT, "_timeframe_comb"), lapply(enc_class_timeframe_comb, 
      function(x) dcast.data.table(x, outcome_id +  empi + t0_date ~ 
      get(paste0("enc_",DT)),fun.aggregate=list(length, function(x) min(x, na.rm=T)), 
      value.var = "time_diff")))

  assign(paste0("enc_",DT, "_timeframe_comb"), 
    feature_var_format(get(paste0("enc_",DT, "_timeframe_comb"))))
  
  lapply(get(paste0("enc_",DT, "_timeframe_comb")), function(x) x[,
      c("NA", "NA_days_to_last", "0", "0_days_to_last") := NULL])

    mapply(function(x, name_ext) setnames(x, grep("1$", names(x), value=T), 
      paste0("enc_enc.enc_enc.count..", DT, name_ext)), x=get(paste0("enc_",DT, 
      "_timeframe_comb")), name_ext_extended)

    mapply(function(x, name_ext) setnames(x, grep("1_days_to_last", names(x), value=T), 
      paste0("enc_enc.enc_enc.count..", DT, "_days_to_last", name_ext)), x=get(paste0("enc_",DT, 
      "_timeframe_comb")), name_ext_extended)

  }

  #-------------------------------------------------------------------------------#
  # reshaping - create clinic_name dummies for all clinic encounters
  clinic_name_timeframe_comb <- lapply(enc_class_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + t0_date ~ clinic_name, 
    fun.aggregate=list(length, function(x) min(x, na.rm=T)), 
      value.var = "time_diff"))

  clinic_name_timeframe_comb <- feature_var_format(clinic_name_timeframe_comb)

  lapply(clinic_name_timeframe_comb, function(x) x[, c("NA", "NA_days_to_last") := NULL])
  
  mapply(function(x, name_ext) setnames(x, setdiff(names(x), c("empi", "outcome_id", 
    "t0_date")), paste0("enc_enc.enc_enc.count_clinic.count_clinic.name..", 
    setdiff(names(x), c("empi", "outcome_id", "t0_date")), name_ext)), 
    x=clinic_name_timeframe_comb, name_ext_extended)

  #-------------------------------------------------------------------------------#
  # reshaping - create clinic_cat dummies for all clinic encounters
  clinic_cat_timeframe_comb <- lapply(enc_class_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + t0_date ~ clinic_cat, 
     fun.aggregate=list(length, function(x) min(x, na.rm=T)), 
      value.var = "time_diff"))
  clinic_cat_timeframe_comb <- feature_var_format(clinic_cat_timeframe_comb)

  lapply(clinic_cat_timeframe_comb, function(x) x[, c("NA", "NA_days_to_last") := NULL])
  mapply(function(x, name_ext) setnames(x, setdiff(names(x), c("empi", "outcome_id", 
    "t0_date")), paste0("enc_enc.enc_enc.count_clinic.count_clinic.cat..", 
    setdiff(names(x), c("empi", "outcome_id", "t0_date")), name_ext)), 
    x=clinic_cat_timeframe_comb, name_ext_extended)

  #-------------------------------------------------------------------------------#
  # reshaping - visit dummies (ed/ip/clinic)
  for (DT in c("ed", "op", "ip")) {
  	assign(paste0("visit_",DT, "_timeframe_comb"), lapply(get(paste0(
    DT,"_timeframe_comb")), function(x) dcast.data.table(x, outcome_id + empi + 
    t0_date ~ get(paste0("enc_",DT)), 
    fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff")))

  assign(paste0("visit_",DT, "_timeframe_comb"), 
    feature_var_format(get(paste0("visit_",DT, "_timeframe_comb"))))
  
  mapply(function(x, name_ext) setnames(x, grep("1$", names(x), value=T), 
    paste0("enc_enc.visit_visit.count..", DT, name_ext)), x=get(paste0("visit_",DT, 
    "_timeframe_comb")), name_ext_extended)

  mapply(function(x, name_ext) setnames(x, grep("1_days_to_last", names(x), value=T), 
    paste0("enc_enc.visit_visit.count..", DT, "_days_to_last", name_ext)), x=get(paste0("visit_",DT, 
    "_timeframe_comb")), name_ext_extended)
  }

  #-------------------------------------------------------------------------------#
  # reshaping - number of inpatient days
	ip_days_timeframe_comb <- lapply(ip_timeframe_comb, function(x) 
    x[, .(ip_days=sum(los)),
    by = c("outcome_id", "empi", "t0_date")])
  mapply(function(x, name_ext) setnames(x, c("ip_days"), 
    paste0("enc_enc.los..ip", name_ext)), x=ip_days_timeframe_comb,
    name_ext_extended)

  #-------------------------------------------------------------------------------#
  # merge utilization feature files
  enc_feature_list <- list("enc_ed_timeframe_comb", "enc_ip_timeframe_comb", 
    "enc_op_timeframe_comb", "clinic_name_timeframe_comb", 
    "clinic_cat_timeframe_comb", "visit_ed_timeframe_comb", 
    "visit_ip_timeframe_comb", "visit_op_timeframe_comb", 
    "ip_days_timeframe_comb")

  timeframe_combine(enc_feature_list)
  
  enc <- Reduce(mymerge, mget(unlist(enc_feature_list)))

  #-------------------------------------------------------------------------------#
  # merge with cohort file - empty records -> 0
  enc <- enc[cohort, mget(names(enc)), on=c("outcome_id", "empi", "t0_date")]
  
  non_days_to_last_var <- setdiff(names(enc),grep("days_to_last", names(enc),value=T))
  set_na_zero(enc, subset_col=non_days_to_last_var)

  days_to_last_var <-grep("days_to_last", names(enc),value=T)
  set_na_zero(enc, subset_col=days_to_last_var, NA)

  #-------------------------------------------------------------------------------#
  # categorize variables to ensure proper treatment in models -- integer 
  enc_integer <- enc[, mget(setdiff(names(enc), c("outcome_id", "t0_date", "empi")))]
  enc_integer[, names(enc_integer):=lapply(.SD, function(x) as.integer(x))]

  enc <- cbind(enc[, mget(c("outcome_id", "t0_date", "empi"))], enc_integer)

  enc[, ':='(enc_time_min=time_min, enc_time_max=time_max)]
  
  if (length(grep("enc_id$|visit_id$", names(enc), value=T))>0) enc[, grep("enc_id$|visit_id$", names(enc), value=T):=NULL]

  feature_var_format_day_to_last(enc)

  #-------------------------------------------------------------------------------#
  # return utilization files & delete key files created in function
  rm(enc_integer)
  rm(enc_class_timeframe_comb)
  rm(list=unlist(enc_feature_list))

	return(enc)

}

#----------------------------------------------------------------------------#
