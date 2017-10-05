#----------------------------------------------------------------------------#

#' @title Generate DFCI enc-related features (enc_oncdrs data).
#'
#' @description \
#'
#' @export
#' @param cohort
#' @param cohort_key_var_merge
#' @return
#' @examples

 enc_oncdrs_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var) {
  
  print("launching enc_oncdrs_feature_gen")
  
  #-------------------------------------------------------------------------------#
  # Load the  modified/pre-processed enc file for the specified data sample -- 
  
  # load the data
  tryCatch(enc_oncdrs <- readRDS_merge(enc_oncdrs_file_mod), warning=function(w)
    print("no classified enc file available for the data sample"))

  # omit missing empi observations
  enc_oncdrs <- enc_oncdrs[!is.na(empi)]
  
  # subset to smaller sample for testing if so specified in control file
  if (test_raw_file==TRUE) {
    store_shorten_file("enc_oncdrs")
  }

  #-------------------------------------------------------------------------------#
  # keep only the relevant columns 

  enc_oncdrs <- enc_oncdrs[, setdiff(names(enc_oncdrs), c(grep("enc_type|enc_dept",
  	names(enc_oncdrs), value=T), "empi", "patient_id", "adm_date")):=NULL]


  #-------------------------------------------------------------------------------#
  # merge enc_oncdrs file with cohort (cohort_key_variables) & format dates
  enc_oncdrs <- enc_oncdrs[empi %in% cohort$empi]

  invisible(parse_date(enc_oncdrs, c("adm_date")))

  enc_oncdrs[, c("adm_date_1","adm_date_2"):=.(adm_date)]

  enc_oncdrs <-foverlaps(enc_oncdrs, cohort[, mget(cohort_key_var_merge)], 
  	by.x=c("empi","adm_date_1", "adm_date_2"), nomatch=0)


  # implement leakage control 
  if (!is.na(leak_oncdrs_enc_day)) {
    enc_oncdrs <- enc_oncdrs[!(t0_date-adm_date_1<=leak_oncdrs_enc_day)]
  }

  #-------------------------------------------------------------------------------#
  # sub setting & dividing into smaller DT based on timeframe  - 
  # return as list (...timeframe_comb)
  invisible(timeframe_split(list("enc_oncdrs"), "adm_date"))

  name_ext_extended <- name_ext_extended[sapply(enc_oncdrs_timeframe_comb, nrow)!=0]
  name_ext <- name_ext_extended[2:length(name_ext_extended)]
  enc_oncdrs_timeframe_comb <- enc_oncdrs_timeframe_comb[sapply(
    enc_oncdrs_timeframe_comb, nrow)!=0]

  time_min <- min(do.call("c", lapply(enc_oncdrs_timeframe_comb, function(x) as.Date(min(x[, 
    adm_date]), "%Y-%m-%d"))))
  time_max <- max(do.call("c", lapply(enc_oncdrs_timeframe_comb, function(x) as.Date(max(x[, 
    adm_date]), "%Y-%m-%d"))))

  enc_type_timeframe_comb <- lapply(enc_oncdrs_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + t0_date ~  
      paste0("enc.dfci_enc.enc_enc.count_enc.type.count..", 
      enc_type), length, subset=.(!enc_type=="" & !is.na(enc_type))))

  invisible(mapply(function(DT,name_ext) setnames(DT, 
    grep("enc.dfci_enc.enc_enc.count_enc.type.count", 
    names(DT), value=T), gsub("\\*", "",
    paste0(grep("enc.dfci_enc.enc_enc.count_enc.type.count", names(DT), value=T),
    name_ext))), DT=enc_type_timeframe_comb ,  name_ext_extended))

  enc_dept_timeframe_comb <- lapply(enc_oncdrs_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + t0_date ~  
      paste0("enc.dfci_enc.enc_enc.count_enc.department.count..", 
      enc_dept), length, subset=.(!enc_dept=="" & !is.na(enc_dept))))

  invisible(mapply(function(DT,name_ext) setnames(DT, 
    grep("enc.dfci_enc.enc_enc.count_enc.department.count", 
    names(DT), value=T), paste0(grep("enc.dfci_enc.enc_enc.count_enc.department.count", 
    names(DT), value=T),name_ext)), DT=enc_dept_timeframe_comb ,  name_ext_extended))

 
  #-------------------------------------------------------------------------------#
  # merge enc_oncdrs feature files
  enc_oncdrs_feature_list <- list("enc_type_timeframe_comb", "enc_dept_timeframe_comb")

  timeframe_combine(enc_oncdrs_feature_list)

  enc_oncdrs <- Reduce(mymerge, mget(unlist(enc_oncdrs_feature_list)))

  #-------------------------------------------------------------------------------#
  # merge with cohort file - empty records -> 0
  enc_oncdrs <- enc_oncdrs[cohort, mget(names(enc_oncdrs)), on=c("outcome_id", "empi", 
    "t0_date")]
  set_na_zero(enc_oncdrs)

  #-------------------------------------------------------------------------------#
  # categorize variables to ensure proper treatment in models -- integer 
  enc_oncdrs_integer <- enc_oncdrs[, mget(setdiff(names(enc_oncdrs), c("outcome_id", 
    "t0_date", "empi")))]
  enc_oncdrs_integer[, names(enc_oncdrs_integer):=lapply(.SD, function(x) as.integer(x))]

  enc_oncdrs <- cbind(enc_oncdrs[, mget(c("outcome_id", "t0_date", "empi"))], enc_oncdrs_integer)

  # set names
  var_rename <- setdiff(names(enc_oncdrs), cohort_key_var_merge)
  setnames(enc_oncdrs,  var_rename , paste0(gsub("_", "_dfci.", gsub("(.*)(\\.\\.)(.*)", "\\1", 
    var_rename)), "..", gsub("(.*)(\\.\\.)(.*)", "\\3", 
    var_rename)))

  enc_oncdrs[, ':='(enc_oncdrs_time_min=time_min, enc_oncdrs_time_max=time_max)]

  if (length(grep("enc_id$", names(enc_oncdrs), value=T))>0) enc_oncdrs[, grep("enc_id$", names(enc_oncdrs), value=T):=NULL]

  #-------------------------------------------------------------------------------#
  # return enc_oncdrs & delete key files 
  rm(enc_oncdrs_integer)
  rm(enc_oncdrs_timeframe_comb)
  rm(list=unlist(enc_oncdrs_feature_list))
  
  return(enc_oncdrs)

}


#----------------------------------------------------------------------------#




