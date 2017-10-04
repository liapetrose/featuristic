#----------------------------------------------------------------------------#

#' @title Generate procedure-related features (prc data).
#'
#' @description /
#'
#' @export
#' @param file_date_var 
#' @param cohort
#' @param cohort_key_var_merge
#' @return
#' @examples


prc_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var, file_date_var="prc_date") {
  
  print("launching prc_feature_gen")
  
  #-------------------------------------------------------------------------#
  # Load the  modified/pre-processed prc file for the specified data sample -- 
    
  # load the data
  tryCatch(prc <- readRDS_merge(prc_file_mod), warning=function(w)
    print("no classified prc file available for the data sample"))

  # subset to smaller sample for testing if so specified in control file
  if (test_raw_file==TRUE) {
    store_shorten_file("prc")
  }
  
  # subset to the variables which are to be employed in the feature construction process

  # select only prognosis code related variables -- drop potential features (provider, 
  # clinic, hospital, inpatient_outpatient)
  prc[, c("prc_code_type","clinic_name", "hospital", "inpatient_outpatient", "provider"):=NULL]

  # select ccs (single/multi & zc) - category names rather than numbers
  prc[, grep("num", names(prc)):=NULL]

  #-------------------------------------------------------------------------#
  # merge procedure file with cohort (cohort_key_variables) & format dates
  prc <- prc[empi %in% cohort$empi]

  invisible(parse_date(prc, c("prc_date")))

  prc[, c("prc_date_1","prc_date_2"):=.(prc_date)]

  prc <-foverlaps(prc, cohort[, mget(cohort_key_var_merge)], 
    by.x=c("empi","prc_date_1" , "prc_date_2"), nomatch=0)

  prc[, time_diff:=as.numeric(difftime(t0_date, get(file_date_var), 
    units="days"))]

  # implement leakage control 
  if (!is.na(leak_prc_day)) {
    prc <- prc[!(t0_date-prc_date_1<=leak_prc_day)]
  }

  #-------------------------------------------------------------------------#
  # sub setting & dividing into smaller DT based on timeframe  - 
  # return as list (...timeframe_comb)
  invisible(timeframe_split(list("prc"), "prc_date"))

  name_ext_extended <- name_ext_extended[sapply(prc_timeframe_comb, nrow)!=0]
  name_ext <- name_ext_extended[2:length(name_ext_extended)]
  prc_timeframe_comb <- prc_timeframe_comb[sapply(prc_timeframe_comb, nrow)!=0]

  time_min <- min(do.call("c", lapply(prc_timeframe_comb, function(x) as.Date(min(x[, 
    prc_date]), "%Y-%m-%d"))))
  time_max <- max(do.call("c", lapply(prc_timeframe_comb, function(x) as.Date(max(x[, 
    prc_date]), "%Y-%m-%d"))))

  #-------------------------------------------------------------------------#
  # reshaping - create procedure code count vars (ccs_single) & impose feature 
  # categorization ("prc_single.ccs.."..."_short/_long")
  ccs_single_timeframe_comb <- lapply(prc_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + t0_date ~  paste0("prc_prc.count_prc.single.ccs..", 
    ccs_single_cat_name), fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff", 
    subset=.(!is.na(ccs_single_cat_name) & ccs_single_cat_name!="" )))

  ccs_single_timeframe_comb <- feature_var_format(ccs_single_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("prc_prc.count_prc.single.ccs", 
    names(DT), value=T), paste0(grep("prc_prc.count_prc.single.ccs", names(DT), value=T),
    name_ext)), DT=ccs_single_timeframe_comb, name_ext_extended))


  #-------------------------------------------------------------------------#
  # reshaping - create prognosis code count vars (ccs_multi) & impose feature 
  # categorization ("prc_multi.ccs.."..."_short/_long")
  ccs_multi_timeframe_comb <- lapply(prc_timeframe_comb, function(x)
    rbindlist(list(x[, .(outcome_id, empi, t0_date, ccs_multi_cat_name=ccs_multi_cat_name_1, time_diff)],
    x[, .(outcome_id, empi, t0_date, ccs_multi_cat_name=ccs_multi_cat_name_2, time_diff)], 
    x[, .(outcome_id, empi, t0_date, ccs_multi_cat_name=ccs_multi_cat_name_3, time_diff)]),
    use.names=T))

  ccs_multi_timeframe_comb <- lapply(ccs_multi_timeframe_comb, function(x)
    dcast.data.table(x, outcome_id + empi + t0_date ~  paste0("prc_prc.count_prc.multi.ccs..",
    ccs_multi_cat_name), fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff", 
    subset=.(!is.na(ccs_multi_cat_name) & ccs_multi_cat_name!="" )))

  ccs_multi_timeframe_comb <- feature_var_format(ccs_multi_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("prc_prc.count_prc.multi.ccs", 
    names(DT), value=T), paste0(grep("prc_prc.count_prc.multi.ccs", names(DT), value=T),
    name_ext)), DT=ccs_multi_timeframe_comb, name_ext_extended))

  #-------------------------------------------------------------------------#
  # merge prc feature files
  prc_feature_list <- list("ccs_single_timeframe_comb","ccs_multi_timeframe_comb")

  timeframe_combine(prc_feature_list)

  prc <- Reduce(mymerge, mget(unlist(prc_feature_list)))
  
  #-------------------------------------------------------------------------#
  # merge with cohort file - empty records -> 0
  prc <- prc[cohort, mget(names(prc)), on=c("outcome_id", "empi", "t0_date")]

  non_days_to_last_var <- setdiff(names(prc),grep("days_to_last", names(prc),value=T))
  set_na_zero(prc, subset_col=non_days_to_last_var)

  #-------------------------------------------------------------------------#
  # categorize variables to ensure proper treatment in models -- integer 
  prc_integer <- prc[, mget(setdiff(names(prc), c("outcome_id", "t0_date", "empi")))]
  prc_integer[, names(prc_integer):=lapply(.SD, function(x) as.integer(x))]

  prc <- cbind(prc[, mget(c("outcome_id", "t0_date", "empi"))], prc_integer)

  prc[, ':='(prc_time_min=time_min, prc_time_max=time_max)]

  prc[, grep("prc_id$", names(prc), value=T):=NULL]

  ## deal with date variables
  feature_var_format_2(prc)

  #-------------------------------------------------------------------------#
  # return prc & delete key files
  rm(prc_integer)
  rm(prc_timeframe_comb)
  rm(list=unlist(prc_feature_list))

  return (prc)

}

#----------------------------------------------------------------------------#

