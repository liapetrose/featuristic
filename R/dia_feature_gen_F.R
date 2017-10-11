#----------------------------------------------------------------------------#

#' @title Generate diagnosis-related features (dia data).
#'
#' @description \
#'
#' @export
#' @param cohort
#' @param cohort_key_var_merge
#' @param cohort_key_var
#' @param dia_file_mod_arg
#' @param leak_dia_day
#' @param combine
#' @param dia_file_mod_ext
#' @param file_date_var
#' @return
#' @examples

 dia_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var, dia_file_mod_arg=dia_file_mod, 
  leak_dia_day_arg=leak_dia_day, combine=FALSE, dia_file_mod_ext=NA, file_date_var="dia_date") {

  print("launching dia_feature_gen")

  #-------------------------------------------------------------------------------#
  # SETUP
  #-------------------------------------------------------------------------------#

  # Load the data & subset
  #-------------------------------------------------------------------------------#
    
  # load data
   tryCatch(dia <- readRDS_merge(dia_file_mod_arg), error=function(e)
    print("no classified dia file available for the data sample"))

  # ensure that date variables exist
  if (!(paste0(file_date_var, "_1") %in% names(dia))) dia[, 
    c(paste0(file_date_var, "_1")):=get(file_date_var)]

  # combine 
  if (combine==TRUE) {
    
    tryCatch(dia_ext <- readRDS_merge(dia_file_mod_ext), error=function(e)
      print("no classified dia file available for the data sample"))
    if (!(paste0(file_date_var, "_1") %in% names(dia_ext))) dia_ext[, 
      c(paste0(file_date_var, "_1")):=get(file_date_var)]

    # ensure that dates are standarised & aligned
    invisible(format_date(list(dia, dia_ext), c(file_date_var,paste0(file_date_var, "_1"))))
    
    # merge
    dia <- rbindlist(list(dia_ext, dia), fill=T, use.names=T)

    dia[, dia_id:=1:nrow(dia)]
 
  }


  print(sprintf("number of observations: %d", nrow(dia)))

  # subset to cohort
  dia <- dia[!is.na(empi)]
  dia <- dia[empi %in% cohort$empi]

  print(sprintf("number of observations - cohort subset: %d", nrow(dia)))
  
  ## SAMPLE SIZE CHECK #1
  assert("observations for cohort patients", nrow(dia)>0)

  # subset to smaller sample (if test_mode==TRUE)
  if (test_mode==TRUE) {
    
    dia <- dia[1:min(test_row, nrow(dia))]

  }
  
  #-------------------------------------------------------------------------------#
  # ADDITIONAL SUBSETTING / FORMATTING
  #-------------------------------------------------------------------------------#

  # remove rule out diagnoses
  dia <- dia[is.na(rule_out)]

  # subset to the variables which are to be employed in the feature construction process
  suppressWarnings(dia[, c("clinic_name", "hospital", "inpatient_outpatient", "provider"):=NULL])

  # select ccs (single/multi & zc) - category names rather than numbers
  suppressWarnings(dia[, grep("num", names(dia)):=NULL])

  #-------------------------------------------------------------------------------#
  # MERGING & TIMEFRAMES
  #-------------------------------------------------------------------------------#

  # Date Formatting & Merge data file with cohort & Implement leakage control
  #-------------------------------------------------------------------------------#
 
  # date formatting
  invisible(format_date(list(dia), c(file_date_var,paste0(file_date_var, "_1"))))

  # foverlaps merge
  dia[, c(paste0(file_date_var, "_1"),paste0(file_date_var, "_2")):=.(get(file_date_var))]

  dia <-  foverlaps(dia, cohort[, mget(cohort_key_var_merge)], by.x=c("empi",
    paste0(file_date_var, "_1"), paste0(file_date_var, "_2")), nomatch=0)

  print(sprintf("number of observations - cohort subset within the relevant timeframe(s): %d", nrow(dia)))

  ## SAMPLE SIZE CHECK #2
  assert("observations - cohort subset within the relevant timeframe(s)", nrow(dia)>0)

  # time difference calculation (event date vs. t0 date)
  dia[, time_diff:=as.numeric(difftime(t0_date, get(file_date_var), 
    units="days"))]

  # implement leakage control 
  if (!is.na(leak_dia_day_arg)) {
    
    dia <- dia[!(t0_date-get(paste0(file_date_var, "_1"))<=leak_dia_day_arg)]

  }

  # split data into feature timeframes
  #-------------------------------------------------------------------------------#
  return_mult[dia_timeframe_comb, time_min, time_max] <- timeframe_split(list("dia"), file_date_var)[[1]]
  name_ext_extended_tmp                               <- gsub("timeframe_comb", "", names(dia_timeframe_comb))
  name_ext_tmp                                        <- name_ext_extended_tmp[2:length(name_ext_extended_tmp)]

  ## SAMPLE SIZE CHECK #3
  assert("observations for cohort patients within relevant timeframes", 
    all(sapply(dia_timeframe_comb, function(x) nrow(x)>0)))

  #-------------------------------------------------------------------------------#
  # FEATURE GENERATION
  #-------------------------------------------------------------------------------#

  # [1] Zocat Features 
  #-------------------------------------------------------------------------------#
  print("feature generation - dia_dia.count_zc")

  zc_timeframe_comb <- lapply(dia_timeframe_comb, function(x) 
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date ~  paste0('dia_dia.count_zc..', zc_cat_name)"), 
    value.var = "time_diff", subset=non_missing("zc_cat_name"), mode="basic"))

  zc_timeframe_comb <- feature_var_format(zc_timeframe_comb)

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("dia_dia.count_zc", 
    names(DT), value=T), new=paste0(grep("dia_dia.count_zc", names(DT), value=T),name_ext_tmp)), 
    DT=zc_timeframe_comb,  name_ext_extended_tmp)

  # [2] Zocat Mod Features (Modified Cancer Classification - Primary/Secondary Breakdown)
  #-------------------------------------------------------------------------------#
  print("feature generation - dia_dia.count_zc.mod")

  zc_mod_timeframe_comb <- lapply(dia_timeframe_comb, function(x) 
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date ~  paste0('dia_dia.count_zc.mod..', zc_mod_prim_onc_cat_name)"), 
    value.var="time_diff", subset=non_missing("zc_mod_prim_onc_cat_name"), mode="basic"))

  zc_mod_timeframe_comb <- feature_var_format(zc_mod_timeframe_comb)

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("dia_dia.count_zc.mod", 
    names(DT), value=T), new=paste0(grep("dia_dia.count_zc.mod", names(DT), value=T),name_ext_tmp)), 
    DT=zc_mod_timeframe_comb,  name_ext_extended_tmp)

  # [3] Single CCS Features
  #-------------------------------------------------------------------------------#
  print("feature generation - dia_dia.count_dia.single.ccs")

  ccs_single_timeframe_comb <- lapply(dia_timeframe_comb, function(x) 
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date ~  paste0('dia_dia.count_dia.single.ccs..', ccs_single_cat_name)"), 
    value.var="time_diff", subset=non_missing("ccs_single_cat_name"),mode="basic"))

  ccs_single_timeframe_comb <- feature_var_format(ccs_single_timeframe_comb)

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("dia_dia.count_dia.single.ccs", 
    names(DT), value=T), new=paste0(grep("dia_dia.count_dia.single.ccs", names(DT), value=T),
    name_ext_tmp)), DT=ccs_single_timeframe_comb, name_ext_extended_tmp)

  # [4] Multi CCS features
  #-------------------------------------------------------------------------------#
  print("feature generation - dia_dia.count_dia.multi.ccs")

  # [*] combine
  ccs_multi_timeframe_comb <- lapply(dia_timeframe_comb, function(x)
    rbindlist(list(x[, .(outcome_id, empi, t0_date, ccs_multi_cat_name=ccs_multi_cat_name_1, time_diff)],
    x[, .(outcome_id, empi, t0_date, ccs_multi_cat_name=ccs_multi_cat_name_2, time_diff)], 
    x[, .(outcome_id, empi, t0_date, ccs_multi_cat_name=ccs_multi_cat_name_3, time_diff)],
    x[, .(outcome_id, empi, t0_date, ccs_multi_cat_name=ccs_multi_cat_name_4, time_diff)]),
    use.names=T))

  ccs_multi_timeframe_comb <- lapply(ccs_multi_timeframe_comb, function(x)
    dcast.data.table_check(data=x, 
    outcome_id + empi + t0_date ~  paste0("dia_dia.count_dia.multi.ccs..", ccs_multi_cat_name), 
    value.var="time_diff", subset=non_missing("ccs_multi_cat_name"),mode="basic"))

  ccs_multi_timeframe_comb <- feature_var_format(ccs_multi_timeframe_comb)

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("dia_dia.count_dia.multi.ccs", 
    names(DT), value=T), new=paste0(grep("dia_dia.count_dia.multi.ccs", names(DT), value=T),
    name_ext_tmp)), DT=ccs_multi_timeframe_comb, name_ext_extended_tmp)

  # [5] Gagne Features
  #-------------------------------------------------------------------------------#
  print("feature generation - dia_dia.count_gagne.cat AND dia_dia.score_gagne..score")
  
  # [*] initialize
  gagne_name        <- gagne_code$gagne
  gagne_weight      <- as.numeric(gsub("_", "-", gagne_code$weight))
  gagne_formula_exp <- ""
  
  # [*] generate the gagne weighting formula
  gagne_formula  <- function(cat, weight, ext) {
    for (i in 1:length(cat)) {
      gagne_formula_exp <- paste(gagne_formula_exp, weight[i], "*", paste0(ext, 
        cat[i]), "+", sep=" ")
    }
    gagne_formula_exp <- gsub("\\+$", "",gagne_formula_exp)
    return(gagne_formula_exp)
  }

  # Gagne Count Features
  #------------------------------------------------------#

  gagne_timeframe_comb <- lapply(dia_timeframe_comb, function(x) 
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date ~  paste0('dia_dia.count_gagne.cat..', gagne)"), 
    value.var = "gagne", subset=non_missing("gagne"), mode="length"))

  # count indicators > binary indicators used in the score calculation
  inv_lapply(gagne_timeframe_comb, function(x) {

    if(length(grep("dia_dia.count_gagne.cat", names(x), value=T))>0) {

      x[, gsub("dia_dia.count_gagne.cat..", "dia_dia.score_gagne..", 
        grep("dia_dia.count_gagne.cat", names(x), value=T)) :=lapply(.SD, function(x) 
        ifelse(x>=1,1,0)), .SDcols=grep("dia_dia.count_gagne.cat", names(x), value=T)]

    }
  })

  # create non-existing binary indicators (for the score calculation)
  inv_lapply(gagne_timeframe_comb, function(x) {

    if(length(setdiff(paste0("dia_dia.score_gagne..",gagne_name), grep("dia_dia.score_gagne..", 
      names(x), value=T)))>0) {

      x[, setdiff(paste0("dia_dia.score_gagne..",gagne_name), grep("dia_dia.score_gagne..", 
        names(x), value=T)):=0]

    }
  })
    
  # Gagne Score Feature
  #------------------------------------------------------#
  gagne_timeframe_comb <- lapply(gagne_timeframe_comb, function(x) 
    x[, dia_dia.score_gagne..score:=eval(parse(text=gagne_formula(gagne_name[!(gagne_weight==0)],  
    gagne_weight[!(gagne_weight==0)], "dia_dia.score_gagne..")))])

  gagne_timeframe_comb <- suppressWarnings(lapply(gagne_timeframe_comb, function(x) 
    x[, setdiff(grep("dia_dia.score_gagne\\.\\.", names(x), value=T), 
    "dia_dia.score_gagne..score"):=NULL]))

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("gagne", 
    names(DT), value=T), new=paste0(grep("gagne", names(DT), value=T),
    name_ext_tmp)), DT=gagne_timeframe_comb, name_ext_extended_tmp)

  #-------------------------------------------------------------------------------#
  # FEATURE MERGING
  #-------------------------------------------------------------------------------#

  print("feature merging")

  dia_feature_list <- list("zc_timeframe_comb", "zc_mod_timeframe_comb", 
    "ccs_single_timeframe_comb","ccs_multi_timeframe_comb", "gagne_timeframe_comb")
  dia_feature_list <- dia_feature_list[which(dia_feature_list %in% ls())]

  # combine features across feature timeframes
  dia_list_tmp <- timeframe_combine(mget(unlist(dia_feature_list)), dia_feature_list)

  # combine features across feature types
  dia <- Reduce(mymerge, dia_list_tmp)

  #-------------------------------------------------------------------------------#
  # MERGE FEATURES WITH COHORT & FORMAT VARIABLES
  #-------------------------------------------------------------------------------#

  print("cohort merging & feature formatting")

  # merge
  dia <- dia[cohort, mget(names(dia)), on=c("outcome_id", "empi", "t0_date")]
  suppressWarnings(dia[, grep("dia_id$", names(dia), value=T):=NULL])

  # format variables
  dia <- feature_type_format(dt=dia, day_to_last=timeframe_day_to_last, 
    num_feature_pattern=NA, int_feature_pattern="...", factor_feature_pattern=NA)

  # add in additional var / drop unnecessary variables
  dia[, ':='(dia_time_min=time_min, dia_time_max=time_max)]

  #-------------------------------------------------------------------------------#
  # CLEAR MEMORY & RETURN FEATURE SET
  #-------------------------------------------------------------------------------#
  rm(dia_timeframe_comb)
  rm(list=unlist(dia_feature_list))
  
  return (dia)

}

#----------------------------------------------------------------------------#

