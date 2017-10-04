#----------------------------------------------------------------------------#

#' @title Generate diagnosis-related features (dia data).
#'
#' @description \
#'
#' @export
#' @param cohort
#' @param cohort_key_var_merge
#' @param dia_file_mod_arg
#' @param leak_dia_day
#' @param combine
#' @param dia_file_mod_ext
#' @param file_date_var
#' @return
#' @examples

 dia_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var, dia_file_mod_arg=dia_file_mod, 
  leak_dia_day_arg=leak_dia_day, combine=FALSE,dia_file_mod_ext=NA, file_date_var="dia_date") {

  print("launching dia_feature_gen")

  #-------------------------------------------------------------------------------#
  # Load the  modified/pre-processed dia file for the specified data sample -- 
    
  # load the data
   tryCatch(dia <- readRDS_merge(dia_file_mod_arg), warning=function(w)
    print("no classified dia file available for the data sample"))

  if (combine==TRUE) {
    tryCatch(dia_ext <- readRDS_merge(dia_file_mod_ext), warning=function(w)
      print("no classified dia file available for the data sample"))
    
    dia <- rbindlist(list(dia, dia_ext), fill=T, use.names=T)
    dia[, dia_id:=1:nrow(dia)]
 
  }

  # remove if empi is missing
  dia <- dia[!is.na(empi)]
  
  # remove rule out diagnoses
  dia <- dia[is.na(rule_out)]

  # subset to smaller sample for testing if so specified in control file
  if (test_raw_file==TRUE) {
    store_shorten_file("dia")
  }

  # subset to the variables which are to be employed in the feature construction process
  # select only diagnosis code related variables -- drop potential features (provider, 
  # clinic, hospital, inpatient_outpatient)
  dia[, c("clinic_name", "hospital", "inpatient_outpatient", "provider"):=NULL]

  # select ccs (single/multi & zc) - category names rather than numbers
  dia[, grep("num", names(dia)):=NULL]

  #-------------------------------------------------------------------------------#
  # merge diagnosis file with cohort (cohort_key_variables) & format dates

  dia <- dia[empi %in% cohort$empi]

  invisible(parse_date(dia, c("dia_date")))

  dia[, c("dia_date_1","dia_date_2"):=.(dia_date)]

  dia <-foverlaps(dia, cohort[, mget(cohort_key_var_merge)], by.x=c("empi",
    "dia_date_1", "dia_date_2"), nomatch=0)

  dia[, time_diff:=as.numeric(difftime(t0_date, get(file_date_var), 
    units="days"))]

  # implement leakage control
  if (!is.na(leak_dia_day_arg)) {
    dia <- dia[!(t0_date-dia_date_1<=leak_dia_day_arg)]
  }
  
  #-------------------------------------------------------------------------------#
  # sub setting & dividing into smaller DT based on timeframe  - 
  # return as list (...timeframe_comb)
  invisible(timeframe_split(list("dia"), "dia_date"))

  name_ext_extended <- name_ext_extended[sapply(dia_timeframe_comb, nrow)!=0]
  name_ext <- name_ext_extended[2:length(name_ext_extended)]
  dia_timeframe_comb <- dia_timeframe_comb[sapply(dia_timeframe_comb, nrow)!=0]
  
  time_min <- min(do.call("c", lapply(dia_timeframe_comb, function(x) as.Date(min(x[, 
    dia_date]), "%Y-%m-%d"))))
  time_max <- max(do.call("c", lapply(dia_timeframe_comb, function(x) as.Date(max(x[, 
    dia_date]), "%Y-%m-%d"))))

  #-------------------------------------------------------------------------------#
  # reshaping - create diagnosis code count vars 

  zc_timeframe_comb <- lapply(dia_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + t0_date ~  paste0("dia_dia.count_zc..", 
    zc_cat_name), fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff", 
    subset=.(!is.na(zc_cat_name) & zc_cat_name!="" )))

  zc_timeframe_comb <- feature_var_format(zc_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("dia_dia.count_zc", 
    names(DT), value=T), paste0(grep("dia_dia.count_zc", names(DT), value=T),name_ext)), 
    DT=zc_timeframe_comb,  name_ext_extended))

  #-------------------------------------------------------------------------------#
  # reshaping - create diagnosis code count vars

  zc_excl_cancer_timeframe_comb <- lapply(dia_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + t0_date ~  paste0("dia_dia.count_zc.excl.cancer..", 
    zc_cat_name), fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var="time_diff", 
    subset=.(!is.na(zc_cat_name) & zc_cat_name!="" & onc_dia==0)))

  zc_excl_cancer_timeframe_comb <- feature_var_format(zc_excl_cancer_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("dia_dia.count_zc.excl.cancer", 
    names(DT), value=T), paste0(grep("dia_dia.count_zc.excl.cancer", names(DT), value=T),name_ext)), 
    DT=zc_excl_cancer_timeframe_comb,  name_ext_extended))

  #-------------------------------------------------------------------------------#
  # reshaping - create diagnosis code count vars 

  zc_mod_timeframe_comb <- lapply(dia_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + t0_date ~  paste0("dia_dia.count_zc.mod..", 
    zc_mod_prim_onc_cat_name), fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var="time_diff", 
    subset=.(!is.na(zc_mod_prim_onc_cat_name) & zc_mod_prim_onc_cat_name!="" )))

  zc_mod_timeframe_comb <- feature_var_format(zc_mod_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("dia_dia.count_zc.mod", 
    names(DT), value=T), paste0(grep("dia_dia.count_zc.mod", names(DT), value=T),name_ext)), 
    DT=zc_mod_timeframe_comb,  name_ext_extended))

  #-------------------------------------------------------------------------------#
  # reshaping - create diagnosis code count vars 

  zc_mod_excl_cancer_timeframe_comb <- lapply(dia_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + t0_date ~  paste0("dia_dia.count_zc.mod.excl.cancer..", 
    zc_mod_prim_onc_cat_name), fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var="time_diff", 
    subset=.(!is.na(zc_mod_prim_onc_cat_name) & 
    zc_mod_prim_onc_cat_name!="" &  onc_dia==0)))

  zc_mod_excl_cancer_timeframe_comb <- feature_var_format(zc_mod_excl_cancer_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("dia_dia.count_zc.mod.excl.cancer", 
    names(DT), value=T), paste0(grep("dia_dia.count_zc.mod.excl.cancer", 
    names(DT), value=T),name_ext)), DT=zc_mod_excl_cancer_timeframe_comb,  name_ext_extended))


  #-------------------------------------------------------------------------------#
  # reshaping - create diagnosis code count vars 

  zc_mod_cancer_timeframe_comb <- lapply(dia_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + t0_date ~  paste0("dia_dia.count_zc.cancer.mod..", 
    zc_mod_prim_onc_cat_name),  fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var="time_diff", 
    subset=.(!is.na(zc_mod_prim_onc_cat_name) & 
    zc_mod_prim_onc_cat_name!="" &  onc_dia==1)))

  zc_mod_cancer_timeframe_comb <- feature_var_format(zc_mod_cancer_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("dia_dia.count_zc.cancer.mod", 
    names(DT), value=T), paste0(grep("dia_dia.count_zc.cancer.mod", names(DT), value=T),name_ext)), 
    DT=zc_mod_cancer_timeframe_comb,  name_ext_extended))

  #-------------------------------------------------------------------------------#
  # reshaping - create diagnosis code count vars 

  zc_cancer_detailed_timeframe_comb <- lapply(dia_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + t0_date ~  paste0("dia_dia.count_zc.cancer.detailed..", 
    zc_cancer_detailed_cat_name), fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var="time_diff", 
    subset=.(!is.na(zc_cancer_detailed_cat_name) & zc_cancer_detailed_cat_name!="" )))

 zc_cancer_detailed_timeframe_comb <- feature_var_format(zc_cancer_detailed_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("dia_dia.count_zc.cancer.detailed", 
    names(DT), value=T), paste0(grep("dia_dia.count_zc.cancer.detailed", names(DT), value=T),name_ext)), 
    DT=zc_cancer_detailed_timeframe_comb, name_ext_extended))

  #-------------------------------------------------------------------------------#
  # reshaping - create diagnosis code count vars 

  zc_cancer_detailed_excl_cancer_timeframe_comb <- lapply(dia_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + t0_date ~  paste0("dia_dia.count_zc.cancer.detailed.excl.cancer..", 
    zc_cancer_detailed_cat_name), fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var="time_diff", 
    subset=.(!is.na(zc_cancer_detailed_cat_name) & zc_cancer_detailed_cat_name!="" & 
     onc_dia==0)))

  zc_cancer_detailed_excl_cancer_timeframe_comb <- feature_var_format(zc_cancer_detailed_excl_cancer_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("dia_dia.count_zc.cancer.detailed.excl.cancer", 
    names(DT), value=T), paste0(grep("dia_dia.count_zc.cancer.detailed.excl.cancer", names(DT), value=T),name_ext)), 
    DT=zc_cancer_detailed_excl_cancer_timeframe_comb, name_ext_extended))

  #-------------------------------------------------------------------------------#
  # reshaping - create diagnosis code count vars 

  zc_cancer_detailed_cancer_timeframe_comb <- lapply(dia_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + t0_date ~  paste0("dia_dia.count_zc.cancer.detailed.cancer..", 
    zc_cancer_detailed_cat_name), fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var="time_diff", 
    subset=.(!is.na(zc_cancer_detailed_cat_name) & zc_cancer_detailed_cat_name!="" & 
     onc_dia==1)))

  zc_cancer_detailed_cancer_timeframe_comb <- feature_var_format(zc_cancer_detailed_cancer_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("dia_dia.count_zc.cancer.detailed.cancer", 
    names(DT), value=T), paste0(grep("dia_dia.count_zc.cancer.detailed.cancer", names(DT), value=T),name_ext)), 
    DT=zc_cancer_detailed_cancer_timeframe_comb, name_ext_extended))

  #-------------------------------------------------------------------------------#
  # reshaping - create diagnosis code count vars 

  ccs_single_timeframe_comb <- lapply(dia_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + t0_date ~  paste0("dia_dia.count_dia.single.ccs..", 
    ccs_single_cat_name), fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var="time_diff", 
    subset=.(!is.na(ccs_single_cat_name) & ccs_single_cat_name!="" )))

  ccs_single_timeframe_comb <- feature_var_format(ccs_single_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("dia_dia.count_dia.single.ccs", 
    names(DT), value=T), paste0(grep("dia_dia.count_dia.single.ccs", names(DT), value=T),
    name_ext)), DT=ccs_single_timeframe_comb, name_ext_extended))

  #-------------------------------------------------------------------------------#
  # reshaping - create diagnosis code count vars 
  ccs_multi_timeframe_comb <- lapply(dia_timeframe_comb, function(x)
    rbindlist(list(x[, .(outcome_id, empi, t0_date, ccs_multi_cat_name=ccs_multi_cat_name_1, time_diff)],
    x[, .(outcome_id, empi, t0_date, ccs_multi_cat_name=ccs_multi_cat_name_2, time_diff)], 
    x[, .(outcome_id, empi, t0_date, ccs_multi_cat_name=ccs_multi_cat_name_3, time_diff)],
    x[, .(outcome_id, empi, t0_date, ccs_multi_cat_name=ccs_multi_cat_name_4, time_diff)]),
    use.names=T))

  ccs_multi_timeframe_comb <- lapply(ccs_multi_timeframe_comb, function(x)
    dcast.data.table(x, outcome_id + empi + t0_date ~  paste0("dia_dia.count_dia.multi.ccs..",
    ccs_multi_cat_name), fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var="time_diff", 
    subset=.(!is.na(ccs_multi_cat_name)  & ccs_multi_cat_name!="" )))

  ccs_multi_timeframe_comb <- feature_var_format(ccs_multi_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("dia_dia.count_dia.multi.ccs", 
    names(DT), value=T), paste0(grep("dia_dia.count_dia.multi.ccs", names(DT), value=T),
    name_ext)), DT=ccs_multi_timeframe_comb, name_ext_extended))

  #-------------------------------------------------------------------------------#
  # generating gagne scores
  
  gagne_name <- gagne_code$gagne
  gagne_weight <- as.numeric(gsub("_", "-", gagne_code$weight))

  gagne_formula_exp <- ""

  gagne_formula  <- function(cat, weight, ext) {
    for (i in 1:length(cat)) {
      gagne_formula_exp <- paste(gagne_formula_exp, weight[i], "*", paste0(ext, 
        cat[i]), "+", sep=" ")
    }
    gagne_formula_exp <- gsub("\\+$", "",gagne_formula_exp)
    return(gagne_formula_exp)
  }

  # (b) reshaping - create diagnosis code count vars 
  gagne_timeframe_comb <- lapply(dia_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + t0_date ~  
    paste0("dia_dia.count_gagne.cat..", gagne), length, value.var = "gagne", 
    subset=.(!is.na(gagne)  & gagne!="" )))

  # (c) generate complete set of gagne category dummies (i.e. 0/1 if present 
  # or not at least once during time period)  - "dia_gagne.."
  gagne_timeframe_comb <- lapply(gagne_timeframe_comb, function(x) 
    x[, gsub("dia_dia.count_gagne.cat..", "dia_dia.score_gagne..", 
    grep("dia_dia.count_gagne.cat", names(x), value=T)) :=lapply(.SD, function(x) 
    ifelse(x>=1, 1,0)), .SDcols=grep("dia_dia.count_gagne.cat", names(x))])

  gagne_timeframe_comb <- lapply(gagne_timeframe_comb, function(x) 
    x[, setdiff(paste0("dia_dia.score_gagne..",gagne_name), grep("dia_dia.score_gagne..", 
    names(x), value=T)):=0])

  # (d) determine the gagne score 
  gagne_timeframe_comb <- lapply(gagne_timeframe_comb, function(x) 
    x[, dia_dia.score_gagne..score:=eval(parse(text=gagne_formula(gagne_name[!(gagne_weight==0)],  
    gagne_weight[!(gagne_weight==0)], "dia_dia.score_gagne..")))])
  gagne_timeframe_comb <- lapply(gagne_timeframe_comb, function(x) 
    x[, setdiff(grep("dia_dia.score_gagne\\.\\.", names(x), value=T), 
    "dia_dia.score_gagne..score"):=NULL])

  # (e) impose feature categorization ("_short/_long")
  invisible(mapply(function(DT,name_ext) setnames(DT, grep("gagne", 
    names(DT), value=T), paste0(grep("gagne", names(DT), value=T),
    name_ext)), DT=gagne_timeframe_comb, name_ext_extended))

  #-------------------------------------------------------------------------------#
  # merge dia feature files
  dia_feature_list <- list("zc_timeframe_comb", "zc_mod_timeframe_comb", 
    "zc_cancer_detailed_timeframe_comb",  
    "zc_excl_cancer_timeframe_comb","zc_mod_excl_cancer_timeframe_comb", 
    "zc_mod_cancer_timeframe_comb","zc_cancer_detailed_excl_cancer_timeframe_comb",
    "zc_cancer_detailed_cancer_timeframe_comb","ccs_single_timeframe_comb",
    "ccs_multi_timeframe_comb", "gagne_timeframe_comb")

  timeframe_combine(dia_feature_list)

  dia <- Reduce(mymerge, mget(unlist(dia_feature_list)))


  #-------------------------------------------------------------------------------#
  # merge with cohort file - empty records -> 0
  dia <- dia[cohort, mget(names(dia)), on=c("outcome_id", "empi", "t0_date")]
 
  non_days_to_last_var <- setdiff(names(dia),grep("days_to_last", names(dia),value=T))
  set_na_zero(dia, subset_col=non_days_to_last_var)

  #-------------------------------------------------------------------------------#
  # categorize variables to ensure proper treatment in models -- integer 
  dia_integer <- dia[, mget(setdiff(names(dia), c("outcome_id", "t0_date", "empi")))]
  dia_integer[, names(dia_integer):=lapply(.SD, function(x) as.integer(x))]

  dia <- cbind(dia[, mget(c("outcome_id", "t0_date", "empi"))], dia_integer)

  dia[, ':='(dia_time_min=time_min, dia_time_max=time_max)]

  dia[, grep("dia_id$", names(dia), value=T):=NULL]

  ## deal with date variables
  feature_var_format_2(dia)

  #-------------------------------------------------------------------------------#
  # return dia & delete key files 
  rm(dia_integer)
  rm(dia_timeframe_comb)
  rm(list=unlist(dia_feature_list))
  
  return (dia)

}


#----------------------------------------------------------------------------#

