#----------------------------------------------------------------------------#

#' @title Generate microbiology-related features (mic data).
#'
#' @description \
#'
#' @export
#' @param file_date_var
#' @param cohort
#' @param cohort_key_var_merge
#' @param cohort_key_var
#' @param mic_file_mod_arg
#' @param leak_mic_day_arg
#' @return
#' @examples

mic_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var, file_date_var="mic_date",
  mic_file_mod_arg=mic_file_mod,leak_mic_day_arg=leak_mic_day) {
  
  print("launching mic_feature_gen")
    
  #-------------------------------------------------------------------------------#
  # SETUP
  #-------------------------------------------------------------------------------#

  # Load the data & subset
  #-------------------------------------------------------------------------------#
    
  # load data
  tryCatch(mic <- readRDS_merge(mic_file_mod_arg), error=function(e)
    print("no classified mic file available for the data sample"))

  # ensure that date variables exist
  if (!(paste0(file_date_var, "_1") %in% names(mic))) mic[, 
    c(paste0(file_date_var, "_1")):=get(file_date_var)]


  print(sprintf("number of observations: %d", nrow(mic)))

  # subset to cohort
  mic <- mic[!is.na(empi)]
  mic <- mic[empi %in% cohort$empi]

  print(sprintf("number of observations - cohort subset: %d", nrow(mic)))
  
  ## SAMPLE SIZE CHECK #1
  assert("observations for cohort patients", nrow(mic)>0)

  # subset to smaller sample (if test_mode==TRUE)
  if (test_mode==TRUE) {
    
    mic <- mic[1:min(test_row, nrow(mic))]

  }

  #-------------------------------------------------------------------------------#
  # ADDITIONAL SUBSETTING / FORMATTING
  #-------------------------------------------------------------------------------#

  # drop irrelevant variables
  mic[, c("mic_time"):=NULL]

  # subset to positive/tier1 or 2 results
  mic <- mic[result=="positive" & result_tier %in% c("tier_1", 
    "tier_2")]

  # group certain and uncertain organism classifications
  mic[org_genus %like% "(uncertain_result)", 
    org_genus:=gsub("_\\(uncertain_result\\)", "", org_genus)]

  print(sprintf("number of observations - cohort subset and result subset: %d", nrow(mic)))

  #-------------------------------------------------------------------------------#
  # MERGING & TIMEFRAMES
  #-------------------------------------------------------------------------------#

  # Date Formatting & Merge data file with cohort & Implement leakage control
  #-------------------------------------------------------------------------------#
 
  # date formatting
  invisible(format_date(list(mic), c(file_date_var,paste0(file_date_var, "_1"))))

  # foverlaps merge
  mic[, c(paste0(file_date_var, "_1"),paste0(file_date_var, "_2")):=.(get(file_date_var))]

  mic <-  foverlaps(mic, cohort[, mget(cohort_key_var_merge)], by.x=c("empi",
    paste0(file_date_var, "_1"), paste0(file_date_var, "_2")), nomatch=0)

  print(sprintf("number of observations - cohort subset within the relevant timeframe(s): %d", nrow(mic)))

  ## SAMPLE SIZE CHECK #2
  assert("observations - cohort subset within the relevant timeframe(s)", nrow(mic)>0)

  # time difference calculation (event date vs. t0 date)
  mic[, time_diff:=as.numeric(difftime(t0_date, get(file_date_var), 
    units="days"))]

  # implement leakage control 
  if (!is.na(leak_mic_day_arg)) {
    
    mic <- mic[!(t0_date-get(paste0(file_date_var, "_1"))<=leak_mic_day_arg)]

  }

  # split data into feature timeframes
  #-------------------------------------------------------------------------------#
  return_mult[mic_timeframe_comb, time_min, time_max] <- timeframe_split(list("mic"), file_date_var)[[1]]
  name_ext_extended_tmp                               <- gsub("timeframe_comb", "", names(mic_timeframe_comb))
  name_ext_tmp                                        <- name_ext_extended_tmp[2:length(name_ext_extended_tmp)]

  ## SAMPLE SIZE CHECK #3
  assert("observations for cohort patients within relevant timeframes", 
    all(sapply(mic_timeframe_comb, function(x) nrow(x)>0)))

  #-------------------------------------------------------------------------------#
  # FEATURE GENERATION
  #-------------------------------------------------------------------------------#

  # [1] Result count features (tier1/tier2)
  #-------------------------------------------------------------------------------#
  print("feature generation - mic_mic.pos.res_mic.blood.urine..mic_count")

  mic_result_timeframe_comb <- lapply(mic_timeframe_comb, function(x) 
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date ~  result + paste0('mic.', result_tier)"), 
    value.var = "time_diff", mode="basic"))
  
  mic_result_timeframe_comb <- feature_var_format(mic_result_timeframe_comb)

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("positive", 
    names(DT), value=T), new=paste0("mic_mic.pos.res_mic.blood.urine_", gsub("_", ".", 
    gsub("positive_", "",grep("positive", names(DT), value=T))), paste0("_mic.count.", 
    gsub("_", ".", gsub(".*(tier_[1-2]).*", "\\1", grep("positive", names(DT), value=T))), "..any"), 
    name_ext_tmp)), DT=mic_result_timeframe_comb , name_ext_extended_tmp)

  inv_lapply(mic_result_timeframe_comb, function(x) 
    setnames_check(x, new=gsub("days\\.to\\.last_(mic\\.count\\.tier\\.[0-9]\\.\\.any_)(.*$)", 
    "\\1day_to_last_\\2", names(x))))

  # [2] Result count features by organism category (tier1/tier2)
  #-------------------------------------------------------------------------------#
  print("feature generation - mic_mic.pos.res_mic.blood.urine...mic_cat_count")

  mic_result_cat_timeframe_comb <- lapply(mic_timeframe_comb, function(x) 
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date ~  result + paste0('mic.', result_tier) + org_genus"), 
    value.var = "time_diff",mode="basic"))

  mic_result_cat_timeframe_comb <- feature_var_format(mic_result_cat_timeframe_comb)

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("positive", 
    names(DT), value=T), new=paste0("mic_mic.pos.res_mic.blood.urine_", 
    gsub("([1-2])\\_", "\\1_mic.cat.count.tier.\\1..", 
    gsub("_([1-2])", ".\\1", gsub("positive_", "", grep("positive", names(DT), value=T)))), 
    name_ext_tmp)), DT=mic_result_cat_timeframe_comb , name_ext_extended_tmp)

  # [3] Antibiotic resistance feature (tier 1 - resistant to at least one antibiotic)
  #-------------------------------------------------------------------------------#
  print("feature generation - mic_mic.pos.res_mic.blood.urine_mic.tier.1_mic.resistant.count.tier.1")

  mic_result_resistant_timeframe_comb <- lapply(mic_timeframe_comb, function(x) 
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date ~  paste0('resistant', tier_1_anti_resis)"), 
    mode="length", value.var = "time_diff",
    subset=as.character(as.quoted("result_tier=='tier_1' & tier_1_anti_resis==1"))))

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("resistant", 
    names(DT), value=T), 
    new=paste0("mic_mic.pos.res_mic.blood.urine_mic.tier.1_mic.resistant.count.tier.1..", 
    gsub("[1-2]", "", grep("resistant", names(DT), value=T)), name_ext_tmp)), 
    DT=mic_result_resistant_timeframe_comb , name_ext_extended_tmp)

  # [4] Antibiotic resistance feature by the number of antibiotics to which resistant 
  #     (tier 1 - resistant to at least one antibiotic))
  #-------------------------------------------------------------------------------#
  print("feature generation - mic_mic.pos.res_mic.blood.urine_mic.tier.1_mic.resistant.anti.count.tier.1")

  mic_result_resistant_count_timeframe_comb <- lapply(mic_timeframe_comb, function(x) 
    dcast.data.table_check(data=x, 
    formula=as.formula("outcome_id + empi + t0_date ~  paste0('resistant_', tier_1_anti_resis_count)"), 
    mode="length",value.var = "time_diff",
    subset=as.character(as.quoted("result_tier=='tier_1' & tier_1_anti_resis_count>0"))))
  
  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("resistant", 
    names(DT), value=T), 
    new=paste0("mic_mic.pos.res_mic.blood.urine_mic.tier.1_mic.resistant.anti.count.tier.1..", 
    grep("resistant", names(DT), value=T), name_ext_tmp)), 
    DT=mic_result_resistant_count_timeframe_comb , name_ext_extended_tmp)

  # [4] Antibiotic resistance feature by the antibiotics to which resistant (tier 1))
  #-------------------------------------------------------------------------------#
  print("feature generation - mic_mic.pos.res_mic.blood.urine_mic.tier.1_mic.resistant.anti.name.tier.1")
  
  # [*] copy mic_timeframe_comb
  mic_result_resistant_anti_timeframe_comb <- copy(mic_timeframe_comb)

  mic_result_resistant_anti_timeframe_comb <- lapply(mic_result_resistant_anti_timeframe_comb, 
    function(x) {

      if (length(grep("anti_res_", names(x), value=T))>0) {
        
        unique(x[, grep("anti_res_", names(x), value=T):=(lapply(.SD, function(y) sum(y, na.rm=T))), 
          .SDcols=grep("anti_res_", names(x), value=T), by=c("outcome_id", "empi", "t0_date")][, mget(
          c(grep("anti_res_", names(x), value=T), "outcome_id", "empi", "t0_date"))], 
          by=c("outcome_id", "empi", "t0_date"))

      } else {

        unique(x["outcome_id", "empi", "t0_date"], by=c("outcome_id", "empi", "t0_date"))

      }
    })

  inv_mapply(function(DT,name_ext_tmp) setnames_check(DT, old=grep("anti_res_", 
    names(DT), value=T), 
    new=paste0("mic_mic.pos.res_mic.blood.urine_mic.tier.1_mic.resistant.anti.name.tier.1..", 
    grep("anti_res_", names(DT), value=T), name_ext_tmp)), 
    DT=mic_result_resistant_anti_timeframe_comb , name_ext_extended_tmp)

  #-------------------------------------------------------------------------------#
  # FEATURE MERGING
  #-------------------------------------------------------------------------------#

  print("feature merging")

  mic_feature_list <- list("mic_result_timeframe_comb",
    "mic_result_cat_timeframe_comb", "mic_result_resistant_count_timeframe_comb",
    "mic_result_resistant_timeframe_comb", 
    "mic_result_resistant_anti_timeframe_comb")
  mic_feature_list <- mic_feature_list[which(mic_feature_list %in% ls())]

  # combine features across feature timeframes
  mic_list_tmp <- timeframe_combine(mget(unlist(mic_feature_list)), mic_feature_list)

  # combine features across feature types
  mic <- Reduce(mymerge, mic_list_tmp)

  #-------------------------------------------------------------------------------#
  # MERGE FEATURES WITH COHORT & FORMAT VARIABLES
  #-------------------------------------------------------------------------------#

  print("cohort merging & feature formatting")

  # merge
  mic <- mic[cohort, mget(names(mic)), on=c("outcome_id", "empi", "t0_date")]
  suppressWarnings(mic[, grep("mic_id$", names(mic), value=T):=NULL])

  # format variables
  mic <- feature_type_format(dt=mic, day_to_last=timeframe_day_to_last, 
    num_feature_pattern=NA, int_feature_pattern="...", 
    factor_feature_pattern=NA)

  # rename variables
  setnames_check(mic, new=gsub("mean_temp", "mean", names(mic)))
 
  # add in additional var / drop unnecessary variables
  mic[, ':='(mic_time_min=time_min, lab_time_max=time_max)]

  #-------------------------------------------------------------------------------#
  # CLEAR MEMORY & RETURN FEATURE SET
  #-------------------------------------------------------------------------------#
  rm(mic_timeframe_comb)
  rm(list=unlist(mic_feature_list))

  return(mic)

}

#----------------------------------------------------------------------------#
