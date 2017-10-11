#----------------------------------------------------------------------------#

#' @title Verify the correct construction of a feature category dataset. 
#'
#' @description \
#'
#' @export
#' @param feature_dt_name 
#' @param feature_dt
#' @param cohort_dt 
#' @param cohort_key_var_list 
#' @param save_name 
#' @return
#' @examples

feature_dataset_check <- function(feature_dt, feature_dt_name, cohort_dt,
  cohort_key_var_list, save_name=TRUE) {

  # Check #1: number of observations 
  # ------------------------------------
  cat ("\n### NUMBER OF OBSERVATIONS/FEATURES\n")
  cat(sprintf("\n* number of cohort observations: %d\n", nrow(cohort_dt)))
  cat(sprintf("* number of observations in %s file: %d\n", feature_dt_name,
    nrow(feature_dt)))
  cat(sprintf("* number of features (overall) in %s file: %d\n", feature_dt_name,
    ncol(feature_dt[,mget(setdiff(names(feature_dt), cohort_key_var_list))])))

  assert("correct number of rows (equal to the number of rows in the cohort) & 
    one feature set record per cohort observation", nrow(cohort_dt)==nrow(feature_dt) & 
    nrow(feature_dt[outcome_id %in% cohort_dt$outcome_id])==nrow(feature_dt))

  # Check #3: variable types 
  # ------------------------------------
  cat ("\n### VARIABLE TYPES\n")

  var_cohort             <- cohort_key_var_list[cohort_key_var_list %in% names(feature_dt)]
  var_type_dt_cohort     <- data.table(var_type=sapply(feature_dt[, mget(c(var_cohort))], 
      function(x) class(x)[1]))
 
  var_feature             <- setdiff(names(feature_dt), c(grep("day_to_last", 
    names(feature_dt), value=TRUE), cohort_key_var_list)) 
  var_feature_num         <- var_feature[sapply(feature_dt[,mget(c(var_feature))], function(x) class(x)[1]=="numeric")]
  var_feature_int         <- var_feature[sapply(feature_dt[,mget(c(var_feature))], function(x) class(x)[1]=="integer")]
  var_feature_factor      <- var_feature[sapply(feature_dt[,mget(c(var_feature))], function(x) class(x)[1]=="factor")]
  var_feature_date        <- var_feature[sapply(feature_dt[,mget(c(var_feature))], function(x) class(x)[1]=="Date")]

  if (length(var_feature)>0) {
    var_type_dt_feature     <- data.table(var_type=sapply(feature_dt[, 
      mget(c(var_feature))], function(x) class(x)[1]))
  }

  var_day_to_last         <- grep("day_to_last", names(feature_dt), value=TRUE) 
  if (length(var_day_to_last)>0) {
    var_type_dt_day_to_last <- data.table(var_type=sapply(feature_dt[, 
      mget(c(var_day_to_last))], function(x) class(x)[1]))
  }

  cat(sprintf("\ncohort_key_var (%d) > variable type:\n", length(var_cohort)))
  if(length(var_cohort)>0 && length(var_type_dt_cohort$var_type)>0)  print(table_mod(var_type_dt_cohort$var_type))
  
  cat(sprintf("\nmain features (%d)  > variable type:\n", length(var_feature)))
  if(length(var_feature)>0 && length(var_type_dt_feature$var_type)>0) print(table_mod(var_type_dt_feature$var_type))

  cat(sprintf("\nday-to-last features (%d)  > variable type:\n", length(var_day_to_last)))
  if(length(var_day_to_last)>0 && length(var_type_dt_day_to_last$var_type)>0) print(table_mod(var_type_dt_day_to_last$var_type))

  # Check #4: variable groups 
  # ------------------------------------
  cat ("\n### VARIABLE GROUPS\n")
  
  var_cat <- gsub("(.*)\\.\\.(.*)", "\\1", var_feature)
  print(table(var_cat))

  # Check #5: missingness
  # ------------------------------------
  cat ("\n### MISSINGNESS\n")

  var_cohort_missing               <- data.frame(na_missing_perc=sapply(feature_dt[,mget(c(var_cohort))],function(x) paste0(missing_zero_perc(x,verbose=TRUE),collapse=" / ")))
  cat("\n## cohort_key_var > missingness/zeroness:\n")
  print(var_cohort_missing)

  if (length(var_feature_num)>0) {
    var_feature_missing_num          <- lapply(feature_dt[,mget(c(var_feature_num))],function(x) missing_zero_perc(x))
    var_feature_missing_num_na       <- summary(sapply(var_feature_missing_num,"[[",1))
    var_feature_missing_num_zero     <- summary(sapply(var_feature_missing_num,"[[",2))
    
    cat("\n## main features (numeric) > missingness/zeroness:\n")
    cat("\nmissingness:\n")
    print(var_feature_missing_num_na)
    cat("\nzeroness:\n")
    print(var_feature_missing_num_zero)

  }

  if (length(var_feature_int)>0) {
    var_feature_missing_int          <- lapply(feature_dt[,mget(c(var_feature_int))],function(x) missing_zero_perc(x))
    var_feature_missing_int_na       <- summary(sapply(var_feature_missing_int,"[[",1))
    var_feature_missing_int_zero     <- summary(sapply(var_feature_missing_int,"[[",2))
  
    cat("\n## main features (integer) > missingness/zeroness:\n")
    cat("\nmissingness:\n")
    print(var_feature_missing_int_na)
    cat("\nzeroness:\n")
    print(var_feature_missing_int_zero)

  }

  if (length(var_feature_factor)>0) {
    var_feature_missing_factor       <- lapply(feature_dt[,mget(c(var_feature_factor))],function(x) missing_zero_perc(x))
    var_feature_missing_factor_na    <- summary(sapply(var_feature_missing_factor,"[[",1))
    var_feature_missing_factor_zero  <- summary(sapply(var_feature_missing_factor,"[[",2))
    
    cat("\n## main features (factor) > missingness/zeroness:\n")
    cat("\nmissingness:\n")
    print(var_feature_missing_factor_na)
    cat("\nzeroness:\n")
    print(var_feature_missing_factor_zero)

  }

  if (length(var_feature_date)>0) {
    var_feature_missing_date         <- lapply(feature_dt[,mget(c(var_feature_date))],function(x) missing_zero_perc(x))
    var_feature_missing_date_na      <- summary(sapply(var_feature_missing_date,"[[",1))
    var_feature_missing_date_zero    <- summary(sapply(var_feature_missing_date,"[[",2))
 
    cat("\n## main features (date) > missingness/zeroness:\n")
    cat("\nmissingness:\n")
    print(var_feature_missing_date_na)
    cat("\nzeroness:\n")
    print(var_feature_missing_date_zero)

  }

  if (length(var_day_to_last)>0) {
    
    var_day_to_last_missing         <- lapply(feature_dt[,mget(c(var_day_to_last))],function(x) missing_zero_perc(x))
    var_day_to_last_missing_na      <- summary(sapply(var_day_to_last_missing,"[[",1))
    var_day_to_last_missing_zero    <- summary(sapply(var_day_to_last_missing,"[[",2))
 
    cat("\n## day-to-last features > missingness/zeroness:\n")
    cat("\nmissingness:\n")
    print(var_day_to_last_missing_na)
    cat("\nzeroness:\n")
    print(var_day_to_last_missing_zero)

  }

  # Save feature names
  # ------------------------------------

  if (save_name==TRUE) {

    feature_name_file_path <-  paste0(temp_folder, "var_name_raw_", 
      feature_dt_name, "_", feature_set_name, ".csv")
   
    feature_name_file     <- data.table(var_name=names(feature_dt), 
      var_type=sapply(feature_dt, function(x) class(x)[1]))

    cat(sprintf("\n >> feature names saved: %s\n",feature_name_file_path))

    write.csv(feature_name_file, feature_name_file_path, row.names=F)
  
  }
}

#----------------------------------------------------------------------------#


