# Miscellaneous Helper Functions
#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
# ps
#----------------------------------------------------------------------------#
ps <- function(char_string, ...) {

  # purpose: 
  # print(sprintf(.....))

     print(sprintf(char_string, ...))
}

#----------------------------------------------------------------------------#
# readRDS_merge
#----------------------------------------------------------------------------#
readRDS_merge <- function(file_name_list, nested=FALSE) {

     # purpose: 
     # read in (a) RDS file (standard) (b) list of RDS files - merge (assumed to be in 
     # master_file format,i.e. to contain a [file_name]_id)

      if (length(file_name_list)==1) {

        return(readRDS(file_name_list[[1]]))

      } else if ( length(file_name_list)>1 & nested==FALSE) {

        for(i in 1:length(file_name_list)) {
           assign(paste0("temp_dt_", i), readRDS(file_name_list[i][[1]]))
        }

        temp <- rbindlist(mget(paste0("temp_dt_", 1:length(file_name_list))),use.names=T, fill=T)
        temp <- unique(temp,by=c(setdiff(names(temp), grep("_id$", names(temp),value=T))))

        file_temp_list <- ls(pattern="temp_dt_[0-9]")
        lapply(file_temp_list, function(x) rm(x))
        return(temp)

  } else if ( length(file_name_list)>1 & nested==TRUE) {
        
        for(i in 1:length(file_name_list)) {
           assign(paste0("temp_dt_", i), readRDS(file_name_list[i][[1]]))
        }

        temp_name <- names(temp_dt_1)
        temp <- lapply(   temp_name, function(table_name) lapply(mget(paste0("temp_dt_", 1:length(file_name_list)), 
          sys.frame(sys.parent(n=2))), function(x) x[[table_name]]))
        temp <- lapply(temp, function(x) rbindlist(x, use.names=T, fill=T))
        temp <- lapply(temp, function(x) unique(x, by=c(setdiff(names(x), 
          grep("_id$", names(x),value=T)))))

        names(temp) <-  temp_name

        file_temp_list <- ls(pattern="temp_dt_[0-9]")
        lapply(file_temp_list, function(x) rm(x))
        return(temp)

  }

}

#----------------------------------------------------------------------------#
# parse_date
#----------------------------------------------------------------------------#
parse_date <- function(dt_vector, column_name_list,column_name_list_new=column_name_list) {
  
  # purpose: 
  # automate detection and standardization of date variables - return verification output 
  # and standardized dates (IDate)

  # arguments:
  # dt_vector: DT or character vector

  #  below options only apply if the input is a DT
  #  column_name_list: list of names of date columns to be parsed
  #  column_name_list_new: list of names of new (parsed) date columns to be generated (default to 
  #                        names of original columns, i.e.replace existing date columns)

  # returns:
  # DT with formatted dates

  if (class(dt_vector)[1]=="data.table") {

    DT <- dt_vector

    lapply(column_name_list, function(x) print(head(DT[!is.na(get(x)) & 
      as.character(get(x))!="" , get(x)],15)))
    lapply(column_name_list, function(x) DT[, c(x):=as.Date(parse_date_time(get(x), 
     c("mdy","ymd","dby","myd", "dmy", "bdy")))])
    lapply(column_name_list, function(x) print(head(DT[!is.na(get(x)), get(x)],15)))
      setnames(DT, column_name_list, column_name_list_new)
    non_date <- names(DT)[sapply(DT, function(x) sum(is.na(x))==nrow(DT))]

    if(length(non_date)>0){
      DT[, c(non_date):=lapply(.SD, function(x) as.character(x)), .SDcols=non_date]
    }

  } 

    if (class(dt_vector)[1]=="character") {

    string <- dt_vector

    print(string[1:15])
    string <- as.Date(parse_date_time(string, 
      c("mdy","ymd","dby","myd", "dmy", "bdy")))
    print(string[1:15])

  } 

}

#----------------------------------------------------------------------------#
# dt_paste
#----------------------------------------------------------------------------#
dt_paste <- function(list_dt, text, length=10, text_col=NA, number_col=NA, line_sep=F) {

  # purpose: 
  # paste elements of a freq dt into a string 

  if (is.na(text_col)) text_col <- names(list_dt)[which(sapply(list_dt,function(x) 
    class(x)[1]) %in% c("character", "factor"))]
  if (is.na(number_col)) number_col <- names(list_dt)[which(sapply(list_dt,function(x) 
    !(class(x)[1] %in% c("character","factor"))))]

   if (line_sep==FALSE)  { 
      comb <- paste0(list_dt[[text_col]][1:length], " - ", list_dt[[number_col]][1:length],  
          "  // ", collapse=" ")
      comb <- gsub(" // $", "",comb)
      comb <- paste(comb, text)

    } else if (line_sep==TRUE) {

     comb <- lapply(1:length, function(x) {

        comb <- paste0(list_dt[[text_col]][x], " - ", list_dt[[number_col]][x],
           "  // ", collapse=" ")
        comb <- gsub(" // $", "",comb)
        comb <- paste(comb, text)

     })

    }

    comb <- gsub("^[ ]*|[ ]*$", "", comb)

    return(comb)

}

#----------------------------------------------------------------------------#
# gagne_score_calc
#----------------------------------------------------------------------------#

gagne_score_calc <- function(id_list, empi_list, date_list, dia_dt, timeframe_day=365, 
  buffer_day=0, gagne_count_var=FALSE) {

  # purpose: 
  # calculate the gagne score (i.e. gagne counts) for a given set of empis over a 
  # given time period leading up to an event

  # sample usage
  # gagne_score <- gagne_score_calc(
  #   dt$ed_enc_id,        # id column (*dt - data table - one row per event (date, patient, id (unique event identifier)))
  #   dt$empi,             # patient column
  #   dt$adm_date_ed,      # date column 
  #   oncdrs_rpdr_dia,     # dia dt containing a list of all diagnoses classified into gagne categories ('gagne column')
  #   timeframe_day=365, 
  #   buffer_day=1, 
  #   gagne_count_var=TRUE)
   
  #-----------------------------------------------------------------------#
  ## gagne cat
  gagne_cat <- fread("/data/zolab/methods_new/dia/gagne_codes/Gagne_codes.csv")

  #-----------------------------------------------------------------------#
  ## id_dt
  id_dt <- data.table(empi=empi_list, event_date=date_list, id_id=id_list)
  id_dt[, event_date_end:=event_date-buffer_day]
  id_dt[, event_date_pre:=event_date_end-timeframe_day]

  #-----------------------------------------------------------------------#
  ## subset columns
  dia_dt <- dia_dt[, .(empi, dia_date, dia_date_1, gagne)]

  #---------------------------------------------------------------------#
  ## subset to cohort 
  dia_dt  <- dia_dt [empi %in% id_dt$empi]

  #---------------------------------------------------------------------#
  ## foverlaps 
  setkey(id_dt, empi, event_date_pre, event_date_end)
  dia_olap <-foverlaps(dia_dt, id_dt, by.x=c("empi","dia_date", "dia_date_1"), nomatch=0)

  print(min(dia_dt$dia_date))
  print(max(dia_dt$dia_date))

  print(min(id_dt$event_date))
  print(max(id_dt$event_date))

  print(min(dia_olap$dia_date))
  print(max(dia_olap$dia_date))


  #---------------------------------------------------------------------#
  ### generating gagne scores

  # gagne_formula_exp <- quote(
  #   5 * metastatic_romano +
  #   2 * chf_romano +
  #   2 * dementia_romano +
  #   2 * renal_elixhauser +
  #   2 * wtloss_elixhauser +
  #   1 * hemiplegia_romano +
  #   1 * alcohol_elixhauser +
  #   1 * tumor_romano +
  #   1 * arrhythmia_elixhauser +
  #   1 * pulmonarydz_romano +
  #   1 * coagulopathy_elixhauser +
  #   1 * compdiabetes_elixhauser +
  #   1 * anemia_elixhauser +
  #   1 * electrolytes_elixhauser +
  #   1 * liver_elixhauser +
  #   1 * pvd_elixhauser +
  #   1 * psychosis_elixhauser +
  #   1 * pulmcirc_elixhauser +
  #  -1 * hivaids_romano +
  #  -1 * hypertension_elixhauser)

  # XXX NOTE: gagne_cat + gagne_weights + gagne_formula -> recreate gagne_formula_exp dynamically 
  gagne_name <- gagne_cat$gagne
  gagne_weight <- as.numeric(gsub("_", "-", gagne_cat$weight))
  gagne_formula_exp <- ""

  gagne_formula  <- function(cat, weight, ext) {
    for (i in 1:length(cat)) {
      gagne_formula_exp <- paste(gagne_formula_exp, weight[i], "*", paste0(ext, 
        cat[i]), "+", sep=" ")
    }
    gagne_formula_exp <- gsub("\\+$", "",gagne_formula_exp)
    return(gagne_formula_exp)
  }

  # (b) reshaping - create diagnosis code count vars (gagne)
  gagne_timeframe_comb <- dcast.data.table(dia_olap, id_id + empi + event_date ~  
    paste0("gagne_count_", gagne), length, value.var = "gagne", subset=.(!is.na(gagne)  & gagne!="" ))

  # expand to ensure that all the ids are represented 
  gagne_timeframe_comb <- gagne_timeframe_comb[id_dt[,.(id_id)], on=c("id_id"), nomatch=NA]
  gagne_timeframe_comb[,  grep("gagne_count", names(gagne_timeframe_comb), value=T) :=lapply(.SD, 
    function(x) ifelse(is.na(x), 0,x)), .SDcols=grep("gagne_count", names(gagne_timeframe_comb))]

  # (c) generate complete set of gagne category dummies (i.e. 0/1 if present 
  # or not at least once during time period) [impute 0]

  gagne_timeframe_comb[,  grep("gagne_count", names(gagne_timeframe_comb), value=T) :=lapply(.SD, 
    function(x) ifelse(x>=1, 1,0)), .SDcols=grep("gagne_count", names(gagne_timeframe_comb))]
  gagne_timeframe_comb[, setdiff(paste0("gagne_count_", gagne_name), names(gagne_timeframe_comb)):=0]

  # (d) determine the gagne score 
  gagne_timeframe_comb[,gagne_score:=eval(parse(text=gagne_formula(gagne_name[as.numeric(gsub("_", 
    "-", gagne_cat$weight))!=0], gagne_weight[as.numeric(gsub("_", "-", gagne_cat$weight))!=0], "gagne_count_")))]

  #---------------------------------------------------------------------#
  ### returning the score

  setnames(gagne_timeframe_comb, "id_id", "id")
  
  if (gagne_count_var==FALSE) {
    return(gagne_timeframe_comb[, .(id, empi, event_date, gagne_score)])
  } else {
    return(gagne_timeframe_comb[, mget(c("id", "empi", "event_date", 
      "gagne_score", grep("gagne_count_*",  names(gagne_timeframe_comb), value=T)))])
  }

}


#----------------------------------------------------------------------------#
# set_zero_na
#----------------------------------------------------------------------------#

set_zero_na <- function(dt, replace=NA) {

  # purpose: 
  # replace (in place) zeros in DT with another value (e.g. NA)

  # arguments:
  #  dt: name of DT
  # replace:value with which to replace 0s (default=NA) 

  # returns:
  # modified data.table (modified in place)


  for (j in seq_len(ncol(dt)))
    set(dt, which(dt[[j]] %in% c(0)), j, replace)
}

#---------------------------------------------------------------------#
#  set_na_zero
#---------------------------------------------------------------------#

set_na_zero <- function(dt, replace=0, subset_col=names(dt)) {

  # purpose: 
  # replace (in place) NAs/+inf/-inf in DT with another value (e.g. 0s)

  # arguments:
  #  dt: name of DT
  #  replace:value with which to replace Nas/+inf/-inf (default=0) 

  # returns:
  # modified data.table (modified in place)

  for (j in which(names(dt) %in% subset_col))
    set(dt, which(is.na(dt[[j]]) | dt[[j]] %in% c(-Inf, +Inf) ), j, replace)

}



#---------------------------------------------------------------------#
# set_missing_na
#---------------------------------------------------------------------#

set_missing_na <- function(dt, replace=NA, subset_col=names(dt)) {

  # purpose: 
  # replace (in place) empty values ("[ ]*" or "") in DT with another value (e.g. NA)

  # arguments:
  #  dt: name of DT
  #  replace: value with which to replace empty values (default=NA) 

  # returns:
  # modified data.table (modified in place)

  for (j in which(names(dt) %in% subset_col))
    set(dt, which(gsub("[ ]*", "", dt[[j]])==""), j, replace)
}

#---------------------------------------------------------------------#
# set_var_na
#---------------------------------------------------------------------#

set_var_na <- function(dt, to_replace=NA, to_replace_partial=NA, replace=NA) {


  # purpose: 
  # replace values (complete and partial matches) in DT with another value

  # arguments:
  #  dt: name of DT
  #  to_replace: character vector of values - elements in the dt 
  #              are sequentially matched against each element in the vector and
  #              are replaced if there is a complete match (default: NA - no replacement)
  #  to_replace_partial: value - elements in the dt 
  #              are matched against this value and
  #              are replaced if they contain the value (partial match) (default: NA - no replacement)
  #  replace: value with which to replace matched/partially matched values (default=NA) 

  # returns:
  # modified data.table (modified in place)


  if (sum(!is.na(to_replace))>0) {
    for (j in seq_len(ncol(dt)))
      {
        set(dt, which(dt[[j]] %in% to_replace), j, replace)
      }
  }
  if (sum(!is.na(to_replace_partial))>0) {
    for (j in seq_len(ncol(dt)))
      {
        set(dt, which(dt[[j]] %like% to_replace_partial), j, replace)
      }
  }

}

#---------------------------------------------------------------------#
# mymerge
#---------------------------------------------------------------------#

mymerge <- function(x,y, var_list=cohort_key_var) {

  # purpose: 
  # merge list of data.tables on a given set of variables 

  # arguments:
  #  x,y: name of DT
  #  by: character vector with names of variables on which to merge 

  # returns:
  # merged DT

  # sample usage:
  #  Reduce(mymerge, list of data tables)

  merge(x,y, all = TRUE, by=var_list)

}

#---------------------------------------------------------------------#
# perc
#---------------------------------------------------------------------#

perc <- function(num, denom, digit=1) {

  # purpose: 
  # convert proportion (numerator, denominator) into formatted percentage value

  # arguments:
  #  num: numerator
  #  denom: denominator
  #  digit: number of digits to round to
 
  # returns:
  # percentage value

  round((num/denom)*100, digit)

}

#---------------------------------------------------------------------#
# table_mod
#---------------------------------------------------------------------#

table_mod <- function(x) {

  # purpose: 
  # ordered table command

  table(x, useNA="always")[order(-table(x, useNA="always"))]
}


#---------------------------------------------------------------------#
# list_table
#---------------------------------------------------------------------#

list_table <- function(list_name, var_name, argument="space", vital_signs_merge=F, 
  vital_signs_old=NA) {
 
  # purpose: 
  # (a) convert list into table and (b) [optional] append table to existing table in a manner 
  # consistent with the generation of 'vital signs'

  # arguments:
  #   list_name: name of named list (can contain elements which are converted into empty rows)
  #   var_name: name of variable which contains the header of the generated table (if vital_signs_merge==T 
  #             var_name %in% names(vital_signs_old) -> the system time is appended to var_name to ensure 
  #             the uniqueness of column names)
  #   argument: list elements == this phrase are converted into empty rows in the generated 
  #             table (default: "space")
  #   vital_signs_merge: whether or not to merge generated table with existing file in a 
  #                      manner consistent with the generation of 'vital signs' (default: F)
  #   vital_signs_old: path of existing csv file with which generated table is to be merged
  #                    if vital_signs_merge==T (default: NA)

  # returns:
  # generated table (merged with existing file if vital_signs_merge==T) [data.table format]


  temp <- as.data.table(t(rbindlist(list(list_name), fill=T)))
  temp$var_name <- names(list_name)
  temp[V1=="space", ':='(var_name="space", V1="")]
  
  if (!var_name %in% names(vital_signs_old)) {
    setnames(temp, c(var_name, "var_name"))
  } else {
    setnames(temp, c(paste(var_name, as.character(format(Sys.time(), "%H:%M")), sep="_"), 
    "var_name"))
  }

  if(vital_signs_merge==T) {

    col_name_old <- copy(names(temp))
    temp <- join(temp, vital_signs_old, by=c("var_name"), "full")
    setcolorder(temp, c(names(vital_signs_old), setdiff(col_name_old, "var_name")))
  } else {
    setcolorder(temp, c("var_name", var_name))
  }
  temp <- as.data.table(temp)

  temp[, drop:=0][var_name==shift(var_name, type="lead") & !(var_name=="space" & 
    shift(var_name, type="lead")=="space"), drop:=1]
  temp <- temp[!drop==1]
  temp[, drop:=NULL]
  temp[var_name=="space", var_name:=""]

  return(temp)
}


#---------------------------------------------------------------------#
## list_space
#---------------------------------------------------------------------#

list_space <- function(list_name, argument="space") {
 
  # purpose: 
  # insert element into list containing a specified phrase (useful when the list is to be 
  # converted into a table which is to contain 'spaces', i.e. empty rows)

  # arguments:
  #   list_name: name of list as characters ("...") 
  #   argument: phrase to be inserted into generated arguments
  
  # returns:
  # modified list in place - i.e. 'returns' modified list with inserted element

  # sample usage:
  #   test <- list()
  #   test$element1 <- "test element"
  #   list_space("test")
 
 count <- get(argument, sys.frame(sys.parent(n=1)))
 temp <- get(list_name, sys.frame(sys.parent(n=1)))
 
 temp[[paste0("space_", count)]] <- "space"
 
 assign(list_name, temp, sys.frame(sys.parent(n=1)))
 assign(argument, count+1, sys.frame(sys.parent(n=1)))

}


#----------------------------------------------------------------------------#
# store_shorten_file
#----------------------------------------------------------------------------#
store_shorten_file <- function(DT, test_row) {

  # purpose: 
  # save copy of DT and replace the DT itself with a shortened version of itself
  # (useful when debugging scripts - run script with subset of data without changing file names)


  # arguments:
  #  DT: name of DT
  #  test_row: number of rows to keep in shortened table
 
  # return:
  #  original DT (name_copy) & shortened table (name) are stored in global environment

  # sample usage:
  #  store_shorten_file("test")

  assign(paste0(DT, "_copy"), copy(get(DT,sys.frame(sys.parent(n=1)))),sys.frame(sys.parent(n=1)))
  assign(DT, get(DT,sys.frame(sys.parent(n=1)))[1:test_row], sys.frame(sys.parent(n=1)))

}

#----------------------------------------------------------------------------#
# inv_lapply
#----------------------------------------------------------------------------#

inv_lapply <- function(X, FUN,...) {

  # purpose: 
  # quiet lapply,i.e.suppress output (to be used when modifying in place)

  # arguments:
  #  normal lapply arguments
 
  # sample usage
  #  inv_lapply(list("aa","ba"),function(x) gsub("a","",x))

  invisible(lapply(X, FUN,...))

}

#----------------------------------------------------------------------------#
# inv_mapply
#----------------------------------------------------------------------------#

inv_mapply <- function(FUN,...) {

  # purpose: 
  # quiet mapply,i.e.suppress output (to be used when modifying in place)

  # arguments:
  #  normal mapply arguments
 
  # sample usage
  #  inv_mapply(list("aa","ba"),function(x) gsub("a","",x))

  invisible(mapply(FUN,...))

}



#----------------------------------------------------------------------------#
