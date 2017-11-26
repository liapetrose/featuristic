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
           assign(paste0("readRDS_merge_DEV_temp_dt_", i), readRDS(file_name_list[i][[1]]))
        }

        temp <- rbindlist(mget(paste0("readRDS_merge_DEV_temp_dt_", 1:length(file_name_list))),use.names=T, fill=T)
        temp <- unique(temp,by=c(setdiff(names(temp), grep("_id$|_id_raw$", names(temp),value=T))))

        file_temp_list <- ls(pattern="readRDS_merge_DEV_temp_dt_[0-9]")
        lapply(file_temp_list, function(x) rm(x))
        return(temp)

  } else if (length(file_name_list)>1 & nested==TRUE) {
        
        for(i in 1:length(file_name_list)) {
           assign(paste0("readRDS_merge_DEV_temp_dt_", i), readRDS(file_name_list[i][[1]]))
        }

        temp_name <- names(readRDS_merge_DEV_temp_dt_1)
        temp      <- lapply(   temp_name, function(table_name) lapply(mget(paste0("readRDS_merge_DEV_temp_dt_", 1:length(file_name_list)), 
          sys.frame(sys.parent(n=2))), function(x) x[[table_name]]))
        temp      <- lapply(temp, function(x) rbindlist(x, use.names=T, fill=T))
        temp      <- lapply(temp, function(x) unique(x, by=c(setdiff(names(x), 
          grep("_id$", names(x),value=T)))))

        names(temp) <-  temp_name

        file_temp_list <- ls(pattern="readRDS_merge_DEV_temp_dt_[0-9]")
        lapply(file_temp_list, function(x) rm(x))
        return(temp)

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

set_zero_na <- function(dt, replace=NA, subset_col=names(dt)) {

  # purpose: 
  # replace (in place) zeros in DT with another value (e.g. NA)

  # arguments:
  #  dt: name of DT
  # replace:value with which to replace 0s (default=NA) 

  # returns:
  # modified data.table (modified in place)


  for (j in which(names(dt) %in% subset_col))
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
# format_date
#----------------------------------------------------------------------------#

format_date <- function(file_list, date_list) {

  lapply(file_list, function(dt) {

    lapply(date_list,function(date_col) {

      dt[, c(date_col[[1]]):=as.Date(get(date_col[[1]]))]

    })
  })

}

#----------------------------------------------------------------------------#
# dcast.data.table_check
#----------------------------------------------------------------------------#

dcast.data.table_check <- function(..., data, mode, subset=as.character(as.quoted("!is.na(empi)"))) {

  # subset
  subset_tmp <- copy(subset)
  data_tmp   <- data[eval(parse(text=subset_tmp))]
  
   # check
  if (nrow(data_tmp)>0) {

    # cast
    if (mode=="numeric")  {
    
      # cast - numeric
      data_cast <- dcast.data.table(..., data=data_tmp, subset=NULL, fun.aggregate=list(min, max, mean, sd))
    
    } else if (mode=="basic") {

      # cast - categorical + day to last
      data_cast <- dcast.data.table(..., data=data_tmp, subset=NULL, fun.aggregate=list(length, function(x) min(x, na.rm=T)))

    } else if (mode=="length") {

      # cast - categorical 
      data_cast <- dcast.data.table(..., data=data_tmp, subset=NULL, fun.aggregate=list(length))

    } else if (mode=="sum") {

      # cast - sum
      data_cast <- dcast.data.table(..., data=data_tmp, subset=NULL, fun.aggregate=function(y) sum(as.numeric(y), na.rm=T))


    }

  } else {

    # no casting
    return(data.table(empi = character(0), outcome_id = numeric(0), t0_date=numeric(0)))

  }

}


#----------------------------------------------------------------------------#
# melt_check
#----------------------------------------------------------------------------#

melt_check <- function(...,variable.name,value.name) {

  # melt
  variable.name_tmp <- "variable.name_xxxx"
  value.name_tmp    <- "value.name_xxxx"

  # ensure that variables are name correctly
  melt_data <-    as.data.table(melt(...,variable.name=variable.name_tmp,value.name=value.name_tmp))

  # proper naming

  if (length(grep(variable.name_tmp,names(melt_data), value=T))>0) {

      setnames(melt_data, grep(variable.name_tmp,names(melt_data), value=T),
        variable.name)
      setnames(melt_data, grep(value.name_tmp,names(melt_data), value=T),
        value.name)

  } else {

      melt_data[, c(variable.name):=""]
      melt_data[, c(value.name):=""]

  }

  # return
  return(melt_data)

}

#----------------------------------------------------------------------------#
# non_missing
#----------------------------------------------------------------------------#

non_missing <- function(var_name)  {

  var_name_tmp <- copy(var_name)
 
  subset_tmp = as.quoted(paste0("!is.na(", var_name_tmp, ") & !is.null(", var_name_tmp, 
    ") & !(", var_name_tmp,"=='')"))

  subset_tmp = as.character(subset_tmp)

  return(subset_tmp)

}


#----------------------------------------------------------------------------#
# equal_to
#----------------------------------------------------------------------------#

equal_to <- function(var_name, value)  {

  var_name_tmp <- copy(var_name)
 
  subset_tmp = as.quoted(paste0(var_name_tmp,"=='", value,"'"))

  subset_tmp = as.character(subset_tmp)

  return(subset_tmp)

}

#----------------------------------------------------------------------------#
# setnames_check
#----------------------------------------------------------------------------#

setnames_check <- function(..., old=NA,new) {

  # check
  if (!(is.null(old)) && !(is.na(old)) && length(old)==length(new)) {

    setnames(..., old=old, new=new)

  } else if (length(old)>0 && !(is.null(old)) && is.na(old)) {

    setnames(..., new)
  }

}

#----------------------------------------------------------------------------#
# one_hot_encoding
#----------------------------------------------------------------------------#

one_hot_encoding <- function (dt, var_list, drop = FALSE) {

    # options
    contr.onehot = function(n, contrasts, sparse = FALSE) {
        contr.sum(n = n, contrasts = FALSE, sparse = sparse)
    }
   
    current.na.action <- options("na.action")
    options(contrasts = c("contr.onehot", "contr.onehot"))
    options(na.action = "na.pass")

    # ensure that at least two levels (otherwise crash)
    level_count <- sapply(var_list, function(x) 
      length(unique(dt[, get(x)])))
    var_list_single    <- var_list[level_count==1]
    var_list           <- var_list[level_count>=2]


    # process factors (single-level) [rename & set to 1]
    inv_lapply(var_list_single, function(x) {

      value_tmp <- as.character(unique(dt[, get(x)]))

      # rename & set to 1
      dt[, c(x):=NULL]
      dt[, c(paste0(x, "_", value_tmp)):=as.integer(1)]

    })

    # process factors (multi-level) [cast]
    dt_factor <- dt[, c(var_list), with=F]

    dt_factor <- lapply(var_list, function(x) {
        
        # ensure that character
        dt_factor[, c(names(dt_factor)):=lapply(.SD, function(x) as.character(x))]

        # rename 
        dt_factor[, c(x):=lapply(.SD, function(y) paste0("_", gsub("(_*)$", "", y))), 
            .SDcols=c(x), by=1:nrow(dt_factor)]

        # cast
        dt_factor_temp <- data.frame(model.matrix(~. - 1, data = dt_factor[,
            mget(x)]))

        # drop one if only two columns
        if (ncol(dt_factor_temp)==2 & names(dt_factor_temp)[1] %like% "(0|1)$") {
            name_orig <- gsub("(0|1)$", "", names(dt_factor_temp)[1])
            dt_factor_temp <- dt_factor_temp[, 1]
            dt_factor_temp <- data.frame(dt_factor_temp)
            names(dt_factor_temp) <- name_orig
        }

        # drop one column - avoid colinearity
        if (drop == TRUE & ncol(dt_factor_temp)>1) {
            dt_factor_temp[[names(dt_factor_temp)[1]]] <- NULL
        }
        return(dt_factor_temp)
    })

    dt_factor <- data.frame(dt_factor)
    dt_factor <- setDT(dt_factor)

    # drop factor columns which end in NA
    na_col   <- grep("_NA$", names(dt_factor), value=T)
    dt_factor[, c(na_col):=NULL]

    # format
    dt_factor[, `:=`(names(dt_factor), lapply(.SD, function(x) as.integer(x)))]
    gc()
    
    # check that row count aligns
    if (nrow(dt_factor) != nrow(dt)){print("warning - one hot encoding () - it appears that rows are dropped during the conversion")}

    # drop factor columns from original dt, bind one-hot encoded back in, return new dt
    dt[, (var_list):=NULL, ]
    dt_new <- cbind(dt, dt_factor)
    rm(dt_factor)
    rm(dt)

    options(na.action = current.na.action)

    # return
    return(dt_new)
}

#----------------------------------------------------------------------------#
# return_mult
#----------------------------------------------------------------------------#

#' Store multiple arguments returned from function as separate objects - function. 

"[<-.result"  <- function(x,...,value) {

   args <- as.list(match.call())
   args <- args[-c(1:2,length(args))]
   length(value) <- length(args)
   for(i in seq(along=args)) {
     a <- args[[i]]
     if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
   }
   x
}


#' Store multiple arguments returned from function as separate objects - structure. 
return_mult <- structure(NA,class="result")



#----------------------------------------------------------------------------#
# group_race
#----------------------------------------------------------------------------#


# Classify given race as one of the conventional medical groups- white, black, hispanic, other.

group_race <- function(data, race_col = "race") {

  # create copy so as to not modify provided data.table
  fn_data <- copy(data)

  # create new temporary column
  fn_data[, race_group := "none", ]

  # race regular expressions
  black_race_regex    <- "black|african"
  hispanic_race_regex <- "hispanic"
  white_race_regex    <- "white|european"

  # convert races to grouped versions 
  fn_data[(tolower(get(race_col)) %like% black_race_regex & race_group == "none"), race_group := "black"]
  fn_data[(tolower(get(race_col)) %like% hispanic_race_regex & race_group == "none"), race_group := "hispanic"]
  fn_data[(tolower(get(race_col)) %like% white_race_regex & race_group == "none"), race_group := "white"]
  fn_data[race_group == "none", race_group := "other"]

  # set none > missing
  fn_data[race_group == "none",race_group :=NA]

  # return
  return(fn_data[,race_group])
}

#----------------------------------------------------------------------------#
# group_religion
#----------------------------------------------------------------------------#

# Classify given religion as one of a number of aggregated categories

group_religion <- function(data, religion_col = "religion") {

  # create copy so as to not modify provided data.table
  fn_data <- copy(data)

  # create new temporary column
  fn_data[, religion_group := "", ]

  # religion regular expressions
  na_religion_regex            <- "other|^none$|^non$|no preference"
  non_religious_religion_regex <- "agnostic|atheist"

  # convert religions to grouped versions 
  fn_data[(tolower(get(religion_col)) %like% na_religion_regex & religion_group == ""), religion_group := "none"]
  fn_data[(tolower(get(religion_col)) %like% non_religious_religion_regex & religion_group == ""), religion_group := "non_religious"]
  fn_data[religion_group == "", religion_group := "religious"]

  # set none > missing
  fn_data[religion_group %in% c("none",""),religion_group :=NA]

  # return
  return(fn_data[,religion_group])
}

#----------------------------------------------------------------------------#
# group_marital_status
#----------------------------------------------------------------------------#

# Classify given marital status as one of a number of aggregated categories

group_marital_status <- function(data, marital_status_col = "marital_status") {

  # create copy so as to not modify provided data.table
  fn_data <- copy(data)

  # create new temporary column
  fn_data[, marital_status_group := "", ]

  # marital_status regular expressions
  na_marital_status_regex            <- "other"
  partnership_marital_status_regex   <- "common law|married|partner"
  separated_marital_status_regex     <- "divorced|separated"
  widowed_marital_status_regex       <- "widowed|separated"
  single_marital_status_regex        <- "single"

  # convert marital_statuss to grouped versions 
  fn_data[(tolower(get(marital_status_col)) %like% na_marital_status_regex & marital_status_group == ""), marital_status_group := "none"]
  fn_data[(tolower(get(marital_status_col)) %like% partnership_marital_status_regex & marital_status_group == ""), marital_status_group := "partnership"]
  fn_data[(tolower(get(marital_status_col)) %like% separated_marital_status_regex & marital_status_group == ""), marital_status_group := "separated"]
  fn_data[(tolower(get(marital_status_col)) %like% widowed_marital_status_regex & marital_status_group == ""), marital_status_group := "widowed"]
  fn_data[(tolower(get(marital_status_col)) %like% single_marital_status_regex & marital_status_group == ""), marital_status_group := "single"]

  # set none > missing
  fn_data[marital_status_group %in% c("","none"),marital_status_group :=NA]

  # return
  return(fn_data[,marital_status_group])
}


#----------------------------------------------------------------------------#
# group_language
#----------------------------------------------------------------------------#

# Classify given language as one of a number of aggregated categories

group_language <- function(data, language_col = "language") {

  # create copy so as to not modify provided data.table
  fn_data <- copy(data)

  # create new temporary column
  fn_data[, language_group := "", ]

  # language regular expressions
  na_language_regex            <- "other|unavailable|declined"
  english_language_regex       <- "english"

  # convert languages to grouped versions 
  fn_data[(tolower(get(language_col)) %like% na_language_regex & language_group == ""), language_group := "none"]
  fn_data[(tolower(get(language_col)) %like% english_language_regex & language_group == ""), language_group := "english"]
  fn_data[language_group == "", language_group := "non_english"]

  # set none > missing
  fn_data[language_group %in% c("","none"),language_group :=NA]

  # return
  return(fn_data[,language_group])
}


#----------------------------------------------------------------------------#
# missing_zero_perc
#----------------------------------------------------------------------------#

missing_zero_perc <- function(var, verbose=FALSE) {

  # missing_perc
  na_perc                      <- perc(sum(is.na(var)),length(var))
  if (verbose==TRUE) na_perc   <- paste0("NA (%): ",na_perc)

  # zero_perc
  zero_perc                    <- perc(sum(var==0, na.rm=TRUE),length(var))
  if (verbose==TRUE) zero_perc <- paste0("Zero (%): ",zero_perc)

  return(c(na_perc,zero_perc))
}

#----------------------------------------------------------------------------#




