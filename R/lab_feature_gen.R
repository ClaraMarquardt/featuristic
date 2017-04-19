#----------------------------------------------------------------------------#

#' @title Generate lab-related features (lab data).
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param cohort
#' @param cohort_key_var_merge
#' @param lab_file_mod_arg
#' @param leak_lab_day_arg
#' @param combine
#' @param lab_file_mod_ext
#' @param file_date_var
#' @return
#' @examples


lab_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var, lab_file_mod_arg=lab_file_mod, 
  leak_lab_day_arg=leak_lab_day, combine=FALSE,lab_file_mod_ext=NA, file_date_var="lab_date") {

  print("launching lab_feature_gen")
  
  ##############################################################################
  ### Load the  modified/pre-processed lab file for the specified data sample -- 
  ### if no such file exists - excute the function_lab_class.R code (access/submit as 
  ### batchmode job using machine/function_class_batchmode.txt)
    
  # (a) load the stored code - return error message if file does not exist
  tryCatch(lab <- readRDS_merge(lab_file_mod_arg), warning=function(w)
    print("no classified lab file available for the data sample"))
    # XXX NOTE: RDS file preserves formatting of empi as character

  if (combine==TRUE) {
    tryCatch(lab_ext <- readRDS_merge(lab_file_mod_ext), warning=function(w)
      print("no classified lab file available for the data sample"))
    
    lab <- rbindlist(list(lab, lab_ext), fill=T, use.names=T)
    lab[, lab_id:=1:nrow(lab)]
 
  }

  # subset to smaller sample for testing if so specified in control file
  if (test_raw_file==TRUE) {
    store_shorten_file("lab")
  }

  # (c) subset to the variables & results which are to be employed in the feature 
  # construction process - select only valid results (result_type based selection) & 
  # select only date not time
  # XXX TO-DO: Determine how to incorperate times
  lab <- lab[lab_result_type %in% c("num", "cat")]
  lab[, c("lab_time"):=NULL]


  ##############################################################################
  ### merge lab file with cohort (cohort_key_variables) & format dates
  # XXX NOTE: foverlaps - ensures that all vital signs max timeframe_long days 
  # prior to outcome date
  lab <- lab[empi %in% cohort$empi]

  invisible(parse_date(lab, c("lab_date")))

  lab[, c("lab_date_1","lab_date_2"):=.(lab_date)]

  lab <-foverlaps(lab, cohort[, mget(cohort_key_var_merge)], by.x=c("empi",
    "lab_date_1", "lab_date_2"), nomatch=0)

  lab[, time_diff:=as.numeric(difftime(pred_date, get(file_date_var), 
    units="days"))]

  ### implement leakage control (as specified in control file - 
  ### omit day of outcome/days pre outcome)
  if (!is.na(leak_lab_day_arg)) {
    lab <- lab[!(pred_date-lab_date_1<=leak_lab_day_arg)]
  }

  ##############################################################################
  ### subsetting & dividing into smaller DT based on timeframe (ST/LT) - 
  ### return as list (...timeframe_comb)
  invisible(timeframe_split(list("lab"), "lab_date"))
  
  name_ext_extended <- name_ext_extended[sapply(lab_timeframe_comb, nrow)!=0]
  name_ext <- name_ext_extended[2:length(name_ext_extended)]
  lab_timeframe_comb <- lab_timeframe_comb[sapply(lab_timeframe_comb, nrow)!=0]

  time_min <- min(do.call("c", lapply(lab_timeframe_comb, function(x) as.Date(min(x[, 
    lab_date]), "%Y-%m-%d"))))
  time_max <- max(do.call("c", lapply(lab_timeframe_comb, function(x) as.Date(max(x[, 
    lab_date]), "%Y-%m-%d"))))

  ##############################################################################
  ### reshapping & generating features -- numeric labs - mean, sd, max, min - 
  ### impose feature  categorisation ("lab_lab.value_lab.max/min.."..."_short/_long")
  ### numeric results
  # Note: By casting multiple functions simulataneously -- each function is 
  # generated for every lvs_cat value even if 100% missing values
  lab_num_timeframe_comb <- lapply(lab_timeframe_comb, function(x)
    dcast.data.table(x, outcome_id + empi + pred_date  ~ 
    paste0("..", lab_cat),list(min, max, mean, sd), value.var="lab_value", 
    subset=.(lab_result_type=="num")))

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("lab_value", 
    names(DT), value=T), paste0(gsub("_value_", "_lab.numeric_lab.", 
    gsub("_\\.\\.", "\\.\\.", grep("lab_value", 
    names(DT), value=T))), name_ext)), DT=lab_num_timeframe_comb, 
    name_ext_extended))

  lab_num_timeframe_comb_time <- lapply(lab_timeframe_comb, function(x)
    dcast.data.table(x, outcome_id + empi + pred_date  ~ 
    paste0("..", lab_cat),fun.aggregate=list(length, function(x) min(x, na.rm=T)), 
    value.var = "time_diff", subset=.(lab_result_type=="num")))

  lab_num_timeframe_comb_time <- feature_var_format(lab_num_timeframe_comb_time)
  invisible(lapply(lab_num_timeframe_comb_time, function(x) x[, c(setdiff(setdiff(names(x), 
    grep("days_to_last",names(x), value=T )), cohort_key_var_merge)):=NULL]))

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("days_to_last", 
    names(DT), value=T), paste0("lab_lab.numeric_lab.mean_temp", 
    grep("days_to_last", names(DT), value=T), name_ext)), 
    DT=lab_num_timeframe_comb_time, name_ext_extended))


  ##############################################################################
  ### reshapping & generating features - count variables for indicators - 
  ### categorical lab classifications  - impose feature  categorisation 
  ### ("lab_lab.indic_value_max/min.."..."_short/_long")
  ### non-numeric results
  # Note: By casting multiple value vars simultaneously - the sum of each value var
  # is created for each lab cat - i.e. every combination of lab cat & indicator - result 
  # in some variables with 100% missing 
  lab_cat_timeframe_comb <- lapply(lab_timeframe_comb, function(x)
    dcast.data.table(x, outcome_id + empi + pred_date  ~ 
    paste0("..", lab_cat), fun.agg = function(y) sum(as.numeric(y), na.rm=T), 
    value.var=grep("indic", names(x), value=T),
    subset=.(lab_result_type=="cat")))

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("indic", 
    names(DT), value=T), gsub("_function_\\.\\.", "..", paste0("lab_lab.non.numeric_lab.",
    gsub("indic\\.", "", grep("indic", names(DT), value=T)),name_ext))), 
    DT=lab_cat_timeframe_comb, name_ext_extended))

  lab_cat_timeframe_comb_time <- lapply(lab_timeframe_comb, function(x)
    dcast.data.table(x, outcome_id + empi + pred_date  ~ 
    paste0("..", lab_cat), fun.aggregate=list(length, function(x) min(x, na.rm=T)), 
    value.var = "time_diff", subset=.(lab_result_type=="cat")))

  lab_cat_timeframe_comb_time <- feature_var_format(lab_cat_timeframe_comb_time)
  invisible(lapply(lab_cat_timeframe_comb_time, function(x) x[, c(setdiff(setdiff(names(x), 
    grep("days_to_last",names(x), value=T )), cohort_key_var_merge)):=NULL]))

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("days_to_last", 
    names(DT), value=T), paste0("lab_lab.non.numeric", 
    grep("days_to_last", names(DT), value=T), name_ext)), 
    DT=lab_cat_timeframe_comb_time, name_ext_extended))

  # XXX NOTE: The dropping of features with 100% missing is taken care of as part 
  # of the compilation process (feature_construction_machinery)
  # lapply(lab_cat_timeframe_comb, function(x) set.zero.na(x))
    
  # lab_cat_timeframe_comb_colname <- lapply(lab_cat_timeframe_comb, function(x)
  #   sapply(x[, mget(grep("indic_", names(x), value=T))], function(y) 
  #   sum(!is.na(y))))
  
  # lab_cat_timeframe_comb_colname <- sapply(lab_cat_timeframe_comb_colname, 
  #   function(x) names(x[x==0]))

  # lab_cat_timeframe_comb <- mapply(function(DT, col_delete) DT[, 
  #   unlist(col_delete):=NULL], DT=lab_cat_timeframe_comb, 
  #   col_delete=lab_cat_timeframe_comb_colname)


  ##############################################################################
  ### merge lab feature files
  lab_feature_list <- list("lab_cat_timeframe_comb", "lab_num_timeframe_comb", 
    "lab_num_timeframe_comb_time", "lab_cat_timeframe_comb_time")

  timeframe_combine(lab_feature_list)

  lab <- Reduce(mymerge, mget(unlist(lab_feature_list)))

  ##############################################################################
  ### generate time.diff features - difference in means between two timeframes & 
  ### impose feature hierachy ("lab_lab.value_lab.mean.diff.."..."_diff")
  
  lab_list <- unique(gsub(paste0(name_ext_extended, collapse="|"), "", gsub(".*\\.\\.", "", 
    names(lab_num_timeframe_comb[, mget(grep("lab.mean", 
    names(lab_num_timeframe_comb), value=T))]))))

  if (length(name_ext)>1) {

    mapply(function(name_ext_1, name_ext_2) lab[, paste0("lab_lab.numeric_lab.mean.diff..", lab_list, 
      name_ext_1, name_ext_2, "_diff"):=lapply(lab_list, function(x) if (length(grep(paste0("mean..", x, name_ext_1), 
      names(lab), value=T))>0 & length(grep(paste0("mean..", x, name_ext_2), 
      names(lab), value=T))>0) {get(grep(paste0("mean..", x, name_ext_1), 
      names(lab), value=T)) - get(grep(paste0("mean..",x, name_ext_2), 
      names(lab), value=T))})], name_ext_1=name_ext[1:(length(name_ext)-1)],
      name_ext_2=name_ext[2:length(name_ext)])

  }

  lab[, paste0("lab_lab.numeric_lab.mean.diff..", lab_list, "_max", 
    "_diff"):=lapply(lab_list, function(x) if (length(grep(paste0("mean..", x, name_ext[1]), 
    names(lab), value=T))>0 & length(grep(paste0("mean..", x, name_ext[length(name_ext)]), 
    names(lab), value=T))>0) {get(grep(paste0("mean..", x, name_ext[1]), 
    names(lab), value=T)) - get(grep(paste0("mean..",x, name_ext[length(name_ext)]), 
    names(lab), value=T))})]

  ##############################################################################
  ### merge with cohort file - standarise NAs & round to two digits(numeric) vs.
  ### convert NAs to 0 (cat variables) 
  lab <- lab[cohort, mget(names(lab)), on=c("outcome_id", "empi", "pred_date")]

  lab_num <- lab[, mget(grep("lab.numeric", names(lab), value=T))]
  
  non_days_to_last_var <- setdiff(names(lab_num),grep("days_to_last", names(lab_num),value=T))
  set_na_zero(lab_num, replace=NA, subset_col=non_days_to_last_var)

  lab_num[, grep("lab_lab.numeric", names(lab_num), value=T):=lapply(.SD, function(x) 
    round(x, digits=2)), .SDcols=grep("lab_lab.numeric", names(lab_num))]

  lab_cat <- lab[, mget(grep("lab.non.numeric", names(lab), value=T))]
  
  non_days_to_last_var <- setdiff(names(lab_cat),grep("days_to_last", names(lab_cat),value=T))
  set_na_zero(lab_cat, subset_col=non_days_to_last_var)

  lab <- cbind(lab[, .(outcome_id, pred_date, empi)], lab_num, lab_cat)
  
  setnames(lab, gsub("mean_temp", "mean", names(lab)))

  ##############################################################################
  ### categorise variables to ensure proper treatment in models -- integer 
  lab_integer <- lab[, mget(grep("lab.non.numeric", names(lab), value=T))]
  lab_integer[, names(lab_integer):=lapply(.SD, function(x) as.integer(x))]

  lab_numeric <- lab[, mget(grep("lab.numeric", names(lab), value=T))]
  lab_numeric[, names(lab_numeric):=lapply(.SD, function(x) as.numeric(x))]

  lab <- cbind(lab[, mget(c("outcome_id", "pred_date", "empi"))], lab_integer,
    lab_numeric)

  lab[, ':='(lab_time_min=time_min, lab_time_max=time_max)]

  lab[, grep("lab_id$", names(lab), value=T):=NULL]

  ## deal with date variables
  feature_var_format_2(lab)

  ##############################################################################
  ### return labs & delete key files 
  rm(lab_numeric, lab_integer)
  rm(lab_timeframe_comb)
  rm(list=unlist(lab_feature_list))
  
  return(lab)

}

#----------------------------------------------------------------------------#

