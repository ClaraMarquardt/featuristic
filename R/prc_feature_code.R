#----------------------------------------------------------------------------#

#' @title Generate procedure-related features (prc data).
#'
#' @description /
#'
#' @export
#' @import data.table
#' @param file_date_var 
#' @return
#' @examples


prc_feature_gen <- function(file_date_var="prc_date") {

  ##############################################################################
  ### Load the  modified/pre-processed prc file for the specified data sample -- 
  ### if no such file exists - excute the function_prc_class.R code (access/submit as 
  ### batchmode job using machine/function_class_batchmode.txt)
    
  # (a) load the stored code - return error message if file does not exist
  tryCatch(prc <- readRDS_merge(prc_file_mod), warning=function(w)
    print("no classified prc file available for the data sample"))
    # XXX NOTE: RDS file preserves formatting of empi as character

  # subset to smaller sample for testing if so specified in control file
  if (test_raw_file==TRUE) {
    store_shorten_file("prc")
  }
  
  # (c) subset to the variables which are to be employed in the feature construction process

  # select only prcgnosis code related variables -- drop potential features (provider, 
  # clinic, hospital, inpatient_outpatient)
  prc[, c("prc_code_type","clinic_name", "hospital", "inpatient_outpatient", "provider"):=NULL]

  # select ccs (single/multi & zc) - category names rather than numbers
  prc[, grep("num", names(prc)):=NULL]

  ##############################################################################
  ### merge procedure file with cohort (cohort_key_variables) & format dates
  # XXX NOTE: foverlaps - ensures that all procedure max timeframe_long days 
  # prior to outcome date
  prc <- prc[empi %in% cohort$empi]

  invisible(parse_date(prc, c("prc_date")))

  prc[, c("prc_date_1","prc_date_2"):=.(prc_date)]

  prc <-foverlaps(prc, cohort[, mget(cohort_key_var_merge)], 
    by.x=c("empi","prc_date_1" , "prc_date_2"), nomatch=0)

  prc[, time_diff:=as.numeric(difftime(pred_date, get(file_date_var), 
    units="days"))]

  ### implement leakage control (as specified in control file - 
  ### omit day of outcome/days pre outcome)
  if (!is.na(leak_prc_day)) {
    prc <- prc[!(pred_date-prc_date_1<=leak_prc_day)]
  }

  ##############################################################################
  ### subsetting & dividing into smaller DT based on timeframe (ST/LT) - 
  ### return as list (...timeframe_comb)
  invisible(timeframe_split(list("prc"), "prc_date"))

  name_ext_extended <- name_ext_extended[sapply(prc_timeframe_comb, nrow)!=0]
  name_ext <- name_ext_extended[2:length(name_ext_extended)]
  prc_timeframe_comb <- prc_timeframe_comb[sapply(prc_timeframe_comb, nrow)!=0]

  time_min <- min(do.call("c", lapply(prc_timeframe_comb, function(x) as.Date(min(x[, 
    prc_date]), "%Y-%m-%d"))))
  time_max <- max(do.call("c", lapply(prc_timeframe_comb, function(x) as.Date(max(x[, 
    prc_date]), "%Y-%m-%d"))))

  ##############################################################################
  ### reshaping - create procedure code count vars (ccs_single) & impose feature 
  ### categorisation ("prc_single.ccs.."..."_short/_long")
  # XXX NOTE: CCS_single count vars == sum of ccs_single over timeperiod in question
  ccs_single_timeframe_comb <- lapply(prc_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + pred_date ~  paste0("prc_prc.count_prc.single.ccs..", 
    ccs_single_cat_name), fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff", 
    subset=.(!is.na(ccs_single_cat_name) & ccs_single_cat_name!="" )))

  ccs_single_timeframe_comb <- feature_var_format(ccs_single_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("prc_prc.count_prc.single.ccs", 
    names(DT), value=T), paste0(grep("prc_prc.count_prc.single.ccs", names(DT), value=T),
    name_ext)), DT=ccs_single_timeframe_comb, name_ext_extended))


  ##############################################################################
  ### reshaping - create prcgnosis code count vars (ccs_multi) & impose feature 
  ### categorisation ("prc_multi.ccs.."..."_short/_long")
  # XXX NOTE: CCS_multi count vars == sum of ccs_multi over timeperiod in question 
  # -> no attention paid to the hierarchy of the codes
  # XXX TO-DO: Figure out a better way of generating count vars for ccs_multi
  # XXX TO-DO: Use melt.data.table
  ccs_multi_timeframe_comb <- lapply(prc_timeframe_comb, function(x)
    rbindlist(list(x[, .(outcome_id, empi, pred_date, ccs_multi_cat_name=ccs_multi_cat_name_1, time_diff)],
    x[, .(outcome_id, empi, pred_date, ccs_multi_cat_name=ccs_multi_cat_name_2, time_diff)], 
    x[, .(outcome_id, empi, pred_date, ccs_multi_cat_name=ccs_multi_cat_name_3, time_diff)]),
    use.names=T))

  ccs_multi_timeframe_comb <- lapply(ccs_multi_timeframe_comb, function(x)
    dcast.data.table(x, outcome_id + empi + pred_date ~  paste0("prc_prc.count_prc.multi.ccs..",
    ccs_multi_cat_name), fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff", 
    subset=.(!is.na(ccs_multi_cat_name) & ccs_multi_cat_name!="" )))

  ccs_multi_timeframe_comb <- feature_var_format(ccs_multi_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("prc_prc.count_prc.multi.ccs", 
    names(DT), value=T), paste0(grep("prc_prc.count_prc.multi.ccs", names(DT), value=T),
    name_ext)), DT=ccs_multi_timeframe_comb, name_ext_extended))

  ##############################################################################
  ### merge prc feature files
  prc_feature_list <- list("ccs_single_timeframe_comb","ccs_multi_timeframe_comb")

  timeframe_combine(prc_feature_list)

  prc <- Reduce(mymerge, mget(unlist(prc_feature_list)))

  ##############################################################################
  ### merge with cohort file - empty records -> 0
  prc <- prc[cohort, mget(names(prc)), on=c("outcome_id", "empi", "pred_date")]

  non_days_to_last_var <- setdiff(names(prc),grep("days_to_last", names(prc),value=T))
  set_na_zero(prc, subset_col=non_days_to_last_var)

  ##############################################################################
  ### categorise variables to ensure proper treatment in models -- integer 
  prc_integer <- prc[, mget(setdiff(names(prc), c("outcome_id", "pred_date", "empi")))]
  prc_integer[, names(prc_integer):=lapply(.SD, function(x) as.integer(x))]

  prc <- cbind(prc[, mget(c("outcome_id", "pred_date", "empi"))], prc_integer)

  prc[, ':='(prc_time_min=time_min, prc_time_max=time_max)]

  prc[, grep("prc_id$", names(prc), value=T):=NULL]

  ## deal with date variables
  feature_var_format_2(prc)

  ##############################################################################
  ### return prc & delete key files
  rm(prc_integer)
  rm(prc_timeframe_comb)
  rm(list=unlist(prc_feature_list))

  return (prc)

}

#----------------------------------------------------------------------------#

