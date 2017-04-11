#----------------------------------------------------------------------------#

#' @title Generate ED-related features (ed_admin data).
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param order_file_date_var
#' @param enc_file_date_var
#' @return
#' @examples

ed_feature_gen <- function(order_file_date_var="order_enter_date", enc_file_date_var="adm_date") {

  ##############################################################################
  ### Load the  modified/pre-processed ed file for the specified data sample -- 
  ### if no such file exists - excute the function_ed_class.R code (access/submit as 
  ### batchmode job using machine/function_class_batchmode.txt)
    
  # (a) load the stored code - return error message if file does not exist
  tryCatch(ed <- readRDS_merge(ed_enc_file_mod,nested=TRUE), warning=function(w)
    print("no classified ed file available for the data sample"))
    # XXX NOTE: RDS file preserves formatting of empi as character

  ed_enc <- ed$ed_enc
  ed_order <- ed$ed_order

  # omit misisng empi
  ed_enc <- ed_enc[!is.na(empi)]
  ed_order <- ed_order[!is.na(empi)]
 
  # subset to smaller sample for testing if so specified in control file
  if (test_raw_file==TRUE) {
    store_shorten_file("ed_enc")
    store_shorten_file("ed_order")

  }

  ##############################################################################
  ### merge ed file with cohort (cohort_key_variables) & format dates
  # XXX NOTE: foverlaps - ensures that all vital signs max timeframe_long days 
  # prior to outcome date
  ed_enc <- ed_enc[empi %in% cohort$empi]
  ed_order <- ed_order[empi %in% cohort$empi]

  invisible(parse_date(ed_enc, c("adm_date")))
  invisible(parse_date(ed_order, c("order_enter_date")))

  ed_enc[, c("adm_date_1","adm_date_2"):=.(adm_date)]
  ed_order[, c("order_enter_date_1","order_enter_date_2"):=.(order_enter_date)]

  ed_enc <-foverlaps(ed_enc, cohort[, mget(cohort_key_var_merge)], by.x=c("empi",
    "adm_date_1", "adm_date_2"), nomatch=0)

  ed_order <- foverlaps(ed_order[!is.na(order_enter_date)], cohort[, mget(cohort_key_var_merge)], 
    by.x=c("empi",
    "order_enter_date_1", "order_enter_date_2"), nomatch=0)

 ed_enc[, time_diff:=as.numeric(difftime(pred_date, get(enc_file_date_var), 
    units="days"))]
 ed_order[, time_diff:=as.numeric(difftime(pred_date, get(order_file_date_var), 
    units="days"))]
 

  ### implement leakage control (as specified in control file - 
  ### omit day of outcome/days pre outcome)
  if (!is.na(leak_ed_day)) {
    ed_enc <- ed_enc[!(pred_date-adm_date_1<=leak_ed_day)]
    ed_order <- ed_order[!(pred_date-order_enter_date_1<=leak_ed_day)]

  }

  ##############################################################################
  ### subsetting & dividing into smaller DT based on timeframe (ST/LT) - 
  ### return as list (...timeframe_comb)
  invisible(timeframe_split(list("ed_enc"), "adm_date"))
  invisible(timeframe_split(list("ed_order"), "order_enter_date"))

  name_ext_extended <- name_ext_extended[sapply(ed_enc_timeframe_comb, nrow)!=0]
  name_ext <- name_ext_extended[2:length(name_ext_extended)]
 
  ed_enc_timeframe_comb <- ed_enc_timeframe_comb[sapply(ed_enc_timeframe_comb, nrow)!=0]
  ed_order_timeframe_comb <- ed_order_timeframe_comb[sapply(ed_order_timeframe_comb, nrow)!=0]

  time_min_ed_enc <- min(do.call("c", lapply(ed_enc_timeframe_comb, function(x) as.Date(min(x[, 
    adm_date]), "%Y-%m-%d"))))
  time_max_ed_enc <- max(do.call("c", lapply(ed_enc_timeframe_comb, function(x) as.Date(max(x[, 
    adm_date]), "%Y-%m-%d"))))

  time_min_ed_order <- min(do.call("c", lapply(ed_order_timeframe_comb, function(x) as.Date(min(x[, 
   order_enter_date]), "%Y-%m-%d"))))
  time_max_ed_order <- max(do.call("c", lapply(ed_order_timeframe_comb, function(x) as.Date(max(x[, 
   order_enter_date]), "%Y-%m-%d"))))


  ##############################################################################
  ### reshaping - create ed cc count vars (zc) & impose feature 
  ### categorisation ("ed_ed.chief.complaint.count.."..."_short/_long")

  ed_cc_timeframe_comb <- lapply(ed_enc_timeframe_comb, function(x)
    melt(x, id.vars =c("outcome_id", "empi", "pred_date","time_diff"),
    measure=patterns("^ed_cc_[0-9]"), variable.name="ed_cc_name", 
    value.name="ed_cc"))

  # XXX NOTE: ED CC count vars == sum of ed cc dummies over timeperiod in question
  ed_cc_timeframe_comb <- lapply(ed_cc_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + pred_date ~  paste0("ed_ed.chief.complaint.count_ed.zc..", 
    ed_cc1), 
    fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff",
    subset=.(!is.na(ed_cc1))))

  ed_cc_timeframe_comb <- feature_var_format(ed_cc_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("ed_ed.chief.complaint.count", 
    names(DT), value=T), paste0(grep("ed_ed.chief.complaint.count_ed.zc", names(DT), value=T),name_ext)), 
    DT=ed_cc_timeframe_comb,  name_ext_extended))

  ##############################################################################
  ### reshaping - create ed med order count vars  (snomed) & impose feature 
  ### categorisation ("ed_ed.med.order.count.."..."_short/_long")

  # XXX NOTE: ED Med order count vars == sum of ed cc dummies over timeperiod in question
  ed_med_order_timeframe_comb <- lapply(ed_order_timeframe_comb, function(x)
    melt(x, id.vars =c("outcome_id", "empi", "pred_date", "time_diff"),
    measure=patterns("^snomed_destin"), variable.name="snomed_destin_name", 
    value.name="snomed_destin"))

  ed_med_order_timeframe_comb <- lapply(ed_med_order_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + pred_date ~  paste0("med_", snomed_destin1), 
    fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff",
    subset=.(!snomed_destin1=="" & !is.na(snomed_destin1))))

  ed_med_order_timeframe_comb <- feature_var_format(ed_med_order_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("med_", 
    names(DT), value=T), paste0("ed_ed.med.order.count_ed.snomed", gsub("(.*?)med_(.*)", "\\1..med_\\2",
    grep("med_", names(DT), value=T)),name_ext)), DT = ed_med_order_timeframe_comb, 
    name_ext_extended))

  ##############################################################################
  ### reshaping - create ed med order count vars  (snomed) NO ANTI & impose feature 
  ### categorisation ("ed_ed.med.order.count.."..."_short/_long")

  # XXX NOTE: ED Med order count vars == sum of ed cc dummies over timeperiod in question
  ed_med_order_excl_anti_timeframe_comb <- lapply(ed_order_timeframe_comb, function(x)
    melt(x, id.vars =c("outcome_id", "empi", "pred_date", "snomed_anti_cat", "time_diff"),
    measure=patterns("^snomed_destin"), variable.name="snomed_destin_name", 
    value.name="snomed_destin"))

  ed_med_order_excl_anti_timeframe_comb <- lapply(ed_med_order_excl_anti_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + pred_date ~  paste0("med_", snomed_destin1), 
    fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff",
    subset=.(!snomed_destin1=="" & !is.na(snomed_destin1) & is.na(snomed_anti_cat))))

  ed_med_order_excl_anti_timeframe_comb <- feature_var_format(ed_med_order_excl_anti_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("med_", 
    names(DT), value=T), paste0("ed_ed.med.order.count_ed.snomed.excl.anti", gsub("(.*?)med_(.*)", 
    "\\1..med_\\2",grep("med_", names(DT), value=T)),name_ext)), 
    DT = ed_med_order_excl_anti_timeframe_comb, name_ext_extended))

  ##############################################################################
  ### reshaping - create ed med order count vars  (snomed) NO CHEMO & impose feature 
  ### categorisation ("ed_ed.med.order.count.."..."_short/_long")

  # XXX NOTE: ED Med order count vars == sum of ed cc dummies over timeperiod in question
  ed_med_order_excl_chemo_timeframe_comb <- lapply(ed_order_timeframe_comb, function(x)
    melt(x, id.vars =c("outcome_id", "empi", "pred_date", "snomed_chemo_cat", "time_diff"),
    measure=patterns("^snomed_destin"), variable.name="snomed_destin_name", 
    value.name="snomed_destin"))

  ed_med_order_excl_chemo_timeframe_comb <- lapply(ed_med_order_excl_chemo_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + pred_date ~  paste0("med_", snomed_destin1), 
    fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff",
    subset=.(!snomed_destin1=="" & !is.na(snomed_destin1) & is.na(snomed_chemo_cat))))

  ed_med_order_excl_chemo_timeframe_comb <- feature_var_format(ed_med_order_excl_chemo_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("med_", 
    names(DT), value=T), paste0("ed_ed.med.order.count_ed.snomed.excl.chemo", 
    gsub("(.*?)med_(.*)", "\\1..med_\\2",grep("med_", names(DT), value=T)),name_ext)), 
    DT = ed_med_order_excl_chemo_timeframe_comb, name_ext_extended))

  ##############################################################################
  ### reshaping - create ed med order count vars  -- ANTIBIOTIC(snomed) & impose feature 
  ### categorisation ("ed_ed.med.order.count.."..."_short/_long")

  # XXX NOTE: ED Med order count vars == sum of ed cc dummies over timeperiod in question
  ed_med_order_anti_timeframe_comb <- lapply(ed_order_timeframe_comb, function(x) 
    if(nrow(x[!snomed_anti_cat=="" & !is.na(snomed_anti_cat)])>0) {dcast.data.table(x, outcome_id + empi + pred_date ~  
    paste0("med_anti_", snomed_anti_cat),
    fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff",
    subset=.(!snomed_anti_cat=="" & !is.na(snomed_anti_cat)))} else {
      data.table(empi = character(0), outcome_id = numeric(0), pred_date=numeric(0))})

  ed_med_order_anti_timeframe_comb <- feature_var_format(ed_med_order_anti_timeframe_comb)

  invisible(mapply(function(DT,name_ext) if (length(grep("med_", 
    names(DT), value=T))>0) {setnames(DT, grep("med_", 
    names(DT), value=T), paste0("ed_ed.med.order.count_ed.snomed.anti", gsub("(.*?)med_anti_(.*)", 
    "\\1..med_anti_\\2",grep("med_", names(DT), value=T)),name_ext))}, 
    DT = ed_med_order_anti_timeframe_comb, name_ext_extended))

  ed_med_order_anti_timeframe_comb <- lapply(ed_med_order_anti_timeframe_comb, function(x) 
    x[, ed_ed.med.order.count_ed.snomed.anti..any:=rowSums(.SD), .SDcols=
    grep("ed_ed.med.order.count_ed.snomed.anti",names(x), value=T)])

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("ed_ed.med.order.count_ed.snomed.anti..any", 
    names(DT), value=T), paste0(grep("ed_ed.med.order.count_ed.snomed.anti..any", 
    names(DT), value=T),name_ext)), DT = ed_med_order_anti_timeframe_comb, name_ext_extended))


  ##############################################################################
  ### reshaping - create ed med order count vars  -- CHEMO(snomed) & impose feature 
  ### categorisation ("ed_ed.med.order.count.."..."_short/_long")

  if (Reduce(`&`, sapply(c(1:length(name_ext)), function(x) 
    nrow(ed_order_timeframe_comb[[x]][!is.na(snomed_chemo_cat)])>0))) {
     # XXX NOTE: ED Med order count vars == sum of ed cc dummies over timeperiod in question
     ed_med_order_chemo_timeframe_comb <- lapply(ed_order_timeframe_comb, function(x) 
       dcast.data.table(x, outcome_id + empi + pred_date ~  paste0("med_chemo_", snomed_chemo_cat) ,
       fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff",
       subset=.(!snomed_chemo_cat=="" & !is.na(snomed_chemo_cat))))

  ed_med_order_chemo_timeframe_comb <- feature_var_format(ed_med_order_chemo_timeframe_comb)

     invisible(mapply(function(DT,name_ext) setnames(DT, grep("med_", 
       names(DT), value=T), paste0("ed_ed.med.order.count_ed.snomed.chemo", 
       gsub("(.*?)med_chemo_(.*)", "\\1..med_chemo_\\2",
       grep("med_", names(DT), value=T)),name_ext)), DT = ed_med_order_chemo_timeframe_comb, 
       name_ext_extended))

     ed_med_order_chemo_timeframe_comb <- lapply(ed_med_order_chemo_timeframe_comb, function(x) 
       x[, ed_ed.med.order.count_ed.snomed.chemo..any:=rowSums(.SD), 
       .SDcols=grep("ed_ed.med.order.count_ed.snomed.chemo",names(x), value=T)])

     invisible(mapply(function(DT,name_ext) setnames(DT, grep("ed_ed.med.order.count_ed.snomed.chemo..any", 
       names(DT), value=T), paste0(grep("ed_ed.med.order.count_ed.snomed.chemo..any", 
       names(DT), value=T),name_ext)), DT = ed_med_order_chemo_timeframe_comb, name_ext_extended))

  }

  ##############################################################################
  ### merge dia feature files
  if (Reduce(`&`, sapply(c(1:length(name_ext)), function(x) 
    nrow(ed_order_timeframe_comb[[x]][!is.na(snomed_chemo_cat)])>0))) {
     ed_feature_list <- list("ed_cc_timeframe_comb", "ed_med_order_timeframe_comb", 
       "ed_med_order_anti_timeframe_comb", "ed_med_order_chemo_timeframe_comb",
       "ed_med_order_excl_anti_timeframe_comb", "ed_med_order_excl_chemo_timeframe_comb")
    } else {
       ed_feature_list <- list("ed_cc_timeframe_comb", "ed_med_order_timeframe_comb", 
       "ed_med_order_anti_timeframe_comb",
       "ed_med_order_excl_anti_timeframe_comb", "ed_med_order_excl_chemo_timeframe_comb")
    }

  timeframe_combine(ed_feature_list)

  ed <- Reduce(mymerge, mget(unlist(ed_feature_list)))

  ##############################################################################
  ### merge with cohort file - empty records -> 0
  ed <- ed[cohort, mget(names(ed)), on=c("outcome_id", "empi", "pred_date")]
  
  non_days_to_last_var <- setdiff(names(ed),grep("days_to_last", names(ed),value=T))
  set_na_zero(ed, subset_col=non_days_to_last_var)


  ##############################################################################
  ### categorise variables to ensure proper treatment in models -- integer 
  ed_integer <- ed[, mget(setdiff(names(ed), c("outcome_id", "pred_date", "empi")))]
  ed_integer[, names(ed_integer):=lapply(.SD, function(x) as.integer(x))]

  ed <- cbind(ed[, mget(c("outcome_id", "pred_date", "empi"))], ed_integer)

  ed[, ':='(ed_time_min=min(time_min_ed_enc,time_min_ed_order), 
    ed_time_max=max(time_max_ed_enc, time_max_ed_order))]

  ed[, grep("ed_(enc|order)_id$", names(ed), value=T):=NULL]

  feature_var_format_2(ed)

  ##############################################################################
  ### return ed & delete key files 
  rm(ed_integer)
  rm(list=unlist(ed_feature_list))
  
  return (ed)
}


#----------------------------------------------------------------------------#











