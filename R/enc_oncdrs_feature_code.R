 ##########################################
# DESCRIPTION: This file defines a function which generates enc_oncdrsgnosis 
# features for a given cohort from a raw .ONCDRS File
# ##########################################
# Creator: Clara Marquardt
# Date: 5th January 
# ##########################################
# Language: R
# ##########################################
# TO-DO-LIST 


################################################################################
########################  CREATE DIAGNOSIS FEATURES ##########################
################################################################################

 enc_oncdrs_feature_gen <- function() {

   ##############################################################################
  ### Load the  modified/pre-processed enc file for the specified data sample -- 
  ### if no such file exists - excute the function_enc_class.R code (access/submit as 
  ### batchmode job using machine/function_class_batchmode.txt)
  # XXX NOTE: Return DT list (4 DT) - enc_class, ed, clinic, ip
  
  # (a) load the stored code - return error message if file does not exist
  tryCatch(enc_oncdrs <- readRDS_merge(enc_oncdrs_file_mod), warning=function(w)
    print("no classified enc file available for the data sample"))
    # XXX NOTE: RDS file preserves formatting of empi as character

  # omit missing empi observations
  enc_oncdrs <- enc_oncdrs[!is.na(empi)]
  
  # subset to smaller sample for testing if so specified in control file
  if (test_raw_file==TRUE) {
    store_shorten_file("enc_oncdrs")
  }

  ##############################################################################
  ###  keep only the relevant columns 

  enc_oncdrs <- enc_oncdrs[, setdiff(names(enc_oncdrs), c(grep("enc_type|enc_dept",
  	names(enc_oncdrs), value=T), "empi", "patient_id", "adm_date")):=NULL]


  ##############################################################################
  ### merge enc_oncdrs file with cohort (cohort_key_variables) & format dates
  # XXX NOTE: foverlaps - ensures that all vital signs max timeframe_long days 
  # prior to outcome date
  enc_oncdrs <- enc_oncdrs[empi %in% cohort$empi]

  invisible(parse_date(enc_oncdrs, c("adm_date")))

  enc_oncdrs[, c("adm_date_1","adm_date_2"):=.(adm_date)]

  enc_oncdrs <-foverlaps(enc_oncdrs, cohort[, mget(cohort_key_var_merge)], 
  	by.x=c("empi","adm_date_1", "adm_date_2"), nomatch=0)


  ### implement leakage control (as specified in control file - 
  ### omit day of outcome/days pre outcome)
  if (!is.na(leak_oncdrs_enc_day)) {
    enc_oncdrs <- enc_oncdrs[!(pred_date-adm_date_1<=leak_oncdrs_enc_day)]
  }

  ##############################################################################
  ### subsetting & dividing into smaller DT based on timeframe (ST/LT) - 
  ### return as list (...timeframe_comb)
  invisible(timeframe_split(list("enc_oncdrs"), "adm_date"))

  name_ext_extended <- name_ext_extended[sapply(enc_oncdrs_timeframe_comb, nrow)!=0]
  name_ext <- name_ext_extended[2:length(name_ext_extended)]
  enc_oncdrs_timeframe_comb <- enc_oncdrs_timeframe_comb[sapply(enc_oncdrs_timeframe_comb, nrow)!=0]

  time_min <- min(do.call("c", lapply(enc_oncdrs_timeframe_comb, function(x) as.Date(min(x[, 
    adm_date]), "%Y-%m-%d"))))
  time_max <- max(do.call("c", lapply(enc_oncdrs_timeframe_comb, function(x) as.Date(max(x[, 
    adm_date]), "%Y-%m-%d"))))

  ##############################################################################
  ### Appointment type
  #### impose categories
  # enc_type_timeframe_comb <- lapply(enc_oncdrs_timeframe_comb, function(x) x[, mget(setdiff(names(x), 
  # 	grep("enc_dept", names(x), value=T)))])

  # enc_type_timeframe_comb <- lapply(enc_oncdrs_timeframe_comb , function(x) setnames(x, grep("enc_type", 
  # 	names(x), value=T), paste0("enc.dfci_enc.enc_enc.count_enc.type.count..", gsub("^_", "", 
  # 	gsub("(enc_type_)(.*)", "\\2", grep("enc_type", names(x), value=T))))))
 
   # enc_type_timeframe_comb <- lapply(enc_type_timeframe_comb , function(x) duplicate_col(
   #  x, method="sum"))

  # invisible(mapply(function(DT,name_ext) setnames(DT, grep("enc.dfci_enc.enc_enc.count_enc.type.count", 
  #   names(DT), value=T), paste0(grep("enc.dfci_enc.enc_enc.count_enc.type.count", names(DT), value=T),
  #   name_ext)), DT=enc_type_timeframe_comb ,  name_ext_extended))

  # ## sum -- > at the outcome_id level
  # enc_type_timeframe_comb <- lapply(enc_type_timeframe_comb, function(x) 
  # 	x[, grep("enc.dfci_enc.enc_enc.count_enc.type.count", names(x), value=T):=lapply(.SD, 
  # 		sum, na.rm=T), by=c("outcome_id"), 
  # 	.SDcols=grep("enc.dfci_enc.enc_enc.count_enc.type.count", names(x), value=T)])

  # enc_type_timeframe_comb <- lapply(enc_type_timeframe_comb, function(x) 
  # 	unique(x[, mget(c("empi", "outcome_id", "pred_date", 
  # 	grep("enc.dfci_enc.enc_enc.count_enc.type.count", names(x), value=T)))], 
  # 	by=c("outcome_id")))

  enc_type_timeframe_comb <- lapply(enc_oncdrs_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + pred_date ~  paste0("enc.dfci_enc.enc_enc.count_enc.type.count..", 
      enc_type), length, subset=.(!enc_type=="" & !is.na(enc_type))))

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("enc.dfci_enc.enc_enc.count_enc.type.count", 
    names(DT), value=T), gsub("\\*", "", paste0(grep("enc.dfci_enc.enc_enc.count_enc.type.count", names(DT), value=T),
    name_ext))), DT=enc_type_timeframe_comb ,  name_ext_extended))

  
  ##############################################################################
  ### Department name
  # dpt_name_timeframe_comb <- lapply(enc_oncdrs_timeframe_comb, function(x) x[, mget(setdiff(names(x), 
  # 	grep("appt_type", names(x), value=T)))])

  # dpt_name_timeframe_comb <- lapply(dpt_name_timeframe_comb, function(x) setnames(x, grep("dept_nm", 
  # 	names(x), value=T), paste0("enc.dfci_enc.enc_enc.count_department.name.count..", gsub("(dept_nm_)(.*)", 
  # 	"\\2", grep("dept_nm", names(x), value=T)))))
 

  # invisible(mapply(function(DT,name_ext) setnames(DT, 
  # 	grep("enc.dfci_enc.enc_enc.count_department.name.count", 
  #   names(DT), value=T), paste0(grep("enc.dfci_enc.enc_enc.count_department.name.count", 
  #   names(DT), value=T),name_ext)), DT=dpt_name_timeframe_comb,  name_ext_extended))

  #  ## sum -- > at the outcome_id level
  # dpt_name_timeframe_comb <- lapply(dpt_name_timeframe_comb, function(x) 
  # 	x[, grep("enc.dfci_enc.enc_enc.count_department.name.count", names(x), value=T):=lapply(.SD, 
  # 		sum, na.rm=T), by=c("outcome_id"), 
  # 	.SDcols=grep("enc.dfci_enc.enc_enc.count_department.name.count", names(x), value=T)])

  # dpt_name_timeframe_comb <- lapply(dpt_name_timeframe_comb, function(x) 
  # 	unique(x[, mget(c("empi", "outcome_id", "pred_date", 
  # 	grep("enc.dfci_enc.enc_enc.count_department.name.count", names(x), value=T)))], 
  # 	by=c("outcome_id")))

  enc_dept_timeframe_comb <- lapply(enc_oncdrs_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + pred_date ~  paste0("enc.dfci_enc.enc_enc.count_enc.department.count..", 
      enc_dept), length, subset=.(!enc_dept=="" & !is.na(enc_dept))))

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("enc.dfci_enc.enc_enc.count_enc.department.count", 
    names(DT), value=T), paste0(grep("enc.dfci_enc.enc_enc.count_enc.department.count", names(DT), value=T),
    name_ext)), DT=enc_dept_timeframe_comb ,  name_ext_extended))

 
  ##############################################################################
  ### merge enc_oncdrs feature files
  enc_oncdrs_feature_list <- list("enc_type_timeframe_comb", "enc_dept_timeframe_comb")

  timeframe_combine(enc_oncdrs_feature_list)

  enc_oncdrs <- Reduce(mymerge, mget(unlist(enc_oncdrs_feature_list)))

  ##############################################################################
  ### merge with cohort file - empty records -> 0
  enc_oncdrs <- enc_oncdrs[cohort, mget(names(enc_oncdrs)), on=c("outcome_id", "empi", 
    "pred_date")]
  set_na_zero(enc_oncdrs)

  ##############################################################################
  ### categorise variables to ensure proper treatment in models -- integer 
  enc_oncdrs_integer <- enc_oncdrs[, mget(setdiff(names(enc_oncdrs), c("outcome_id", "pred_date", "empi")))]
  enc_oncdrs_integer[, names(enc_oncdrs_integer):=lapply(.SD, function(x) as.integer(x))]

  enc_oncdrs <- cbind(enc_oncdrs[, mget(c("outcome_id", "pred_date", "empi"))], enc_oncdrs_integer)

  ### set names
  var_rename <- setdiff(names(enc_oncdrs), cohort_key_var_merge)
  setnames(enc_oncdrs,  var_rename , paste0(gsub("_", "_dfci.", gsub("(.*)(\\.\\.)(.*)", "\\1", 
    var_rename)), "..", gsub("(.*)(\\.\\.)(.*)", "\\3", 
    var_rename)))

  enc_oncdrs[, ':='(enc_oncdrs_time_min=time_min, enc_oncdrs_time_max=time_max)]

  enc_oncdrs[, grep("enc_id$", names(enc_oncdrs), value=T):=NULL]

  ##############################################################################
  ### return enc_oncdrs & delete key files 
  rm(enc_oncdrs_integer)
  rm(enc_oncdrs_timeframe_comb)
  rm(list=unlist(enc_oncdrs_feature_list))
  
  return(enc_oncdrs)

}


################################################################################
##################################  END  #######################################
################################################################################




