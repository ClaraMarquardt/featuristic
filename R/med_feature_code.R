
################################################################################
# DESCRIPTION: This file defines a function which generates medicine 
# features for a given cohort from a raw RPDR .Med and .Lme file
# ##########################################
# Creator: Clara Marquardt
# Date: 5th January 
# ##########################################
# Language: R
# ##########################################
# TO-DO-LIST 



################################################################################
################################  CREATE MED FEATURES ##########################
################################################################################

med_feature_gen <- function(med_file_mod=med_file_mod, leak_med_day=leak_med_day, 
  combine=FALSE,  med_file_mod_ext=NA, med_file_mod_ext_ext=NA, 
  file_date_var="med_date") {


  ##############################################################################
  ### Load the  modified/pre-processed med file for the specified data sample -- 
  ### if no such file exists - excute the function_med_class.R code (access/submit as 
  ### batchmode job using machine/function_class_batchmode.txt)
    
  # (a) load the stored code - return error message if file does not exist
  tryCatch(med <- readRDS_merge(med_file_mod), warning=function(w)
    print("no classified med file available for the data sample"))
    # XXX NOTE: RDS file preserves formatting of empi as character
  
  if (combine==TRUE) {
    tryCatch(med_ext <- readRDS_merge(med_file_mod_ext), warning=function(w)
      print("no classified med file available for the data sample"))
    
    tryCatch(med_ext_ext <- readRDS_merge(med_file_mod_ext_ext), warning=function(w)
      print("no classified med file available for the data sample"))

    # temp
    med[, med_date_1:=med_date] 
    med_ext[, med_date_1:=med_date] 
    med_ext_ext[, med_date_1:=med_date]

    med_ext_ext[, activation_dt:=NULL]
    med_ext_ext[, indication_dt:=NULL]
    
    med <- rbindlist(list(med_ext, med), fill=T, use.names=T)
    

    med <- rbindlist(list(med, med_ext_ext), fill=T, use.names=T)
    med[, med_id:=1:nrow(med)]
 
  }

  # remove if empi is missing
  med <- med[!is.na(empi)]

  # subset to smaller sample for testing if so specified in control file
  if (test_raw_file==TRUE) {
    store_shorten_file("med")
  }


  ##############################################################################
  ### load the dia_feature code

  ## if no med_form -- assume administered
  if(!("med_form" %in% names(med))) med[,med_form:=""]
  med[is.na(med_form)|med_form=="", med_form:="adm"]
  
  ## if no chemo cat nci
  if(!("chemo_cat_nci" %in% names(med))) med[,chemo_cat_nci:=""]

  ##############################################################################
  ### merge med file with cohort (cohort_key_variables) & format dates
  # XXX NOTE: foverlaps - ensures that all vital signs max timeframe_long days 
  # prior to outcome date
  med <- med[empi %in% cohort$empi]

  invisible(parse_date(med, c("med_date")))

  med[, c("med_date_1","med_date_2"):=.(med_date)]

  med <-foverlaps(med, cohort[, mget(cohort_key_var_merge)], by.x=c("empi",
    "med_date_1", "med_date_2"), nomatch=0)

  med[, time_diff:=as.numeric(difftime(pred_date, get(file_date_var), 
    units="days"))]

  ### implement leakage control (as specified in control file - 
  ### omit day of outcome/days pre outcome)
  if (!is.na(leak_med_day)) {
    med <- med[!(pred_date-med_date_1<=leak_med_day)]
  }


  ##############################################################################
  ### subsetting & dividing into smaller DT based on timeframe (ST/LT) - 
  ### return as list (...timeframe_comb)
  invisible(timeframe_split(list("med"), "med_date"))

  name_ext_extended <- name_ext_extended[sapply(med_timeframe_comb, nrow)!=0]
  name_ext <- name_ext_extended[2:length(name_ext_extended)]
  med_timeframe_comb <- med_timeframe_comb[sapply(med_timeframe_comb, nrow)!=0]


  time_min <- min(do.call("c", lapply(med_timeframe_comb, function(x) as.Date(min(x[, 
    med_date]), "%Y-%m-%d"))))
  time_max <- max(do.call("c", lapply(med_timeframe_comb, function(x) as.Date(max(x[, 
    med_date]), "%Y-%m-%d"))))


  ##############################################################################
  ### reshaping - create med count vars  & impose feature 
  ### categorisation ("med_count_adm.." or "med_count_pres.." "..."_short/_long")
  # XXX NOTE: med count vars == sum of med over timeperiod in question (no hierarchy)
  # XXX TO-DO: Fix value.name - currently names "med_destin1" automatically even if specify 
  # med_destin as name
  med_snomed_timeframe_comb <- lapply(med_timeframe_comb, function(x)
    melt(x, id.vars =c("outcome_id", "empi", "pred_date", "med_form", "time_diff"),
    measure=patterns("^snomed_destin"), variable.name="med_destin_name", 
    value.name="med_destin"))

  med_snomed_timeframe_comb <- lapply(med_snomed_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + pred_date ~  med_form + med_destin1, 
    fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff", 
    subset=.(!med_destin1=="" & !is.na(med_destin1))))

  med_snomed_timeframe_comb <- feature_var_format(med_snomed_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("pres|adm", 
    names(DT), value=T), paste0("med_med.count_med.snomed_med.", gsub("(.*?)_(.*)", "\\1..\\2",
    grep("pres|adm", names(DT), value=T)),name_ext)), DT = med_snomed_timeframe_comb, 
    name_ext_extended))


  ##############################################################################
  ### reshaping - create med count vars  & impose feature 
  ### categorisation ("med_count_adm.." or "med_count_pres.." "..."_short/_long")
  # XXX NOTE: med count vars == sum of med over timeperiod in question (no hierarchy)
  # XXX TO-DO: Fix value.name - currently names "med_destin1" automatically even if specify 
  # med_destin as name
  med_atc_timeframe_comb <- lapply(med_timeframe_comb, function(x)
    melt(x, id.vars =c("outcome_id", "empi", "pred_date", "med_form", "time_diff"),
    measure=patterns("^atc_cat_"), variable.name="med_destin_name", 
    value.name="med_destin"))

  med_atc_timeframe_comb <- lapply(med_atc_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + pred_date ~  med_form + med_destin1, 
    fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff",
     subset=.(!med_destin1=="" & !is.na(med_destin1))))

  med_atc_timeframe_comb <- feature_var_format(med_atc_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("pres|adm", 
    names(DT), value=T), paste0("med_med.count_med.atc_med.", gsub("(.*?)_(.*)", "\\1..\\2",
    grep("pres|adm", names(DT), value=T)),name_ext)), DT = med_atc_timeframe_comb, 
    name_ext_extended))


  #############################################################################
  ## reshaping - create ed med order count vars  (snomed) NO ANTI & impose feature 
  ## categorisation ("ed_ed.med.order.count.."..."_short/_long")

  med_snomed_excl_anti_timeframe_comb <- lapply(med_timeframe_comb, function(x)
    melt(x, id.vars =c("outcome_id", "empi", "pred_date", "med_form", "snomed_anti_cat", "time_diff"),
    measure=patterns("^snomed_destin"), variable.name="med_destin_name", 
    value.name="med_destin"))

  med_snomed_excl_anti_timeframe_comb <- lapply(med_snomed_excl_anti_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + pred_date ~  med_form + med_destin1, 
    fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff",
     subset=.(!med_destin1=="" & !is.na(med_destin1) & is.na(snomed_anti_cat))))

  med_snomed_excl_anti_timeframe_comb <- feature_var_format(med_snomed_excl_anti_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("pres|adm", 
    names(DT), value=T), paste0("med_med.count_med.snomed.excl.anti_excl.anti.med.", gsub("(.*?)_(.*)", "\\1..\\2",
    grep("pres|adm", names(DT), value=T)),name_ext)), DT = med_snomed_excl_anti_timeframe_comb, 
    name_ext_extended))


  ##############################################################################
  ### reshaping - create ed med order count vars  (snomed) NO CHEMO & impose feature 
  ### categorisation ("ed_ed.med.order.count.."..."_short/_long")

  # XXX NOTE: ED Med order count vars == sum of ed cc dummies over timeperiod in question
  med_snomed_excl_chemo_timeframe_comb <- lapply(med_timeframe_comb, function(x)
    melt(x, id.vars =c("outcome_id", "empi", "pred_date", "med_form", "snomed_chemo_cat", "time_diff"),
    measure=patterns("^snomed_destin"), variable.name="med_destin_name", 
    value.name="med_destin"))

  med_snomed_excl_chemo_timeframe_comb <- lapply(med_snomed_excl_chemo_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + pred_date ~  med_form + med_destin1, 
    fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff",
    subset=.(!med_destin1=="" & !is.na(med_destin1) & is.na(snomed_chemo_cat))))

  med_snomed_excl_chemo_timeframe_comb <- feature_var_format(med_snomed_excl_chemo_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("pres|adm", 
    names(DT), value=T), paste0("med_med.count_med.snomed.excl.chemo_excl.chemo.med.", gsub("(.*?)_(.*)", "\\1..\\2",
    grep("pres|adm", names(DT), value=T)),name_ext)), DT = med_snomed_excl_chemo_timeframe_comb, 
    name_ext_extended))


  ##############################################################################
  ### reshaping - create ed med order count vars  -- ANTIBIOTIC(snomed) & impose feature 
  ### categorisation ("ed_ed.med.order.count.."..."_short/_long")

  # XXX NOTE: ED Med order count vars == sum of ed cc dummies over timeperiod in question
  med_snomed_anti_timeframe_comb <- lapply(med_timeframe_comb, function(x)
    melt(x, id.vars =c("outcome_id", "empi", "pred_date", "med_form", "snomed_anti_cat", "time_diff"),
    measure=patterns("^snomed_destin"), variable.name="med_destin_name", 
    value.name="med_destin"))

  med_snomed_anti_timeframe_comb <- lapply(med_snomed_anti_timeframe_comb, function(x) 
    if(nrow(x[!med_destin1=="" & !is.na(snomed_anti_cat)])>0) { dcast.data.table(x, outcome_id + empi + pred_date ~  
    med_form + snomed_anti_cat, 
    fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff",
    subset=.(!med_destin1=="" & !is.na(snomed_anti_cat)))} else {data.table (empi = character(0), 
    outcome_id = numeric(0), pred_date=numeric(0))})

  med_snomed_anti_timeframe_comb <- feature_var_format(med_snomed_anti_timeframe_comb)

  invisible(mapply(function(DT,name_ext) if (length(grep("pres|adm", 
    names(DT), value=T))>0) {setnames(DT, grep("pres|adm", 
    names(DT), value=T), paste0("med_med.count_med.snomed.anti_anti.med.", gsub("(.*?)_(.*)", "\\1..\\2",
    grep("pres|adm", names(DT), value=T)),name_ext))}, DT = med_snomed_anti_timeframe_comb, 
    name_ext_extended))

  med_snomed_anti_timeframe_comb <- lapply(med_snomed_anti_timeframe_comb, 
    function(x) if(length(grep("med_med.count_med.snomed.anti_anti.med.pres",
    names(x), value=T))>0) {x[, med_med.count_med.snomed.anti_anti.med.pres..any:=
    rowSums(.SD), .SDcols=grep("med_med.count_med.snomed.anti_anti.med.pres",
    names(x), value=T)]} else {x})

  med_snomed_anti_timeframe_comb <- lapply(med_snomed_anti_timeframe_comb, 
    function(x) if(length(grep("med_med.count_med.snomed.anti_anti.med.adm",
    names(x), value=T))>0) {x[, med_med.count_med.snomed.anti_anti.med.adm..any:=
    rowSums(.SD), .SDcols=grep("med_med.count_med.snomed.anti_anti.med.adm",
    names(x), value=T)]} else {x})

  invisible(mapply(function(DT,name_ext) if(length(grep("med_med.count_med.snomed.anti_anti.med.*any", 
    names(DT), value=T))>0) {setnames(DT, grep("med_med.count_med.snomed.anti_anti.med.*any", 
    names(DT), value=T), paste0(grep("med_med.count_med.snomed.anti_anti.med.*any", 
    names(DT), value=T),name_ext))}, DT = med_snomed_anti_timeframe_comb, name_ext_extended))


  ##############################################################################
  ### reshaping - create ed med order count vars  -- ANTIBIOTIC(snomed) & impose feature 
  ### categorisation ("ed_ed.med.order.count.."..."_short/_long")

  # XXX NOTE: ED Med order count vars == sum of ed cc dummies over timeperiod in question

  med_anti_timeframe_comb <- lapply(med_timeframe_comb, function(x) if(nrow(x[!is.na(snomed_anti_cat)])>0) {
    dcast.data.table(x, outcome_id + empi + pred_date ~  paste0("..anti_", snomed_anti_cat), 
    fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff",
    subset=.(!is.na(snomed_anti_cat)))} else {data.table(empi = character(0), 
      outcome_id = numeric(0), pred_date=numeric(0))})

  med_anti_timeframe_comb <- feature_var_format(med_anti_timeframe_comb)

  invisible(mapply(function(DT,name_ext) if(length(grep("..anti_", 
    names(DT), value=T))>0) {setnames(DT, grep("..anti_", 
    names(DT), value=T), paste0("med_med.count_med.anti", grep("..anti_", names(DT), 
    value=T),name_ext))}, DT = med_anti_timeframe_comb, name_ext=name_ext_extended))


  ##############################################################################
  ### reshaping - create ed med order count vars  -- ANTIBIOTIC(snomed) & impose feature 
  ### categorisation ("ed_ed.med.order.count.."..."_short/_long")

  # XXX NOTE: ED Med order count vars == sum of ed cc dummies over timeperiod in question

  med_chemo_timeframe_comb <- lapply(med_timeframe_comb, function(x) if(nrow(x[!is.na(snomed_chemo_cat)])>0) {
    dcast.data.table(x, outcome_id + empi + pred_date ~  paste0("..chemo_", snomed_chemo_cat), 
    fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff",
     subset=.(!is.na(snomed_chemo_cat)))} else {data.table (empi = character(0), 
      outcome_id = numeric(0), pred_date=numeric(0))})

  med_chemo_timeframe_comb <- feature_var_format(med_chemo_timeframe_comb)

  invisible(mapply(function(DT,name_ext) if(length(grep("..chemo_", 
    names(DT), value=T))>0) { setnames(DT, grep("..chemo_", 
    names(DT), value=T), paste0("med_med.count_med.chemo", grep("..chemo_", names(DT), 
    value=T),name_ext))}, DT = med_chemo_timeframe_comb, name_ext=name_ext_extended))


  ##############################################################################
  ### reshaping - create ed med order count vars  -- CHEMO(snomed) & impose feature 
  ### categorisation ("ed_ed.med.order.count.."..."_short/_long")

  # XXX NOTE: ED Med order count vars == sum of ed cc dummies over timeperiod in question
  med_snomed_chemo_timeframe_comb <- lapply(med_timeframe_comb, function(x)
    melt(x, id.vars =c("outcome_id", "empi", "pred_date", "med_form", "snomed_chemo_cat", "time_diff"),
    measure=patterns("^snomed_destin"), variable.name="med_destin_name", 
    value.name="med_destin"))

  med_snomed_chemo_timeframe_comb <- lapply( med_snomed_chemo_timeframe_comb , function(x) 
    if (nrow(x[!!med_destin1=="" & !is.na(snomed_chemo_cat)])>0) {dcast.data.table(x, outcome_id + empi + pred_date ~  med_form + snomed_chemo_cat, 
    fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff",
    subset=.(!med_destin1=="" & !is.na(snomed_chemo_cat)))} else {data.table (empi = character(0), 
      outcome_id = numeric(0), pred_date=numeric(0))})

  med_snomed_chemo_timeframe_comb <- feature_var_format(med_snomed_chemo_timeframe_comb)

  invisible(mapply(function(DT,name_ext) if (length(grep("pres|adm", 
    names(DT), value=T))>0) {setnames(DT, grep("pres|adm", 
    names(DT), value=T), paste0("med_med.count_med.snomed.chemo_chemo.med.", gsub("(.*?)_(.*)", "\\1..\\2",
    grep("pres|adm", names(DT), value=T)),name_ext))}, DT = med_snomed_chemo_timeframe_comb, 
    name_ext_extended))

  med_snomed_chemo_timeframe_comb <- lapply(med_snomed_chemo_timeframe_comb, 
    function(x) if(length(grep("med_med.count_med.snomed.chemo_chemo.med.pres",
    names(x), value=T))>0) {x[, med_med.count_med.snomed.chemo_chemo.med.pres..any:=
    rowSums(.SD), .SDcols=grep("med_med.count_med.snomed.chemo_chemo.med.pres",
    names(x), value=T)]} else {x})

  med_snomed_chemo_timeframe_comb <- lapply(med_snomed_chemo_timeframe_comb, 
    function(x) if(length(grep("med_med.count_med.snomed.chemo_chemo.med.adm",
    names(x), value=T))>0) {x[, med_med.count_med.snomed.chemo_chemo.med.adm..any:=
    rowSums(.SD), .SDcols=grep("med_med.count_med.snomed.chemo_chemo.med.adm",
    names(x), value=T)]} else {x})

  invisible(mapply(function(DT,name_ext) if(length(grep("med_med.count_med.snomed.chemo_chemo.med.*any", 
    names(DT), value=T))>0) {setnames(DT, grep("med_med.count_med.snomed.chemo_chemo.med.*any", 
    names(DT), value=T), paste0(grep("med_med.count_med.snomed.chemo_chemo.med.*any", 
    names(DT), value=T),name_ext))}, DT = med_snomed_chemo_timeframe_comb, name_ext_extended))


  ##############################################################################
  ### reshaping - create nci chemo features

  if (nrow(med_timeframe_comb[[length(name_ext_extended)]][!(is.na(chemo_cat_nci)|chemo_cat_nci=="null")])>0) {
      # XXX NOTE: ED Med order count vars == sum of ed cc dummies over timeperiod in question
      med_nci_chemo_timeframe_comb <- lapply(med_timeframe_comb, function(x)
        melt(x, id.vars =c("outcome_id", "empi", "pred_date", "time_diff"),
        measure=patterns("^chemo_cat_nci"), variable.name="chemo_cat_nci_name", 
        value.name="chemo_cat_nci"))

      med_nci_chemo_timeframe_comb  <- lapply(med_nci_chemo_timeframe_comb, function(x) 
        dcast.data.table(x, outcome_id + empi + pred_date ~  paste0("..chemo_nci_", chemo_cat_nci1), 
        fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff",
        subset=.(!(is.na(chemo_cat_nci1)|chemo_cat_nci1=="null"))))

      med_nci_chemo_timeframe_comb <- feature_var_format(med_nci_chemo_timeframe_comb)

      invisible(mapply(function(DT,name_ext) setnames(DT, grep("..chemo_nci", 
        names(DT), value=T), paste0("med_med.count_med.chemo.nci", grep("..chemo_nci", names(DT), 
        value=T),name_ext)), DT = med_nci_chemo_timeframe_comb , name_ext=name_ext_extended))
   }


  ##############################################################################
  ### merge med feature files
  med_feature_list <- list("med_snomed_timeframe_comb", 
    "med_snomed_excl_anti_timeframe_comb",
    "med_snomed_excl_chemo_timeframe_comb",
    "med_snomed_anti_timeframe_comb",
    "med_snomed_chemo_timeframe_comb", 
    "med_anti_timeframe_comb",
    "med_chemo_timeframe_comb", 
    "med_nci_chemo_timeframe_comb", 
    "med_atc_timeframe_comb")

  med_feature_list <- med_feature_list[which(med_feature_list %in% ls())]

  timeframe_combine(med_feature_list)

  lapply(med_feature_list, function(x) get(x)[,outcome_id:=as.character(outcome_id)])
  med <- Reduce(mymerge, mget(unlist(med_feature_list)))


  ##############################################################################
  ### merge with cohort file - empty records -> 0
  #med[,outcome_id:=as.character(outcome_id)]
  #cohort[,outcome_id:=as.character(outcome_id)]
  med <- med[cohort, mget(names(med)), on=c("outcome_id", "empi", "pred_date")]

  non_days_to_last_var <- setdiff(names(med),grep("days_to_last", names(med),value=T))

  set_na_zero(med, subset_col=non_days_to_last_var)


  ##############################################################################
  ### categorise variables to ensure proper treatment in models -- integer 
  med_integer <- med[, mget(setdiff(names(med), c("outcome_id", "pred_date", "empi")))]
  med_integer[, names(med_integer):=lapply(.SD, function(x) as.integer(x))]

  med <- cbind(med[, mget(c("outcome_id", "pred_date", "empi"))], med_integer)

  med[, ':='(med_time_min=time_min, med_time_max=time_max)]

  med[, grep("med_id$", names(med), value=T):=NULL]

  feature_var_format_2(med)


  ##############################################################################
  ### return med features & delete key files creted in function
  rm(med_integer)
  rm(list=unlist(med_feature_list))

  return (med)


}
################################################################################
##################################  END  #######################################
################################################################################
