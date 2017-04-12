#----------------------------------------------------------------------------#

#' @title Generate microbiology-related features (mic data).
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param file_date_var
#' @param cohort
#' @param cohort_key_var_merge
#' @return
#' @examples

mic_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var, file_date_var="mic_date") {
  
  print("launching mic_feature_gen")
  
  ##############################################################################
  ### Load the  modified/pre-processed mic file for the specified data sample -- 
  ### if no such file exists - excute the function_mic_class.R code (access/submit as 
  ### batchmode job using machine/function_class_batchmode.txt)
    
  # (a) load the stored code - return error message if file does not exist
  tryCatch(mic <- readRDS_merge(mic_file_mod), warning=function(w)
    print("no classified mic file available for the data sample"))
    # XXX NOTE: RDS file preserves formatting of empi as character

  # subset to smaller sample for testing if so specified in control file
  if (test_raw_file==TRUE) {
    store_shorten_file("mic")
  }

  # (c) subset to the variables which are to be employed in the 
  # feature construction process -- drop potential features (mic_time)
  mic[, c("mic_time"):=NULL]
  
  mic <- mic[result=="positive" & result_tier %in% c("tier_1", 
    "tier_2")]

  # (d) specify how to treat uncertain result_cat - for tier 1 results 
  # where taxise package returns multiple matches - forced to use first match 
  # -- (a) drop these results - i.e. re-categorise as "non_classifiable_organism"
  # -- (b) keep the result_cat as it is - not sensible (?) as thereby create 
  # separatefeature for candida and candida uncertain where uncertainty is NOT a feature 
  # of the 'state of health' that we are aiming to model
  # -- (c) ignore uncertainty and group e.g. candida (uncertain_result) with candida
  # XXX TO-DO: Decide how to better deal with these cases/ reduce such cases
  mic[org_genus %like% "(uncertain_result)", 
    org_genus:=gsub("_\\(uncertain_result\\)", "", org_genus)]


  ##############################################################################
  ### merge microbiology file with cohort (cohort_key_variables) & format dates
  # XXX NOTE: foverlaps - ensures that all microbiology max timeframe_long days 
  # prior to outcome date
  mic <- mic[empi %in% cohort$empi]

  invisible(parse_date(mic, c("mic_date")))

  mic[, c("mic_date_1","mic_date_2"):=.(mic_date)]

  mic <-foverlaps(mic, cohort[, mget(cohort_key_var_merge)], by.x=c("empi",
    "mic_date_1" , "mic_date_2"), nomatch=0)

  mic[, time_diff:=as.numeric(difftime(pred_date, get(file_date_var), 
    units="days"))]

  ### implement leakage control (as specified in control file - 
  ### omit day of outcome/days pre outcome)
  if (!is.na(leak_mic_day)) {
    mic <- mic[!(pred_date-mic_date_1<=leak_mic_day)]
  }

  ##############################################################################
  ### subsetting & dividing into smaller DT based on timeframe (ST/LT) - 
  ### return as list (...timeframe_comb)
  invisible(timeframe_split(list("mic"), "mic_date"))

  name_ext_extended <- name_ext_extended[sapply(mic_timeframe_comb, nrow)!=0]
  name_ext <- name_ext_extended[2:length(name_ext_extended)]
  mic_timeframe_comb <- mic_timeframe_comb[sapply(mic_timeframe_comb, nrow)!=0]
  
  time_min <- min(do.call("c", lapply(mic_timeframe_comb, function(x) as.Date(min(x[, 
    mic_date]), "%Y-%m-%d"))))
  time_max <- max(do.call("c", lapply(mic_timeframe_comb, function(x) as.Date(max(x[, 
    mic_date]), "%Y-%m-%d"))))


  ##############################################################################
  ### reshaping - create microbiology features - highest hierarchy - i.e. tier1/tier 2 
  ### positive results - count variable 
  # XXX NOTE: microbiology count = simple count of number of occurences of positive result 
  # over time period in question -- focus only on tier 1 and tier 2 results
  mic_result_timeframe_comb <- lapply(mic_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + pred_date ~  result + 
    paste0("mic.", result_tier), 
    fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff"))
  
  mic_result_timeframe_comb <- feature_var_format(mic_result_timeframe_comb)

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("positive", 
    names(DT), value=T), paste0("mic_mic.pos.res_mic.blood.urine_", gsub("_", ".", 
    gsub("positive_", "",grep("positive", names(DT), value=T))), paste0("_mic.count.", 
    gsub("_", ".", gsub(".*(tier_[1-2]).*", "\\1", grep("positive", names(DT), value=T))), "..any"), 
    name_ext)), DT=mic_result_timeframe_comb , name_ext_extended))

  inv_lapply(mic_result_timeframe_comb, function(x) setnames(x, gsub("days\\.to\\.last_(mic\\.count\\.tier\\.[0-9]\\.\\.any_)(.*$)", 
    "\\1days_to_last_\\2", names(x))))

  ##############################################################################
  ### reshaping - create microbiology features - lower hierarchy - i.e. tier1/tier 2 
  ### positive results by category - count variable 
  # XXX NOTE: microbiology count = simple count of number of occurences of positive result of each cat
  # over time period in question -- focus only on tier 1 and tier 2 results
  # XXX TO-DO: Decide whether to keep or drop (i.e. how to deal with "non_classifiable organism" here)
  mic_result_cat_timeframe_comb <- lapply(mic_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + pred_date ~  result + 
    paste0("mic.", result_tier) + org_genus, 
    fun.aggregate=list(length, function(x) min(x, na.rm=T)), value.var = "time_diff"))
  # mic_result_cat_timeframe_comb <- lapply(mic_result_cat_timeframe_comb, function(x)
  #   x[,grep("invalid/ambigious|negative|tier_other", names(x), value=T):=NULL])

  mic_result_cat_timeframe_comb <- feature_var_format(mic_result_cat_timeframe_comb)

  # XXX TO-DO: Fix naming rules 
  invisible(mapply(function(DT,name_ext) setnames(DT, grep("positive", 
    names(DT), value=T), paste0("mic_mic.pos.res_mic.blood.urine_", 
    gsub("([1-2])\\_", "\\1_mic.cat.count.tier.\\1..", 
    gsub("_([1-2])", ".\\1", gsub("positive_", "", grep("positive", names(DT), value=T)))), 
    name_ext)), 
    DT=mic_result_cat_timeframe_comb , name_ext_extended))

  ##############################################################################
  ### reshaping - create microbiology features - resistnat tier 1 infections (i.e. 
  #### resisitant to at least one antibiotic)
  mic_result_resistant_timeframe_comb <- lapply(mic_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + pred_date ~  paste0("resistant", 
    tier_1_anti_resis), length, subset=.(result_tier=="tier_1" & result=="positive" & 
    tier_1_anti_resis==1)))

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("resistant", 
    names(DT), value=T), paste0("mic_mic.pos.res_mic.blood.urine_mic.tier.1_mic.resistant.count.tier.1..", 
    gsub("[1-2]", "", grep("resistant", names(DT), value=T)), name_ext)), 
    DT=mic_result_resistant_timeframe_comb , name_ext_extended))

  ##############################################################################
  ### reshaping - create microbiology features - resistnat tier 1 infections --
  ### number of infections resistant to -- count only results for which resistant to at least one antibiotic
  mic_result_resistant_count_timeframe_comb <- lapply(mic_timeframe_comb, function(x) 
    dcast.data.table(x, outcome_id + empi + pred_date ~  paste0("resistant_", 
    tier_1_anti_resis_count), length, subset=.(result_tier=="tier_1" & result=="positive" & 
    tier_1_anti_resis_count>0)))
  
  invisible(mapply(function(DT,name_ext) setnames(DT, grep("resistant", 
    names(DT), value=T), paste0("mic_mic.pos.res_mic.blood.urine_mic.tier.1_mic.resistant.anti.count.tier.1..", 
    grep("resistant", names(DT), value=T), name_ext)), 
    DT=mic_result_resistant_count_timeframe_comb , name_ext_extended))

  ##############################################################################
  ### reshaping - create microbiology features - resistnat tier 1 infections --
  ### resistant to...
  mic_result_resistant_anti_timeframe_comb <- copy(mic_timeframe_comb)
  mic_result_resistant_anti_timeframe_comb <- lapply(mic_result_resistant_anti_timeframe_comb, 
    function(x) unique(x[, grep("anti_res_", names(x), value=T):=(lapply(.SD, function(y) sum(y, na.rm=T))), 
    .SDcols=grep("anti_res_", names(x), value=T), by=c("outcome_id", "empi", "pred_date")][, mget(
    c(grep("anti_res_", names(x), value=T), "outcome_id", "empi", "pred_date"))], 
    by=c("outcome_id", "empi", "pred_date")))

  invisible(mapply(function(DT,name_ext) setnames(DT, grep("anti_res_", 
    names(DT), value=T), 
    paste0("mic_mic.pos.res_mic.blood.urine_mic.tier.1_mic.resistant.anti.name.tier.1..", 
    grep("anti_res_", names(DT), value=T), name_ext)), 
    DT=mic_result_resistant_anti_timeframe_comb , name_ext_extended))

  # mic_result_resistant_anti_timeframe_comb <- lapply(mic_timeframe_comb, function(x)
  #   melt(x, id.vars =c("outcome_id", "empi", "pred_date", "result_tier", "result", 
  #     "tier_1_anti_resis_count"), measure=patterns("^anti_res_.*"), 
  #     variable.name="anti_res_name", value.name="anti_res"))

  # ## shorten
  # lapply(mic_result_resistant_anti_timeframe_comb, function(x) shorten_label(x, 
  #   list("anti_res1")))

  # mic_result_resistant_anti_timeframe_comb <- lapply(mic_result_resistant_anti_timeframe_comb, 
  #   function(x) dcast.data.table(x, outcome_id + empi + pred_date ~   paste0("anti_",
  #   anti_res1), length, subset=.(result_tier=="tier_1" & result=="positive" & 
  #   !is.na(anti_res1))))

  # invisible(mapply(function(DT,name_ext) setnames(DT, grep("anti", 
  #   names(DT), value=T), paste0("mic_mic.pos.res_mic.blood.urine_mic.tier.1_mic.resistant.anti.name.tier.1..", 
  #   grep("anti", names(DT), value=T), name_ext)), 
  #   DT=mic_result_resistant_anti_timeframe_comb , name_ext_extended))

  ##############################################################################
  ### merge mic feature files
  mic_feature_list <- list("mic_result_timeframe_comb",
    "mic_result_cat_timeframe_comb", "mic_result_resistant_count_timeframe_comb",
    "mic_result_resistant_timeframe_comb", 
    "mic_result_resistant_anti_timeframe_comb")

  timeframe_combine(mic_feature_list)

  mic <- Reduce(mymerge, mget(unlist(mic_feature_list)))

  ##############################################################################
  ### merge with cohort file - empty records -> 0
  mic <- mic[cohort, mget(names(mic)), on=c("outcome_id", "empi", "pred_date")]

  non_days_to_last_var <- setdiff(names(mic),grep("days_to_last", names(mic),value=T))
  set_na_zero(mic, subset_col=non_days_to_last_var)

  ##############################################################################
  ### categorise variables to ensure proper treatment in models -- integer 
  mic_integer <- mic[, mget(setdiff(names(mic), c("outcome_id", "pred_date", "empi")))]
  mic_integer[, names(mic_integer):=lapply(.SD, function(x) as.integer(x))]

  mic <- cbind(mic[, mget(c("outcome_id", "pred_date", "empi"))], mic_integer)

  mic[, ':='(mic_time_min=time_min, mic_time_max=time_max)]

  mic[, grep("mic_id$", names(mic), value=T):=NULL]

  feature_var_format_2(mic)

  ##############################################################################
  ### return mic features & delete key files creted in function
  rm(mic_integer)
  rm(mic_timeframe_comb)
  rm(list=unlist(mic_feature_list))

  return (mic)

}

#----------------------------------------------------------------------------#
