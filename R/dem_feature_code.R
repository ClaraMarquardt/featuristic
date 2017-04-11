#----------------------------------------------------------------------------#

#' @title Generate demographics-related features (dem data).
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param 
#' @return
#' @examples

dem_feature_gen <- function() {
 
  ##############################################################################
  ### load the raw file & load requisite helpers

  ## helpers
  required_helpers <- c(
     "zip_class",
     "flu_data_cdc"
  )
  # load_helpers(required_helpers)

  ## raw file
  dem <- readRDS_merge(dem_file_mod)
  dem <- unique(dem, by=c("empi"))

  # subset to smaller sample for testing if so specified in control file
  if (test_raw_file==TRUE) {
    store_shorten_file("dem")
  }


  ##############################################################################
  ### merge demographics file with cohort (cohort_key_var_merge) & format/rename 
  ### dates & report number of patients with no complete dem information 
  # XXX NOTE: multiple rows per patient - to allow for the incorperation of time 
  # dependent data (age, flu data...)
  # XXX NOTE: remove missing observations, cohort observations with no dem record (empi)
  dem <- dem[cohort, as.list(c(mget(setdiff(names(dem), "empi")),mget(cohort_key_var_merge))), 
    on=c("empi"), nomatch=0]

  print(sprintf("number of cohort observations with no dem record: %d, 
  	number of patients with no dem record: %d, number of cohort observations with a dem_record -
    this should be the final number of rows:%d", nrow(cohort[!empi %in% dem$empi]),
  	nrow(unique(cohort[!empi %in% dem$empi], by=c("empi"))),
    nrow(cohort[empi %in% dem$empi])))

  # XXX NOTE: Always check date output
  invisible(parse_date(dem, c("date_of_birth", "date_of_death"), 
    c("dob", "dod")))

  ##############################################################################
  ##############################################################################

  # age
  dem[,age:=difftime(pred_date,dob, units="days")/365]
  
  # impose variable categors (dem_dem.basic..//dem_dem.basic_race..)
  setnames(dem, setdiff(names(dem), cohort_key_var_merge),
    paste0("dem_dem.basic..", setdiff(names(dem), cohort_key_var_merge)))
  setnames(dem, grep("race.", names(dem), value=T), gsub("\\.\\.", "_race..",
    grep("race.", names(dem), value=T)))
  
  ##############################################################################
  ##############################################################################
  ### create demographic features "external" - e.g. infections over time 
  ### (dem_dem.ext..)

	# (a) generate  time frame influenza levels 
	# at the state level (mean of activity levels - dummies)
	# XXX TO-DO: Identify a more complete source of influenza data (even if at a 
	# less granular level & explore the getflu data - at the HHS regional level)

	# # merge the flu_data_cdc with the dem file (foverlap)
	# flu <- flu_data_cdc[state %in% dem$dem_dem.basic..state]

	# # XXX NOTE: Always check date output
	# invisible(parse_date(flu, c("flu_date")))

	# flu[, c("flu_date_1","flu_date_2"):=.(flu_date)]

	# setkeyv(dem, c("dem_dem.basic..state", paste0("pred_date_beg", name_ext[length(name_ext)]), "pred_date"))

 #  flu <- foverlaps(flu,dem[,mget(c(cohort_key_var_merge, "dem_dem.basic..state", 
 #    "dem_dem.basic..city"))], by.x=c("state", "flu_date_1","flu_date_2"), nomatch=0)

 #  ### implement leakage control (as specified in control file - 
 #  ### omit day of outcome/days pre outcome)
 #  if (!is.na(leak_dem_day)) {
 #    flu <- flu[!(pred_date-flu_date_1<=leak_dem_day)]
 #  }

 #  ##############################################################################
 #  ### subsetting & dividing into smaller DT based on timeframe (ST/LT) - > 
 #  ## return ... timeframe_comb

 #  timeframe_split(list("flu"), "flu_date")

 #  name_ext_extended <- name_ext_extended[sapply(flu_timeframe_comb, nrow)!=0]
 #  name_ext <- name_ext_extended[2:length(name_ext_extended)]
 #  flu_timeframe_comb <- flu_timeframe_comb[sapply(flu_timeframe_comb, nrow)!=0]

 #  time_min <- min(do.call("c", lapply(flu_timeframe_comb, function(x) as.Date(min(x[, 
 #    flu_date]), "%Y-%m-%d"))))
 #  time_max <- max(do.call("c", lapply(flu_timeframe_comb, function(x) as.Date(max(x[, 
 #    flu_date]), "%Y-%m-%d"))))

 #  ##############################################################################
 #  ## generate features -- sum and collapse at the outcome_id-timeframe level - 
 #  ## generate mean activity levels & setnames/impose hierarchy
 #  flu_timeframe_comb <- lapply(flu_timeframe_comb, function(x) x[, mean_activity_level:=
 #    as.list(colMeans(.SD)),by=c("outcome_id", "empi", "pred_date"), 
 #    .SDcols=c("activity_level")][, setdiff(names(x), c("outcome_id", "empi", 
 #      "pred_date",  "mean_activity_level")):=NULL])

 #  flu_timeframe_comb <- lapply(flu_timeframe_comb, function(x) unique(x,
 #    by=c("outcome_id", "empi", "pred_date")))

 #  invisible(mapply(function(DT, name_ext) setnames(DT, "mean_activity_level", 
 #    paste0("dem_dem.ext_dem.flu..flu_activity", name_ext)), DT=flu_timeframe_comb, 
 #    name_ext=name_ext_extended))


 #  ##############################################################################
 #  ### collapse flu_timeframe_comb & combine with dem static file
 #  # collapse flu_timeframe_comb
 #  flu_feature_list <- list("flu_timeframe_comb")


 #  timeframe_combine(flu_feature_list)
  
 #  # merge the flu_timeframe_comb (dynamic dem data) with the dem file (static dem data)
 #  dem <- Reduce(mymerge, mget(c("flu_timeframe_comb", "dem")))

 #  #############################################################################
 #  ## impute the flu activity levels where missing (i) impute by assuming that
 #  ## constant at the month, year level (ii) impute that constant at the 
 #  ## month level (across years) & round
 #  ## XXX TO-DO: Decide if imputation is a good idea & if strategy works 
  
 #  for (i in name_ext_extended) {
 #     dem[, c(paste0("dem_dem.ext_dem.flu..flu_activity", i)):=
 #      lapply(.SD, function(x)  ifelse(is.na(x), mean(x, na.rm=T), x)), 
 #      by=.(month(pred_date)), .SDcols=c(paste0("dem_dem.ext_dem.flu..flu_activity", i))]

 #     dem[, c(paste0("dem_dem.ext_dem.flu..flu_activity", i)):=
 #      lapply(.SD, function(x) round(x, digits=0)), .SDcols=c(
 #      paste0("dem_dem.ext_dem.flu..flu_activity", i))]
 #  }

 #  ##############################################################################
 #  ### subset to the relevant columns
 #  cols <- c(grep("dem_", names(dem), value=T))

 #  dem[, setdiff(names(dem), c("empi", "pred_date", "outcome_id", 
 #    grep("dem_", names(dem), value=T))):=NULL]


  ##############################################################################
  ### subset to the relevant columns
  remove_var <- grep("dob$|dod$|vital_status$", names(dem), value=T)
  dem[, c(remove_var):=NULL]

  dem[, grep("dem_id", names(dem), value=T):=NULL]
  
  ##############################################################################
  ### categorise variables to ensure proper treatment in models -- integer & factor
  dem_factor <- dem[, mget(setdiff(grep("dem.basic", names(dem), value=T), 
    c("dem_dem.basic..age")))]
  dem_factor[, names(dem_factor):=lapply(.SD, function(x) as.factor(x))]

  # dem_numeric <- dem[, mget(c("dem_dem.basic..age",paste0("dem_dem.ext_dem.flu..flu_activity", 
  #   name_ext_extended)))]
  dem_numeric <- dem[, mget(c("dem_dem.basic..age"))]
  dem_numeric[, names(dem_numeric):=lapply(.SD, function(x) as.numeric(round(x, digits=2)))]

  dem <- cbind(dem[, mget(c("outcome_id", "pred_date", "empi"))], dem_factor, 
    dem_numeric)

  # dem[, ':='(dem_time_min=time_min, dem_time_max=time_max)]

  ##############################################################################
  ### return demographic file  & delete key files creted in function
  # rm(dem, dem_factor, dem_integer,flu_timeframe_comb, flu)

  print(sprintf("final number of rows: %d", nrow(dem)))

  rm(dem_factor, dem_numeric)
  return(dem)



}

#----------------------------------------------------------------------------#

