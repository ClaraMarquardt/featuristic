#----------------------------------------------------------------------------#

#' @title Verify the correct compilation of a feature set. 
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param dt 
#' @param cohort_key_var_list 
#' @param cohort_extra_var_list 
#' @return
#' @examples


obs_check <- function(dt, cohort_key_var_list=cohort_key_var, 
  cohort_extra_var_list=names(cohort_extra_col)) {

  timepattern_1 <- paste0(paste0(name_ext_extended, "$"), collapse="|")
  timepattern_2 <- paste0(paste0(name_ext_name_extended, "$"), collapse="|")
  timepattern_3 <- "_days_to_last_.*|timeframe.*$"
  timepattern   <- paste(timepattern_1, timepattern_2, timepattern_3, sep="|")

  timepattern_diff_1 <- paste0("(", "(", paste0(name_ext_extended,collapse="|"), ")", "(", 
    paste0(name_ext_extended,collapse="|"), ")", "_diff$", ")")
  timepattern_diff_2 <- paste0("(", "(", paste0(name_ext_name_extended,collapse="|"), ")", "(", 
    paste0(name_ext_name_extended,collapse="|"), ")", "_diff$", ")")
  timepattern_diff_3 <- "_max_diff$|(timeframe_diff_.*$)"
  timepattern_diff <- paste(timepattern_diff_1, timepattern_diff_2, timepattern_diff_3, sep="|")

  timepattern_comb <- paste(timepattern, timepattern_diff, sep="|")

  ps("number of rows: %f, number of complete cases: %f, numer of complete cases as a perc of all rows: %f, number of columns: %d, number of features: %d, number of features excl. timeframe max: %d, number of unique features exl.timeframe max: %d", 
    nrow(dt), 
    nrow(dt[complete.cases(dt[, mget(setdiff(names(dt),
      c(cohort_key_var_list, cohort_extra_var_list) ))])]),
    nrow(dt[complete.cases(dt[, mget(setdiff(names(dt),
      c(cohort_key_var_list, cohort_extra_var_list)))])])/nrow(dt)*100, 
    ncol(dt),
    ncol(dt[, mget(setdiff(names(dt),c(cohort_key_var_list, cohort_extra_var_list)))]),
    ncol(dt[, mget(setdiff(names(dt),c(cohort_key_var_list, cohort_extra_var_list, 
      grep("_max$|_max_diff$", names(dt),value=T))))]), 
    length(unique(gsub("_$", "", gsub(timepattern_comb, "", names(dt[, mget(setdiff(names(dt),c(cohort_key_var_list, 
      cohort_extra_var_list, grep("_max$|_max_diff$",names(dt),value=T))))]))))))

}

#----------------------------------------------------------------------------#
