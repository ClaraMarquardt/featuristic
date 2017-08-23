#----------------------------------------------------------------------------#

#' @title Collapse features into a category-level tabulation. 
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


feature_coll <- function(dt, cohort_key_var_list=cohort_key_var, 
  cohort_extra_var_list=names(cohort_extra_col)) {

  dt_temp <- dt[, mget(setdiff(names(dt), union(cohort_key_var_list, 
    cohort_extra_var_list)))]

  temp <- data.table(var_cat=names(dt_temp))

  temp[var_cat %like% "timeframe" | var_cat %like% paste0(name_ext, collapse="|") | var_cat %like% paste0(name_ext_extended, collapse="|"), var_type:="time"]
  temp[var_cat %like% "timeframe_diff" | var_cat %like% "diff$", var_type:="diff"]
  temp[is.na(var_type), var_type:="static"]

  temp[, var_cat:=gsub("^var_", "", var_cat)]
  temp[, var_source:=gsub("([^_]*)_.*", "\\1", var_cat)]
 
  temp[, var_cat:=gsub("(.*)\\.\\..*", "\\1", var_cat)]
  temp[, var_cat:=gsub("([^_]*)_(.*)", "\\2", var_cat)]

  temp[, var_count:=.N, by=c("var_cat")]


  temp[,var_type:=do.call(paste0, list(unique(var_type),collapse=";")), by=c("var_cat")]

  temp <- unique(temp, by=c("var_cat"))
  temp <- rbindlist(list(temp, data.table(var_count=sum(temp$var_count), 
    var_cat="all features")), use.names=T, fill=T)

  return(temp)

}

#----------------------------------------------------------------------------#
