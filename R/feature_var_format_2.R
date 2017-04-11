#----------------------------------------------------------------------------#

#' @title Format features. 
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param dt 
#' @param timeframe_split_var 
#' @return
#' @examples

feature_var_format_2 <- function(dt, timeframe_split_var=time_split) {

  day_count_var <- grep("days_to_last", names(dt), value=T)

    if (timeframe_split_var==FALSE) {
    
    dt[, c(day_count_var):=lapply(.SD, function(x) as.numeric(x)), 
      .SDcols=day_count_var]

    set_na_zero(dt, replace=NA, subset_col=day_count_var)

    } else if (timeframe_split_var==TRUE) {

     dt[, c(day_count_var):=NULL]

  } 

}

#----------------------------------------------------------------------------#

