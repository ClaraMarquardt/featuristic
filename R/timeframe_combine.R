#----------------------------------------------------------------------------#

#' @title Combine data split into different timeframes. 
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param DT_list 
#' @return
#' @examples


timeframe_combine <- function(DT_list) {

  lapply(DT_list, function(x) {
   assign(x, (Reduce(function(...) merge(..., all = TRUE, 
    by=cohort_key_var), get(x, sys.frame(sys.parent(n=3))))), sys.frame(sys.parent(n=3)))
    # assign(x, as.data.table(merge(get(x,sys.frame(sys.parent(n=3)))$timeframe_short, 
    #   get(x, sys.frame(sys.parent(n=3)))$timeframe_long, 
    #   all = TRUE, by=cohort_key_var)), sys.frame(sys.parent(n=3)))
  })     
}

#----------------------------------------------------------------------------#

