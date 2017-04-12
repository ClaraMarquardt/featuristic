#----------------------------------------------------------------------------#

#' @title Split data into timeframes. 
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param DT_list 
#' @param colname 
#' @return
#' @examples

timeframe_split <- function(DT_list, colname) {

  lapply(DT_list, function(x) {
    timeframe_comb_list <- list()
    timeframe_comb_list[[1]] <- get(x,sys.frame(
        sys.parent(n=3)))[get(colname) >= get(paste0("pred_date_beg", 
          name_ext[length(name_ext)])) & get(colname) <=pred_date]
    timeframe_comb_list[[2]] <- get(x,sys.frame(
        sys.parent(n=3)))[get(colname) >= get(paste0("pred_date_beg", 
          name_ext[1])) & get(colname) <=pred_date]

    if(length(name_ext)>1) {
      for (i in 2:length(name_ext)) {
        timeframe_comb_list[[i+1]] <- get(x,sys.frame(
          sys.parent(n=3)))[get(colname) >= get(paste0("pred_date_beg", name_ext[i])) & 
          get(colname) < get(paste0("pred_date_beg", name_ext[i-1]))]
      }
    }

    names(timeframe_comb_list) <- c("timeframe_comb_max",paste0("timeframe_comb", name_ext))

    assign(paste0(x, "_timeframe_comb"),timeframe_comb_list, 
      envir = sys.frame(sys.parent(n=3)))
  })
}

#----------------------------------------------------------------------------#



