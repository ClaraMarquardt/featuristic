#----------------------------------------------------------------------------#

#' @title Format features. 
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param dt_list 
#' @param function_ext_1
#' @param function_ext_2
#' @return
#' @examples

feature_var_format <- function(dt_list, function_ext_1="_length_", 
  function_ext_2="_function_") {
  
  lapply(dt_list, function(x)  if(nrow(x)>1) {setnames(x, grep(paste0("time_diff", function_ext_2), 
    names(x), value=T), paste0(grep(paste0("time_diff", function_ext_2), names(x), 
    value=T), "_days_to_last"))})

  lapply(dt_list, function(x)  if(nrow(x)>1) {setnames(x, gsub(paste0("time_diff", 
     "(", function_ext_1, "|", function_ext_2,")"), "", names(x)))})

  return(dt_list)
  
}

#----------------------------------------------------------------------------#
