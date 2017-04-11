#----------------------------------------------------------------------------#

#' @title Generate DFCI & RPDR (combined) diagnosis-related features (dia_oncdrs & dia data).
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param 
#' @return
#' @examples

 dia_oncdrs_rpdr_feature_gen <- function() {

  ##############################################################################
  ### load the dia_feature code
  source(dia_feature_code)
  dia_oncdrs_rpdr <- dia_feature_gen(dia_oncdrs_file_mod, 
  	leak_dia_day, combine=TRUE, dia_file_mod)

  ### return
  return(dia_oncdrs_rpdr)

}


#----------------------------------------------------------------------------#



