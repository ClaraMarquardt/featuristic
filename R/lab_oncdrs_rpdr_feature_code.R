#----------------------------------------------------------------------------#

#' @title Generate DFCI & RPDR (combined) lab-related features (lab_oncdrs & lab data).
#'
#' @description \
#'
#' @export
#' @import data.table
#' @return
#' @examples


lab_oncdrs_rpdr_feature_gen <- function() {
  
  ##############################################################################
  ### load the dia_feature code
  source(lab_feature_code)
  lab_oncdrs_rpdr <- lab_feature_gen(lab_oncdrs_file_mod, leak_lab_day,
  	combine=TRUE, lab_file_mod)

  ### return
  return(lab_oncdrs_rpdr)


}

#----------------------------------------------------------------------------#

