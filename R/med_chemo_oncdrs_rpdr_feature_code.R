#----------------------------------------------------------------------------#

#' @title Generate DFCI & RPDR (combined) medication and chemotherapy-related 
#' features (med_oncdrs & med & chemo_oncdrs data).
#'
#' @description \
#'
#' @export
#' @import data.table
#' @return
#' @examples


 med_chemo_oncdrs_rpdr_feature_gen <- function() {

  
  ##############################################################################
  ### load the dia_feature code
  source(med_feature_code)
  med_chemo_oncdrs_rpdr <- med_feature_gen(med_oncdrs_file_mod, leak_med_day, 
  	combine=TRUE, med_file_mod, chemo_oncdrs_file_mod)

  ### return
  return(med_chemo_oncdrs_rpdr)

}


#----------------------------------------------------------------------------#

