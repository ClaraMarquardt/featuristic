#----------------------------------------------------------------------------#

#' @title Generate DFCI & RPDR (combined) medication and chemotherapy-related 
#' features (med_oncdrs & med & chemo_oncdrs data).
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param cohort
#' @param cohort_key_var_merge
#' @return
#' @examples


 med_chemo_oncdrs_rpdr_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var) {

  print("launching med_chemo_oncdrs_rpdr_feature_gen")
  
  ##############################################################################
  ### load the dia_feature code
  med_chemo_oncdrs_rpdr <- med_feature_gen(cohort, cohort_key_var_merge, cohort_key_var, 
  	med_oncdrs_file_mod, leak_med_day, 
  	combine=TRUE, med_file_mod, chemo_oncdrs_file_mod)

  ### return
  return(med_chemo_oncdrs_rpdr)

}


#----------------------------------------------------------------------------#

